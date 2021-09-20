library(tidyverse)
library(scales)
library(RPostgreSQL)
library(ggplot2)
library(xlsx)

rm(list=ls())

#1.  Attaching Old Growth Scores with Coordinate----
dbDisconnect(con)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host = "aspen", dbname = "tfl48")
summary(con)

# Retrieving VRI Reference Year from TFL48 Resultant 
reference_year <-
  dbGetQuery(con,"SELECT feature_id,polygon_id, reference_year,harvest_date FROM res_16june2021_tfl48") %>% 
  distinct()

# Connecting to plot database
dbDisconnect(con)
con <- dbConnect(drv, host = "aspen", dbname = "plot_db")
summary(con)

# Retrieving plot (with vri polygon attached) correcting VRI proj_age_1 with reference year
tfl48_vri_polygon <- 
  dbGetQuery(con,"SELECT DISTINCT tfl48_plot_polygon_id_glen.*, measurement_date
  FROM tfl48_plot_polygon_id_glen, plots, plot_measurements
  WHERE tfl48_plot_polygon_id_glen.plot_name = plots.plot_name AND
  plots.plot_id = plot_measurements.plot_id
  ORDER BY plot_name;") %>% # YSM causing duplicates, more than one measurement dates
  inner_join(reference_year, by = "polygon_id") %>%
  mutate(
    correction = 2021 - reference_year,
    proj_age_1 = proj_age_1 + correction,
    proj_age_1_glen_corrected = ifelse(!is.na(harv_yr), 
                                       ifelse(harv_yr <= 2021, 2021-harv_yr, curr_age),
                                       curr_age),
    # Flag and remove, plots that have been harvested
    harvested_plot = ifelse(!is.na(harv_yr) & harv_yr < as.numeric(str_sub(measurement_date, 1, 4)), 
                           "Harvested", 
                           "Not-harvested")
  )

# Filter Harvest Plots and writing to Postgres
harvested_plots <- 
  tfl48_vri_polygon %>% 
  filter(harvested_plot == "Harvested") %>% 
  select(plot_name,measurement_date,harv_yr,`feature_id.x`,polygon_id) %>% 
  mutate(
    harv_yr_source = "tfl48_plot_polygon_id_glen"
  )

dbWriteTable(con, "tfl48_harvested_plot_name", harvested_plots, overwrite = T, row.names = F)

# Reading in Crag's Seral Scores in
setwd("P:/canfor/201883_chetwynd_seral_stage/forestry/analysis/r")
data <- read_xlsx("Old Growth Scores.xlsx", sheet = "Old Growth Scores")

result_feature <- data %>% 
  inner_join(tfl48_vri_polygon, by = c("feature_id"= "feature_id.y")) %>% 
  mutate(uid = paste(utm_easting,utm_northing))

# # 2  ----
# 
# tfl48_vri_polygon <- 
#   dbGetQuery(con,"SELECT * FROM tfl48_vri_polygon_tree") %>% 
#   filter(utilization_id == 12.5) # Filtering to Utilization of 12.5
# 
# # Filtering to the latest measurement id 
# meas_id <- 
#   tfl48_vri_polygon %>% 
#   group_by(plot_id) %>% 
#   summarise(
#     measurement_id = max(measurement_id)
#   )
# 
# # Filtering to the latest years
# meas_yr <- 
#   tfl48_vri_polygon %>% 
#   group_by(plot_id) %>% 
#   summarise(
#     meas_yr = max(meas_yr)
#   )
# 
# # Inner Joining meas_yr and meas_id
# tfl48_vri_polygon <- 
#   tfl48_vri_polygon %>% 
#   inner_join(meas_id, by = c("plot_id","measurement_id")) %>% 
#   inner_join(meas_yr, by = c("plot_id","meas_yr"))
# 
# all_filter <- tfl48_vri_polygon %>% 
#   group_by(plot_name,feature_id) %>% 
#   mutate(spc_count = n()) %>%
#   ungroup() %>% 
#   group_by(feature_id,plot_name,species) %>% 
#   summarize(
#     species_per = n()/spc_count
#   ) %>% ungroup() %>% 
#   group_by(plot_name,feature_id) %>% 
#   filter(species_per == max(species_per)) %>% 
#   filter(row_number()==1) %>% ungroup()
#   
# ld_species_plots <- 
#   all_filter %>% inner_join(data, by = "feature_id") %>% 
#   distinct(feature_id,plot_name, .keep_all = T) %>% 
#   mutate(
#     site_index = round(as.numeric(site_index))
#   )

# Reading in Raw TFL48 data ----
setwd("P:/canfor/ke14033cfp_provincial_fibre_flow/gis/plot_data/PSP/plot_data/TFL48_plots")

raw_data <- "TFL48_PSP_VRI_codylai_2021aug03.xlsx"
raw_data <- read_xlsx(raw_data, sheet = "sample")%>%
  filter(util == "12.5") 

# Filter to last measurement_id
meas_id <- 
  raw_data %>% 
  group_by(SAMP_ID) %>% 
  summarise(
    no_meas = max(no_meas)
  )

# Filtering to the latest years
meas_yr <- 
  raw_data %>% 
  group_by(SAMP_ID) %>% 
  summarise(
    meas_yr = max(meas_yr)
  )

# Inner Joing to filter latest years and last measurement
rolled_up_join <- 
  raw_data %>% 
  inner_join(meas_id, by = c("SAMP_ID","no_meas")) %>% 
  inner_join(meas_yr, by = c("SAMP_ID","meas_yr")) %>% 
  #Selecting Only Data Needed
  select(tot_stand_age,SAMP_ID,meas_yr)

#Inner Join to plot data
total_age <- 
  result_feature %>%
  inner_join(rolled_up_join, by = c("plot_name"="SAMP_ID")) %>% 
  select(feature_id,`% Score`,polygon_id.x,proj_age_1.x,tot_stand_age,meas_no, harv_yr,meas_yr,proj_age_1_glen_corrected)%>% 
  filter(tot_stand_age > 0) %>% 
  mutate(
    proj_age_1 = as.numeric(proj_age_1.x),
    tot_stand_age = ifelse(!is.na(harv_yr), # Take OUT 
                           ifelse(harv_yr <= 2021, 99999, tot_stand_age),
                           tot_stand_age)
  ) %>% 
  filter(tot_stand_age != 99999)

ggplot(total_age , aes(x = tot_stand_age, y = proj_age_1_glen_corrected))+
  geom_point(size = 2)+
  geom_abline(intercept = 0, slope = 1, color = "red")

width = 50
center = 25

binned_age <- 
  total_age %>% 
  mutate(
    proj_age_1_glen_corrected = cut_width(proj_age_1_glen_corrected, width, center = center),
    tot_stand_age = cut_width(tot_stand_age, width, center = center))#

# binned_age$Plot_mean_bin <- factor(binned_age$VRI_bin,
#                                    levels = c( "[0,50]", "(50,100]", "(100,150]", "(150,200]", "(200,250]", "(250,300]"," (300,350]", "(400,450]"))

binned_age$proj_age_1_glen_corrected <- factor(binned_age$proj_age_1_glen_corrected,
                                   levels = c( "[0,50]", "(50,100]", "(100,150]", "(150,200]", "(200,250]", "(250,300]"," (300,350]", "(400,450]"))

confusionMatrix(binned_age$proj_age_1_glen_corrected,binned_age$tot_stand_age)

lmAge = lm(proj_age_1_glen_corrected~tot_stand_age + I(tot_stand_age^2), data = total_age)
qqnorm(lmAge$residuals)
qqline(lmAge$residuals)

lmAge = lm(proj_age_1_glen_corrected~tot_stand_age, data = total_age)
qqnorm(lmAge$residuals)
qqline(lmAge$residuals)


plot(total_age$proj_age_1_glen_corrected ~total_age$tot_stand_age)
abline(lmAge, col = "blue")
abline(a = 0, b = 1, col = "red")

plot(lmAge$residuals); abline(h = c(-100,100), col = "blue"); abline(h = 0, col = "red")

# Retrieving utm coord for these plots
check <- total_age %>% filter(proj_age_1_glen_corrected < 50 & tot_stand_age> 50) %>% 
  inner_join(result_feature, by = "feature_id")

check200_400 <- total_age %>% filter(proj_age_1 > 200 & tot_stand_age > 400) %>% 
  inner_join(result_feature, by = "feature_id")

setwd("P:/canfor/201883_chetwynd_seral_stage/forestry/analysis/r")

write.csv(check,"plot_harvested.csv")

# 
# TO DO 3Sept2021
# 
# - Recreate Plot Count By Current Age Class
#    - Switch VRI Current Age with Total Stand Age (from plot database)

# - Confirm total stand age -> Years To Breast Height
# - Change back the age of tree on the plot -> potentially drop later
# - Check what types of plots are being logged
# - Check out any outliers
# - Figure out what plots to drop
# GRAPHS
# - Graph --> Low Medium High Seral Score by ld_species
# - Graph -->  Low Medium High Seral Score by 5 m site index classes


