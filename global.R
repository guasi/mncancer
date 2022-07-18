library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)

# MN CANCER DATA----------------------------------------------------------
# From REST API https://mndatarest.web.health.state.mn.us/rest/cancer
dat <- readRDS("data/cancer.rds")$content

dat <- dat %>% 
  filter(!suppress) %>% #filter suppress = TRUE (count = "*")
  mutate(count = as.numeric(count),
         rate  = gsub(" \\(UR\\)", "", rate),
         rate  = as.numeric(rate),
         type  = gsub("Breast","Female Breast",type),
         fips  = substr(fips,3,5))

temp <- dat %>%  # duplicate type = breast & sex = female, to sex = all
  filter(type == "Female Breast") %>% 
  mutate(sex = "All")
dat <- rbind(dat, temp)
rm(temp)

dat_control_vars  <- c(location   = "Location",
                      type        = "Indicator",
                      year        = "Year",
                      age         = "Age Group",
                      sex         = "Sex")

map_control_vars  <- dat_control_vars[c(2,5)]
plot_control_vars <- dat_control_vars[1:2] 


# MAP DATA ---------------------------------------------------
shapefile <- "data/map/bdry_mn_county_open_data_status.shp"
mn_map    <- sf::st_read(shapefile) %>% 
             sf::st_as_sf() %>% 
             sf::st_transform(crs = "+proj=longlat +datum=WGS84")

# THEME -----------------------------------------------------
td_theme  <- bslib::bs_theme(version = 5,
                            bootswatch = "cerulean",
                            font_scale = .8,
                            "table-cell-padding-y" = ".2rem")

