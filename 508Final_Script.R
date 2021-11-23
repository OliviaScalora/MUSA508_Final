#Setup -------------------

#load libraries
library(tidyverse)
library(sf)
library(RSocrata)
library(viridis)
library(spatstat)
library(raster)
library(spdep)
library(FNN)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)

options(scipen=999)
options(tigris_class = "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")
palette1 <- c('#DAD7CD','#A3B18A','#588157', '#3A5A40','#344E41')
color= '#F0563F'
color2 = '#a32714'
color3 = '#539470'

#load data

opioid_data <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Fire_and_Medical_Opioid_Overdose_Incidents.csv")

student_dem <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Student_Demographics_-_Mesa_Public_Schools.csv")

UFB_dist <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/UFB_Food_Distribution_2021.csv")

zoning_district <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning Districts.geojson")

bus_ridership<- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Bus_Ridership.csv")

city_boundary<- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/City_Boundary.csv",crs = 'EPSG:4326')
city_boundary <- city_boundary::st_as_sf( city_boundary, wkt = "geometry" )


glimpse(city_boundary)



city_boundary%>%ggplot()+
  geom_sf()
