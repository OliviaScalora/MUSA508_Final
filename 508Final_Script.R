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
tracts17 <- get_acs(geography = "tract", variables = c("B25026_001E","B19013_001E","B25058_001E",
                                                       "B06012_002E", "B25003_003E", "B25003_002E", "B25004_001E" ), 
                    year=2017, state=04, county=013, geometry=T,output='wide') %>% 
  st_transform('EPSG:2224')

opioid_data <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Fire_and_Medical_Opioid_Overdose_Incidents.csv")

student_dem <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Student_Demographics_-_Mesa_Public_Schools.csv")

UFB_dist <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/UFB_Food_Distribution_2021.csv")

zoning_district <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning Districts.geojson")

bus_ridership<- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Bus_Ridership.csv")

city_boundary<- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/City_Boundary.csv")

city_boundary <- st_as_sf(city_boundary, wkt = 'Geometry',
                        crs = 4326, agr = 'constant') %>%
  st_transform(st_crs(tracts17))

parcels<- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/City_Parcel_Polygons.csv")

parcels<- st_as_sf(parcels, wkt = 'Geometry',
                          crs = 4326, agr = 'constant') %>%
  st_transform(st_crs(tracts17))

ggplot()+ geom_sf(data=city_boundary,
                  color = 'red',
                  size = 1)+
            geom_sf(data=parcels)
 
fishnet <- 
  st_make_grid(city_boundary,
               cellsize = 500, 
               square = TRUE) %>%
  .[city_boundary] %>%            # <- MDH Added
  st_sf() %>%
  mutate(uniqueID = rownames(.))



glimpse(city_boundary)



city_boundary%>%ggplot()+
  geom_sf()
