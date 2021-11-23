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
library(RColorBrewer)
library(ggthemes)

colors<-brewer.pal(9, 'BuPu')
options(scipen=999)
options(tigris_class = "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")
palette1 <- c('#DAD7CD','#A3B18A','#588157', '#3A5A40','#344E41')
color= '#F0563F'
color2 = '#a32714'
color3 = '#539470'
colors<-brewer.pal(9, 'BuPu')

#load data
tracts17 <- get_acs(geography = "tract", variables = c("B25026_001","B19013_001","B25058_001",
                                                       "B06012_002", "B25003_003", "B25003_002", "B25004_001" ), 
                    year=2017, state=04, county=013, geometry=T) %>% 
  st_transform('EPSG:2224')%>%
  filter(GEOID != '04013010102')%>%
  filter(GEOID != '04013941300')%>%
  filter(GEOID != '04013422308')%>%
  filter(GEOID != '04013422307')%>%
  filter(GEOID != '04013422508')%>%
  filter(GEOID != '04013422506')%>%
  filter(GEOID != '04013422622')%>%
  filter(GEOID != '04013318400')%>%
  filter(GEOID != '04013420100')%>%
  filter(GEOID != '04013810300')%>%
  filter(GEOID != '04013523104')%>%
  filter(GEOID != '04013420110')%>%
  filter(GEOID != '04013816900')%>%
  filter(GEOID != '04013815902')%>%
  filter(GEOID != '04013810800')

mesa_tracts17 <- tracts17[city_boundary,]%>%
                 dplyr::select( -NAME, -moe) %>%
                 spread(variable, estimate) %>%
                 dplyr::select(-geometry) %>%
                 rename(TotalPop = B25026_001, 
                        MedHHInc = B19013_001, 
                        MedRent = B25058_001,
                        TotalPoverty = B06012_002,
                        TotalRent = B25003_003,
                        TotalOwn = B25003_002,
                        VacantUnits = B25004_001) 

opioid_data <- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/qufy-tzv6.json")), 
                        coords = c("longitude", "latitude"), 
                        crs = 4326, agr = "constant")%>%
               st_transform(st_crs(tracts17))%>%filter(location.longitude != -112.225)%>%
               mutate(point = paste(opioid_data$location.latitude, opioid_data$location.longitude))

count <- count(opioid_data,point)

opioid_data <- st_join(opioid_data, count, all.x = all)
opioid_data <-opioid_data%>%dplyr::select(-point.x,-point.y)%>%
                rename(count = n)


  
count<- count(opioid_data,point)



opioid_data <- st_join(opioid_data, count)



student_dem <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Student_Demographics_-_Mesa_Public_Schools.csv")

UFB_dist <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/UFB_Food_Distribution_2021.csv")

zoning_district <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning%20Districts.geojson")

bus_ridership<- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Bus_Ridership.csv")

city_boundary<- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/City_Boundary.csv")

city_boundary <- st_as_sf(city_boundary, wkt = 'Geometry',
                        crs = 4326, agr = 'constant') %>%
  st_transform(st_crs(tracts17))

parcels<- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/City_Parcel_Polygons.csv")

parcels<- st_as_sf(parcels, wkt = 'Geometry',
                          crs = 4326, agr = 'constant') %>%
  st_transform(st_crs(tracts17))





mesa_tracts17 = st_filter(mesa_tracts17, city_boundary, sparse = TRUE, prepared = TRUE)
ggplot()+geom_sf(data = mesa_tracts17,
                  aes(fill = q5(TotalPoverty)),
                  color = "black")+
         scale_fill_brewer(palette=4)+
         geom_sf(data=city_boundary,
                 color = 'red',
                 size = 1,
                 fill = NA)


#geom_sf(data=zoning_district,
#        aes(fill = zoning),
#        color = NA)+



fishnet <- 
  st_make_grid(city_boundary,
               cellsize = 500, 
               square = TRUE) %>%
  .[city_boundary] %>%            # <- MDH Added
  st_sf() %>%
  mutate(uniqueID = rownames(.))


ggplot()+
  geom_sf(data=city_boundary,
          color = 'black',
          size = 1,
          fill = 'grey95')+
  geom_sf(data = opioid_data,
          aes(color = count, 
              size = count))+
  scale_color_distiller(palette=4, 
                     direction = 1,
                     guide =)+
  scale_color_continuous(breaks=seq(1, 73, by=10))+
  scale_size_continuous(breaks=seq(1, 73, by=10),
                        range = c(1,10))+
  guides(color= guide_legend(), size=guide_legend())+ 
  theme_classic()

#join overdoses to the fishnet
opioid_net <- 
  dplyr::select(opioid_data) %>% 
  mutate(countoverdose = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countoverdose = replace_na(countoverdose, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), size=nrow(fishnet), replace = TRUE))

ggplot() +
  geom_sf(data = opioid_net, aes(fill = countoverdose)) +
  scale_fill_viridis() +
  labs(title = "Count of Overdoses for the fishnet") +
  mapTheme()
