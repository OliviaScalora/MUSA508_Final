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
library(sp)
library(rgeos)
library(maptools)

options(scipen=999)
options(tigris_class = "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

#Load Data ----------------

#census tracts
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
  filter(GEOID != '04013810800')%>%
  filter(GEOID != '04013815600')%>%
  filter(GEOID != '04013815200')%>%
  filter(GEOID != '04013319904')%>%
  filter(GEOID != '04013319402')%>%
  filter(GEOID != '04013319404')%>%
  filter(GEOID !="04013319906")


#City Boundary
city_boundary <- st_as_sf(st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/City_Boundary.csv"), 
                          wkt = 'Geometry', crs = 4326, agr = 'constant')%>%
                  st_transform(st_crs(tracts17))

#filter tracts within city boundary
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

# ggplot()+geom_sf(data = mesa_tracts17,
#                  aes(fill =TotalPop),
#                  color = "black")+
#   scale_fill_distiller(palette=4)+
#    geom_sf(data=city_boundary,
#           color = 'red',
#           size = 1,
#           fill = NA)


#Load opioid data - add count column to count amount of overdoses at each point at 1/3 mi interval grid

opioid_data <- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/qufy-tzv6.json")), 
                        coords = c("longitude", "latitude"), 
                        crs = 4326, agr = "constant")%>%
               st_transform(st_crs(tracts17))%>%filter(location.longitude != -112.225)
opioid_data <- opioid_data%>%mutate(point = paste(opioid_data$location.latitude, opioid_data$location.longitude))
count <- count(opioid_data,point)
opioid_data <- (st_join(opioid_data, count, all.x = all)%>%dplyr::select(-point.x,-point.y)%>%
  rename(count = n))
  


#city properties - parks
park_names<- c("Park Facilities","Park/Utility Facilities", "Parks/ Utility Facilities", "Parks",
  "Parks ", "Urban Garden - Lease Non Profit","Parsk/ Utility  Facilities", "Pocket Park",
  "Park/Public Safety", "Golf Course", "Citrus Grove")
parks<- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                 coords = c("longitude", "latitude"),
                 crs = 4326, agr = "constant")%>%
        st_transform(st_crs(tracts17))%>%
        filter(property_use %in% park_names)



#city properties - police and fire stations
police_fire<- c("Public Safety--Fire/Police", "Park/Public Safety")
mesa_police_fire<-st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                 coords = c("longitude", "latitude"),
                 crs = 4326, agr = "constant")%>%
                 st_transform(st_crs(tracts17))%>%
                 filter(property_use %in% police_fire)


#city properties - vacant lots
vacant_cat<- c("Vacant", "Vacant (ADOT remnant)")
vacant<- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                            coords = c("longitude", "latitude"),
                            crs = 4326, agr = "constant")%>%
             st_transform(st_crs(tracts17))%>%
             filter(property_use %in% vacant_cat)



#city properties - child crisis center
ccc<- "Child Crisis Center"
cccenter<- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                  coords = c("longitude", "latitude"),
                  crs = 4326, agr = "constant")%>%
  st_transform(st_crs(tracts17))%>%
  filter(property_use %in% ccc)


#city properties - arts and education centera
arts_edu_cat<- c("Mesa Arts Center","Museums","Libraries","Sequoia Charter School")
arts_edu<- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                    coords = c("longitude", "latitude"),
                    crs = 4326, agr = "constant")%>%
  st_transform(st_crs(tracts17))%>%
  filter(property_use %in% arts_edu_cat)



#city properties - public housing
pub_house_cat<- c("Housing", "Escobedo Housing", "NSP", "Residential Property")
public_housing<- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                    coords = c("longitude", "latitude"),
                    crs = 4326, agr = "constant")%>%
  st_transform(st_crs(tracts17))%>%
  filter(property_use %in% pub_house_cat)


#Zoning data
zoning_district <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning%20Districts.geojson")%>%
  st_transform(st_crs(tracts17))


#Residential
res<- c("RM-3","RM-2","RM-4","RS-15","RS-35","RS-43","RS-6","RS-90","RS-9","RSL-2.5","RSL-4.5","RSL-2.5","T4N","PC","T4NF")
Residential <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning%20Districts.geojson")%>%
  st_transform(st_crs(tracts17))%>%
  filter(zoning %in% res)


#Commercial
com<- c("GC","LC","NC","OC","RSL-4.0")
Commercial <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning%20Districts.geojson")%>%
  st_transform(st_crs(tracts17))%>%
  filter(zoning %in% com)


#Downtown
dt<- c("DC","DB-2","DB-1", "DR-2", "DR-3", "DR-1")
Downtown <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning%20Districts.geojson")%>%
  st_transform(st_crs(tracts17))%>%
  filter(zoning %in% dt)


#Industrial
ind<- c("LI", "HI", "AG", "AG")
Industrial <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning%20Districts.geojson")%>%
  st_transform(st_crs(tracts17))%>%
  filter(zoning %in% ind)



ggplot()+
  geom_sf(data=city_boundary,
          color = 'black',
          size = 1,
          fill = 'grey95')+
  geom_sf(data= Residential,
          color = NA,
          fill = 'blue')+
  geom_sf(data= Commercial,
          color = NA,
          fill = 'red')+
  geom_sf(data= Downtown,
          color = NA,
          fill = 'orange')+
  geom_sf(data= Industrial,
          color = NA,
          fill = 'yellow',
          alpha= .45)



#load parcel data - necessary?
#parcels<- st_as_sf((st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/City_Parcel_Polygons.csv")), wkt = 'Geometry',
#                    crs = 4326, agr = 'constant') %>%
#                    st_transform(st_crs(tracts17))



#make fishnet
fishnet <- 
  st_make_grid(city_boundary,
               cellsize = 1760, 
               square = TRUE) %>%
  .[city_boundary] %>%  
  st_sf() %>%
  mutate(uniqueID = rownames(.))


#myPalette <- colorRampPalette(rev(brewer.pal(11, "YlOrRd")))


#plot opioid overdoses 
ggplot()+
  geom_sf(data = mesa_tracts17,
          aes(fill = q5(TotalPop)),
          color = "black")+
  scale_fill_brewer(palette=1)+
  geom_sf(data = opioid_data,
          aes(size = count),
          color = 'red',
          alpha = .5)+
  scale_size_continuous(breaks=seq(1, 73, by=10),
                        range = c(1,10))+
 # scale_color_continuous(breaks=seq(1, 73, by=10))+
#  scale_colour_gradientn(colours = (colorRampPalette(rev(brewer.pal(9, "YlOrRd"))))(10),
#                         breaks=seq(1, 73, by=10))+
  guides(color= guide_legend(), 
         size= guide_legend(title = 'Overdose Incidents'),
         fill = guide_legend(title = 'Total Living Below Poverty'))+ 
  geom_sf(data=city_boundary,
          color = 'black',
          size = 1,
          fill = NA)+
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
  geom_sf(data = opioid_net, aes(fill = countoverdose), color= NA)+
  scale_fill_viridis(option = 'F',direction = -1) +
  geom_sf(data=city_boundary,color="black", fill=NA)+
  labs(title = "Count of Overdoses for the fishnet") +
  theme_classic()+theme(panel.backgroun = element_rect(fill = 'grey35'))

Industrial_group = poly2nb(Industrial, queen = TRUE)


?unionSpatialPolygons()
