#Setup -------------------
install.packages('units')
install.packages('geojsonio')
install.packages("data.table")
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
library(dplyr)
library(units)
library(geojsonio)
library(data.table)

options(scipen=999)
options(tigris_class = "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

census_api_key("d5e25f48aa48bf3f0766baab06d59402ea032067", overwrite = TRUE)

#load functions
create_regions <- function(data) {
  group <- rep(NA, length(data))
  group_val <- 0
  while(NA %in% group) {
    index <- min(which(is.na(group)))
    nb <- unlist(data[index])
    nb_value <- group[nb]
    is_na <- is.na(nb_value)
    if(sum(!is_na) != 0){
      prev_group <- nb_value[!is_na][1]
      group[index] <- prev_group
      group[nb[is_na]] <- prev_group
    } else {
      group_val <- group_val + 1
      group[index] <- group_val
      group[nb] <- group_val
    }
  }
  group
}


clusterSF <- function(sfpolys, thresh){
  dmat = st_distance(sfpolys)
  hc = hclust(as.dist(dmat>(thresh%>%set_units(ft))), method="single")
  groups = cutree(hc, h=0.5)
  d = st_sf(
    geom = do.call(c,
                   lapply(1:max(groups), function(g){
                     st_union(sfpolys[groups==g,])
                   })
    )
  )
  d$group = 1:nrow(d)
  d
}

mapTheme<- function(base_size = 12, title_size = 16) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = title_size,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.text.x = element_text(size = 14),
    legend.key = element_rect(fill=NA))
}


#----CENSUS TRACTS AND CITY BOUNDARY----

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

#census block groups
blockgroups17 <- get_acs(geography = "block group", variables = c("B25026_001","B19013_001","B25058_001",
                                                       "B06012_002", "B25003_003", "B25003_002", "B25004_001" ), 
                    year=2017, state=04, county=013, geometry=T) %>% 
  st_transform('EPSG:2224')

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

mesa_bg17 <- blockgroups17[city_boundary,]%>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B25026_001, 
         MedHHInc = B19013_001, 
         MedRent = B25058_001,
         TotalPoverty = B06012_002,
         TotalRent = B25003_003,
         TotalOwn = B25003_002,
         VacantUnits = B25004_001)%>% 
  mutate(area = st_area(geometry))%>%
  filter(GEOID != '040130101021')%>%
  filter(GEOID != '1040133184002')

#create census variables from block group centroids

mean(mesa_bg17$MedHHInc)




Low_HH_Inc<- st_centroid(Industrial)%>%mutate(Legend = "Low_inc")

ggplot()+
  geom_sf(data=mesa_tracts17, aes(fill = q5(MedHHInc)),color = 'red')+
  geom_sf(data=city_boundary, color = 'black', fill= NA)+mapTheme()

ggplot()+
  geom_sf(data=mesa_bg17, fill = 'grey', color = 'red')+
  geom_sf(data=city_boundary, color = 'black', fill= NA)+mapTheme()

#----OPIOID DATA----
opioid_data <- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/qufy-tzv6.json")), 
                        coords = c("longitude", "latitude"), 
                        crs = 4326, agr = "constant")%>%
               st_transform(st_crs(tracts17))%>%filter(location.longitude != -112.225)
opioid_data <- opioid_data%>%mutate(point = paste(opioid_data$location.latitude, opioid_data$location.longitude))

#count number of overdose occurances at each 1/3 mile interval point and join count column to opioid data set
count <- count(opioid_data,point)
opioid_data <- (st_join(opioid_data, count, all.x = all)%>%dplyr::select(-point.x,-point.y)%>%
  rename(count = n))

#filter points that are within city boundary
opioid_data <- opioid_data[city_boundary,]


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
  guides(color= guide_legend(), 
         size= guide_legend(title = 'Overdose Incidents'),
         fill = guide_legend(title = 'Total Living Below Poverty'))+ 
  geom_sf(data=city_boundary,
          color = 'black',
          size = 1,
          fill = NA)+mapTheme()

# point_list<- opioid_data%>%dplyr::select(count, geometry)%>%
#               mutate(point = as.character(geometry))


#kernel density plot
ggplot()+
  geom_sf(data=city_boundary, fill='#fff9f8', color='grey65')+
   stat_density2d(data= data.frame(st_coordinates(opioid_data)),
                  aes(X,Y, fill=..level..,alpha=..level..),
                  size=0.01, bins=40, geom='polygon')+
  scale_fill_viridis(option = 'F', direction = -1) +
  scale_alpha(range=c(0.00,0.35), guide='none')+
  geom_sf(data = opioid_data,
          aes(size = count),
          color = 'grey65',
          alpha = .25)+
  labs(fill = 'Density',
       size = 'Count at nearest\n 1/3 mi. interval')+
  scale_size_continuous(breaks=seq(1, 75, by=10),
                        range = c(1,10))+ mapTheme()

# test<-data.frame(st_coordinates(opioid_data))
#----UNUSED DATA SETS----

# city parcel shapes
# city_parcels <- st_as_sf(read.socrata("https://data.mesaaz.gov/resource/8mhi-jyh8.csv"),wkt = 'geometry', 
#                          crs = 4326, agr = "constant")%>%
#   st_transform(st_crs(tracts17))


# ####code doesnt work yet - still trouble shooting - i dont think we need these shapefiles anyways
# highway <- st_as_sf(read.socrata("https://data.mesaaz.gov/resource/y3aj-3i5y.csv"),wkt = 'Geometry', 
#                      crs = 4326, agr = "constant")%>%filter(SpeedLimit = '65')%>%dplyr::select('Full Street Name', Geometry, SpeedLimit)
#   st_transform(st_crs(tracts17))


#----CITY PROPERTIES----

#parks
parks<- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                 coords = c("longitude", "latitude"),
                 crs = 4326, agr = "constant")%>%
        st_transform(st_crs(tracts17))%>%
        filter(property_use %in%  
                 c("Park Facilities","Park/Utility Facilities", "Parks/ Utility Facilities", "Parks",
                   "Parks ", "Urban Garden - Lease Non Profit","Parsk/ Utility  Facilities", "Pocket Park",
                   "Park/Public Safety", "Golf Course", "Citrus Grove"))%>%
        mutate(ID = 1:n(), Legend = "Park")%>%
        dplyr::select(ID, Legend, geometry)


#police and fire stations
mesa_police_fire<-st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                            coords = c("longitude", "latitude"),
                            crs = 4326, agr = "constant")%>%
                 st_transform(st_crs(tracts17))%>%
                 filter(property_use %in% c("Public Safety--Fire/Police", "Park/Public Safety"))%>%
                 mutate(ID = 1:n(),Legend = "Police&Fire Station")%>%
                 dplyr::select(ID,Legend,geometry)


#vacant lots
vacant<- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                            coords = c("longitude", "latitude"),
                            crs = 4326, agr = "constant")%>%
         st_transform(st_crs(tracts17))%>%
         filter(property_use %in% c("Vacant", "Vacant (ADOT remnant)"))%>%
         mutate(ID = 1:n(), Legend = "Vacant Property")%>%
         dplyr::select(ID, Legend,geometry)


#child crisis center
cccenter<- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                  coords = c("longitude", "latitude"),
                  crs = 4326, agr = "constant")%>%
          st_transform(st_crs(tracts17))%>%
          filter(property_use %in% "Child Crisis Center")%>%
          mutate(ID = 1:n(),Legend = "Child Crisis Center")%>%
          dplyr::select(ID, Legend,geometry)

#arts and education centerS
arts_edu<- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                    coords = c("longitude", "latitude"),
                    crs = 4326, agr = "constant")%>%
           st_transform(st_crs(tracts17))%>%
           Filter(property_use %in% c("Mesa Arts Center","Museums","Libraries","Sequoia Charter School"))%>%
           mutate(ID = 1:n(), Legend = "Arts and Education")%>%
           dplyr::select(ID, Legend,geometry)


#public housing
public_housing<- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                    coords = c("longitude", "latitude"),
                    crs = 4326, agr = "constant")%>%
                st_transform(st_crs(tracts17))%>%
                filter(property_use %in% c("Housing", "Escobedo Housing", "NSP", "Residential Property"))%>%
                mutate(ID = 1:n(), Legend = "Public Housing")%>%
                dplyr::select(ID, Legend,geometry)


#city properties test plot
ggplot()+
  geom_sf(data= mesa_tracts17,fill=NA,color='black')+
  geom_sf(data=arts_edu, color = 'red')+
  geom_sf(data=mesa_police_fire, color='blue')+
  geom_sf(data=parks, color='green')+
  geom_sf(data=public_housing,color='orange')+
  geom_sf(data=cccenter, color = 'yellow')+
  mapTheme()



#----ZONING----

#High Density residential
HD_Residential <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning%20Districts.geojson")%>%
  st_transform(st_crs(tracts17))%>%
  filter(zoning %in% c("RM-3","RM-2","RM-4"))%>%
  dplyr::select(objectid, geometry)%>%mutate(zone = 'HD')

#Low Density Residential- "single residence" zoning
LD_Residential <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning%20Districts.geojson")%>%
  st_transform(st_crs(tracts17))%>%
  filter(zoning %in% c("RS-15","RS-35","RS-43","RS-6","RS-90","RS-9","PC","RSL-2.5","RSL-4.5","RSL-2.5"))%>%
  dplyr::select(objectid, geometry)%>%mutate(zone = 'LD')

#Commercial
#com<- c("GC","LC","NC","OC","RSL-4.0")
Commercial <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning%20Districts.geojson")%>%
  st_transform(st_crs(tracts17))%>%
  filter(zoning %in% c("GC","LC","NC","OC","RSL-4.0"))%>%
  dplyr::select(objectid, geometry)%>%mutate(zone = 'com')

#Downtown
#dt<- c("DC","DB-2","DB-1", "DR-2", "DR-3", "DR-1")
Downtown <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning%20Districts.geojson")%>%
  st_transform(st_crs(tracts17))%>%
  filter(zoning %in% c("DC","DB-2","DB-1", "DR-2", "DR-3", "DR-1"))%>%
  dplyr::select(objectid, geometry)%>%mutate(zone = 'DT')

#Industrial
#ind<- c("LI", "HI", "AG", "AG")
Industrial <- st_read("https://raw.githubusercontent.com/OliviaScalora/MUSA508_Final/main/Data/Zoning%20Districts.geojson")%>%
  st_transform(st_crs(tracts17))%>%
  filter(zoning %in% c("LI", "HI", "AG", "AG"))%>%
  dplyr::select(objectid, geometry)%>%mutate(zone = 'ind')


zoning<- rbind(Commercial, HD_Residential, LD_Residential, Downtown, Industrial)
zoning$zones <- factor(zoning$zones, levels=c('High Density Res','Low Density Res','Commercial','Downtown','Industrial'))

#ZonePlot1
# ggplot()+
#   geom_sf(data=city_boundary,
#           color = 'black',
#           size = 1,
#           fill = 'grey95')+
#   geom_sf(data= LD_Residential,
#           aes(fill= 'zone'),
#           color = NA,
#           fill = 'lightblue')+
#   geom_sf(data= HD_Residential,
#           aes(fill= 'zone'),
#           color = NA,
#           fill = 'blue')+
#   geom_sf(data= Commercial,
#           aes(fill= 'zone'),
#           color = NA,
#           fill = 'red')+
#   geom_sf(data= Downtown,
#           aes(fill= 'zone'),
#           color = NA,
#           fill = 'orange')+
#   geom_sf(data= Industrial,
#           aes(fill= 'zone'),
#           color = NA,
#           fill = 'yellow',
#           alpha= .45)+
#   geom_sf(data = opioid_data,
#           aes(size = count),
#           color = 'black',
#           alpha = .25)+
#   scale_fill_manual(name = "zones") +
#   scale_size_continuous(breaks=seq(1, 75, by=10),
#                         range = c(1,10))+
#   labs(size = 'Count at nearest\n 1/3 mi. interval')+
#   mapTheme()

#ZonePlot1 - for powerpoint
ggplot()+
  geom_sf(data=city_boundary,
          color = 'black',
          size = 1,
          fill = 'grey95')+
  geom_sf(data= zoning,
          aes(fill= zone),
          color = NA,)+
  geom_sf(data = opioid_data,
          aes(size = count),
          color = '#e5383b')+
  scale_fill_manual(values = c("#fe938c", "#9cafb7", "#e6b89c", "#4281a4","#ead2ac"),
                    labels=c('Commercial','Downtown','High Density Res','Industrial','Low Density Res')) +
  scale_size_continuous(breaks=seq(1, 75, by=10),
                        range = c(1,10))+
  labs(size = 'Count at nearest\n 1/3 mi. interval', fill = 'Zone Type',
       title = 'Mesa, AZ Zoning and Opioid Overdoses')+
  mapTheme()+theme(panel.background = element_rect(fill = "#f0efeb"),
                   legend.background = element_rect(fill="#f0efeb"),
                   plot.background = element_rect(fill = '#f0efeb'),
                   legend.text = element_text(color='#233d4d'),
                   legend.title = element_text(color = '#233d4d'),
                   plot.title = element_text(color = '#233d4d', size = 30),
                   panel.grid = element_blank())


#----MAKE FISHNET----

fishnet <- 
  st_make_grid(city_boundary,
               cellsize = 1760, 
               square = TRUE) %>%
  .[city_boundary] %>%  
  st_sf() %>%
  mutate(uniqueID = rownames(.))

#join overdoses to the fishnet
opioid_net <- 
  dplyr::select(opioid_data) %>% 
  mutate(countoverdose = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countoverdose = replace_na(countoverdose, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), size=nrow(fishnet), replace = TRUE))

#opioid fishnet plot
ggplot() +
  geom_sf(data = opioid_net, aes(fill = countoverdose), color= NA)+
  scale_fill_viridis(option = 'F', direction = -1) +
  labs(title = "Count of Overdoses for the fishnet",
       subtitle = "Mesa, AZ",
       caption = "Lighter pixels represent areas of higher observed opioid overdose incidents.") +
  mapTheme()


#----JOIN ZONING DATA TO FISHNET----

#CREATE CLUSTERS - commented code are unused attempts to join polygon clusters - ended up
#just finding the centroid of each zone polygon and joining the centroid to the fishnet

#Grouping Zoning Cluster together
#Industrial
#convert Industrial sf to sp class
# ind_sp<- as(Industrial, Class="Spatial")
# #Industrial Neighbor List
# ind_nb <- poly2nb(ind_sp, queen = TRUE)
# #create regions
# region_ind <- create_regions(ind_nb)
# ind_rgn <- spCbind(ind_sp, region_ind)
# ind_sp_2 <- unionSpatialPolygons(ind_rgn, region_ind)
# Industrial_R<- st_as_sf(ind_sp_2)%>%mutate(ID = 1:n())

# cluster industrial polygons that are within 100 ft of one another
# Industrial_R<- clusterSF(Industrial,100)
#Create polygon centroid
Ind_center<- st_centroid(Industrial)%>%mutate(Legend = "Industrial")


#Commercial
#convert Commercial sf to sp class
# com_sp<- as(Commercial, Class="Spatial")
# #Commercial Neighbor List
# com_nb <- poly2nb(com_sp, queen = TRUE)
# #create regions
# region_com <- create_regions(com_nb)
# com_rgn <- spCbind(com_sp, region_com)
# com_sp_2 <- unionSpatialPolygons(com_rgn, region_com)
# Commercial_R<- st_as_sf(com_sp_2)%>%mutate(ID = 1:n())

# cluster commercial polygons that are within 100 ft of one another
# Commercial_R<- clusterSF(Commercial,1000)
#Create polygon centroid
Comm_center<- st_centroid(Commercial)%>%mutate(Legend = "Commercial")


#Residential
#convert Residential sf to sp class
# res_sp<- as(Residential, Class="Spatial")
# #Residential Neighbor List
# res_nb <- poly2nb(res_sp,queen = TRUE)
# #create regions
# region_res <- create_regions(res_nb)
# res_rgn <- spCbind(res_sp, region_res)
# res_sp_2 <- unionSpatialPolygons(res_rgn, region_res)
# Residential_R<- st_as_sf(res_sp_2)%>%mutate(ID = 1:n())

# cluster low density residential polygons that are within 100 ft of one another
      # takes way too long to run 
# LD_Residential_R<- clusterSF(LD_Residential,100)
#Create polygon centroid
LDR_center<- st_centroid(LD_Residential)%>%mutate(Legend = "Low Density Residential")


# cluster high density residential polygons that are within 100 ft of one another
# HD_Residential_R<- clusterSF(HD_Residential,100)
#Create polygon centroid
HDR_center<- st_centroid(HD_Residential)%>%mutate(Legend = "High Density Residential")


# cluster downtown polygons that are within 100 ft of one another
# Downtown_R<- clusterSF(Downtown,1000)
#Create polygon centroid
DT_center<- st_centroid(Downtown)%>%mutate(Legend = "Downtown")


#join to fishnet + plot
zone_vars_net <- 
  rbind(DT_center, HDR_center, LDR_center, Comm_center, Ind_center) %>%
  st_join(., fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID, Legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet) %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()

zone_vars_net.long <- 
  gather(zone_vars_net, Variable, value, -geometry, -uniqueID)
zone_vars <- unique(zone_vars_net.long$Variable)
zone_mapList <- list()

for(i in zone_vars){
  zone_mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(zone_vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(option = 'F', direction = -1) +
    labs(title=i) +
    mapTheme()
  }

do.call(grid.arrange,c(zone_mapList, ncol=3, top="Density of Zone Type by Fishnet"))

#----JOIN POINT VARIABLES TO FISHNET----

#nearest neighbor function for point data
point_vars_net <- 
  rbind(arts_edu,cccenter,mesa_police_fire,parks,public_housing,vacant) %>%
  st_join(., fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID,Legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet) %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()

point_vars_net.long <- 
  gather(point_vars_net, Variable, value, -geometry, -uniqueID)
point_vars <- unique(point_vars_net.long$Variable)
point_mapList <- list()

for(i in point_vars){
  point_mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(point_vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(option = 'F', direction = -1) +
    labs(title=i) +
    mapTheme()
}

do.call(grid.arrange,c(point_mapList, ncol=3, top="Risk Factor by Fishnet"))

#----JOIN POINT NN VARIABLES TO FISHNET----

point_vars_net.nn <-
  point_vars_net %>%
  mutate(
    Park.nn =
      nn_function(st_coordinates(st_centroid(point_vars_net)), st_coordinates(parks),3),
    Child.Crisis.Center.nn =
      nn_function(st_coordinates(st_centroid(point_vars_net)), st_coordinates(cccenter),3),
    Arts.and.Education.nn =
      nn_function(st_coordinates(st_centroid(point_vars_net)), st_coordinates(arts_edu),3),
    Police.Fire.nn =
      nn_function(st_coordinates(st_centroid(point_vars_net)), st_coordinates(mesa_police_fire),3),
    Public.Housing.nn =
      nn_function(st_coordinates(st_centroid(point_vars_net)), st_coordinates(public_housing),3),
    Vacant.Property.nn =
      nn_function(st_coordinates(st_centroid(point_vars_net)), st_coordinates(vacant),3))%>%
  dplyr::select(ends_with(".nn"),uniqueID, geometry)

point_vars_net.nn.long <- 
  gather(point_vars_net.nn, Variable, value, -geometry, -uniqueID)
point_vars.nn <- unique(point_vars_net.nn.long$Variable)
point_mapList.nn <- list()

for(i in point_vars.nn){
  point_mapList.nn[[i]] <- 
    ggplot() +
    geom_sf(data = filter(point_vars_net.nn.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(option = 'F', direction = -1) +
    labs(title=i) +
    mapTheme()
}

do.call(grid.arrange,c(point_mapList.nn, ncol=3, top="Nearest Neighbor Risk Factors by Fishnet"))


#MERGE VARIABLE NETS 

vars_net<- cbind(point_vars_net,(st_drop_geometry(zone_vars_net)%>%dplyr::select(-uniqueID)))
vars_net<- cbind(vars_net,
                 (st_drop_geometry(point_vars_net.nn)%>%
                    dplyr::select(-uniqueID)))


#----LOCAL MORAN'S I----

final_net <-
  left_join(opioid_net, st_drop_geometry(vars_net), by="uniqueID") 

final_net <-
  st_centroid(final_net) %>%
  st_join(dplyr::select(mesa_tracts17, GEOID), by = "uniqueID") %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(final_net, geometry, uniqueID)) %>%
  st_sf() %>%
  na.omit()

## generates warnings from PROJ issues
## {spdep} to make polygon to neighborhoods... 
final_net.nb <- poly2nb(as_Spatial(final_net), queen=TRUE)
## ... and neighborhoods to list of weigths
final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE)

#print(final_net.weights, zero.policy=TRUE)

## see ?localmoran
local_morans <- localmoran(final_net$countoverdose, final_net.weights, zero.policy=TRUE) %>% 
  as.data.frame()

# join local Moran's I results to fishnet
final_net.localMorans <- 
  cbind(local_morans, as.data.frame(final_net)) %>% 
  st_sf() %>%
  dplyr::select(overdose_count = countoverdose, 
                Local_Morans_I = Ii, 
                P_Value = `Pr(z != E(Ii))`) %>%
  mutate(Significant_Hotspots = ifelse(P_Value <= 0.001, 1, 0)) %>%
  gather(Variable, Value, -geometry)


vars <- unique(final_net.localMorans$Variable)
varList <- list()


for(i in vars){
  varList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(final_net.localMorans, Variable == i), 
            aes(fill = Value), colour=NA) +
    scale_fill_viridis(option = "F", direction = -1) +
    labs(title=i) +
    mapTheme() + theme(legend.position="bottom")}


do.call(grid.arrange,c(varList, ncol = 4, top = "Local Morans I statistics, Mesa, AZ Overdose Incidents", bottom = "fig 4"))

final_net <- final_net %>% 
  mutate(overdose.isSig = 
           ifelse(local_morans[,5] <= 0.001, 1, 0)) %>%
  mutate(overdose.isSig.dist = 
           nn_function(st_coordinates(st_centroid(final_net)),
                       st_coordinates(st_centroid(filter(final_net, 
                                           overdose.isSig == 1))),k = 1))
final_net.long <- 
  gather(final_net, Variable, value, -geometry, -uniqueID)


#Plot distance to nearest significant hotspot
ggplot() +
      geom_sf(data = filter(final_net.long, Variable == "overdose.isSig.dist"), 
              aes(fill=as.numeric(value)), colour=NA) +
      scale_fill_viridis(option = 'F', direction = -1) +
      labs(fill = 'Distance (ft)',
           title= 'Distance to Significant Overdose Hotspot',
           subtitle = 'Mesa, AZ') +
      mapTheme()

ggplot() +
  geom_sf(data = final_net, 
          aes(fill=GEOID), colour='grey75', size =.25) +
  scale_fill_viridis(option = 'F', direction = -1, discrete = TRUE) +
  labs(title= 'Tracts ',
       subtitle = 'Mesa, AZ') +
  mapTheme()+theme(legend.position = 'none')

#----CORRELATION PLOTS----

#### for counts
correlation.long <-
  st_drop_geometry(final_net) %>%
  dplyr::select(-uniqueID, -cvID, -GEOID) %>%
  gather(Variable, Value, -countoverdose)

##### for nn
correlation.cor <-
  correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, countoverdose, use = "complete.obs"))

ggplot(correlation.long, aes(Value, countoverdose)) +
  geom_point(size = 1, color = '#223843') +
  geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Narcotics Incidents count as a function of risk factors",
       caption = "fig 5") +
  plotTheme()

