#Setup -------------------
install.packages('ggmap')
# install.packages('geojsonio')
# install.packages("data.table")
# install.packages("RgoogleMaps")
# install.packages("ggmap")
# install.packages('rstudioapi')
# install.packages('ggpubr')

# #load libraries
library(tidyverse)
library(sf)
library(RSocrata)
library(ggmap)
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
library(RgoogleMaps)
library(rstudioapi)
library(ggpubr)

options(scipen=999)
options(tigris_class = "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

census_api_key("d5e25f48aa48bf3f0766baab06d59402ea032067", overwrite = TRUE)
register_google(key = "AIzaSyBxeFrC2A-d2XQ_3mqIN8JUbZcE5236Khc")

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

#change cross validate function
crossValidate<- function(dataset, id, dependentVariable, indVariables) {
  
  allPredictions <- data.frame()
  cvID_list <- unique(dataset[[id]])
  
  for (i in cvID_list) {
    
    thisFold <- i
    cat("This hold out fold is", thisFold, "\n")
    
    fold.train <- filter(dataset, dataset[[id]] != thisFold) %>% as.data.frame() %>% 
      dplyr::select(id, geometry, indVariables, dependentVariable)
    fold.test  <- filter(dataset, dataset[[id]] == thisFold) %>% as.data.frame() %>% 
      dplyr::select(id, geometry, indVariables, dependentVariable)
    
    regression <-
      glm(countoverdose ~ ., family = "poisson", 
          data = fold.train %>% 
            dplyr::select(-geometry, -id))
    
    thisPrediction <- 
      mutate(fold.test, Prediction = predict(regression, fold.test, type = "response"))
    
    allPredictions <-
      rbind(allPredictions, thisPrediction)
    
  }
  return(st_sf(allPredictions))
}


#----CENSUS TRACTS AND CITY BOUNDARY----

VARS<- load_variables(2017, 'acs5')
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


council_districts<- st_as_sf(read.socrata("https://data.mesaaz.gov/resource/4scp-vfkf.csv"),
                             wkt = 'geometry',crs = 4326, agr = "constant")%>%
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

#census block groups
 
blockgroups17 <- get_acs(geography = "block group", variables = c("B01003_001","B19013_001","B25058_001",
                                                                  "B25003_003", "B25003_002", "B25004_001", "B25001_001",
                                                                  "B02001_002", "B03002_012"), 
                         year=2017, state=04, county=013, geometry=T) %>% 
  st_transform('EPSG:2224')


mesa_bg17 <- blockgroups17[city_boundary,]%>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B01003_001, 
         MedHHInc = B19013_001, 
         MedRent = B25058_001,
         TotalRent = B25003_003,
         TotalOwn = B25003_002,
         VacantUnits = B25004_001,
         TotalUnits = B25001_001,
         TotalWhite = B02001_002,
         HispTotal = B03002_012)%>% 
  mutate(area = st_area(geometry),
         Pct.Vacant = VacantUnits/TotalUnits,
         Pct.White = TotalWhite/TotalPop,
         Pct.Hisp = HispTotal/TotalPop)%>%
  filter(GEOID != '040130101021')%>%
  filter(GEOID != '1040133184002')%>%
  filter(GEOID != '040139413002')%>%
  filter(GEOID != '040138169001')%>%
  filter(GEOID != '040139413004')%>%
  filter(GEOID != '040139413003')%>%
  filter(GEOID != '040133184002')

#----CENSUS VARIABLES----

#block groups in the bottom quintile = low_income 
mesa_bg17_HHInc <-mesa_bg17 %>% drop_na(MedHHInc)%>%mutate(HHInc_q5 = q5(MedHHInc))
low_inc_BG <- mesa_bg17_HHInc%>%filter(HHInc_q5 == 1)%>%dplyr::select(GEOID,geometry,)%>%mutate(Legend = 'Low Income')

#block groups where med rent is in lower 2 quintile
mesa_bg17_rent <-mesa_bg17 %>% drop_na(MedRent)%>%mutate(MedRent_q5 = as.numeric(q5(MedRent)))
low_med_rent_BG <- mesa_bg17_rent%>%filter(MedRent_q5 <=2)%>%dplyr::select(GEOID,geometry,)%>%mutate(Legend = 'Low Rent')

#block groups where vacancy rate is higher than 25%
high_vacancy_bg <- mesa_bg17%>%filter(Pct.Vacant >= .25)%>%dplyr::select(GEOID,geometry,)%>%mutate(Legend = 'High Vacancy')

#block groups where majority hispanic
hisp_bg <- mesa_bg17%>%filter(Pct.Hisp >= .5)%>%dplyr::select(GEOID,geometry,)%>%mutate(Legend = 'Majority Hispanic')

#block groups where vast majority white
white_bg <- mesa_bg17%>%filter(Pct.White >= .85)%>%dplyr::select(GEOID,geometry,)%>%mutate(Legend = 'Majority White')

# ggplot()+
#   geom_sf(data=city_boundary, fill= NA, color = 'black')+
#    geom_sf(data =low_inc_BG, fill = 'lightblue', color = 'black', alpha = .5)+mapTheme()

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


opioid_data17 <- opioid_data%>%filter(year=='2017')
opioid_data20 <- opioid_data%>%filter(year=='2020')

#----PLAYING AROUND WITH PLOTS-----

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  }


#plot opioid overdoses 2017-2020
grid.arrange(
ggplot()+
  geom_sf(data = mesa_tracts17,
          fill = NA,
          color = "black")+
  scale_fill_brewer(palette=1)+
  geom_sf(data = opioid_data17,
          aes(size = count),
          color = 'red',
          alpha = .25)+
  scale_size_continuous(breaks=seq(1, 61, by=15),
                         range = c(1,20))+
  guides(color= guide_legend(), 
          size= guide_legend(title = 'count',
                             override.aes = list(size = c(1,3,5,7,11))),
         fill = guide_legend(title = 'Total Living Below Poverty'))+ 
  labs(subtitle = "2017")+
  geom_sf(data=city_boundary,
          color = 'black',
          size = 1,
          fill = NA)+mapTheme(),
ggplot()+
  geom_sf(data = mesa_tracts17,
          fill = NA,
          color = "black")+
  scale_fill_brewer(palette=1)+
  geom_sf(data = opioid_data20,
          aes(size = count),
          color = 'red',
          alpha = .25)+
  scale_size_continuous(breaks=seq(1, 61, by=15),
                        range = c(1,20))+
  guides(color= guide_legend(), 
          size= guide_legend(title = 'count',
                             override.aes = list(size = c(1,3,5,7,11))),
         )+ 
  labs(subtitle = "2020")+
  geom_sf(data=city_boundary,
          color = 'black',
          size = 1,
          fill = NA)+mapTheme(), nrow=1)

# point_list<- opioid_data%>%dplyr::select(count, geometry)%>%
#               mutate(point = as.character(geometry))


#kernel density plot 2017-2021

 p1<- ggplot()+
    geom_sf(data=city_boundary, fill='#22192b', color='grey65')+
    stat_density2d(data= data.frame(st_coordinates(opioid_data17)),
                   aes(X,Y, fill=..level..,alpha=..level..),
                   size=0.01, bins=40, geom='polygon')+
    scale_fill_viridis(option = 'F',  direction = 1) +
    scale_alpha(range=c(0.00,0.35), guide='none')+
    geom_sf(data = opioid_data17,
            aes(size = count),
            color = '#fffaf2',
            alpha = .1)+
    guides(size = guide_legend(override.aes = list(color = '#2f273d', alpha=.2)))+
    labs(subtitle = '2017',
         fill = 'Density',
         size = 'Count at nearest\n 1/3 mi. interval')+
   scale_size_continuous(breaks=seq(1, 61, by=15),
                         range = c(1,10))+ mapTheme()+theme(legend.key.size = unit(.75, 'cm'))
 
 p2<- ggplot()+
    geom_sf(data=city_boundary, fill='#22192b', color='grey65')+
    stat_density2d(data= data.frame(st_coordinates(opioid_data20)),
                   aes(X,Y, fill=..level..,alpha=..level..),
                   size=0.01, bins=40, geom='polygon')+
    scale_fill_viridis(option = 'F',  direction = 1) +
    scale_alpha(range=c(0.00,0.35), guide='none')+
    geom_sf(data = opioid_data20,
            aes(size = count),
            color = '#fffaf2',
            alpha = .1)+
    guides(size = guide_legend(override.aes = list(color = '#2f273d', alpha=.2)))+
    labs(subtitle = '2020',
         fill = 'Density',
         size = 'Count at nearest\n 1/3 mi. interval')+
   scale_size_continuous(breaks=seq(1, 61, by=15),
                         range = c(1,10))+ mapTheme()
mylegend<-g_legend(p1)

grid.arrange(p1 + theme(legend.position="none"),
             p2 + theme(legend.position="none"),
             mylegend ,nrow=1,widths=c(2,2,.75), top = "Opioid Overdose Occurances in Mesa, AZ")
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
           filter(property_use %in% c("Mesa Arts Center","Museums","Libraries","Sequoia Charter School"))%>%
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
zoning$zone <- factor(zoning$zone, levels = c('HD','LD','DT','com','ind'))

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

#ZonePlot1
ggplot()+
  geom_sf(data=city_boundary,
          color = 'black',
          size = 1,
          fill = 'grey95')+
  geom_sf(data= zoning,
          aes(fill= zone),
          color = NA,)+
  geom_sf(data = opioid_data17,
          aes(size = count),
          color = '#e5383b', alpha = .5)+
  scale_fill_manual(values = c("#4281a4","#9cafb7", "#db534b", "#fe938c", "#ead2ac"),
                    labels=c('High Density Res','Low Density Res','Downtown','Commercial','Industrial')) +
  scale_size_continuous(breaks=seq(1, 4, by=1),
                        range = c(1,20))+
  guides(size = guide_legend(override.aes = list(size = c(1,4,8,10))))+
  labs(size = 'Count at nearest\n 1/3 mi. interval', fill = 'Zone Type',
       title = 'Zoning and Opioid Overdoses',
       subtitle= 'Mesa, Az; 2017')+
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
  dplyr::select(opioid_data17) %>% 
  mutate(countoverdose = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countoverdose = replace_na(countoverdose, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), size=nrow(fishnet), replace = TRUE))
# 
# opioid_net17 <- 
#   dplyr::select(opioid_data17) %>% 
#   mutate(countoverdose = 1) %>% 
#   aggregate(., fishnet, sum) %>%
#   mutate(countoverdose = replace_na(countoverdose, 0),
#          uniqueID = rownames(.),
#          cvID = sample(round(nrow(fishnet) / 24), size=nrow(fishnet), replace = TRUE))

#opioid fishnet plot
ggplot() +
  geom_sf(data = opioid_net, aes(fill = countoverdose), color= NA)+
  scale_fill_viridis(option = 'F', direction = -1) +
  labs(title = "Count of Overdoses for the fishnet",
       subtitle = "Mesa, AZ",
       caption = "Lighter pixels represent areas of higher observed opioid overdose incidents.") +
  mapTheme()

# #opioid fishnet plot
# ggplot() +
#   geom_sf(data = opioid_net17, aes(fill = countoverdose), color= NA)+
#   scale_fill_viridis(option = 'F', direction = -1) +
#   labs(title = "Count of Overdoses for the fishnet",
#        subtitle = "Mesa, AZ; 2017",
#        caption = "Lighter pixels represent areas of higher observed opioid overdose incidents.") +
#   mapTheme()



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


#join zones to fishnet + plot
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

#Point data to fishnet
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

#join nearest neighbor to fishnet
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

#----JOINING CENSUS VARIABLES TO FISHNET----

#Point data to fishnet
census_vars_net<- 
  rbind(high_vacancy_bg, low_med_rent_BG, low_inc_BG, white_bg, hisp_bg) %>%
  st_join(fishnet, ., join=st_intersects) %>%
  st_drop_geometry() %>%
  group_by(uniqueID,Legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet) %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()


census_vars_net.long <- 
  gather(census_vars_net, Variable, value, -geometry, -uniqueID)
census_vars <- unique(census_vars_net.long$Variable)
census_mapList <- list()

for(i in census_vars){
  census_mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(census_vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(option = 'F', direction = -1) +
    labs(title=i) +
    mapTheme()
}

#GRID SHOWS how many block groups each cell of fishnet intersects with that are defined as each variable. 
#max interesection is 4,5,and 6. 

do.call(grid.arrange,c(census_mapList, ncol=3, top="Risk Factor by Fishnet"))

#create nearest neighbor count for each cell-which make more sense?
#i think the categorical  - how many block groups are you touching for each variable - makes more sense 
# thank the nearest neighbor distance count because you are either in a block group that is defined as low rent or not
# block groups are arbitrary boundaries created by the US Census that arent exactly representational of actual 
# spatial grouping in a city- do not represent neighborhood boundaries. even for neighborhoods, it is unrealistic to 
# expect a neighborhood boundary will or will not contain attributes such as affordability,  vacancy and racial diversity
# these are also always changing
##therefore nearest neighbor distance calculations for each fishnet cell is not as indicative of what is happening in reality as a
#calculation of whether the fishnet is touching a block group with these attributes

census_vars_net.nn <-
  census_vars_net %>%
  mutate(
    high_vacancy.nn =
      nn_function(st_coordinates(st_centroid(census_vars_net)), st_coordinates(st_centroid(high_vacancy_bg)),3),
    low_med_rent.nn =
      nn_function(st_coordinates(st_centroid(point_vars_net)), st_coordinates(st_centroid(low_med_rent_BG)),3),
    low_inc.nn =
      nn_function(st_coordinates(st_centroid(point_vars_net)), st_coordinates(st_centroid(low_inc_BG)),3),
    maj.hisp.nn =
      nn_function(st_coordinates(st_centroid(point_vars_net)), st_coordinates(st_centroid(hisp_bg)),3),
    maj.white.nn =
      nn_function(st_coordinates(st_centroid(point_vars_net)), st_coordinates(st_centroid(white_bg)),3))%>%
  dplyr::select(ends_with(".nn"),uniqueID, geometry)

census_vars_net.nn.long <- 
  gather(census_vars_net.nn, Variable, value, -geometry, -uniqueID)
census_vars.nn <- unique(census_vars_net.nn.long$Variable)
census_mapList.nn <- list()

for(i in census_vars.nn){
  census_mapList.nn[[i]] <- 
    ggplot() +
    geom_sf(data = filter(census_vars_net.nn.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(option = 'F', direction = -1) +
    labs(title=i) +
    mapTheme()
}

do.call(grid.arrange,c(census_mapList.nn, ncol=3, top="Risk Factor by Fishnet"))

#----MERGE ALL VARIABLES TO FISHNET----

vars_net<- cbind(point_vars_net,(st_drop_geometry(zone_vars_net)%>%dplyr::select(-uniqueID)))
vars_net<- cbind(vars_net,(st_drop_geometry(point_vars_net.nn))%>%
                   dplyr::select(-uniqueID))
vars_net<- cbind(vars_net,(st_drop_geometry(census_vars_net))%>%
                   dplyr::select(-uniqueID))
vars_net<- cbind(vars_net,(st_drop_geometry(census_vars_net.nn))%>%
                    dplyr::select(-uniqueID))

final_net <-
  left_join(opioid_net, st_drop_geometry(vars_net), by="uniqueID") 


final_net <-
  st_centroid(final_net) %>%
  st_join(dplyr::select(mesa_bg17, GEOID), by = "uniqueID") %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(final_net, geometry, uniqueID)) %>%
  st_sf() %>%
  na.omit()

# final_net17 <-
#   left_join(opioid_net17, st_drop_geometry(vars_net), by="uniqueID") 
# 
# final_net17 <-
#   st_centroid(final_net17) %>%
#   st_join(dplyr::select(mesa_tracts17, GEOID), by = "uniqueID") %>%
#   st_drop_geometry() %>%
#   left_join(dplyr::select(final_net17, geometry, uniqueID)) %>%
#   st_sf() %>%
#   na.omit()

#----LOCAL MORAN'S I----

## generates warnings from PROJ issues
## {spdep} to make polygon to neighborhoods... 
final_net.nb <- poly2nb(as_Spatial(final_net), queen=TRUE)
## .. and neighborhoods to list of weigths
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

#Plot distance to nearest significant hotspot
ggplot() +
      geom_sf(data = filter(final_net.long, Variable == "overdose.isSig.dist"), 
              aes(fill=as.numeric(value)), colour=NA) +
      scale_fill_viridis(option = 'F', direction = -1) +
      labs(fill = 'Distance (ft)',
           title= 'Distance to Significant Overdose Hotspot',
           subtitle = 'Mesa, AZ') +
      mapTheme()

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
  facet_wrap(~Variable, ncol = 5, scales = "free") +
  labs(title = "Narcotics Incidents count as a function of risk factors",
       caption = "fig 5") +
  plotTheme()

#---- HISTOGRAMS----

final_net %>%
  ggplot(aes(countoverdose,))+
  geom_histogram(bins = 50, colour="black", fill = '#223843') +
  scale_x_continuous(breaks = seq(0, 100, by = 20)) + 
  scale_y_continuous(breaks = seq(0,1500, by = 200))+
  labs(title="Distribution of Overdose Occurances", subtitle = "Mesa, AZ",
       x="Overdose Occurance Count", y="Frequency", 
       caption = "fig 6") +plotTheme()

#----CROSS-VALIDATED POISSON REGRESSION----

#generalizability is important 1) to test model performance on new data and 2) on different spatial group contexts
# like neighborhoods. 

#Spatial cross-validation
# LOGO-CV
#hold out one local area, train the model on the remaining n-1 areas, predict the hold out, record goodness of fit

colnames(final_net)
# View(crossValidate)

## define the variables we want

#Just Risk Factors
reg.vars <- c("Commercial", "High.Density.Residential", "Low.Density.Residential", "Industrial", "Downtown", 
                 "Park.nn", "Child.Crisis.Center", "Arts.and.Education.nn", "Police.Fire.Station", "Public.Housing", "Vacant.Property", "Low.Income", "Low.Rent",
                 "Majority.White", "Majority.Hispanic")

#Include Local Moran's I Statistic
reg.ss.vars <- c("Commercial", "High.Density.Residential", "Low.Density.Residential", "Industrial", "Downtown", 
                 "Park.nn", "Child.Crisis.Center", "Arts.and.Education.nn", "Police.Fire.Station", "Public.Housing", "Vacant.Property", "Low.Income", "Low.Rent",
                 "Majority.White", "Majority.Hispanic", "overdose.isSig.dist", "overdose.isSig")

## RUN REGRESSIONS

#### K-fold
#Just Risk Factors
reg.CV <- crossValidate(
  dataset = final_net,
  id = "cvID",                           
  dependentVariable = "countoverdose",
  indVariables = reg.vars) %>%
  dplyr::select(cvID, countoverdose, Prediction, geometry)

#Spatial Process
reg.ss.CV <- crossValidate(
  dataset = final_net,
  id = "cvID",                           
  dependentVariable = "countoverdose",
  indVariables = reg.ss.vars) %>%
  dplyr::select(cvID, countoverdose, Prediction, geometry)


#### Spatial LOGO-CV
#Just Risk Factors
reg.spatialCV <- crossValidate(
  dataset = final_net,
  id = "GEOID",                           
  dependentVariable = "countoverdose",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = GEOID, countoverdose, Prediction, geometry)


#Spatial Process
reg.ss.spatialCV <- crossValidate(
  dataset = final_net,
  id = "GEOID",                           
  dependentVariable = "countoverdose",
  indVariables = reg.ss.vars) %>%
  dplyr::select(cvID = GEOID, countoverdose, Prediction, geometry)

#### bind

reg.summary <- 
  rbind(
    mutate(reg.CV,        Error = Prediction - countoverdose,
           Regression = "Random k-fold CV: Just Risk Factors"),
    mutate(reg.ss.CV,        Error = Prediction - countoverdose,
           Regression = "Random k-fold CV: Spatial Process"),
    mutate(reg.spatialCV, Error = Prediction - countoverdose,
           Regression = "Spatial LOGO-CV: Just Risk Factors"),
    mutate(reg.ss.spatialCV, Error = Prediction - countoverdose,
           Regression = "Spatial LOGO-CV: Spatial Process")) %>%
  st_sf() 

# calculate errors by Block Group
error_by_reg_and_fold <- 
  reg.summary  %>%
  group_by(Regression, cvID) %>% 
  summarize(Mean_Error = mean(Prediction - countoverdose, na.rm = T),
            MAE = mean(abs(Mean_Error), na.rm = T),
            SD_MAE = mean(abs(Mean_Error), na.rm = T)) %>%
  ungroup()


#----HISTOGRAMS----
error_by_reg_and_fold %>% ggplot(aes(MAE))+
  geom_histogram(bins=30, color = 'black', fill = '#f5af7b')+
  facet_wrap(~Regression)+
  geom_vline(xintercept= 0)+scale_x_continuous(breaks=seq(0,8,by=1))+
  labs(title='Distribution of MAE', subtitle='k-fold cross validation vs. LOGO-CV',
       x= 'Mean Absolute Erorr', y = 'Count')+plotTheme()

#-----SPATIAL PLOTS-----

ggplot() +
  geom_sf(data = reg.summary, aes(fill = Prediction, colour = Prediction)) +
  scale_fill_viridis(option = "F", direction = -1) +
  scale_colour_viridis(option = "F", direction = -1) +
  facet_wrap(~Regression) +  
  labs(title="Narcotics Incidents Errors", subtitle = "Random K-Fold and      Spatial Cross Validation", caption = "fig #") +
  mapTheme()

#---TABLE----

st_drop_geometry(error_by_reg_and_fold) %>%
  group_by(Regression) %>% 
  summarize(Mean_MAE = round(mean(MAE), 2),
            SD_MAE = round(sd(MAE), 2)) %>%
  kable(caption = 'Table 1') %>%
  kable_material() 

error_by_reg_and_fold%>%
  filter(str_detect(Regression, 'LOGO'))%>%
  ggplot()+
  geom_sf(aes(fill=MAE))+
  facet_wrap(~Regression)+
  scale_fill_viridis(option = "F", direction = -1) +
  labs(title='Distribution of MAE', subtitle='k-fold cross validation vs. LOGO-CV',
       x= 'Mean Absolute Erorr', y = 'Count')+mapTheme()


#----NEIGHBORHOOD WEIGHTS-----
  
neighborhood.weights <-
  filter(error_by_reg_and_fold, Regression == "Spatial LOGO-CV: Spatial Process") %>%
  group_by(cvID) %>%
  poly2nb(as_Spatial(.), queen=TRUE) %>%
  nb2listw(., style="W", zero.policy=TRUE)

filter(error_by_reg_and_fold, str_detect(Regression, "LOGO"))  %>% 
  st_drop_geometry() %>%
  group_by(Regression) %>%
  summarize(Morans_I = moran.mc(abs(Mean_Error), neighborhood.weights, 
                                nsim = 999, zero.policy = TRUE, 
                                na.action=na.omit)[[1]],
            p_value = moran.mc(abs(Mean_Error), neighborhood.weights, 
                               nsim = 999, zero.policy = TRUE, 
                               na.action=na.omit)[[3]])%>%
  kable(caption = 'Table 2') %>%
  kable_material() 


grid.arrange(
reg.summary%>%filter(Regression == "Spatial LOGO-CV: Spatial Process")%>%
  ggplot() + geom_sf(aes(fill = Prediction, colour = Prediction))+
  scale_fill_viridis(option = "F", direction = -1) +
  scale_colour_viridis(option = "F", direction = -1)+
  labs(title='Spatial LOGO-CV: Spatial Process')+mapTheme()+theme(panel.border=element_blank()),
reg.summary%>%filter(Regression == "Spatial LOGO-CV: Just Risk Factors")%>%
  ggplot() + geom_sf(aes(fill = Prediction, colour = Prediction))+
  scale_fill_viridis(option = "F", direction = -1) +
  scale_colour_viridis(option = "F", direction = -1)+
  labs(title='Spatial LOGO-CV: Just Risk Factors')+mapTheme()+theme(panel.border=element_blank()),
ggplot() +
  geom_sf(data = reg.summary, aes(fill = countoverdose, colour = countoverdose))+
  scale_fill_viridis(option = "F", direction = -1) +
  scale_colour_viridis(option = "F", direction = -1)+
  labs(title='Observed Overdoses')+mapTheme()+theme(panel.border=element_blank()),nrow=1)

# left off here trying to create a plot that compares both LOGO regression types to the observed overdose incidents 
# i got close but the first plot is facet wrapped and the second is not, it makes it look really bad
# the solution might be to create three separate plots instead of facet wrapping the first two

#Test  generalizability across neighborhood

st_drop_geometry(reg.summary) %>%
  group_by(Regression) %>%
  mutate(overdose_Decile = ntile(countoverdose, 10)) %>%
  group_by(Regression, overdose_Decile) %>%
  summarize(meanObserved = mean(countoverdose, na.rm=T),
            meanPrediction = mean(Prediction, na.rm=T)) %>%
  gather(Variable, Value, -Regression, -overdose_Decile) %>%          
  ggplot(aes(overdose_Decile, Value, shape = Variable)) +
  geom_point(size = 2) + geom_path(aes(group = overdose_Decile), colour = "black") +
  scale_shape_manual(values = c(2, 17)) +
  facet_wrap(~Regression) + xlim(0,10) +
  labs(title = "Predicted and observed burglary by observed burglary decile")  +
  plotTheme()


#Kernel Density Plot
opioid_ppp <- as.ppp(st_coordinates(opioid_data), W = st_bbox(final_net))
opioid_KD <- density.ppp(opioid_ppp, 1000)

as.data.frame(opioid_KD) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
  aggregate(., final_net, mean) %>%
  ggplot() +
  geom_sf(aes(fill=value)) +
  geom_sf(data = sample_n(opioid_data, 1500), size = 1, color = 'blue') +
  scale_fill_viridis(option = "F", direction = -1) +
  labs(title = "Kernel density of opioid overdoses in 2019 ") +
  mapTheme()


#Get 2019 Data
blockgroups19 <- get_acs(geography = "block group", variables = c("B01003_001","B03002_012"), 
                         year=2019, state=04, county=013, geometry=T) %>% 
  st_transform('EPSG:2224')  %>% 
  dplyr::select(variable, estimate, GEOID) %>%
  spread(variable, estimate) %>%
  rename(TotalPop = B01003_001,
         HispTotal = B03002_012) %>%
  mutate(Pct.Hisp = HispTotal/TotalPop,
         raceContext = ifelse(Pct.Hisp > .5, "Majority_Hispanic", "Majority_Non_Hispanic")) %>%
  .[council_districts,]

reg.summary %>%
  st_centroid() %>%
  st_join(blockgroups19) %>%
  na.omit() %>%
  st_drop_geometry() %>%
  group_by(Regression, raceContext) %>%
  summarize(mean.Error = mean(Error, na.rm = T)) %>%
  spread(raceContext, mean.Error) %>%
  kable(caption = "Table 2") %>%
  kable_material()

remove.packages('cli')
install.packages('cli')
remove.packages('stringi')
remove.packages('stringi')
remove.packages('rlang')
remove.packages('glue')
install.packages('rlang')


devtools::install_github("dkahle/ggmap")

mesa_google<- get_googlemap("mesa, arizona", 
                                  zoom = 12, maptype = 'terrain', color = 'color')%>%ggmap()


geom_point(data = )
print(mesa_google)

