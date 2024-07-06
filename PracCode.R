library(openxlsx)
library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(knitr)
library(kableExtra)
library(tidyverse)
library(magrittr)
library(glue)
library(here)
library(tibble)
library(rvest)
library(tidyselect)
library(deductive)
library(deducorrect)
library(validate)
library(Hmisc)
library(stringr)
library(ggplot2)
library(ggnewscale)
library(moments)
library(sn)
#install.packages("ff", dependencies = TRUE )
#install.packages("MVN", dependencies = TRUE )
#install.packages("outliers", dependencies = TRUE )
#library(plyr)
#install.packages("forecast")
#detach("package:plyr", unload = TRUE)
#install.packages("purrr", dependencies = TRUE )
library(purrr)
# install.packages("maptiles", dependencies = TRUE )
# install.packages("tidyterra", dependencies = TRUE )
#install.packages("OpenStreetMap", dependencies = TRUE )
#install.packages("RColorBrewer")
#install.packages("ggsci ")
library(maptiles)
library(tidyterra)
library(sf)
library(OpenStreetMap)
library(geosphere)
library(stringr)
library(scales)
library(egg)
library(moments)
library(forecast)
library(ggsci)
library(RColorBrewer)
#install.packages("flextable", dependencies = TRUE )
#install.packages("scales", dependencies = TRUE )
#install.packages("egg", dependencies = TRUE )
# https://stackoverflow.com/questions/9564489/read-all-files-in-a-folder-and-apply-a-function-to-each-data-frame
csvFileNames <- list.files("../../Data",pattern = "*.csv", full.names = TRUE)
csvFileNames


vicRoadsDFList <- sapply(csvFileNames, function(i) read_csv(i, show_col_types = FALSE))

testDFNames <- read.csv("../../Data/ACCIDENT.csv")
testDFNames1 <- read_csv("../../Data/ACCIDENT.csv")
#Return key to manageable file name
names(vicRoadsDFList) <- c(gsub("../../Data/","", names(vicRoadsDFList)))
str(vicRoadsDFList[1])

councilsVic_sf <- read_sf("../../Councils/georef-australia-local-government-area.geojson") %>% 
  filter (ste_name == "Victoria")
namesLGAVic <- unique(councilsVic_sf$lga_name) %>%
  append(., "Merri-bek")


# # https://stackoverflow.com/questions/32066402/how-to-perform-multiple-left-joins-using-dplyr-in-r 
# temp <- purrr::reduce(vicRoadsDFList, dplyr::left_join, by = "ACCIDENT_NO") # Adds duplicated lines
# 
# temp2 <- purrr::reduce(vicRoadsDFList, dplyr::left_join)
# 
# posnOfDF <- 1
# for (tmpDF in vicRoadsDFList) {
#   if (n_distinct(tmpDF$ACCIDENT_NO) < dim(tmpDF)[[1]]) {
#     cat(names(vicRoadsDFList)[[posnOfDF]],"\n")
#   }
#   posnOfDF <- posnOfDF + 1
# }
# baseDFG <- vicRoadsDFList[[2]]
# tmp <- vicRoadsDFList[[2]] %>%
#        group_by( ACCIDENT_NO ) 
# 
# oneId <- tmp[6:8,]
# 
# tmpOne <- oneId %>%
#           pivot_wider(names_from = c(2:4),
#                       values_from = c(5:11))
# 
# 
# print(csvFileNames)
# 
# nodeJoinLeftLoc <- left_join(vicRoadsDFList[[3]],vicRoadsDFList[[5]], by = c("ACCIDENT_NO","NODE_ID"))
# vecColNamesDF <- list()
# posnCN <- 1
# for (df in vicRoadsDFList) {
#   vecColNamesDF[[names(vicRoadsDFList)[[posnCN]]]] <-  colnames(df)
#  
#   posnCN <- posnCN + 1
# }
# 
# for (posn in 1:length(vecColNamesDF)) {
#   
#   cat(names(vecColNamesDF)[[posn]],"\n")
#   
#   for(dfAttr in (vecColNamesDF)[[posn]]){
#     cat("\t",dfAttr)
#   }
#   cat("\n")
# }
# 
# vecColNamesDF[[1]][[1]]
# 
# # cat(names(vicRoadsDFList)[[posnCN]],"\n")
# # cat("\t",colnames(df),"\n")
# print(intersect(vecColNamesDF[[1]],vecColNamesDF[[2]]))
# 
# #fileOnlyName <- gsub("../../Data/","", names(vicRoadsDFList))
# 
# #install.packages("sf", dependencies = TRUE)
# 
# my_sf <- read_sf("../../Data/VICTORIAN_ROAD_CRASH_DATA.geojson")
# 
# plotACCTime <- my_sf %>% select(ACCIDENT_TIME,geometry) 
# 
# ggplot(data = plotACCTime,aes(geometry = geometry)) + geom_sf()
# 
# 
# namesOfFile <- names(vecColNamesDF)
# for (posnOne in 1:length(namesOfFile)) {
#   cat(paste(namesOfFile[[posnOne]],"at list no.",posnOne, "intersects with he following file:\n"))
#   if (posnOne == length(namesOfFile)) {
#     break
#   }
#   for (posnTwo in (posnOne + 1):length(namesOfFile)) {
#    cat(paste("\t",namesOfFile[[posnTwo]],"at list no.",posnTwo," with these attributes:\n"))
#    cat(paste("\t",intersect(vecColNamesDF[[posnOne]],vecColNamesDF[[posnTwo]])),"\n")
#     
#     posnTwo <- posnTwo + 1
#   }
#   cat("\n")
#   posnOne <- posnOne + 1
#   
# }
# 
# dim(my_sf)
# 
# n_distinct(vicRoadsDFList[[1]]$ACCIDENT_NO)
# 
# nodeJoinLeftPersVeh <- left_join(vicRoadsDFList[[6]],vicRoadsDFList[[9]],
#                                  by = c("ACCIDENT_NO","VEHICLE_ID")) %>%
#                        arrange(ACCIDENT_NO) %>%
#                        filter(ROAD_USER_TYPE_DESC == "Drivers")
# 
# 
#                        
# str(nodeJoinLeftPersVeh)                                 
# 
#                                  
# n_distinct(nodeJoinLeftPersVeh$ACCIDENT_NO)
# n_distinct(nodeJoinLeftLoc$ACCIDENT_NO)
# 
# # #install.packages("RSAGA")
# # library(RSAGA)
# # 
# # mapECW <- rsaga.import.gdal("../../Data/76945_zone50_mga.ecw","testMap", env = rsaga.env())
# 
# # install.packages("VicmapR", dependencies = TRUE)
# #install.packages("mapview", dependencies = TRUE)
# # library(VicmapR)
# # library(mapview)
# # all_layers <- listLayers()
# # search <- listLayers(pattern = "road", ignore.case = T)
# # road_route <- vicmap_query(layer = "open-data-platform:tr_road") %>% 
# #   collect() 
# # mapview(road_route)
# 
# 
# 
# # testOSMPlus <- my_sf[,c("LATITUDE", "LONGITUDE")]
# # acc_merc <- st_transform(testOSMPlus,crs = "+init=epsg:4326")
# # acc_osm <- get_tiles(acc_merc, provider="OpenStreetMap", zoom = 13)
# # ggplot() +
# #   geom_spatraster_rgb(data = acc_osm)
# testOSMPlus <- my_sf[,c("LATITUDE", "LONGITUDE","DAY_OF_WEEK")]
# 
# sfOSMPlus <- st_as_sf(testOSMPlus)
# lat <- c(-34, -39.2)
# long <- c(140, 150)
# victoriaAus <- openmap( c( lat[1], long[1] ), c( lat[2], long[2] ),zoom = 8,type = 'osm',
#                         mergeTiles = TRUE)
# victoriaAus2 <- openproj(victoriaAus)
# 
# osmMapTest <- as.data.frame(projectMercator(lat = testOSMPlus$LATITUDE ,
#                                             long = testOSMPlus$LONGITUDE)) 
# osmMapTest$Day <- testOSMPlus$DAY_OF_WEEK
# upperLeft <- victoriaAus2$bbox$p1
# lowerRight <- victoriaAus2$bbox$p2
# 
# 
# autoplot.OpenStreetMap(victoriaAus) +
#   geom_point(data = osmMapTest, aes( x = x , y =y), size =0.5)
# 
# 
# # install.packages("leaflet",dependencies = TRUE)
# # # install.packages("ggmap",dependencies = TRUE)
# # library(leaflet)
# # library(ggmap)
# 
# autoplot.OpenStreetMap(victoriaAus2) +
#   xlim(upperLeft[[1]], lowerRight[[1]]) +
#   ylim(upperLeft[[2]], lowerRight[[2]]) 
# 
# 
# df <- st_as_sf(testOSMPlus, crs=  "+proj=lonlat")
# 
# # ggplot(df) +
# #   geom_sf() +
# #   theme_void()
# 
# df_merc<- st_transform(df, 4326)
# 
# dc <- get_tiles(df_merc, provider = "OpenStreetMap", zoom = 8)
# 
# ggplot() +
#   geom_spatraster_rgb(data = dc) +
#   geom_sf(data = df,aes( geometry = geometry, colour = DAY_OF_WEEK), size =0.3) +
#   coord_sf(crs = 4326)
# 
# # vecColNamesDF <- list()
# # posnCN <- 1
# # for (df in vicRoadsDFList) {
# #   vecColNamesDF[[names(vicRoadsDFList)[[posnCN]]]] <-  colnames(df)
# #   
# #   posnCN <- posnCN + 1
# # }
# max_len <- max(lengths(vecColNamesDF))
# df <- purrr::map_df(vecColNamesDF, ~ c(., rep('', max_len - length(.))))
# 
# View(vicRoadsDFList$VEHICLE.csv)
# 
# 
# accDateTimeDay <- vicRoadsDFList$ACCIDENT.csv %>%
#   select(ACCIDENT_NO,ACCIDENT_DATE,ACCIDENT_TIME,DAY_OF_WEEK,DAY_WEEK_DESC)
# str(accDateTimeDay)
# 
# accLatLongLGA <- vicRoadsDFList$NODE.csv %>%
#   select(ACCIDENT_NO, LATITUDE,LONGITUDE,LGA_NAME)
# str(accLatLongLGA)
# 
# accPers <- vicRoadsDFList$PERSON.csv %>%
#   select(ACCIDENT_NO,PERSON_ID,VEHICLE_ID,SEX, AGE_GROUP, ROAD_USER_TYPE,ROAD_USER_TYPE_DESC)
# str(accPers)
# 
# accVeh <- vicRoadsDFList$VEHICLE.csv %>%
#   select(ACCIDENT_NO,VEHICLE_ID,VEHICLE_YEAR_MANUF,VEHICLE_MAKE,VEHICLE_MODEL,VEHICLE_POWER)
# str(accVeh)
# unique(accPers$ROAD_USER_TYPE_DESC)
# unique(accPers$VEHICLE_ID)
# unique(accPers$PERSON_ID)
# table(accPers$PERSON_ID)
# 
# tmpWiderPers<- accPers%>% 
#         filter(ROAD_USER_TYPE_DESC %in% c("Drivers","Motorcyclists"))%>%
#         group_by(by = ACCIDENT_NO)%>%
#         pivot_wider(names_from = PERSON_ID, values_from = VEHICLE_ID)
# View(accPers)
# view(table(tmpWiderPers$ACCIDENT_NO))
# 
# accDateTimeDay %<>% mutate( Timestamp = lubridate::ymd_hms(paste(ACCIDENT_DATE,ACCIDENT_TIME),
#                                                            tz = "Australia/Melbourne"),
#                             Full_Day = factor(DAY_WEEK_DESC, ordered = TRUE,
#                                               levels= c("Monday","Tuesday","Wednesday","Thursday",
#                                                         "Friday","Saturday","Sunday" ))) %>%
#   select(-c(DAY_OF_WEEK,DAY_WEEK_DESC))%>%
#   rename(Date_char = ACCIDENT_DATE , Time_char = ACCIDENT_TIME)
# str(accDateTimeDay)
# 
# is.infinite(-Inf)
# 
#   
# msgNA <- checkNAInCols(list(accDateTimeDay), c("accDateTimeDay"))
# 
# str(msgNA)
# ab <- c(one = c(1:3), two = c(4:6))
# ab
# 
checkNAInCols <- function(checkDF,checkDFVarNames) {

  chkedAllDFList <- list()

  for ( posn in 1:length(checkDF)) {
    chkIndDFList <- list()
    checkCol <- colnames(checkDF[[posn]])
    count <- 1
    for (inputCol in checkCol) {
      sumNA <- checkDF[[1]][inputCol] %>%
        summarise(numNa = sum(is.na(.)))
      if (is.numeric(checkDF[[1]][inputCol][[1]]) | is.integer(checkDF[[1]][inputCol][[1]])) {
        sumNAN <- sum(is.nan(checkDF[[1]][[inputCol]]))
        sumINF <- sum(is.infinite(checkDF[[1]][[inputCol]]))
      }
      else {
        sumNAN <- 0
        sumINF <- 0
      }
      tmpSums <- list( sumNA = sumNA[[1]],
                       sumNAN = sumNAN[[1]],
                       sumINF = sumINF[[1]])
      tmpSumsByCol <- list(tmpColName = c(tmpSums))
      names(tmpSumsByCol) <- inputCol
      chkIndDFList <- append(chkIndDFList,tmpSumsByCol)
      count <- count + 1
    }
    tmpDFSUMCOLS <- list(tmpDFName = c(chkIndDFList))
    names(tmpDFSUMCOLS) <- checkDFVarNames[[posn]]
    chkedAllDFList<- append(chkedAllDFList,tmpDFSUMCOLS)
  }
  return(tmpDFSUMCOLS)
}

matrixNANANINF <- function(inputListsSums) {
  matrixDF <- matrix(NA, nrow = length(inputListsSums[[1]]), ncol = length(inputListsSums[[1]][[1]]))
  rownames(matrixDF) <- names(inputListsSums[[1]])
  colnames(matrixDF) <-names(inputListsSums[[1]][[1]])
  for (row in 1:length(inputListsSums[[1]])) {
    for (col in 1:length(inputListsSums[[1]][[1]])) {
      matrixDF[c(row),c(col)] <- inputListsSums[[1]][[row]][[col]]
    }
  }
  return(matrixDF)
}
# 
# accPerson <- vicRoadsDFList$PERSON.csv %>%
#   select(ACCIDENT_NO,PERSON_ID, VEHICLE_ID,SEX, AGE_GROUP,ROAD_USER_TYPE_DESC)
# 
# accPerson %<>% mutate(AGE_GROUP = factor(AGE_GROUP, ordered = TRUE,
#                                          levels= c("0-4","5-12","13-15","16-17","18-21",
#                                                    "22-25","26-29","30-39","40-49",
#                                                    "50-59","60-64","65-69","70+","Unknown"))) %>%
#   filter(ROAD_USER_TYPE_DESC %in% c("Drivers","Motorcyclists")) %>%
#   group_by( by = ACCIDENT_NO) %>%
#   pivot_wider(names_from = PERSON_ID, values_from = AGE_GROUP, names_prefix = "persId",names_sort = TRUE ) %>%
#   ungroup()
# 
# 
# chkDF4  <- checkNAInCols(list(accPerson), c("accPerson"))
# # Utility Function hidden to save page space. Code available on request.
# matrixDF4 <- matrixNANANINF(chkDF4)
# names(chkDF4)
# View(accPerson)

accPerson <- vicRoadsDFList$PERSON.csv %>%
  select(ACCIDENT_NO,PERSON_ID, VEHICLE_ID,SEX, AGE_GROUP,ROAD_USER_TYPE_DESC)
str(accPerson)
accPerson %<>% mutate(AGE_GROUP = factor(AGE_GROUP, ordered = TRUE,
                                         levels= c("0-4","5-12","13-15","16-17","18-21",
                                                   "22-25","26-29","30-39","40-49",
                                                   "50-59","60-64","65-69","70+","Unknown"))) %>%
  filter(ROAD_USER_TYPE_DESC %in% c("Drivers","Motorcyclists"))%>%
  group_by( by = ACCIDENT_NO) %>%
  pivot_wider(names_from = PERSON_ID, values_from = AGE_GROUP, 
              names_prefix = "persId",names_sort = TRUE )%>%
  ungroup()
accVehicle <- vicRoadsDFList$VEHICLE.csv %>%
  select(ACCIDENT_NO,VEHICLE_ID,VEHICLE_YEAR_MANUF, VEHICLE_MAKE,VEHICLE_MODEL, VEHICLE_POWER,
         VEHICLE_TYPE_DESC, NO_OF_CYLINDERS) %>%
  mutate(NO_OF_CYLINDERS = as.integer(NO_OF_CYLINDERS),
         VEHICLE_YEAR_MANUF = as.integer(VEHICLE_YEAR_MANUF))

str(accVehicle)
accVehicle %<>% select(-(VEHICLE_POWER))

accCombDF <- left_join(accPerson, accVehicle, by= c("ACCIDENT_NO","VEHICLE_ID")) %>%
             pivot_longer(cols = c(6:30),names_to = "Dvr_Id", values_to = "Age_Group" ) %>%
             filter(!is.na(Age_Group)) %>%
             select(-c("VEHICLE_ID","Dvr_Id","by"))
str(accCombDF)
colnames(vicRoadsDFList[[1]])
accDateTimeDay <- vicRoadsDFList$ACCIDENT.csv %>%
  select(ACCIDENT_NO,ACCIDENT_DATE,ACCIDENT_TIME,DAY_OF_WEEK,DAY_WEEK_DESC)
accDateTimeDay %<>% mutate( Timestamp = lubridate::ymd_hms(paste(ACCIDENT_DATE,ACCIDENT_TIME),
                                                           tz = "Australia/Melbourne"),
                            Full_Day = factor(DAY_WEEK_DESC, ordered = TRUE,
                                              levels= c("Monday","Tuesday","Wednesday","Thursday",
                                                        "Friday","Saturday","Sunday" )),
                            Time_As_Float = round((as.numeric(format(Timestamp,"%H")) + 
                                                     (as.numeric(format(Timestamp,"%M")))/60), digits = 2) ) %>%
  rename(Date_char = ACCIDENT_DATE , Time_char = ACCIDENT_TIME) %>%
  select(-c("DAY_OF_WEEK","DAY_WEEK_DESC"))
accCombDF <- left_join(accCombDF, accDateTimeDay, by= c("ACCIDENT_NO"))

accNode <- vicRoadsDFList$NODE.csv %>%
  select(ACCIDENT_NO,LGA_NAME,LATITUDE,LONGITUDE)
accCombDF <- left_join(accCombDF, accNode, by= c("ACCIDENT_NO"), relationship = "many-to-many")
colnames(accCombDF) <- c("Id","Gender","Dvr/Cyclist", "Veh_Year", "Veh_Make", "Veh_Model", "Veh_Type",
                         "Cylinders","Age_Group", "Date_char","Time_char" , "Timestamp",
                         "Full_Day","Time_Float","LGA_Name", "Latitude", "Longitude")

combDFChk <- checkNAInCols(list(accCombDF), c("accCombDF"))
matCHK <- matrixNANANINF(combDFChk)
unique(accPerson$ACCIDENT_NO)
x <- setdiff(unique(accPerson$ACCIDENT_NO), unique(accNode$ACCIDENT_NO))
length(x)

missingAccNo <- accCombDF %>%
                filter(Id %in% x)

table(accCombDF$Age_Group)
ageChk <- accCombDF%>% 
          filter(Age_Group %in% c("0-4","5-12","13-15"))
               
accCombDF$Age_Group%>%
   boxplot()


ggplot(accCombDF) + geom_boxplot(aes(x="",y=Latitude))

ageGpDF <- accCombDF %>%
           select(Age_Group) %>%
           group_by(by = Age_Group) %>%
           mutate(Obs =  1)%>%
           summarise(freq = sum(Obs))

ggplot(accCombDF)+
  geom_bar(aes(x= Age_Group))

ggplot(accCombDF)+
  geom_boxplot(aes(x=Longitude, y= Latitude), outliers = TRUE,notch= TRUE)

ggplot(accCombDF)+
  geom_boxplot(aes(x=Age_Group, y= Time_Float), outliers = TRUE,notch= TRUE)

ggplot(accCombDF)+
  geom_point(aes(x=Age_Group, y= Time_Float))

ggplot(accCombDF)+
  geom_point(aes(x=Time_Float, y= Age_Group, colour =Gender))

ggplot(accCombDF)+
  geom_histogram(aes(x= Time_Float))

as.numeric("9")


# vehMakes <- unique(accVehicle$VEHICLE_MAKE)
# vehMakes
# 
# vehModel <- unique(accVehicle$VEHICLE_MODEL)
# vehModel
# 
# accLGA <- unique(accCombDF$LGA_Name)
# accLGA


councilsVic_sf <- read_sf("../../Councils/georef-australia-local-government-area.geojson") %>% 
                  filter (ste_name == "Victoria")
unique(councilsVic_sf$lga_name)

plot(st_geometry(councilsVic_sf))

parse_number(strsplit(councilsVic_sf$geo_point_2d[[1]],split=":")[[1]][[2]])


parse_number(strsplit(councilsVic_sf$geo_point_2d[[1]],split=":")[[1]][[3]])

councilsVic_sf["Longitude"] <- sapply(councilsVic_sf$geo_point_2d, function(in2d) parse_number(strsplit(in2d,split=":")[[1]][[2]]))
councilsVic_sf["Latitude"] <- sapply(councilsVic_sf$geo_point_2d, function(in2d) parse_number(strsplit(in2d,split=":")[[1]][[3]]))

testOSMPlus1 <- councilsVic_sf[,c("Latitude", "Longitude")]

df1 <- st_as_sf(testOSMPlus1, crs=  "+proj=lonlat")

df_merc1<- st_transform(df1, 4326)

dc1 <- get_tiles(df_merc1, provider = "OpenStreetMap", zoom = 8)

ggplot() +
  geom_spatraster_rgb(data = dc1) +
  geom_sf(data = df1,aes( geometry = geometry), size =0.3, alpha= 0) +
  coord_sf(crs = 4326)
str (accCombDF)


#install.packages("geosphere" ,dependencies = TRUE)

gpoLatLong <- c(144.9627,-37.125)

accCombDF$LONGITUDE[[1]]

dist1 <- geosphere::distVincentyEllipsoid(gpoLatLong, c(accCombDF$Longitude[[1]],accCombDF$Latitude[[1]]))
dist1

lat <- c( -39.2,-34)
long <- c(140, 150)

distToPointAcc <- function(fromLongLat, toLongLat) {
  
  calcDist <- geosphere::distVincentyEllipsoid(fromLongLat, toLongLat)
  return(calcDist)
}
#First Check If Long and Lat in -180 to 180, -90 to 90

chkLongLatEarth <- accCombDF %>% 
  filter((Longitude <= -180 | Longitude >= 180) | 
           (Latitude <= -90 | Latitude >= 90))

gpoMelbLatLong <- c(144.9627,-37.125)

accCombDF %<>% mutate(Dist_GPO =case_when( !((is.na(Longitude) | is.na(Latitude))) ~      
                                             mapply(function(inLong,inLat)
                                               distToPointAcc(gpoMelbLatLong,c(inLong,inLat)),
                                               Longitude,Latitude)) )

VICLATLONG <- c( lat,long) 
dayLabelsLevels <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday" ) 
chkId <- accCombDF %>% filter(!str_detect(accCombDF$Id,"^T2")) 
str_detect(accCombDF$Id,"^T2")
chkGender <- accCombDF %>% filter(!accCombDF$Gender %in% c("M","F"))

accCombDF %<>% filter(accCombDF$Gender %in% c("M","F"))
chkDvr <- accCombDF %>% filter(!`Dvr/Cyclist` %in% c("Drivers","Motorcyclists"))

chkVehYear <- accCombDF %>% filter(!(between(Veh_Year,1908,as.numeric(format(Sys.Date(),"%Y")))))


vehYearMean <- round(mean(accCombDF$Veh_Year, na.rm = TRUE),digits = 0)
accCombDF %<>% mutate(Veh_Year = case_when(!(between(Veh_Year,1908,as.numeric(format(Sys.Date(),"%Y"))))
                                           ~ vehYearMean,
                                           TRUE ~ Veh_Year))
chkCyl1 <- accCombDF %>% filter(!(Cylinders >= 1 & Cylinders <= 12)) 

accCombDF %<>% mutate(Cylinders = case_when((Cylinders >= 60 & Cylinders < 70) ~ 6,
                                            (Cylinders == 88) ~ 8,
                                            (Id == "T20140005298" |Id == "T20210010866" ) ~ 4,
                                            (Id == "T20140015002"| Id == "T20180015095" |
                                               Id == "T20190004933" | Id == "T20230015484") ~ 6,
                                            (Id == "T20170016461") ~ 4,
                                            (Id == "T20150002025" |Id == "T20160020971"|
                                               Id == "T20180001536" |Id == "T20230019609"  ) ~2,
                                            TRUE ~ Cylinders
))
chkCyl2 <- accCombDF %>% filter(is.na(Cylinders))

#unique(chkCyl2$Veh_Type)
accCombDF %<>% mutate(Cylinders = case_when((str_detect(Veh_Type,"Prime|Heavy|Truck") & 
                                               is.na(Cylinders)) ~ 8,
                                            str_detect(Veh_Type, "Train|Tram|Horse|Parked") 
                                            ~ 0,
                                            (str_detect(Veh_Type, "Car|Taxi|Light") &
                                               is.na(Cylinders)) ~ 4,
                                            (str_detect(Veh_Type, "Panel|Station|Utility|Other") &
                                               is.na(Cylinders)) ~ 6,
                                            (str_detect(Veh_Type, "Bus|Plant") &
                                               is.na(Cylinders)) ~ 6,
                                            (str_detect(Veh_Type, "Moped|Cycle|Scooter|Quad") &
                                               is.na(Cylinders)) ~ 2,
                                            (str_detect(Veh_Type, "Car|Not|") &
                                               is.na(Cylinders)) ~ 4,
                                            TRUE ~ Cylinders
))
#Age Group showing 0 NA not checked. Would show NA as factor.
# Next temporal attributes show no na, nan if check for range
chkDateChar <- accCombDF %>% filter(!(between(Date_char, ymd("2012-01-01"),now()))) 

ChkTimeChar <- accCombDF %>% filter(!(between(hour(Time_char),0,24)))

#Timestamp created as local Melbourne
chkTimestamp <- accCombDF %>% filter(!(between(Timestamp, ymd_hms("2011-12-31 24:00:0",
                                                                  tz  = "Australia/Melbourne"),now()))) 

chkDay <- accCombDF %>% filter(!(Full_Day %in% dayLabelsLevels ))

chkTMFLoat <- accCombDF %>% filter(!(between(Time_Float,0.00, 24.00)))

chkLGAName <- accCombDF %>% filter(!(LGA_Name %in% str_to_upper(namesLGAVic )))

unique(chkLGAName$LGA_Name)
apineLGA <- c("(MOUNT HOTHAM)", "(FALLS CREEK)","(MOUNT BAW BAW)")
mansLGA <- c("MOUNT BULLER ALPINE RESOR","(MOUNT BULLER)","(MOUNT STIRLING)" )
accCombDF %<>% mutate(LGA_Name = case_when(LGA_Name == "GEELONG" ~ "GREATER GEELONG",
                                           LGA_Name == "BENDIGO" ~ "GREATER BENDIGO",
                                           LGA_Name == "DANDENONG"~ "GREATER DANDENONG",
                                           LGA_Name == "SHEPPARTON"~ "GREATER SHEPPARTON",
                                           LGA_Name %in% apineLGA ~ "ALPINE",
                                           LGA_Name %in% mansLGA ~ "MANSFIELD",
                                           LGA_Name == "(LAKE MOUNTAIN)"~ "MURRINDINDI",
                                           LGA_Name == "(MOUNT BAW BAW)" ~ "BAW BAW",
                                           LGA_Name == "(FRENCH ISLAND)" ~ "UNINCORPORATED VIC",
                                           TRUE ~ LGA_Name))
chkLat <- accCombDF %>% filter( is.na(Latitude) | !(between(Latitude,VICLATLONG[[1]],VICLATLONG[[2]])))

chkLong <- accCombDF %>% filter(is.na(Longitude) |!(between(Longitude,VICLATLONG[[3]], VICLATLONG[[4]])))

barAgeGp <- ggplot(accCombDF) +
            geom_bar(aes(x = Age_Group)) +
            theme(axis.text.x = element_text(face = "bold",
                                             colour = "blue",angle= 90))
histDist <- ggplot(accCombDF) +
            geom_histogram(aes(x= Dist_GPO))+
            scale_x_continuous(name= "Distance Kms",labels = function(x) x/1000 )
histCyl <- ggplot(accCombDF)+ 
           geom_histogram(aes(x = Cylinders))
outDist <- ggplot(accCombDF) + 
           geom_boxplot(aes( y =Dist_GPO)) + 
           theme(axis.text.y = element_text(face = "bold", colour = "blue",angle= 45),
                 axis.title.y = element_text(angle = 0, vjust = 0.5)) +
           scale_y_continuous(name= "Distance Kms",labels = function(x) x/1000 )
figure1 <- ggarrange(barAgeGp,histDist,histCyl,outDist,
                     nrow = 2, ncol =2)
figure1

BanPerson <- vicRoadsDFList$PERSON.csv %>%
  select(ACCIDENT_NO,PERSON_ID, VEHICLE_ID,SEX, AGE_GROUP,ROAD_USER_TYPE_DESC) %>% 
  mutate(AGE_GROUP = factor(AGE_GROUP, ordered = TRUE, levels= c("0-4","5-12","13-15","16-17","18-21",
         "22-25","26-29","30-39","40-49", "50-59","60-64","65-69","70+","Unknown")))
str(BanPerson)

BanVehicle <- vicRoadsDFList$VEHICLE.csv %>%
  select(ACCIDENT_NO,VEHICLE_ID,VEHICLE_YEAR_MANUF, VEHICLE_MAKE,VEHICLE_MODEL, VEHICLE_POWER,
         VEHICLE_TYPE_DESC, NO_OF_CYLINDERS) %>%
  mutate(NO_OF_CYLINDERS = as.integer(NO_OF_CYLINDERS),
         VEHICLE_YEAR_MANUF = as.integer(VEHICLE_YEAR_MANUF))

BanCombDF <- left_join(accPerson, accVehicle, by= c("ACCIDENT_NO","VEHICLE_ID"))

BanDateTimeDay <- vicRoadsDFList$ACCIDENT.csv %>%
  select(ACCIDENT_NO,ACCIDENT_DATE,ACCIDENT_TIME,DAY_OF_WEEK,DAY_WEEK_DESC)
BanDateTimeDay %<>% mutate( Timestamp = lubridate::ymd_hms(paste(ACCIDENT_DATE,ACCIDENT_TIME),
                                                           tz = "Australia/Melbourne"),
                            Full_Day = factor(DAY_WEEK_DESC, ordered = TRUE,
                                              levels= c("Monday","Tuesday","Wednesday","Thursday",
                                                        "Friday","Saturday","Sunday" )),
                            Time_As_Float = round((as.numeric(format(Timestamp,"%H")) + 
                                                     (as.numeric(format(Timestamp,"%M")))/60), digits = 2) ) %>%
  rename(Date_char = ACCIDENT_DATE , Time_char = ACCIDENT_TIME) %>%
  select(-c("DAY_OF_WEEK","DAY_WEEK_DESC"))
BanCombDF <- left_join(BanCombDF, BanDateTimeDay, by= c("ACCIDENT_NO"))

BanNode <- vicRoadsDFList$NODE.csv %>%
  select(ACCIDENT_NO,LGA_NAME,LATITUDE,LONGITUDE)
BanCombDF <- left_join(BanCombDF, BanNode, by= c("ACCIDENT_NO"), relationship = "many-to-many")

unique(BanCombDF$LGA_NAME)
BanCombDF %<>% filter(LGA_NAME == "BANYULE") %>% select(-c("by"))
#pivot_longer(cols = c(6:30),names_to = "Dvr_Id", values_to = "Age_Group" )
BanCombDF %<>% pivot_longer(cols = c(5:29),names_to = "Pers_Id", values_to = "Age_Group") %>%
                         filter(!(is.na(Age_Group)))

banCoucilVic_sf <- councilsVic_sf %>% filter(lga_name == "Banyule")
banCoucilVic_sf["Longitude"] <- sapply(banCoucilVic_sf$geo_point_2d, function(in2d) parse_number(strsplit(in2d,split = ":")[[1]][[2]]))
banCoucilVic_sf["Latitude"] <- sapply(banCoucilVic_sf$geo_point_2d, function(in2d) parse_number(strsplit(in2d,split = ":")[[1]][[3]]))
testOSMPlus2 <- banCoucilVic_sf[,c("Latitude", "Longitude")]
df2 <- st_as_sf(testOSMPlus2, crs=  "+proj=lonlat")
df_merc2<- st_transform(df2, 4326)
dc2 <- get_tiles(df_merc2, provider = "OpenStreetMap", zoom = 11)
# maleAccDf <- accCombDF %>% filter(Gender == "M" & !(is.na(Dist_GPO))) %>% select(Dist_GPO,Age_Group, Longitude,Latitude)
mycolours = c(brewer.pal(name="BuPu", n = 9)[4:7], brewer.pal(name="Blues", n = 9)[5:9],brewer.pal(name="Oranges", n = 9)[4:9])
banAllMap <- ggplot() + geom_spatraster_rgb(data = dc2) +
  geom_sf(data = df2,aes( geometry = geometry), size =0.3, alpha= 0, color ="red") + 
  geom_point(data = BanCombDF, aes(x = LONGITUDE, y = LATITUDE,colour= Age_Group), size = 0.1 ) +
  scale_color_manual(values = mycolours) + coord_sf(crs = 4326)
ggsave("banmap.svg" ,plot= banAllMap,width = 40, height = 40, units = "cm", device ="svg")

