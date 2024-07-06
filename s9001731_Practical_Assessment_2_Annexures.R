## ----chunkGlobal, message=FALSE, warning=FALSE, include=FALSE, excho= FALSE---------------------------------------------------------------
knitr::opts_chunk$set(strip.white = TRUE,
                      comment = "")


## ----setup, message=FALSE, warning=FALSE, include=FALSE-----------------------------------------------------------------------------------
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
library(purrr)
library(sf)
library(maptiles)
library(tidyterra)
library(OpenStreetMap)
library(geosphere)
library(scales)
library(egg)
library(outliers)
library(MVN)
library(forecast)
#install.packages("mvoutlier")
library(mvoutlier)


# Remove files to await update

loaded_pkg <- c((.packages())) # Get loaded packages
if (file.exists("bibliography.bibtex")) {
  file.remove("bibliography.bibtex")
}
if (file.exists("tmp.bibtex")) {
  file.remove("tmp.bibtex")
}




## ----wrap-hook, message=FALSE, warning=FALSE,  echo = FALSE-------------------------------------------------------------------------------
# Taken from https://github.com/yihui/knitr-examples/blob/master/077-wrap-output.md
library(knitr)
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = xfun::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})



## ----chunk-hook, message=FALSE, warning=FALSE,  echo = FALSE------------------------------------------------------------------------------
#https://stackoverflow.com/questions/74914584/setting-code-chunk-size-on-quarto-pdf-output
default_chunk_hook  <- knitr::knit_hooks$get("chunk")

latex_font_size <- c("Huge", "huge", "LARGE", "Large", 
                     "large", "normalsize", "small", 
                     "footnotesize", "scriptsize", "tiny")

knitr::knit_hooks$set(chunk = function(x, options) {
  x <- default_chunk_hook(x, options)
  if (options$size %in% latex_font_size) {
    paste0("\n \\", options$size, "\n\n", 
      x, 
      "\n\n \\normalsize"
    )
  } else {
    x
  }
})



## ----CoverSheet,echo= FALSE, message=FALSE, warning=FALSE, out.width= "50%", out.height = "50%",fig.pos='H'-------------------------------
  include_graphics("assignment-cover-sheet_Page_1.png")
  include_graphics("assignment-cover-sheet_Page_2.png")


## ----studentNameTable, echo=FALSE---------------------------------------------------------------------------------------------------------
na<- c(" Mark Randall")
no<- c(" s9001731")
pc<- c("100")

s<- data.frame(cbind(na,no,pc))
colnames(s)<- c("Student name", "Student number", "Percentage of contribution")

s %>% kbl(caption = "Group information") %>%
  kable_styling(latex_options = "HOLD_position")%>%
  kable_classic(full_width = F, html_font = "Cambria")
  


## ----displayPackageRef, echo= FALSE, results="asis"---------------------------------------------------------------------------------------

# Code snippet to list all loaded packages and reference them
# Iterate over vector to add a citation key, convert to Bibtex, and store

for (pkg in loaded_pkg) {
  tmp <- citation(pkg, auto = TRUE) # Get citation
  tmp$key <- paste0("R-",pkg) # Add cite key
  tmp_bib <- toBibtex(tmp) # Convert to Bibtex
  write(tmp_bib,file = "tmp.bibtex", append = TRUE, sep = "\n") # Write to file
} # write_bib, and write.bib did not do what I wanted.
#ENDNOTE_FILE <- read.bib("endnote_dw.txt") 
ans_copy_file <- file.copy("endnote_dw.txt","bibliography.bibtex")
ans_App_file <- file.append("bibliography.bibtex","tmp.bibtex")

count <- 1 # Establish package number
# Iterate over package list to add citation notation for R Markdown, and produce Markdown text.
for (pkg in loaded_pkg) {
  cite_text <- paste0(" [@R-",pkg,"]") # Create citation notation
  #Output R markdown 
  cat(paste0("Package ", count," : ", pkg ," ",cite_text,"  ")) # Double space at the end of a line forces \n
  cat("\n") # Not executed in rendering R-Markdown, see above
  count <- count + 1 # Increment package number 
}
rm(pkg,loaded_pkg)


## ----hiddenInputs,echo=FALSE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, messages= FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='asis',collapse= TRUE,size = "tiny",fig.align="left"----

csvFileNames <- list.files("../../Data",pattern = "*.csv", full.names = TRUE)
fullFileNames <- list.files("../../Data", full.names = TRUE)

#Create data frames fro csv files
vicRoadsDFList <- sapply(csvFileNames, function(incsv) read_csv(incsv,show_col_types = FALSE))
#Change Key of list to more HR form
names(vicRoadsDFList) <- c(gsub("../../Data/","", names(vicRoadsDFList)))

councilsVic_sf <- read_sf("../../Councils/georef-australia-local-government-area.geojson") %>% 
                  filter (ste_name == "Victoria")
namesLGAVic <- unique(councilsVic_sf$lga_name) %>%
               append(., "Merri-bek")



## ----utilityFunc, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='markup',collapse= TRUE,size = "tiny",fig.align="left"----
# Global Utility functions could be removed to source file.
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
    chkedAllDFList <- append(chkedAllDFList,tmpDFSUMCOLS)
  }
  return(tmpDFSUMCOLS)
}

matrixNANANINF <- function(inputListsSums) {
  matrixDF <- matrix(NA, nrow = length(inputListsSums[[1]]), ncol = length(inputListsSums[[1]][[1]]))
  rownames(matrixDF) <- names(inputListsSums[[1]])
  colnames(matrixDF) <- names(inputListsSums[[1]][[1]])
  for (row in 1:length(inputListsSums[[1]])) {
    for (col in 1:length(inputListsSums[[1]][[1]])) {
      matrixDF[c(row),c(col)] <- inputListsSums[[1]][[row]][[col]]
    }
  }
  return(matrixDF)
}

dispHead <- function(chkAttrDf) {
  if (dim(chkAttrDf)[[1]] > 0) {
     res <- head(chkAttrDf,5)
  } else{
    res <- cat(paste(deparse(substitute(chkAttrDf)),"is empty.\n"))
  }
  return(res)
}

lat <- c( -39.2,-34)
long <- c(140, 150)

VICLATLONG <- c( lat,long) 


## ----accDateTimeDay, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='markup',collapse= TRUE,size = "tiny",fig.align="left"----
accDateTimeDay <- vicRoadsDFList$ACCIDENT.csv %>%
  select(ACCIDENT_NO,ACCIDENT_DATE,ACCIDENT_TIME,DAY_OF_WEEK,DAY_WEEK_DESC)
str(accDateTimeDay)
# Utility Function hidden to save page space. Code available on request.
chkDF1  <- checkNAInCols(list(accDateTimeDay), c("accDateTimeDay"))
# Utility Function hidden to save page space. Code available on request.
matrixDF1 <- matrixNANANINF(chkDF1)
matrixDF1 %>% kable(caption = glue("Table of Sums of NA,NAN, and Inf for {names(chkDF1)}"),longtable = TRUE,
                format = "latex", booktabs = TRUE) %>%
          kable_styling(font_size = 8)
dayLabelsLevels <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday" ) 
accDateTimeDay %<>% mutate( Timestamp = lubridate::ymd_hms(paste(ACCIDENT_DATE,ACCIDENT_TIME),
                                                   tz = "Australia/Melbourne"),
                            Full_Day = factor(DAY_WEEK_DESC, ordered = TRUE,
                                                 levels = dayLabelsLevels,
                                                 labels = dayLabelsLevels),
                            Time_As_Float = round((as.numeric(format(Timestamp,"%H")) + 
                                            (as.numeric(format(Timestamp,"%M")))/60), digits = 2) ) %>%
                      rename(Date_char = ACCIDENT_DATE , Time_char = ACCIDENT_TIME) %>%
                    select(-c("DAY_OF_WEEK","DAY_WEEK_DESC"))
str(accDateTimeDay)
n_distinct(accDateTimeDay$ACCIDENT_NO)
rm(matrixDF1,chkDF1)


## ----accNode, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='markup',collapse= TRUE,size = "tiny",fig.align="left"----
accNode <- vicRoadsDFList$NODE.csv %>%
  select(ACCIDENT_NO,LGA_NAME,LATITUDE,LONGITUDE)
str(accNode)
# Utility Function hidden to save page space. Code available on request.
chkDF2  <- checkNAInCols(list(accNode), c("accNode"))
# Utility Function hidden to save page space. Code available on request.
matrixDF2 <- matrixNANANINF(chkDF2)
matrixDF2 %>% kable(caption = glue("Table of Sums of NA,NAN, and Inf for {names(chkDF2)}"),longtable = TRUE,
                format = "latex", booktabs = TRUE) %>%
          kable_styling(font_size = 8) 
str(accNode)
n_distinct(accNode$ACCIDENT_NO)
rm(matrixDF2,chkDF2)


## ----accPers, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='markup',collapse= TRUE,size = "tiny",fig.align="left"----
accPerson <- vicRoadsDFList$PERSON.csv %>%
  select(ACCIDENT_NO,PERSON_ID, VEHICLE_ID,SEX, AGE_GROUP,ROAD_USER_TYPE_DESC)
str(accPerson)
# Utility Function hidden to save page space. Code available on request.
chkDF3  <- checkNAInCols(list(accPerson), c("accPerson"))
# Utility Function hidden to save page space. Code available on request.
matrixDF3 <- matrixNANANINF(chkDF3)
matrixDF3 %>% kable(caption = glue("Table of Sums of NA,NAN, and Inf for {names(chkDF3)}"),
                    longtable = TRUE,format = "latex", booktabs = TRUE) %>%
              kable_styling(font_size = 8) 
str(accPerson)
n_distinct(accPerson$ACCIDENT_NO)
unique(accPerson$AGE_GROUP)
accPerson %<>% mutate(AGE_GROUP = factor(AGE_GROUP, ordered = TRUE,
                                             levels = c("0-4","5-12","13-15","16-17","18-21",
                                                       "22-25","26-29","30-39","40-49",
                                                       "50-59","60-64","65-69","70+","Unknown"))) %>%
              filter(ROAD_USER_TYPE_DESC %in% c("Drivers","Motorcyclists")) %>%
              group_by( by = ACCIDENT_NO) %>%
              pivot_wider(names_from = PERSON_ID, values_from = AGE_GROUP, 
                               names_prefix = "persId",names_sort = TRUE ) %>%
              ungroup()
# Utility Function hidden to save page space. Code available on request.
chkDF4  <- checkNAInCols(list(accPerson), c("accPerson"))
# Utility Function hidden to save page space. Code available on request.
matrixDF4 <- abs(-t(matrixNANANINF(chkDF4)))
matrixDF4[,1:10] %>% kable(caption = glue("Table of Sums of NA,NAN, and Inf for {names(chkDF4)} Wider cols 1 to 10 "),
                    longtable = TRUE,format = "latex", booktabs = TRUE) %>%
              kable_styling(font_size = 6)
matrixDF4[,11:20] %>% kable(caption = glue("Table of Sums of NA,NAN, and Inf for {names(chkDF4)} cols 11 to 20"),
                    longtable = TRUE,format = "latex", booktabs = TRUE) %>%
              kable_styling(font_size = 6)
matrixDF4[,21:30] %>% kable(caption = glue("Table of Sums of NA,NAN, and Inf for {names(chkDF4)} cols 21 to 30"),
                    longtable = TRUE,format = "latex", booktabs = TRUE) %>%
              kable_styling(font_size = 6)
rm(matrixDF3,matrixDF4,chkDF4,chkDF3)


## ----accVeh, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='markup',collapse= TRUE,size = "tiny",fig.align="left"----
accVehicle <- vicRoadsDFList$VEHICLE.csv %>%
              select(ACCIDENT_NO,VEHICLE_ID,VEHICLE_YEAR_MANUF, VEHICLE_MAKE,VEHICLE_MODEL, VEHICLE_POWER,
              VEHICLE_TYPE_DESC, NO_OF_CYLINDERS) %>%
              mutate(NO_OF_CYLINDERS = as.integer(NO_OF_CYLINDERS),
                     VEHICLE_YEAR_MANUF = as.integer(VEHICLE_YEAR_MANUF))
str(accVehicle)
# Utility Function hidden to save page space. Code available on request.
chkDF5  <- checkNAInCols(list(accVehicle), c("accVehicle"))
# Utility Function hidden to save page space. Code available on request.
matrixDF5 <- matrixNANANINF(chkDF5)
matrixDF5 %>% kable(caption = glue("Table of Sums of NA,NAN, and Inf for {names(chkDF5)}"),
                    longtable = TRUE,format = "latex", booktabs = TRUE) %>%
              kable_styling(font_size = 8)
vehMakes <- unique(accVehicle$VEHICLE_MAKE)
vehModel <- unique(accVehicle$VEHICLE_MODEL)
n_distinct(accVehicle$ACCIDENT_NO)
accVehicle %<>% select(-(VEHICLE_POWER))
rm(matrixDF5,chkDF5,vehMakes,vehModel)


## ----combPersVehDFPH1, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='markup',collapse= TRUE,size = "tiny",fig.align="left"----
accCombDF <- left_join(accPerson, accVehicle, by = c("ACCIDENT_NO","VEHICLE_ID")) %>%
             pivot_longer(cols = c(6:30),names_to = "Dvr_Id", values_to = "Age_Group" ) %>%
             filter(!is.na(Age_Group)) %>%
             select(-c("VEHICLE_ID","Dvr_Id","by"))
str(accCombDF)



## ----combPersVehDF, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE,warnings= FALSE, message= FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='markup',collapse= TRUE,size = "tiny",fig.align="left"----
accCombDF <- left_join(accCombDF, accDateTimeDay, by = c("ACCIDENT_NO"))
accCombDF <- left_join(accCombDF, accNode, by = c("ACCIDENT_NO"), relationship = "many-to-many") %>%
             distinct() 
colnames(accCombDF) <- c("Id","Gender","Dvr/Cyclist", "Veh_Year", "Veh_Make", "Veh_Model", "Veh_Type",
                         "Cylinders","Age_Group", "Date_char","Time_char" , "Timestamp",
                         "Full_Day","Time_Float","LGA_Name", "Latitude", "Longitude")

str(accCombDF)



## ----distGPO, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='markup',collapse= TRUE,size = "tiny",fig.align="left"----

distToPointAcc <- function(fromLongLat, toLongLat) {
  
  calcDist <- geosphere::distVincentyEllipsoid(fromLongLat, toLongLat)
  return(calcDist)
}
#First Check If Long and Lat in -180 to 180, -90 to 90

chkLongLatEarth <- accCombDF %>% 
                    filter((Longitude <= -180 | Longitude >= 180) | 
                          (Latitude <= -90 | Latitude >= 90))
dispHead(chkLongLatEarth)
gpoMelbLatLong <- c(144.9627,-37.125)

accCombDF %<>% mutate(Dist_GPO =case_when( !((is.na(Longitude) | is.na(Latitude))) ~      
                                          mapply(function(inLong,inLat)
                                              distToPointAcc(gpoMelbLatLong,c(inLong,inLat)),
                                              Longitude,Latitude)) )
n_distinct(accCombDF$Id)
# Utility Function hidden to save page space. Code available on request.
chkDF6  <- checkNAInCols(list(accCombDF), c("accCombDF"))
# Utility Function hidden to save page space. Code available on request.
matrixDF6 <- abs(-t(matrixNANANINF(chkDF6)))
matrixDF6[,1:9] %>% kable(caption = glue("Table of Sums of NA,NAN, and Inf for {names(chkDF6)} cols 1 to 9"),
                    longtable = TRUE,format = "latex", booktabs = TRUE) %>%
              kable_styling(font_size = 6)
matrixDF6[,10:17] %>% kable(caption = glue("Table of Sums of NA,NAN, and Inf for {names(chkDF6)} cols 10 to 18"),
                    longtable = TRUE,format = "latex", booktabs = TRUE) %>%
              kable_styling(font_size = 6)
missId <- setdiff(unique(accPerson$ACCIDENT_NO), unique(accNode$ACCIDENT_NO))
numMissing <- length(missId)

missingAccNo <- accCombDF %>%
                filter(Id %in% missId)
rm(matrixDF6,accDateTimeDay,accNode,accPerson, accVehicle,chkDF6)


## ----dataDictionary, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='markup',collapse= TRUE,size = "tiny",fig.align="left"----

accDFAttr <- colnames(accCombDF)
accDFType <- c("Character","Character", "Character", "Integer", "Character","Character","Character", "Integer",
               "Ordered Factor Character", "Date Format Y-m-d","hms num", "POSIXct","Ordered Factor Character",
               "Double","Character", "Double","Double","Double")
accDFDesc <- c("Unique Accident Identifier Starts with T followed by Year (T2024...).",
               "Gender of driver in accident.  Range M or F",
               "Whether Drivers or Motorcyclists.",
               "Year of Vehicle Manufacture.  Range 1908 to present",
               "Vehicle Manufacturer.",
               "Vehicle Model.",
               "Vehicle Body Type.",
               "Numbers of Cylinders of the vehicle. Range 1 to 12 (Special case for train, tram, horse, trailers = 0)",
               "Age Group of the driver.",
               "Date of Accident. Range 2012-01-01 to present",
               "Time of Accident. Range 0 to 12",
               "Timestamp. Range 2012-01-01 00:00:00.00 to present",
               "Day of Week. range Monday to Sunday",
               "Decimal time of Accident. Range 0 to 24",
               "Name of Local Government Authority.  See Victorian Government Source",
               "Latitude. Range -34 to -39.2",
               "Longitude. Range 140 to 150",
               "Distance Meters to Melb GPO. Greater or equal to 0")
accDFDataDict <- data.frame(cbind(accDFAttr, accDFType , accDFDesc))
colnames(accDFDataDict) <- c("Attribute", "Data Type", "Description")


accDFDataDict %>% kbl(caption = "Data Dictionary Victorian Road Accidents Distilled") %>% 
                  kable_styling( font_size= 8) %>%
                  column_spec(1,width="2.5cm") %>%
                  column_spec(2,width="2cm") %>%
                  column_spec(3,width="10cm") %>%
                  kable_classic(full_width = F)

rm(accDFAttr,accDFType,accDFDesc,accDFDataDict)


## ----attrChecks, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='markup',collapse= TRUE,size = "tiny",fig.align="left"----

chkId <- accCombDF %>% filter(!str_detect(accCombDF$Id,"^T2")) 
dispHead(chkId)
chkGender <- accCombDF %>% filter(!accCombDF$Gender %in% c("M","F"))
dispHead(chkGender)  # 1.93 percent so deleted 
accCombDF %<>% filter(accCombDF$Gender %in% c("M","F"))
chkDvr <- accCombDF %>% filter(!`Dvr/Cyclist` %in% c("Drivers","Motorcyclists"))
dispHead(chkDvr)
chkVehYear <- accCombDF %>% filter(!(between(Veh_Year,1908,as.numeric(format(Sys.Date(),"%Y")))))

dispHead(chkVehYear) # Replace with the mean easy to impute. But if time brute force.
vehYearMean <- round(mean(accCombDF$Veh_Year, na.rm = TRUE),digits = 0)
accCombDF %<>% mutate(Veh_Year = case_when(!(between(Veh_Year,1908,as.numeric(format(Sys.Date(),"%Y"))))
                                          ~ vehYearMean,
                                          TRUE ~ Veh_Year))
chkCyl1 <- accCombDF %>% filter(!(Cylinders >= 1 & Cylinders <= 12)) 
dispHead(chkCyl1) # Best done by brute force intuition.
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
dispHead(chkCyl2)
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
dispHead(chkDateChar)
ChkTimeChar <- accCombDF %>% filter(!(between(hour(Time_char),0,24)))
dispHead(ChkTimeChar)
#Timestamp created as local Melbourne
chkTimestamp <- accCombDF %>% filter(!(between(Timestamp, ymd_hms("2011-12-31 24:00:0",
                                                                  tz  = "Australia/Melbourne"),now()))) 
dispHead(chkTimestamp)
chkDay <- accCombDF %>% filter(!(Full_Day %in% dayLabelsLevels ))
dispHead((chkDay))
chkTMFLoat <- accCombDF %>% filter(!(between(Time_Float,0.00, 24.00)))
dispHead((chkTMFLoat))
chkLGAName <- accCombDF %>% filter(!(LGA_Name %in% str_to_upper(namesLGAVic )))
dispHead((chkLGAName))
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
dispHead((chkLat))
chkLong <- accCombDF %>% filter(is.na(Longitude) |!(between(Longitude,VICLATLONG[[3]], VICLATLONG[[4]])))
dispHead((chkLong))
rm(chkCyl1,chkCyl2,chkDateChar,chkDay,chkDvr,chkGender,chkId,chkLat,
   chkLong,chkLongLatEarth,chkLGAName,chkTimestamp,chkTMFLoat,ChkTimeChar,chkVehYear)                                     


## ----outliers, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='markup',collapse= TRUE,size = "tiny",fig.align="left",fig.cap= "Plots showing  distribution \nof attributes Of the DataFrame"----
basePlot <- ggplot(accCombDF) +
            theme( axis.title.y = element_text(angle = 0, vjust = 0.5))

barAgeGp <- basePlot +
            geom_bar(aes(x = Age_Group)) +
            ggtitle("Bar Plot Age Group")+
            theme(axis.text.x = element_text(face = "bold",
                                             colour = "blue",angle= 90))
barAgeGp2 <- accCombDF %>% filter(Age_Group %in% c("0-4","5-12","13-15","16-17"))%>%
            ggplot() +
            geom_bar(aes(x = Age_Group)) +
            theme( axis.title.y = element_text(angle = 0, vjust = 0.5)) +
            ggtitle("Bar Plot Age \nGroup <=17")+
            theme(axis.text.x = element_text(face = "bold",colour = "blue",angle= 90))
histTime <- basePlot+
            ggtitle("Histogram Time of day") +
            geom_histogram(aes(x = Time_Float)) +
            theme(axis.text.x = element_text(face = "bold",colour = "blue",angle= 90))
histDist <- basePlot +
            geom_histogram(aes(x= Dist_GPO))+
            ggtitle("Histogram Distance to GPO")+
            theme(axis.text.x = element_text(face = "bold", colour = "blue",angle= 45)) + 
            scale_x_continuous(name= "Distance Kms",labels = function(x) x/1000 )
histCyl <- basePlot+
           ggtitle("Histogram Cylinders")+
           geom_histogram(aes(x = Cylinders))
outDist <- basePlot + 
           geom_boxplot(aes( y =Dist_GPO)) + 
           ggtitle("Box Plot Distance to GPO")+  
           theme(axis.text.y = element_text(face = "bold", colour = "blue",angle= 45)) +
           scale_y_continuous(name= "Distance Kms",labels = function(x) x/1000 )
outDist2 <- basePlot + 
            geom_boxplot(aes(x= Age_Group, y =Dist_GPO, colour = Gender)) + 
            ggtitle("Box Plot Age/ Distance to GPO")+  
            theme(axis.text.y = element_text(face = "bold", colour = "blue",angle= 45)) +
            scale_y_continuous(name= "Distance Kms",labels = function(x) x/1000 )+
            theme(axis.text.x = element_text(face = "bold",
                                           colour = "blue",angle= 90))

figure1 <- grid.arrange(outDist2, barAgeGp, barAgeGp2, histTime, histDist, histCyl, outDist,
                        nrow = 3, ncol =3, layout_matrix = rbind(c(1,1,1),c(2,3,4),c(5,6,7)))


distZScores <- accCombDF %>% filter(!(is.na(Dist_GPO))) %>%
  select(Dist_GPO) %>%
  outliers::scores(type = c("z"))

summary(distZScores) 
length(which(abs(distZScores) > 3 ))
gcVar <- c()

#memory.size( TRUE)
# memory.limit(size = 800000)

maleAcc <- accCombDF %>% filter(Gender == "M" &  !(is.na(Dist_GPO))) %>%
  select(Time_Float,Dist_GPO)
#results <- mvn(data = maleAcc, multivariateOutlierMethod = "quan", showOutliers = TRUE)
# mvOutlier(data = maleAcc, qqplot = TRUE, alpha = 0.5, tol = 1e-25, method = c("quan", "adj.quan"),  label = TRUE, position = NULL, offset = 0.5)

baseDistGPODF <- accCombDF %>% filter(!(is.na(Dist_GPO))) %>%
  select(Dist_GPO)

library(moments)
moments::skewness(baseDistGPODF$Dist_GPO, na.rm = TRUE)

sqrtDist <- sqrt (baseDistGPODF)
cubertDist <- baseDistGPODF^(1/3)
recipDist <- 1/baseDistGPODF
bCDist <- BoxCox(baseDistGPODF, lambda = "auto")
log10Dist <- log10(baseDistGPODF)

basePlot2 <- ggplot(accCombDF) +
  theme( axis.title.y = element_text(angle = 0, vjust = 0.5))

histDist3 <- basePlot2 +
  geom_histogram(aes(x= Dist_GPO))+
  ggtitle("Histogram Distance to GPO")+
  theme(axis.text.x = element_text(face = "bold",
                                   colour = "blue",angle= 45)) + 
  scale_x_continuous(name= "Distance Kms",labels = function(x) x/1000 )
histDist3

distZScores1 <- sqrtDist %>%
  select(Dist_GPO) %>%
  outliers::scores(type = c("z"))

summary(distZScores1) 
length(which(abs(distZScores1) > 3 ))

distZScores2 <- cubertDist %>%
  select(Dist_GPO) %>%
  outliers::scores(type = c("z"))

summary(distZScores2) 
length(which(abs(distZScores2) > 3 ))

distZScores3 <- recipDist %>%
  select(Dist_GPO) %>%
  outliers::scores(type = c("z"))

summary(distZScores3) 
length(which(abs(distZScores3) > 3 ))

distZScores4 <- bCDist %>%
  select(Dist_GPO) %>%
  outliers::scores(type = c("z"))
summary(distZScores4) 
length(which(abs(distZScores4) > 3 ))

distZScores5 <- log10Dist %>%
  select(Dist_GPO) %>%
  outliers::scores(type = c("z"))
summary(distZScores5) 
length(which(abs(distZScores5) > 3 ))

meanAccDistTrans <- c(base::mean(unlist(distZScores1) ,na.rm = TRUE),
                      base::mean(unlist(distZScores2) ,na.rm = TRUE),
                      base::mean(unlist(distZScores3) ,na.rm = TRUE),
                      base::mean(unlist(distZScores4) ,na.rm = TRUE),
                      base::mean(unlist(distZScores5) ,na.rm = TRUE))
medianAccDistTrans <- c(stats::median(unlist(distZScores1) ,na.rm = TRUE),
                        stats::median(unlist(distZScores2) ,na.rm = TRUE),
                        stats::median(unlist(distZScores3) ,na.rm = TRUE),
                        stats::median(unlist(distZScores4) ,na.rm = TRUE),
                        stats::median(unlist(distZScores5) ,na.rm = TRUE))
sdAccDistTrans <- c(stats::sd(unlist(distZScores1) ,na.rm = TRUE),
                    stats::sd(unlist(distZScores2) ,na.rm = TRUE),
                    stats::sd(unlist(distZScores3) ,na.rm = TRUE),
                    stats::sd(unlist(distZScores4) ,na.rm = TRUE),
                    stats::sd(unlist(distZScores5) ,na.rm = TRUE))

skewAccDistTrans <- c(moments::skewness(distZScores1$Dist_GPO ,na.rm = TRUE),
                      moments::skewness(distZScores2$Dist_GPO ,na.rm = TRUE),
                      moments::skewness(distZScores3$Dist_GPO ,na.rm = TRUE),
                      moments::skewness(distZScores4$Dist_GPO ,na.rm = TRUE),
                      moments::skewness(distZScores5$Dist_GPO ,na.rm = TRUE))

numOutAccDistTrans <- c(length(which(abs(distZScores1) > 3 )),
                        length(which(abs(distZScores2) > 3 )),
                        length(which(abs(distZScores3) > 3 )),
                        length(which(abs(distZScores4) > 3 )),
                        length(which(abs(distZScores5) > 3 )))
transAccDF <- data.frame( transFun = c("Square root", "Cube Root", "Reciprocal", "BoxCox", "Log10"),
                          mean = meanAccDistTrans,
                          median = medianAccDistTrans,
                          std_dev = sdAccDistTrans,
                          skew = skewAccDistTrans,
                          Outlier_Num = numOutAccDistTrans)
base::mean(unlist(distZScores4) ,na.rm = TRUE)

ggplot(transAccDF)+ 
      geom_point(aes(x= transFun,y = mean))
## -----------------------------------------------------------------------------------------------------------------------------------------
# This is the R chunk for the Scan I



## -----------------------------------------------------------------------------------------------------------------------------------------
# This is the R chunk for the Scan II



## -----------------------------------------------------------------------------------------------------------------------------------------
# This is the R chunk for the Transform Section



## ----fileList,echo= TRUE, results="asis", tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100 ,size = "tiny"----------------
csvFileNames <- list.files("../../Data",pattern = "*.csv", full.names = TRUE)
fullFileNames <- list.files("../../Data", full.names = TRUE)

for (file in fullFileNames) {
  cat(paste("-\t", file,"\n"))
}
#Create data frames fro csv files
vicRoadsDFList <- sapply(csvFileNames, read.csv)
#Change Key of list to mor HR form
names(vicRoadsDFList) <- c(gsub("../../Data/","", names(vicRoadsDFList)))


## ----duplicateACCKEY,echo= TRUE, results="asis",tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100 ,size = "tiny"----------
posnOfDF <- 1
cat("These files have duplicated accident number keys in the data frame:")
for (tmpDF in vicRoadsDFList) {
  if (n_distinct(tmpDF$ACCIDENT_NO) < dim(tmpDF)[[1]]) {
    cat(paste("-\t",names(vicRoadsDFList)[[posnOfDF]],"\n"))
  }
  posnOfDF <- posnOfDF + 1
}


## ----dfColNames, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='markup',collapse= TRUE,size = "tiny",fig.align="left"----
vecColNamesDF <- list()
posnCN <- 1
for (df in vicRoadsDFList) {
  vecColNamesDF[[names(vicRoadsDFList)[[posnCN]]]] <-  colnames(df)
 
  posnCN <- posnCN + 1
}
#https://stackoverflow.com/questions/60199801/how-to-view-a-list-like-table-style-in-r
max_len <- max(lengths(vecColNamesDF))
df <- purrr::map_df(vecColNamesDF, ~ c(., rep('', max_len - length(.))))
df[,1:5] %>% 
          kable(caption = "Attributes Files 1-5",longtable = TRUE,
                format = "latex", booktabs = TRUE) %>%
          kable_styling(font_size = 5) 
df[,6:9] %>% 
          kable(caption = "Attributes Files 6-9",longtable = TRUE,
                format = "latex", booktabs = TRUE) %>%
          kable_styling(font_size = 5) 


## ----intersectDF, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='asis',collapse= TRUE,size = "tiny",fig.align="left"----
namesOfFile <- names(vecColNamesDF)
for (posnOne in 1:length(namesOfFile)) {
  cat(namesOfFile[[posnOne]],"at list no.",posnOne, "intersects with the following file:")
  cat("  \n")
  if (posnOne == length(namesOfFile)) {
    break
  }
  for (posnTwo in (posnOne + 1):length(namesOfFile)) {
   cat("-\t",namesOfFile[[posnTwo]],"at list no.",posnTwo," with these attributes:")
   cat("  \n")
   cat("\t\t-\t",intersect(vecColNamesDF[[posnOne]],vecColNamesDF[[posnTwo]]))
   cat("  \n")
    
    posnTwo <- posnTwo + 1
  }
  cat("  \n")
  posnOne <- posnOne + 1
  
}


## ----LGAVic, echo=TRUE,fig.pos='H',out.width= "100%",tidy=TRUE, keep.source =FALSE, tidy.opts=list(width.cutoff=55,keep.source=FALSE), linewidth= 100, results ='asis',collapse= TRUE,size = "tiny",fig.align="left"----
councilsVic_sf <- read_sf("../../Councils/georef-australia-local-government-area.geojson") %>% 
                  filter (ste_name == "Victoria")
namesLGAVic <- unique(councilsVic_sf$lga_name) %>%
               append(., "Merri-bek")


## ----purlChunk, echo = FALSE, messages = FALSE--------------------------------------------------------------------------------------------
#knitr::purl("s9001731_Practical_Assessment_2_Annexures.Rmd")
endOfFile <- "end of file"

data(iris)
setosa <- iris[1:50, 1:4]
mvnTest = "mardia"
#result <- mvn(data = setosa, mvnTest = "mardia")
#result$multivariateNormality
str(iris)
maleAcc <- accCombDF %>% filter(Gender == "M" &  !(is.na(Dist_GPO))) %>%
  select(Time_Float,Dist_GPO)

saveRDS(maleAcc, file = "testDF",)
testMale <- maleAcc[1:10000,]

#results <- mvn(data = as.data.frame(maleAcc), multivariateOutlierMethod = "quan", showOutliers = TRUE)
# 
str(maleAcc)

dd.plot(maleAcc,quan= 1/2, alpha = 0.025)
color.plot(as.data.frame (maleAcc),quan= 1/2, alpha = 0.025)

chisq.plot(maleAcc, quan = 0.5, ask = TRUE)

#install.packages("regclass", dependencies = TRUE)
library(regclass)

