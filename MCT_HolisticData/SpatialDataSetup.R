# --------------------------------------------------------------------------#
#                     Setting Data up for Spatial Analyses
#            (run this script first if spatial data is not yet created)
#
#                          G. English, Dec 2025
#---------------------------------------------------------------------------#

## |--------------Set up libraries and holistic data ---------------------------|####
#load packages
require (CircStats)
require (TeachingDemos)
require (PBSmapping)
require (akima)
require (gstat)
require (fields)
require (splancs)
require (RColorBrewer)
require (spatstat)
require (RPMG)
require (rmapshaper)
require (lubridate)
require (tidyverse)
require (sf)
require(maptools)
require(forcats)
library(ROracle)
require(ggspatial)
require(concaveman)
require(lme4)
require(readxl)
require(openxlsx)

#ROracle
uid <- un.englishg
pwd <- pw.englishg
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#set year 
survey.year <- 2026  #survey year
area <- "BF" #SPA6, BF, SFA29, SPA2
cruise.year <- paste0(area,survey.year)
path.directory <- "Y:/Projects/Holistic_sampling_Inshore/"

#set up directory to save plot
saveplot.dir <- paste0(path.directory,"Holistic_sampling_",area,"/",survey.year,"/Figures/")
saveplot.dir.fr <- paste0(saveplot.dir,"FrenchFigures_indicies/")

#load holistic data
data <- read_excel(paste0(path.directory,"Holistic_sampling_",area,"/",survey.year,"/Data/",area,"_",survey.year,"_HolisticSamples.xlsx"), sheet=2) 
metadata <- read_excel(paste0(path.directory,"Holistic_sampling_",area,"/",survey.year,"/Data/",area,"_",survey.year,"_HolisticSamples.xlsx"), sheet=1)

# Data formatting 
names(data)
str(data)
dim(data)
#609


#format data - sort into factors and numeric
cols.factor <- c("Cruise","Tow", "Scallop_ID", "Boring_worm",  "Shock_marks", "Meat_colour", "Mycobacteria", "Sex", "Maturity_stage")
cols.numeric <- c("Shell_height_mm", "Shell_weight_g",  "Whole_weight_g", "Meat_weight_g", "Gonad_weight_g")

data[,cols.factor] <- lapply(data[,cols.factor],as.factor)
data[,cols.numeric] <- lapply(data[,cols.numeric], function(x){ as.numeric(x)})
str(data)

# read in depth information and add it to the holistic anaylsis dataframe
# there is no strata_id in Olex file to select on, need a unique identifier for tows to link to depth ($ID)
data$ID <- paste(data$Cruise, data$Tow,sep='.')
uniqueID <- unique(data$ID)
## This is unique IDs for database 

#get depth standardized to mean water level for all tows 
OlexTows_all <- read.csv("Y:/Inshore/Assessment/StandardDepth/towsdd_StdDepth.csv")
names(OlexTows_all)[which(colnames(OlexTows_all)=="RASTERVALU")] <- "DEPTH_M"   #rename "RASTERVALU" column
OlexTows_all$DEPTH_M[OlexTows_all$DEPTH_M==-9999] <- NA
OlexTows_all$ID <- paste(OlexTows_all$CRUISE,OlexTows_all$TOW_NO,sep='.')
OlexTows_bof <- subset(OlexTows_all, ID%in%uniqueID)
#should match length(uniqueID)
dim(OlexTows_bof)

dim(data)
dat.all <- merge(data,subset(OlexTows_bof,select=c("ID","DDSlat","DDSlon","DEPTH_M")), all.x=T)
dim(dat.all) #should only increase by 3 columns (i.e. depth, lat, long)
#609 samples from GM2024
head(dat.all)

dat.all$ADJ_DEPTH <- -1*dat.all$DEPTH_M

#save as xlsx file with spatial data ####
#Create a new workbook
wb <- createWorkbook()
# Add first sheet for metadata
addWorksheet(wb, "Metadata")
writeData(wb, "Metadata", metadata)
# Add second sheet for data
addWorksheet(wb, paste0(area,"_",year,"_HolisticSamples"))
writeData(wb, paste0(area,"_",year,"_HolisticSamples"), dat.all)

saveWorkbook(wb, paste0(path.directory,"Holistic_sampling_",area,"/",survey.year,"/Data/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), overwrite = TRUE)

# check tow lengths ####
#Db Query:
quer2 <- paste(
  "SELECT * 			                ",
  "FROM SCALLSUR.sctows			",
  # "where strata_id in (30, 31, 32)    ",
  sep=""
)


#If ROracle:
dat.tows <- dbGetQuery(chan, quer2)
dat.tows  <- dat.tows[,1:18]

dat.tows <- dat.tows %>%
  filter(CRUISE == cruise.year) %>%
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date
  mutate(lat.s = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon.s = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  mutate(lat.e = convert.dd.dddd(END_LAT)) %>% #Convert to DD
  mutate(lon.e = convert.dd.dddd(END_LONG)) %>%
  dplyr::rename(Tow = TOW_NO) %>%
  unite(ID, c("CRUISE", "Tow"), sep = ".", remove = FALSE)  #Creates ID column with cruise and tow number

# Create line geometries
lines_sf <- dat.tows %>%
  rowwise() %>%
  mutate(geometry = st_sfc(st_linestring(matrix(c(lon.s, lon.e, lat.s, lat.e), ncol = 2)), crs = 4326)) %>%
  st_as_sf()

# Transform to projected CRS for length calculation
lines_sf <- st_transform(lines_sf, 32620)

# Calculate length in meters
lines_sf <- lines_sf %>%
  mutate(length_m = st_length(geometry)) %>%
  st_drop_geometry()

dat.all <- merge(dat.all,subset(lines_sf,select=c("ID","length_m")), all.x=T)
#all holistic tows are between 701-803m in length
