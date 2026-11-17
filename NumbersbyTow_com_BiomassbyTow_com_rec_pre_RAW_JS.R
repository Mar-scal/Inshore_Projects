library (ROracle)
library (tidyverse)

uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

uid <- un.sameotoj
pwd <- pw.sameotoj


#set year 
survey.year <- 2026 #survey year
assessmentyear <- 2026 #year in which you are providing advice for- determines where to save files to
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

# ----Import Source functions and polygons---------------------------------------------------------------------

#### Import Mar-scal functions 
funcs <- c(#"https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
  "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
  #"https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r"
) 

dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}
# -----------------------------Import BOF SHF data (live)--------------------------------------------

##.. BOF ..## 

#Commercial data >=80 mm -- take from unlined gear - scallsur.scunlinedlive# 
#Db Query:
quer2 <- "
  SELECT s.*, t.gear_type_ID, t.tow_date, t.tow_len
  FROM scallsur.scunlinedlive s
  LEFT JOIN SCALLSUR.sctows t 
    ON s.cruise = t.cruise 
   AND s.tow_no = t.tow_no
  WHERE s.strata_id IN (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
                        11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
                        22, 23, 24, 30, 31, 32, 35, 37, 38, 39, 
                        47, 48, 49, 50, 51, 52, 53, 54, 55, 56)
"

#If ROracle: 
ScallopSurv <- dbGetQuery(chan, quer2)
head(ScallopSurv)
dim(ScallopSurv)

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and commercial, recruit and prerecruit data columns
ScallopSurv <- ScallopSurv %>% 
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  mutate(com = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE)) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195 
  unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE)  #Creates ID column with cruise and tow number
 
 
table(ScallopSurv$year)
summary(ScallopSurv)

#remove SHF bins & other fields not needed 
dim(ScallopSurv)
head(ScallopSurv[,14:53])
head(ScallopSurv[,c(1:13, 54:60)])
ScallopSurv <- ScallopSurv[,c(1:13, 54:60)]
ScallopSurv <- ScallopSurv %>% select(-DIST_FR_SHORE, -GEOPHYS_ID) 

#Calculated actual gear width sampled for scallop 
ScallopSurv$gear_width_sampled_m <- (ScallopSurv$NUM_UNLINED_FREQ)*ScallopSurv$DRAG_WIDTH*0.3048 # NUM_LINED_FREQ == number of lined drags measured; NUM_UNLINED_FREQ = number of unlined drags measured ; 1 foot = 0.3048 m ; #NOTE: NUM_LINED_FREQ doesn't apply to commerical size since commerical size only counted from unlined gear 

ScallopSurv$Area_Swept_m2 <- ScallopSurv$gear_width_sampled_m*ScallopSurv$TOW_LEN

head(ScallopSurv)

#NUMBERS
ScallopSurv.BOF <- ScallopSurv %>%
  mutate(SPA = case_when(STRATA_ID %in% c(6,  7, 11, 12, 13, 14, 15, 16, 17, 18, 19, 39, 48, 56) ~ "SPA1A",
                       STRATA_ID %in% c(35, 37, 38, 49, 51, 50, 52, 53, 54, 55) ~ "SPA1B",
                       STRATA_ID %in% c(26,57) ~ "SPA2",
                       STRATA_ID %in% c(22,23,24) ~ "SPA3",
                       STRATA_ID %in% c(1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 20, 47) ~ "SPA4",
                       STRATA_ID %in% c(30,31,32) ~ "SPA6")) %>% 
  dplyr::select(ID, CRUISE, TOW_NO, GEAR_TYPE_ID, SPA, STRATA_ID, TOW_DATE, year, lat, lon, com, Area_Swept_m2) 

summary(ScallopSurv.BOF)
unique(ScallopSurv.BOF$SPA)

#Separate SPAs for files

#SPA1A, 1B, and 4
ScallopSurv.SPA1A.1B.4 <- ScallopSurv.BOF %>% 
  filter(SPA %in% c("SPA1A", "SPA1B", "SPA4"))
unique(ScallopSurv.SPA1A.1B.4$SPA)

write.csv(ScallopSurv.SPA1A.1B.4, "W:/InshoreScallop/scallop_abundance/BoF_TowLevel_Numbers_Com_SPA1A_1B_4_1981-2026.csv", row.names = F)

#SPA3
ScallopSurv.SPA3 <- ScallopSurv.BOF %>% 
  filter(SPA == "SPA3")
unique(ScallopSurv.SPA3$SPA)
unique(ScallopSurv.SPA3$year)

write.csv(ScallopSurv.SPA3, "W:/InshoreScallop/scallop_abundance/BoF_TowLevel_Numbers_Com_SPA3_1991-2026.csv", row.names = F)

#SPA6
ScallopSurv.SPA6 <- ScallopSurv.BOF %>% 
  filter(SPA == "SPA6")
unique(ScallopSurv.SPA6$SPA)
unique(ScallopSurv.SPA6$year)

write.csv(ScallopSurv.SPA6, "W:/InshoreScallop/scallop_abundance/BoF_TowLevel_Numbers_Com_SPA6_1997-2026.csv", row.names = F)


# -----------------------------Import SFA29W SHF data (live)--------------------------------------------

##.. SFA29W ..## 

#Different size class breakout - so ran separate from BOF.

quer1 <- "
    SELECT s.*, t.gear_type_ID, t.tow_date, t.tow_len
  FROM scallsur.scunlinedlive s
  LEFT JOIN SCALLSUR.sctows t 
    ON s.cruise = t.cruise 
   AND s.tow_no = t.tow_no
  WHERE s.strata_id IN (41, 42, 43, 44, 45)
"
#ROracle: 
ScallopSurv.29W <- dbGetQuery(chan, quer1)
#ScallopSurv.29W  <- ScallopSurv.29W[,1:52] #Remove duplicate column and filter Cruise SFA29 for survey year
#filter(grepl(paste0("SFA29",survey.year), CRUISE, fixed = TRUE))

#setup data 
ScallopSurv.29W <- ScallopSurv.29W %>%
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date - works with SFA29
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  mutate(com = dplyr::select(., BIN_ID_100:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% 
unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE)  #Creates ID column with cruise and tow number

#remove SHF bins & other fields not needed 
dim(ScallopSurv.29W)
head(ScallopSurv.29W[,14:53])
head(ScallopSurv.29W[,c(1:13, 54:60)])
ScallopSurv.29W <- ScallopSurv.29W[,c(1:13, 54:60)]
ScallopSurv.29W <- ScallopSurv.29W %>% select(-DIST_FR_SHORE, -GEOPHYS_ID) 


#Calculated actual gear width sampled for scallop 
ScallopSurv.29W$gear_width_sampled_m <- (ScallopSurv.29W$NUM_UNLINED_FREQ)*ScallopSurv.29W$DRAG_WIDTH*0.3048 # NUM_LINED_FREQ == number of lined drags measured; NUM_UNLINED_FREQ = number of unlined drags measured ; 1 foot = 0.3048 m ; #NOTE: NUM_LINED_FREQ doesn't apply to commerical size since commerical size only counted from unlined gear 

ScallopSurv.29W$Area_Swept_m2 <- ScallopSurv.29W$gear_width_sampled_m*ScallopSurv.29W$TOW_LEN

head(ScallopSurv.29W)

ScallopSurv.29W
table(ScallopSurv.29W$year, ScallopSurv.29W$NUM_UNLINED_FREQ)
summary(ScallopSurv.29W)

#NUMBERS
ScallopSurv.SFA29W <- ScallopSurv.29W %>%
  mutate(SPA = "SFA29W") %>% 
  dplyr::select(ID, CRUISE, TOW_NO, GEAR_TYPE_ID, SPA, STRATA_ID, TOW_DATE, year, lat, lon, com, Area_Swept_m2) 

summary(ScallopSurv.SFA29W)
unique(ScallopSurv.SFA29W$SPA)

write.csv(ScallopSurv.SFA29W, "W:/InshoreScallop/scallop_abundance/SFA29W_TowLevel_Numbers_Com_SFA29W_1992-2026.csv", row.names = F)

# -----------------------------Import BOF Biomass data --------------------------------------------

#BIOMASS

#SPA1A 1B and 4

max.yr <- assessmentyear#max(na.omit(ScallopSurv$year))
Year <- seq((max.yr-43),max.yr)
Year <- Year[! Year %in% 2020] #No 2020 data - remove from data query.
num.years <- length(Year)

#SPA1A1B4and5
BFliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BFliveweight",Year[i],".csv",sep=""), header=T)
  BFliveweight <- rbind(BFliveweight,temp)
}

liveweight <- BFliveweight
summary(liveweight)
#check data
head(liveweight)


#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and biomass for commercial, recruit and prerecruit data columns
ScallopSurv.kg <- liveweight %>%
  dplyr::rename(year = YEAR) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com.bm = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195 /1000
  mutate(rec.bm = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75 /1000
  mutate(pre.bm = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) # Pre-recruit scallop - BIN_ID_0:BIN_ID_60 /1000

#Add in Gear_type_id & actual gear area 
ScallopSurv.kg <- ScallopSurv.kg %>%
  left_join(ScallopSurv %>% select(ID, GEAR_TYPE_ID,  Area_Swept_m2), by = "ID")

#table(test$GEAR_TYPE_ID,  useNA = "ifany")
#ScallopSurv %>% filter(CRUISE == "GM2001" & TOW_NO == 315)

table(ScallopSurv.kg$GEAR_TYPE_ID,  useNA = "ifany")

ScallopSurv.kg.new <- ScallopSurv.kg %>% 
  filter(STRATA_ID != 21) %>% 
  mutate(SPA = case_when(STRATA_ID %in% c(6,  7,  11, 12, 13, 14, 15, 16, 17, 18, 19,  39, 48, 56) ~ "SPA1A",
                         STRATA_ID %in% c(35, 37, 38, 49, 51, 50, 52, 53, 54, 55) ~ "SPA1B",
                         STRATA_ID %in% c(26,57) ~ "SPA2",
                         STRATA_ID %in% c(22,23,24) ~ "SPA3",
                         STRATA_ID %in% c(1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 20, 47) ~ "SPA4",
                         STRATA_ID %in% c(30,31,32) ~ "SPA6")) %>% 
  select(ID, CRUISE, TOW_NO, GEAR_TYPE_ID, SPA, STRATA_ID, year, lat, lon, com.bm, rec.bm, pre.bm,  Area_Swept_m2)

head(ScallopSurv.kg.new)

#Area_Swept_m2 is true area swept but biomass value are associated with biomass for an area of 800 m x 17.5 feet = 800*5.334 =  4267.2 m2 = 4267.2/1*10^-6 km2 -- need to convert biomass values back to equivalent value with area swept 

ScallopSurv.kg.new$AreaStd_m2 <- 4267.2
ScallopSurv.kg.new$ratio <- ScallopSurv.kg.new$Area_Swept_m2/ScallopSurv.kg.new$AreaStd_m2
ScallopSurv.kg.new$com.bm.orig <- ScallopSurv.kg.new$ratio*ScallopSurv.kg.new$com.bm
ScallopSurv.kg.new$rec.bm.orig <- ScallopSurv.kg.new$ratio*ScallopSurv.kg.new$rec.bm
ScallopSurv.kg.new$pre.bm.orig <- ScallopSurv.kg.new$ratio*ScallopSurv.kg.new$pre.bm
head(ScallopSurv.kg.new)

ScallopSurv.kg.new <- ScallopSurv.kg.new %>% select(ID, CRUISE, TOW_NO, GEAR_TYPE_ID, SPA, STRATA_ID, year, lat, lon, com.bm.orig,  rec.bm.orig, pre.bm.orig, Area_Swept_m2) 

summary(ScallopSurv.kg.new)
unique(ScallopSurv.kg.new$SPA)
head(ScallopSurv.kg.new)
table(ScallopSurv.kg.new$GEAR_TYPE_ID)

write.csv(ScallopSurv.kg.new, "W:/InshoreScallop/scallop_biomass/BoF_TowLevel_Biomass_ComRecPre_SPA1A_1B_4_1983-2026.csv", row.names = F)


#SPA3

max.yr <- assessmentyear#max(na.omit(ScallopSurv$year))
Year <- seq((max.yr-30),max.yr)
Year <- Year[! Year %in% 2020] #No 2020 data - remove from data query.
num.years <- length(Year)

BIliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3/BIliveweight",Year[i],".csv",sep=""), header=T)
  BIliveweight <- rbind(BIliveweight,temp)
}

liveweight <- BIliveweight
summary(liveweight)
#check data
head(liveweight)

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and biomass for commercial, recruit and prerecruit data columns
ScallopSurv.kg <- liveweight %>%
  dplyr::rename(year = YEAR) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD
   unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com.bm = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195 /1000
  mutate(rec.bm = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75 /1000
  mutate(pre.bm = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) # Pre-recruit scallop - BIN_ID_0:BIN_ID_60 /1000

ScallopSurv.kg <- ScallopSurv.kg %>%
  left_join(ScallopSurv %>% select(ID, GEAR_TYPE_ID, Area_Swept_m2), by = "ID")

ScallopSurv.kg.new <- ScallopSurv.kg %>% 
  filter(STRATA_ID != 21) %>% 
  mutate(SPA = case_when(STRATA_ID %in% c(6,  7, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 39, 48, 56) ~ "SPA1A",
                         STRATA_ID %in% c(35, 37, 38, 49, 51, 50, 52, 53, 54, 55) ~ "SPA1B",
                         STRATA_ID %in% c(26,57) ~ "SPA2",
                         STRATA_ID %in% c(22,23,24) ~ "SPA3",
                         STRATA_ID %in% c(1,  2 , 3,  4,  5,  6,  7,  8,  9, 47) ~ "SPA4",
                         STRATA_ID %in% c(30,31,32) ~ "SPA6")) %>% 
  select(ID, CRUISE, TOW_NO, GEAR_TYPE_ID, SPA, STRATA_ID, year, lat, lon, com.bm, rec.bm, pre.bm, Area_Swept_m2) 


ScallopSurv.kg.new$AreaStd_m2 <- 4267.2
ScallopSurv.kg.new$ratio <- ScallopSurv.kg.new$Area_Swept_m2/ScallopSurv.kg.new$AreaStd_m2
ScallopSurv.kg.new$com.bm.orig <- ScallopSurv.kg.new$ratio*ScallopSurv.kg.new$com.bm
ScallopSurv.kg.new$rec.bm.orig <- ScallopSurv.kg.new$ratio*ScallopSurv.kg.new$rec.bm
ScallopSurv.kg.new$pre.bm.orig <- ScallopSurv.kg.new$ratio*ScallopSurv.kg.new$pre.bm
head(ScallopSurv.kg.new)

ScallopSurv.kg.new <- ScallopSurv.kg.new %>% select(ID, CRUISE, TOW_NO, GEAR_TYPE_ID, SPA, STRATA_ID, year, lat, lon, com.bm.orig,  rec.bm.orig, pre.bm.orig, Area_Swept_m2) 


summary(ScallopSurv.kg.new)
unique(ScallopSurv.kg.new$SPA)
head(ScallopSurv.kg.new)

write.csv(ScallopSurv.kg.new, "W:/InshoreScallop/scallop_biomass/BoF_TowLevel_Biomass_ComRecPre_SP3_1996-2026.csv", row.names = F)


#SPA6

max.yr <- assessmentyear#max(na.omit(ScallopSurv$year))
Year <- seq((max.yr-21),max.yr)
Year <- Year[! Year %in% 2020] #No 2020 data - remove from data query.
num.years <- length(Year)


GMliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6/GMliveweight",Year[i],".csv",sep=""), header=T)
  GMliveweight <- rbind(GMliveweight,temp)
}

liveweight <- GMliveweight
summary(liveweight)
#check data
head(liveweight)

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and biomass for commercial, recruit and prerecruit data columns
ScallopSurv.kg <- liveweight %>%
  dplyr::rename(year = YEAR) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com.bm = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195 /1000
  mutate(rec.bm = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75 /1000
  mutate(pre.bm = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) # Pre-recruit scallop - BIN_ID_0:BIN_ID_60 /1000

ScallopSurv.kg <- ScallopSurv.kg %>%
  left_join(ScallopSurv %>% select(ID, GEAR_TYPE_ID, Area_Swept_m2 ), by = "ID")

ScallopSurv.kg.new <- ScallopSurv.kg %>% 
  filter(STRATA_ID != 21) %>% 
  mutate(SPA = case_when(STRATA_ID %in% c(6,  7, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 39, 48, 56) ~ "SPA1A",
                         STRATA_ID %in% c(35, 37, 38, 49, 51, 50, 52, 53, 54, 55) ~ "SPA1B",
                         STRATA_ID %in% c(26,57) ~ "SPA2",
                         STRATA_ID %in% c(22,23,24) ~ "SPA3",
                         STRATA_ID %in% c(1,  2 , 3,  4,  5,  6,  7,  8,  9, 47) ~ "SPA4",
                         STRATA_ID %in% c(30,31,32) ~ "SPA6")) %>% 
  select(ID, CRUISE, TOW_NO, GEAR_TYPE_ID, SPA, STRATA_ID, year, lat, lon, com.bm, rec.bm, pre.bm, Area_Swept_m2)

ScallopSurv.kg.new$AreaStd_m2 <- 4267.2
ScallopSurv.kg.new$ratio <- ScallopSurv.kg.new$Area_Swept_m2/ScallopSurv.kg.new$AreaStd_m2
ScallopSurv.kg.new$com.bm.orig <- ScallopSurv.kg.new$ratio*ScallopSurv.kg.new$com.bm
ScallopSurv.kg.new$rec.bm.orig <- ScallopSurv.kg.new$ratio*ScallopSurv.kg.new$rec.bm
ScallopSurv.kg.new$pre.bm.orig <- ScallopSurv.kg.new$ratio*ScallopSurv.kg.new$pre.bm
head(ScallopSurv.kg.new)

ScallopSurv.kg.new <- ScallopSurv.kg.new %>% select(ID, CRUISE, TOW_NO, GEAR_TYPE_ID, SPA, STRATA_ID, year, lat, lon, com.bm.orig,  rec.bm.orig, pre.bm.orig, Area_Swept_m2) 


head(ScallopSurv.kg.new)
summary(ScallopSurv.kg.new)
unique(ScallopSurv.kg.new$SPA)

write.csv(ScallopSurv.kg.new, "W:/InshoreScallop/scallop_biomass/BoF_TowLevel_Biomass_ComRecPre_SP6_2005-2026.csv", row.names = F)



# -----------------------------Import SFA29W Biomass data --------------------------------------------

#SFA29W

#this was ran before the 2026 survey was completed, so need to update years
assessmentyear <- 2026
survey.year <- 2025 

ScallopSurv.kg <- read.csv(paste0("Y:/Inshore/Assessment/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29liveweight2014to",survey.year,".csv"))  

# 2014 to current year
ScallopSurv.kg <- ScallopSurv.kg %>% dplyr::select(-X) %>%  #removes index column X
  dplyr::rename(year = YEAR) %>%
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE) %>% 
  mutate(com.bm = dplyr::select(., BIN_ID_100:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Commercial scallop - >=100mm; BINS 100 to 195
  mutate(rec.bm = dplyr::select(., BIN_ID_90:BIN_ID_95) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Recruit scallop - 90-99; BINS 90 to 95
  mutate(pre.bm = dplyr::select(., BIN_ID_0:BIN_ID_85) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>%  # Pre-recruit scallop - 0-85 mm; BINS 0 to 85
  mutate(SPA = "SFA29W") %>% 
  left_join(ScallopSurv.29W %>% select(ID, GEAR_TYPE_ID,   Area_Swept_m2), by = "ID") %>% 
  select(ID, CRUISE, TOW_NO, GEAR_TYPE_ID, SPA, STRATA_ID, year, lat, lon, com.bm, rec.bm, pre.bm, Area_Swept_m2) 

head(ScallopSurv.kg)
ScallopSurv.kg$AreaStd_m2 <- 4267.2
ScallopSurv.kg$ratio <- ScallopSurv.kg$Area_Swept_m2/ScallopSurv.kg$AreaStd_m2
ScallopSurv.kg$com.bm.orig <- ScallopSurv.kg$ratio*ScallopSurv.kg$com.bm
ScallopSurv.kg$rec.bm.orig <- ScallopSurv.kg$ratio*ScallopSurv.kg$rec.bm
ScallopSurv.kg$pre.bm.orig <- ScallopSurv.kg$ratio*ScallopSurv.kg$pre.bm
head(ScallopSurv.kg)

ScallopSurv.kg <- ScallopSurv.kg %>% select(ID, CRUISE, TOW_NO, GEAR_TYPE_ID, SPA, STRATA_ID, year, lat, lon, com.bm.orig,  rec.bm.orig, pre.bm.orig, Area_Swept_m2) 



summary(ScallopSurv.kg)
unique(ScallopSurv.kg$SPA)

write.csv(ScallopSurv.kg, "W:/InshoreScallop/scallop_biomass/SFA29W_TowLevel_Biomass_ComRecPre_SFA29W_2014-2025.csv", row.names = F)
  
