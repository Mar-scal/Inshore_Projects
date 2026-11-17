library (ROracle)
library (tidyverse)

#uid <- keyring::key_list("Oracle")[1,2]
#pwd <- keyring::key_get("Oracle", uid)

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

#Recruit and prerecuit data are < 80 mm Shell height and are taken from lined gear - scallsur.sclinedlive 
#Db Query:
quer2 <- "
  SELECT s.*, t.gear_type_ID, t.tow_date, t.tow_len
  FROM scallsur.sclinedlive s
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
  mutate(rec = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE)) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE)) %>% # Pre-recruit scallop - BIN_ID_0:BIN_ID_60
  unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE)  #Creates ID column with cruise and tow number
 
 
table(ScallopSurv$year)
summary(ScallopSurv)

#remove SHF bins & other fields not needed 
dim(ScallopSurv)
head(ScallopSurv[,14:53])
head(ScallopSurv[,c(1:13, 54:61)])
ScallopSurv <- ScallopSurv[,c(1:13, 54:61)]
ScallopSurv <- ScallopSurv %>% select(-DIST_FR_SHORE, -GEOPHYS_ID) 

#Calculated actual gear width sampled for scallop 
ScallopSurv$gear_width_sampled_m <- (ScallopSurv$NUM_LINED_FREQ)*ScallopSurv$DRAG_WIDTH*0.3048 # NUM_LINED_FREQ == number of lined drags measured; NUM_UNLINED_FREQ = number of unlined drags measured ; 1 foot = 0.3048 m ; #NOTE: NUM_LINED_FREQ doesn't apply to commerical size since commerical size only counted from unlined gear 

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
  dplyr::select(ID, CRUISE, TOW_NO, GEAR_TYPE_ID, SPA, STRATA_ID, TOW_DATE, year, lat, lon, rec, pre, Area_Swept_m2) 

summary(ScallopSurv.BOF)
unique(ScallopSurv.BOF$SPA)

#Separate SPAs for files

#SPA1A, 1B, and 4
ScallopSurv.SPA1A.1B.4 <- ScallopSurv.BOF %>% 
  filter(SPA %in% c("SPA1A", "SPA1B", "SPA4"))
unique(ScallopSurv.SPA1A.1B.4$SPA)

write.csv(ScallopSurv.SPA1A.1B.4, "W:/InshoreScallop/scallop_abundance/BoF_TowLevel_Numbers_Rec_Pre_SPA1A_1B_4_1981-2026.csv", row.names = F)

#SPA3
ScallopSurv.SPA3 <- ScallopSurv.BOF %>% 
  filter(SPA == "SPA3")
unique(ScallopSurv.SPA3$SPA)
unique(ScallopSurv.SPA3$year)

write.csv(ScallopSurv.SPA3, "W:/InshoreScallop/scallop_abundance/BoF_TowLevel_Numbers_Rec_Pre_SPA3_1991-2026.csv", row.names = F)

#SPA6
ScallopSurv.SPA6 <- ScallopSurv.BOF %>% 
  filter(SPA == "SPA6")
unique(ScallopSurv.SPA6$SPA)
unique(ScallopSurv.SPA6$year)

write.csv(ScallopSurv.SPA6, "W:/InshoreScallop/scallop_abundance/BoF_TowLevel_Numbers_Rec_Pre_SPA6_1997-2026.csv", row.names = F)


# -----------------------------Import SFA29W SHF data (live)--------------------------------------------

##.. SFA29W ..## 

#Different size class breakout - so ran separate from BOF.

quer1 <- "
    SELECT s.*, t.gear_type_ID, t.tow_date, t.tow_len
  FROM scallsur.sclinedlive s
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
unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE) %>% #Creates ID column with cruise and tow number
mutate(rec = dplyr::select(., BIN_ID_90:BIN_ID_95) %>% rowSums(na.rm = TRUE)) %>%  # Recruit scallop - 90-99; BINS 90 to 95
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_85) %>% rowSums(na.rm = TRUE))# Pre-recruit scallop - 0-85 mm; BINS 0 to 85

#remove SHF bins & other fields not needed 
dim(ScallopSurv.29W)
head(ScallopSurv.29W[,14:53])
head(ScallopSurv.29W[,c(1:13, 54:61)])
ScallopSurv.29W <- ScallopSurv.29W[,c(1:13, 54:61)]
ScallopSurv.29W <- ScallopSurv.29W %>% select(-DIST_FR_SHORE, -GEOPHYS_ID) 


#Calculated actual gear width sampled for scallop 
ScallopSurv.29W$gear_width_sampled_m <- (ScallopSurv.29W$NUM_LINED_FREQ)*ScallopSurv.29W$DRAG_WIDTH*0.3048 # NUM_LINED_FREQ == number of lined drags measured; NUM_UNLINED_FREQ = number of unlined drags measured ; 1 foot = 0.3048 m ; #NOTE: NUM_LINED_FREQ doesn't apply to commerical size since commerical size only counted from unlined gear 

ScallopSurv.29W$Area_Swept_m2 <- ScallopSurv.29W$gear_width_sampled_m*ScallopSurv.29W$TOW_LEN

head(ScallopSurv.29W)

ScallopSurv.29W
table(ScallopSurv.29W$year, ScallopSurv.29W$NUM_LINED_FREQ)
summary(ScallopSurv.29W)

#NUMBERS
ScallopSurv.SFA29W <- ScallopSurv.29W %>%
  mutate(SPA = "SFA29W") %>% 
  dplyr::select(ID, CRUISE, TOW_NO, GEAR_TYPE_ID, SPA, STRATA_ID, TOW_DATE, year, lat, lon, rec, pre, Area_Swept_m2) 

summary(ScallopSurv.SFA29W)
unique(ScallopSurv.SFA29W$SPA)
head(ScallopSurv.SFA29W)

write.csv(ScallopSurv.SFA29W, "W:/InshoreScallop/scallop_abundance/SFA29W_TowLevel_Numbers_rec_pre_SFA29W_1992-2026.csv", row.names = F)

