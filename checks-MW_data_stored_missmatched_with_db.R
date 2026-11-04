
library(tidyverse)
library(sf)
library(ROracle)

#This script is looking into missmatch between number of tows for cruises "UB1998" "BI2001" "GM2001" and "RF2012" and the number of tows in the SCTOWS database


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


uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#Load data (file was created using BFliveweight.csvs)
bm.BF <- read.csv("W:/InshoreScallop/scallop_biomass/BoF_TowLevel_Biomass_ComRecPre_SPA1A_1B_4_1983-2026.csv")


NA.geartype <- bm.BF %>% filter(is.na(GEAR_TYPE_ID))


NA.geartype <- NA.geartype %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

unique(NA.geartype$CRUISE)
#[1] "UB1998" "BI2001" "GM2001" "RF2012" "BF2023"
ids <- unique(NA.geartype$ID)


mapview::mapview(NA.geartype)

mapview::mapview(NA.geartype %>% filter(CRUISE == "UB1998")) #Upper bay (SPA1B)
mapview::mapview(NA.geartype %>% filter(CRUISE == "BI2001")) #These tows are in SPA4
mapview::mapview(NA.geartype %>% filter(CRUISE == "GM2001")) #These tows are in SPA1B
mapview::mapview(NA.geartype %>% filter(CRUISE == "RF2012")) #These tows are in SPA1A and 4?
mapview::mapview(NA.geartype %>% filter(CRUISE == "BF2023")) #Just a single tow - likely missed when entering (In Spencer's Island strata)


#Now Look at SCTOWS db
quer2 <- "
  SELECT *
  FROM SCALLSUR.sctows"
  #WHERE s.strata_id IN (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
  #                      11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
  #                      22, 23, 24, 30, 31, 32, 35, 37, 38, 39, 
  #                      47, 48, 49, 50, 51, 52, 53, 54, 55, 56)
#"

all.tows <- dbGetQuery(chan, quer2)

#tows.w.NA.geartype <- all.tows %>% filter(is.na(GEAR_TYPE_ID)) #Jamie fixed these in the db

problem.tows <- all.tows %>% 
  unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE) %>% 
  filter(ID %in% ids)

unique(problem.tows$CRUISE)
[1] "UB1998" "BF2023"  #tows from CRUISES "BI2001" "GM2001" "RF2012" not entered into the database?


check.cruises <- all.tows %>% filter(CRUISE %in% c("BI2001", "GM2001", "RF2012"))


check.cruise.RF2012 <- all.tows %>% filter(CRUISE == "RF2012") %>% 
  filter(TOW_NO %in% c(380,381,382,383,328)) #No data available

check.cruise.BI2001 <- all.tows %>% filter(CRUISE == "BI2001") %>% 
  filter(TOW_NO %in% c(303:308)) #No data available,   #could be re-labled as Tows 149-154 (these are the tows from cruise BI2001 in file BoF_TowLevel_Numbers_Com_SPA1A_1B_4_1981-2026.csv) 

check.cruise.GM2001 <- all.tows %>% filter(CRUISE == "GM2001") %>% 
  filter(TOW_NO %in% c(309:350)) #No data available  #could be re-labled as Tows 94-135 (these are the tows from cruise GM2001 in file BoF_TowLevel_Numbers_Com_SPA1A_1B_4_1981-2026.csv) 

check.cruise.UB1998 <- all.tows %>% filter(CRUISE == "UB1998") %>% 
  filter(TOW_NO %in% c(1:20, 23:35, 45:70)) #No data available


#BF2001
BF2001 <- all.tows %>% filter(CRUISE == "BF2001")

