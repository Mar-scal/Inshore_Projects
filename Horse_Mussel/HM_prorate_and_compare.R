
library(compareDF)

library(tidyverse)
library(ROracle)
require(maptools)
require(sf)
require(lubridate)
library(magrittr)


# Load functions ----------------------------------------------------------

#ROracle - credentials
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

#dir <- "Y:/Inshore/Databases/Scallsur/ScallsurUpdates/2022/Db_Update_HorseMusselAddition/ptran_load/"

cruise <- "SFA292021"
survey.year <- 2021
Year <- c(2018:survey.year)
Year <- Year[! Year %in% 2020]

dir <- paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/",survey.year,"/")

#### Import Source functions####
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r") 
direct <- getwd()
for(fun in funcs) 
{
  temp <- direct
  download.file(fun,destfile = basename(fun))
  source(paste0(direct,"/",basename(fun)))
  file.remove(paste0(direct,"/",basename(fun)))
}

# Read in Tow data from database ------------------------------------------

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

quer2 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.sctows s			",
  sep=""
)

tows.dat <- dbGetQuery(chan, quer2)
#str(tows.dat)

tows.dat <- tows.dat %>%
  mutate(YEAR = year(TOW_DATE)) %>% 
  filter(YEAR %in% Year) %>% 
  rename(TOW = TOW_NO) %>% 
  dplyr::select(CRUISE, TOW, TOW_TYPE_ID, STRATA_ID, START_LAT, START_LONG, END_LAT, END_LONG, TOW_LEN,NUM_LINED_FREQ, MGT_AREA_ID, DEPTH) %>% 
  unite(ID, c("CRUISE", "TOW"), sep = ".", remove = FALSE)

tows.dat$NUM_LINED_FREQ <- as.numeric(tows.dat$NUM_LINED_FREQ)


# Live Horse Mussel Raw --------------------------------------------------

hm.live <- read.csv(paste0(dir,cruise,"_horsemussellive.csv")) %>% 
  unite(ID, c("CRUISE", "TOW"), sep = ".", remove = FALSE)  #Creates ID column with cruise and tow number

colnames(hm.live) <- str_replace(colnames(hm.live), "X", "BIN_ID_") #Rename bin headers

# Join the tow data and horse mussel data ---------------------------------------------------------------

#Check if number of sctows records = number of horse mussel records
#nrow(tows.dat) == nrow(hm.live)

hm.live <- left_join(hm.live, tows.dat, by = "ID") %>% 
  dplyr::select(!c(CRUISE.y, COMMENTS, TOW.y)) %>% #remove columns
  rename(CRUISE = CRUISE.x) %>% 
  rename(TOW_NO = TOW.x)

#replace all NAs in bin and total columns with 0s
hm.live[, 6:45][is.na(hm.live[, 6:45])] <- 0

for(i in 1:nrow(hm.live)) {
  for(j in 6:45){
    hm.live[i,j] <- (hm.live[i,j]*hm.live$PRORATE.FACTOR[i])*(800/(hm.live$TOW_LEN[i])) #already accounted for in updated datafile(2/hm.live$NUM_LINED_FREQ[i])*
    hm.live[i,j] <- hm.live[i,j] *(17.5/4)
  }
}

hm.live <- hm.live %>% dplyr::select(CRUISE, TOW_NO, TOW_TYPE_ID, MGT_AREA_ID, START_LAT, START_LONG, STRATA_ID, DEPTH, BIN_ID_0:BIN_ID_195)%>%
  mutate(dplyr::select(., BIN_ID_0:BIN_ID_195) %>% round(1)) 

write.csv(hm.live, paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/Prorated/",cruise,"_live_standardize_R_version.csv"), row.names = FALSE)


# COMPARISONS -------------------------------------------------------------

#LIVE
hm.live <- read.csv(paste0(dir,cruise,"_live_standardize_R_version.csv"))
#hm.live <- hm.live %>% mutate(dplyr::select(., BIN_ID_0:BIN_ID_195) %>% round(1))

#File loaded to TTRAN
old <- read.csv(paste0(dir,"sclivehorsemussel_std_vw_",cruise,".csv"))

# # do the comparisons and save out the table
output <- compare_df(df_new = hm.live, df_old=old, group_col = c("CRUISE","TOW_NO"), stop_on_error = FALSE)
create_output_table(output, output_type = "xlsx", file_name=paste0(dir,cruise,"_live_comparison.xlsx"))


# Dead Horse Mussel Raw --------------------------------------------------

hm.dead <- read.csv(paste0(dir,cruise,"_horsemusseldead.csv")) %>% 
  unite(ID, c("CRUISE", "TOW"), sep = ".", remove = FALSE)  #Creates ID column with cruise and tow number

colnames(hm.dead) <- str_replace(colnames(hm.dead), "X", "BIN_ID_") #Rename bin headers

# Join the tow data and horse mussel data ---------------------------------------------------------------

#Check if number of sctows records = number of horse mussel records
nrow(tows.dat) == nrow(hm.dead)

hm.dead <- left_join(hm.dead, tows.dat, by = "ID") %>% 
  dplyr::select(!c(CRUISE.y, COMMENTS, TOW.y)) %>% #remove columns
  rename(CRUISE = CRUISE.x) %>% 
  rename(TOW_NO = TOW.x)

#replace all NAs in bin and total columns with 0s
hm.dead[, 6:45][is.na(hm.dead[, 6:45])] <- 0

for(i in 1:nrow(hm.dead)) {
  for(j in 6:45){
    hm.dead[i,j] <- (hm.dead[i,j]*hm.dead$PRORATE.FACTOR[i])*(800/(hm.dead$TOW_LEN[i])) #*(2/hm.dead$NUM_LINED_FREQ[i])
    hm.dead[i,j] <- hm.dead[i,j] *(17.5/4)
  }
}

hm.dead <- hm.dead %>% dplyr::select(CRUISE, TOW_NO, TOW_TYPE_ID, MGT_AREA_ID, START_LAT, START_LONG, STRATA_ID, DEPTH, BIN_ID_0:BIN_ID_195)%>%
  mutate(dplyr::select(., BIN_ID_0:BIN_ID_195) %>% round(1)) 

write.csv(hm.dead, paste0(dir,cruise,"_dead_standardize_R_version.csv"), row.names = FALSE)


# COMPARISONS -------------------------------------------------------------

#DEAD
hm.dead <- read.csv(paste0(dir,cruise,"_dead_standardize_R_version.csv"))

#hm.dead <- hm.dead %>% mutate(dplyr::select(., BIN_ID_0:BIN_ID_195) %>% round(1))


old <- read.csv(paste0(dir,"scdeadhorsemussel_std_vw_",cruise,".csv"))

# # do the comparisons and save out the table
# *Won't generate file if the same*
output <- compare_df(df_new = hm.dead, df_old=old, group_col = c("CRUISE","TOW_NO"), stop_on_error = FALSE)
create_output_table(output, output_type = "xlsx",  file_name=paste0(dir,cruise,"_dead_comparison.xlsx"))


# CHECKS ------------------------------------------------------------------

hm.live <- read.csv("Y:/Inshore/Databases/Scallsur/ScallsurUpdates/2022/Db_Update_HorseMusselAddition/ptran_load/Feb2322_GM2021/comparisons/GM2021_horsemussellive.csv")

#SPECIES.CODE can only be 4332
table(hm.live$SPECIES.CODE)
which(hm.live$SPECIES.CODE !=4332)

#PRORATE.FACTOR can only be 1 or 2
table(hm.live$PRORATE.FACTOR)
which(hm.live$PRORATE.FACTOR >=3)


#Tow numbers should match tow records in tow data.


#Should only be one unique cruise
unique(hm.live$CRUISE)




