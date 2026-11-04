#Incremental Ageing
#Most of this code is modified from Offshore (https://github.com/Mar-scal/Framework/blob/master/DataInputs/Ageing/Ageing.Rmd)

#potential resource for plotting multiple area curves once final models are decided: https://derekogle.com/fishR/2020-01-02-ggplot-vonB-fitPlot-2

library(tidyverse)
library(openxlsx)
library(ROracle)
library(sf)

#Load functions:
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r") 

dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

#Shapefiles for plotting
#SFA29extent <- st_read("Y:/Projects/Condition Project/AZMP/2025/GabrielaVieiraLopes_Honours/GIS_data/SFA29_Extent.shp")
Land <- st_read(paste0("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/Atl_region_land.shp")) %>% 
  st_transform(crs = 4326) %>% 
  filter(PROVINCE %in% c("Maine", "Nova Scotia", "New Brunswick"))
inshore_strata <- st_read(paste0("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/PolygonSCSTRATAINFO_rm46-26-57.shp"))

# Define:
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

# ---- Obtain data ---- 

#SQL query 1:
quer1 <- paste0("SELECT *
FROM (
  SELECT cruise, tow_no, start_lat, start_long, strata_id, 0 gear_id, shell_no , 
  age_ring, hgt_at_ring, tot_hgt , cond_id, ager_id
  FROM SCALLSUR.SCTOWS, SCALLSUR.SCAGEANALYSIS 
  WHERE sctows.tow_seq = scageanalysis.tow_seq)")

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle
#detailed sampling data
age.dat <- dbGetQuery(chan, quer1)

summary(age.dat)

# ---- Format data ---- 

age.dat <- age.dat[!is.na(age.dat$AGE_RING),]
age.dat <- age.dat[!is.na(age.dat$HGT_AT_RING),]

unique(age.dat$CRUISE)
#[1] "BF1993"    "BI1991"    "BI1992"    "BI1993"    "BI1994"    "BI1995"   
#[7] "BF1982"    "BF1984"    "BF1985"    "BF1986"    "BF1987"    "BF1988"   
#[13] "BF1989"    "BF1983"    "BF2016"    "SFA292016" "GM2016"    "BI2019"   
#[19] "BI2016"    "GM2024"    "BI2024"   

age.dat <- age.dat %>% 
  mutate(SPA = case_when(STRATA_ID %in% c(6,  7, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 39) ~ "SPA1A",
                         STRATA_ID %in% c(35, 37, 38, 49, 51, 52, 53, 54) ~ "SPA1B",
                         STRATA_ID %in% c(26,57) ~ "SPA2",
                         STRATA_ID %in% c(22,23,24) ~ "SPA3",
                         STRATA_ID %in% c(1,  2 , 3,  4,  5,  6,  7,  8,  9, 21, 47) ~ "SPA4and5",
                         STRATA_ID %in% c(30,31,32) ~ "SPA6",
                         STRATA_ID %in% c(41,42,43,44,45) ~ "SFA29W")) %>% 
  mutate(Year = as.numeric(str_sub(CRUISE, -4, -1))) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>%
  mutate(lon = convert.dd.dddd(START_LONG)) %>% 
  mutate(ID = paste0(CRUISE, ".", TOW_NO))

#for spatial checks:
age.sf <- st_as_sf(age.dat, coords = c("lon","lat"), crs = 4326)

#check strata 46
#mapview::mapview(age.sf %>% filter(STRATA_ID == 46))+
#   mapview::mapview(SFA29extent)
#BI1992, BI1994, BI1995 - Offshore Georges Bank - 6 tows? - remove Strata ID 46 for now.
age.dat <- age.dat %>% filter(STRATA_ID != 46)

summary(age.dat)
#table(age.dat$SPA)
#table(age.dat %>% 
#  filter(is.na(SPA)) %>% select(STRATA_ID))

#Note the NAs in TOT_HGT, not sure if these will need to be removed for further analysis.
age.dat %>% filter(is.na(TOT_HGT))
unique(age.dat %>% filter(is.na(TOT_HGT)) %>% select(CRUISE))
#CRUISE
#1    BF1983
#9    BF2016
#3963 BI2016

#Age Ranges
agesum <- age.dat %>% group_by(CRUISE) %>%
  dplyr::summarize(minage=min(AGE_RING),maxage=max(AGE_RING))
agesum

#Starting values for the optimization algorithm
#require(FSA)
#f.starts <- findGrowthStarts(HGT_AT_RING~AGE_RING,data=age.dat)
#f.starts

#Linf         K        t0 
#160.88635   0.14069   0.42535 
#Probably need to run this for each area separately?

# ---- Summarize ---- 

age.summary <- age.dat %>%
  group_by(CRUISE, Year, TOW_NO, SPA, AGER_ID) %>%
  summarize(shells = length(unique(SHELL_NO)),
            increments=n()) %>% 
  arrange(SPA)

increments <- age.dat  %>%
  dplyr::mutate(ID=paste0(ID, ".", SHELL_NO)) %>%
  group_by(AGE_RING, SPA) %>%
  summarize(n_inc = length(unique(ID)),
            med_height = median(HGT_AT_RING),
            sd_height=sd(HGT_AT_RING))

age.summary <- age.summary %>%
  group_by(AGER_ID, Year, SPA) %>%
  summarize(increments=sum(increments),
            shells=sum(shells),
            tows=length(unique(TOW_NO)))

all.summary <- age.summary %>%
  group_by(AGER_ID) %>%
  summarize(increments=sum(increments),
            shells=sum(shells),
            tows=sum(tows))


#write.csv(increments, "Y:/Projects/Holistic_sampling_Inshore/Holistic_sampling_SPA2/2024/Ageing/data/increments.csv")
#write.csv(age.summary, "Y:/Projects/Holistic_sampling_Inshore/Holistic_sampling_SPA2/2024/Ageing/data/age_summary.csv")


# ---- Plot data ---- 

#Plot age.summary (can be faceted by Ager_ID):
ggplot() +
  geom_bar(data=age.summary, aes(as.factor(Year), increments, group = SPA, fill=SPA),
           stat="identity",
           position="dodge") +
  #geom_text(data=age.summary,
  #          aes(as.factor(Year), increments,  group = SPA,
  #              label=paste0("Shells = ", shells, "\nTows = ", tows)),
  #          position=position_dodge(width=0.9)) +
  scale_fill_viridis_d(option = "viridis") +
  theme_minimal() +
  xlab("Year") +
  ylab("Number of increments") #+
#facet_wrap(~AGER_ID)


#Look at spatial distribution
age.sf <- age.sf %>% filter(STRATA_ID != 46)

shells <- age.dat %>% 
  dplyr::group_by(CRUISE, Year, TOW_NO, SPA) %>%
  dplyr::summarize(n_samples = length(unique(SHELL_NO)))

age.sf <- left_join(age.sf, shells)

ggplot() + 
  geom_sf(data=inshore_strata)+
  geom_sf(data=age.sf, aes(size=n_samples)) +
  #scale_shape_manual(values=c(1:16), name="Year")+
  scale_size_continuous(name="Number of shells") +
  theme_bw()+
  facet_wrap(~Year, ncol = 6)
