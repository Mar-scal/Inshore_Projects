#Max Age
#Most of this code is modified from Offshore (https://github.com/Mar-scal/Framework/blob/master/DataInputs/Ageing/Ageing.Rmd)

library(tidyverse)
library(openxlsx)
library(ROracle)
library(sf)
require(Polychrome)

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
FROM SCALLSUR.SCWGT_HGT_AGE")

# ROracle
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle
#detailed sampling data
age.dat <- dbGetQuery(chan, quer1)

summary(age.dat)

#Combine AGE and AGEI into one column:
#AGE - Shell age in years. Held in SCSHELLDETAILS table
#AGEI - Shell age from increment ageing, data is the max age ring. Held in SCAGEANALYSIS table

age.dat <- age.dat %>% filter(!is.na(AGEI) | !is.na(AGE))
age.dat$AGE_COMB <- coalesce(age.dat$AGEI, age.dat$AGE)

summary(age.dat)
unique(age.dat$CRUISE)

#Add SPA names, extract Year from CRUISE, convert coords, and create unique ID..
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

#spatial checks:
age.sf <- st_as_sf(age.dat, coords = c("lon","lat"), crs = 4326)

#check strata 46
age.sf %>% filter(STRATA_ID == 46)
#mapview::mapview(age.sf %>% filter(STRATA_ID == 46))+
#   mapview::mapview(SFA29extent)
#BI1992, BI1994, BI1995 - remove Strata ID 46.
age.dat <- age.dat %>% filter(STRATA_ID != 46)

summary(age.dat)
#table(age.dat$SPA)
#table(age.dat %>% 
#  filter(is.na(SPA)) %>% select(STRATA_ID))

#Note 1 NA in HEIGHT.. remove.
age.dat %>% filter(is.na(HEIGHT))
#CRUISE   TOW_NO
#BF1983    85
age.dat <- age.dat %>% filter(!is.na(HEIGHT))


# ---- Summarize ---- 

#Age Ranges by CRUISE
agesum <- age.dat %>% group_by(CRUISE) %>%
  dplyr::summarize(minage=min(AGE_COMB),maxage=max(AGE_COMB))
agesum

#In what Years do SPAs have age data
spas.with.age.dat <- age.dat %>% group_by(SPA, Year) %>% 
  summarize(num_tows = n_distinct(TOW_NO))
spas.with.age.dat 

#Plot:
#Occurrences per age by year and area
ggplot(age.dat, aes(x = AGE_COMB, group = SPA, fill=SPA)) +
  geom_bar() +
  scale_fill_manual(values = as.vector(palette36.colors(7)))+
  theme_minimal() +
  labs(x = "Age", y = "Count", title = "Number of Individuals by Age")+
  facet_wrap(~Year)

#Number of individuals by age grouped by year and area
ggplot(age.dat %>% filter(Year %in% c(2014:2025)), aes(x = AGE_COMB, group = SPA, fill=SPA)) +
  geom_bar() +
  scale_fill_manual(values = as.vector(palette36.colors(7)))+
  theme_minimal() +
  labs(x = "Age", y = "Count", title = "Number of Individuals by Age")+
  facet_grid(Year ~ SPA)

#Number of tows by year (Each SPA plotted seperately)
#SPA1A
SPA1A.num.tows.bycruise <- age.dat %>% 
  filter(SPA == "SPA1A") %>% 
  group_by(Year, CRUISE) %>% 
  summarise(num_tows = n_distinct(TOW_NO)) %>% 
  ungroup() %>% 
  complete(Year = 1982:2025, fill = list(value = NA))
SPA1A.plot <- ggplot(SPA1A.num.tows.bycruise, aes(x = factor(Year), y = num_tows)) +
  geom_col(fill = "steelblue") +
  labs(x = "Year", y = "number of tows", title = "SPA 1A") +
  geom_hline(yintercept = 10, color = "red")+ #any cruises with < 10 tows?
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0, 250)
SPA1A.plot 

#ggsave(plot = SPA1A.plot, "Y:/Projects/Inshore_Ageing/figures/SPA1A_tows_perYear.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

#SPA1B
SPA1B.num.tows.bycruise <- age.dat %>% 
  filter(SPA == "SPA1B") %>% 
  group_by(Year, CRUISE) %>% 
  summarise(num_tows = n_distinct(TOW_NO)) %>% 
  ungroup() %>% 
  complete(Year = 1982:2025, fill = list(value = NA))
SPA1B.plot <- ggplot(SPA1B.num.tows.bycruise, aes(x = factor(Year), y = num_tows)) +
  geom_col(fill = "steelblue") +
  labs(x = "Year", y = "number of tows", title = "SPA 1B") +
  geom_hline(yintercept = 10, color = "red")+ #any cruises with < 10 tows?
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0, 250)
SPA1B.plot 

#ggsave(plot = SPA1B.plot, "Y:/Projects/Inshore_Ageing/figures/SPA1B_tows_perYear.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)


#SPA2 - 1996-2025
SPA2.num.tows.bycruise <- age.dat %>% 
  filter(SPA == "SPA2") %>% 
  group_by(Year, CRUISE) %>% 
  summarise(num_tows = n_distinct(TOW_NO)) %>% 
  ungroup() %>% 
  complete(Year = 1982:2025, fill = list(value = NA))
SPA2.plot <- ggplot(SPA2.num.tows.bycruise, aes(x = factor(Year), y = num_tows)) +
  geom_col(fill = "steelblue") +
  labs(x = "Year", y = "number of tows", title = "SPA 2") +
  geom_hline(yintercept = 10, color = "red")+ #any cruises with < 10 tows?
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0, 250)
SPA2.plot 

#ggsave(plot = SPA2.plot, "Y:/Projects/Inshore_Ageing/figures/SPA2_tows_perYear.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

#SPA3
SPA3.num.tows.bycruise <- age.dat %>% 
  filter(SPA == "SPA3") %>% 
  group_by(Year, CRUISE) %>% 
  summarise(num_tows = n_distinct(TOW_NO)) %>% 
  ungroup() %>% 
  complete(Year = 1982:2025, fill = list(value = NA))
SPA3.plot <- ggplot(SPA3.num.tows.bycruise, aes(x = factor(Year), y = num_tows)) +
  geom_col(fill = "steelblue") +
  labs(x = "Year", y = "number of tows", title = "SPA 3") +
  geom_hline(yintercept = 10, color = "red")+ #any cruises with < 10 tows?
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0, 250)
SPA3.plot 

#ggsave(plot = SPA3.plot, "Y:/Projects/Inshore_Ageing/figures/SPA3_tows_perYear.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

#SPA4
SPA4.num.tows.bycruise <- age.dat %>% 
  filter(SPA == "SPA4and5") %>% 
  group_by(Year, CRUISE) %>% 
  summarise(num_tows = n_distinct(TOW_NO)) %>% 
  ungroup() %>% 
  complete(Year = 1982:2025, fill = list(value = NA))
SPA4.plot <- ggplot(SPA4.num.tows.bycruise, aes(x = factor(Year), y = num_tows)) +
  geom_col(fill = "steelblue") +
  labs(x = "Year", y = "number of tows", title = "SPA 4") +
  geom_hline(yintercept = 10, color = "red")+ #any cruises with < 10 tows?
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0, 250)
SPA4.plot 

#ggsave(plot = SPA4.plot, "Y:/Projects/Inshore_Ageing/figures/SPA4_tows_perYear.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

#SPA6
SPA6.num.tows.bycruise <- age.dat %>% 
  filter(SPA == "SPA6") %>% 
  group_by(Year, CRUISE) %>% 
  summarise(num_tows = n_distinct(TOW_NO)) %>% 
  ungroup() %>% 
  complete(Year = 1982:2025, fill = list(value = NA))
SPA6.plot  <- ggplot(SPA6.num.tows.bycruise, aes(x = factor(Year), y = num_tows)) +
  geom_col(fill = "steelblue") +
  labs(x = "Year", y = "number of tows", title = "SPA 6") +
  geom_hline(yintercept = 10, color = "red")+ #any cruises with < 10 tows?
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0, 250)
SPA6.plot 

#ggsave(plot = SPA6.plot, "Y:/Projects/Inshore_Ageing/figures/SPA6_tows_perYear.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

#Stacked plot for comparison:
require(cowplot)
combined_plot <- plot_grid(
  SPA1A.plot, SPA1B.plot, SPA2.plot, SPA3.plot, SPA4.plot, SPA6.plot, 
  ncol = 1,           # Force all plots into 1 column
  align = "v",        # Vertically align the plot panels
  labels = NA
)
combined_plot

#Look at spatial distribution
age.sf <- age.sf %>% filter(STRATA_ID != 46)

shells <- age.dat %>% 
  dplyr::group_by(CRUISE, Year, TOW_NO, SPA) %>%
  dplyr::summarize(n_samples = length(unique(SHELL_NO)))

age.sf <- left_join(age.sf, shells)

#All years
ggplot() + 
  geom_sf(data=inshore_strata)+
  geom_sf(data=age.sf, aes(size=n_samples)) +
  #scale_shape_manual(values=c(1:16), name="Year")+
  scale_size_continuous(name="Number of shells") +
  theme_bw()+
  facet_wrap(~Year, ncol = 6)


#Most recent years (x5)
ggplot() + 
  geom_sf(data=inshore_strata)+
  geom_sf(data=age.sf %>% filter(Year %in% c(2014:2024)), aes(size=n_samples)) +
  #scale_shape_manual(values=c(1:16), name="Year")+
  scale_size_continuous(name="Number of shells") +
  theme_bw()+
  facet_wrap(~Year, ncol = 6)


