# ------- Frequency of licences plots ---------

# required packages
require (fields)
require (splancs)
require (RColorBrewer)
require (RPMG)
require (lubridate)
require (tidyverse)
require (sf)
require(scales)
require(forcats)
library(ROracle)
library(mapview)
library(data.table)
library(raster)

sf::sf_use_s2(FALSE)
last.fishing.yr <- 2022
start.year <- last.fishing.yr - 5 #Get data from latest year back 5 years (6 years inclusive).
fleet <- "Full Bay" #"Full Bay" , "Mid-Bay"  , "Upper-Bay"
area <- "SPA3"


uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

# ----Import Source functions----------------------------------------------

funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/convert_coords.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r") 
# Note: uses older contour.gen.r version (working on alternative to contour.gen altogether).
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

# Read in spatial boundaries -----------------------------------------------------------

#Read in the inshore boundaries shapefile
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles
SPA1A <- st_read(paste0(temp2, "/SPA1A_polygon_NAD83.shp")) %>% mutate(ET_ID = "SPA1A")
SPA1B <- st_read(paste0(temp2, "/SPA1B_polygon_NAD83.shp")) %>% mutate(ET_ID = "SPA1B")
SPA2 <- st_read(paste0(temp2, "/SPA2_polygon_NAD83.shp")) %>% mutate(ET_ID = "SPA2")
SPA3 <- st_read(paste0(temp2, "/SPA3_polygon_NAD83.shp")) %>% mutate(ET_ID = "SPA3")
SPA4 <- st_read(paste0(temp2, "/SPA4_polygon_NAD83.shp")) %>% mutate(ET_ID = "SPA4")
SPA5 <- st_read(paste0(temp2, "/SPA5_polygon_NAD83.shp")) %>% mutate(ET_ID = "SPA5")
SPA6A <- st_read(paste0(temp2, "/SPA6A_polygon_NAD83.shp")) %>% mutate(ET_ID = "6A")
SPA6B <- st_read(paste0(temp2, "/SPA6B_polygon_NAD83.shp")) %>% mutate(ET_ID = "6B")
SPA6C <- st_read(paste0(temp2, "/SPA6C_polygon_NAD83.shp")) %>% mutate(ET_ID = "6C")
SPA6D <- st_read(paste0(temp2, "/SPA6D_polygon_NAD83.shp")) %>% mutate(ET_ID = "6D")
mgmt.zone <- rbind(SPA1A, SPA1B, SPA2, SPA3, SPA4, SPA5, SPA6A, SPA6B, SPA6C, SPA6D)



#SPA1A <- st_read("Y:/INSHORE SCALLOP/BoFBoundaries/SPABoundaries_Redrawn2014/SPA New Polys/shp polygons/SPA1A_polygon_NAD83.shp") %>% mutate(ET_ID = "6A")
#mapview::mapview(SPA6A)+
#mapview::mapview(SPA6B)+
#mapview::mapview(SPA6C)+
#mapview::mapview(SPA6D)

#Read in land shapefile - used for filtering out points on land.
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/other_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

land <- st_read(paste0(temp2, "/Atl_region_land.shp"), crs = 4326) %>% 
  st_transform(crs = 4269) %>%
  filter(PROVINCE %in% c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador")) %>% 
  st_make_valid() %>%  #shapefile contains invalid geometry
  dplyr::select(PROVINCE)

# Select Commercial data --------------------------------------------------

quer2 <- paste(
  "SELECT * 			                             ",
  "FROM scallop.scallop_log_marfis s		         ",
  "WHERE s.assigned_area in ('1A','1B','SPA2','SPA3','SPA4','SPA5','6A','6B','6C','6D')       ",
  " 	AND  s.date_fished >= to_date('",start.year, "-09-30','YYYY-MM-DD') and s.date_fished < to_date('",last.fishing.yr,"-10-01','YYYY-MM-DD') ",
  "	AND (s.data_class = 1                        ",
  "OR (s.data_class = 2 AND s.quality_flag =',4' ) ",
  "OR (s.data_class = 2 AND s.quality_flag =',1,4') ",
  "OR (s.data_class = 2 AND s.quality_flag =',2,4')) ",
  sep=""
)

chan <- dbConnect(drv = dbDriver("Oracle"), username=uid,  password = pwd,  dbname = "ptran", believeNRows=FALSE)

logs <- dbGetQuery(chan, quer2)


################## End of Data Pull ############################

# Format data -------------------------------------------------------------

dim(logs)

logs <- logs %>% 
  mutate(YEAR = as.Date(logs$DATE_FISHED, format="%Y-%m-%d")) %>%  #assuming character and in format 'YYYY-XX-XX' or "YYYY/XX/XX'
  rename(AREA = ASSIGNED_AREA) %>% 
  mutate(DDSlat = convert.dd.dddd(LATITUDE/100)) %>% 
  mutate(DDSlon = -convert.dd.dddd(LONGITUDE/100)) %>%
  mutate(ID = 1:nrow(logs)) %>% 
  mutate(EFFORT_HOURS = (AVG_TOW_TIME * NUM_OF_TOWS)/60)

str(logs)

#Select data by Fleet and Area (defined at start of script)
logs <- logs |> 
  filter(FLEET == fleet, AREA == area)
unique(logs$FLEET)
unique(logs$AREA)

logs <- logs |> 
  dplyr::select(ID, FLEET, AREA, DDSlon, DDSlat, DAY_CATCH_KG, EFFORT_HOURS, YEAR, LICENCE_ID)

# Make spatial sf object -----------------------------------------------------

logs.sf <- st_as_sf(logs, coords = c("DDSlon", "DDSlat"), crs = 4326) %>% 
  st_transform(crs = 4269) #convert projection to match management zone boundaries (NAD83)

dim(logs.sf)


# ----Remove any data points outside the management zone boundaries--------------

logs.sf.cor <- logs.sf %>% 
  st_crop(mgmt.zone |> filter(ET_ID == area))

#plot
mapview::mapview(logs.sf.cor)


# ----Remove any data points on land---------------------------------------------------------------------

logs.sf.cor <- st_difference(logs.sf.cor, st_combine(land)) #Takes a few minutes

mapview::mapview(logs.sf.cor)

# print # of records removed
cat(nrow(logs.sf)-nrow(logs.sf.cor), "record(s) removed due to coordinate errors")

#Read in the inshore boundaries shapefile
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/inshore_survey_strata.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the SPA shapefile
#SMB <- st_read(paste0(temp2, "/SMB_SurveyStrata.shp")) |> st_transform(crs = 4269)
STRATA <- st_read(paste0(temp2, "/PolygonSCSTRATAINFO_rm46-26-57.shp")) |> st_transform(crs = 4269)
#crs(log.priv.sf)
mapview(STRATA)

#SPA3 <- STRATA |> filter(STRATA_ID == c(22, 23, 24))
#plot(SPA3)
#####

logs.sf.new <- st_intersection(logs.sf.cor, STRATA)|> #This unfortunetly does filter out some legitimate points... because they are within the management boundaries, but not the Strata shapefile... not sure if there is a way to avoid this.
  mutate(SPA = case_when(STRATA_ID %in% c(50,51,51,49,39, 48,47) ~ "SPA1A", 
                         STRATA_ID  %in% c(41,53,38,37,54,56) ~ "SPA1B",
                         STRATA_ID  %in% c(23,24) ~ "BrierLurcher",
                         STRATA_ID  %in% c(22) ~ "SMB",
                         STRATA_ID  %in% c(11,18,12,19,13,20,14,15,16,17,7,6,5,4,10,3,9,2,8,1) ~ "SPA4",
                         STRATA_ID  %in% 21 ~ "SPA5",
                         STRATA_ID  %in% c(30,32,31) ~ "SPA6"))
#mapview(logs.strata)
mapview(logs.sf.new)+
mapview(logs.sf.cor)

dim(logs.sf.new)
#logs.sf.new |> filter(is.na(SPA))


#First get total landings for each licence by year from all of the area (SPA3 in this case)
bylicence <- logs.sf.new |> 
  mutate(year = year(YEAR)) |>
  mutate(year = as.factor(year)) |>
  mutate(LICENCE_ID = as.factor(LICENCE_ID)) |>
  group_by(LICENCE_ID, year) |> 
  dplyr::summarise(totcatch_by_licence=sum(DAY_CATCH_KG, na.rm=T)) |> 
  st_drop_geometry()

#Now get total landings for each licence by year by subarea... SPA in this case is Brier and Lurcher combined and SMB
#filter for SMB
bylicencestrat <- logs.sf.new |> 
  mutate(year = year(YEAR)) |>
  mutate(year = as.factor(year)) |>
  mutate(LICENCE_ID = as.factor(LICENCE_ID)) |>
  group_by(LICENCE_ID, year, SPA) |> 
  dplyr::summarise(totcatch_by_strata=sum(DAY_CATCH_KG, na.rm=T)) |> 
  st_drop_geometry()

head(bylicence)
head(bylicencestrat)


#combine
bylicencestrat <- bylicencestrat |> left_join(bylicence, by=c('LICENCE_ID','year'))

#Get proportion of catch
bylicencestrat <- bylicencestrat |>
  mutate(proportion_catch = totcatch_by_strata/totcatch_by_licence)

#By licence
ggplot(bylicencestrat |> filter(SPA == "SMB"), aes(proportion_catch, LICENCE_ID)) +
  geom_col()+
  facet_grid(vars(SPA), vars(year))

# Number of licences by proportion of catch
ggplot(data = bylicencestrat, aes(x=proportion_catch)) +
  geom_histogram(fill="#69b3a2", color="#2c7fb8", alpha=0.9, binwidth = 0.1) +
  labs(x = "Proportion of Catch", y = "Frequency") +
  scale_x_continuous(labels = scales::number_format(scale = 1, trim = T))+
  facet_grid(vars(SPA), vars(year))



