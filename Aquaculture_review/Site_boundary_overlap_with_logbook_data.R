# required packages
require (fields)
require (splancs)
require (RColorBrewer)
require (RPMG)
require (lubridate)
require (tidyverse)
require (sf)
#require(maptools)
require(forcats)
library(ROracle)
library(mapview)
library(data.table)
library(raster)

#read in data and convert to degdec
sites <- read.csv("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/GIS_info/Proposed_Site_Boundaries.txt")

sf::sf_use_s2(FALSE)

uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

last.fishing.yr <- 2022
start.year <- last.fishing.yr - 20 #Get data from latest year back 5 years (6 years inclusive).
cruise <- "SPA1A" #Set Cruise parameters based on where site is located


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

# Now read in the SPA shapefile
SPA3 <- st_read(paste0(temp2, "/SPA3_polygon_NAD83.shp"))
mgmt_zone <- SPA3
SMB <- st_read(paste0(temp2, "/SMB_SurveyStrata.shp"))
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

direct <- "Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/GIS_info"
data.1449 <- read.table(paste0(direct,"/1449_pez_benthic_sink0.3cms_2023-02-23.ll")) |> 
  mutate(Site = as.factor(1449)) |> st_as_sf(coords = c("V1", "V2"), crs = 4326) %>% #May need to adjust Lat long headers in coords = c()
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_transform(crs = 4269)#convert projection to match managment zone boundaries (NAD83)
data.1450 <- read.table(paste0(direct,"/1450_pez_benthic_sink0.3cms_2023-02-23.ll")) |> 
  mutate(Site = as.factor(1450))|> st_as_sf(coords = c("V1", "V2"), crs = 4326) %>% #May need to adjust Lat long headers in coords = c()
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_transform(crs = 4269)#convert projection to match managment zone boundaries (NAD83)
data.1451 <- read.table(paste0(direct,"/1451_pez_benthic_sink0.3cms_2023-02-23.ll")) |> 
  mutate(Site = as.factor(1451))|> st_as_sf(coords = c("V1", "V2"), crs = 4326) %>% #May need to adjust Lat long headers in coords = c()
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_transform(crs = 4269)#convert projection to match managment zone boundaries (NAD83)
data.1452 <- read.table(paste0(direct,"/1452_pez_benthic_sink0.3cms_2023-02-23.ll")) |> 
  mutate(Site = as.factor(1452))|> st_as_sf(coords = c("V1", "V2"), crs = 4326) %>% #May need to adjust Lat long headers in coords = c()
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_transform(crs = 4269)

data.1449 <- data.1449 |> 
  st_cast("POLYGON")
data.1452 <- data.1452 |> 
  st_cast("POLYGON")
zoi.sf <- st_union(data.1449, data.1452)



quer2 <- paste(
  "SELECT * 			                             ",
  "FROM scallop.scallop_log_marfis s		         ",
  "WHERE s.assigned_area in ('1A')       ",
  " 	AND  s.date_fished >= to_date('",start.year, "-09-30','YYYY-MM-DD') and s.date_fished < to_date('",last.fishing.yr,"-10-01','YYYY-MM-DD') ",
  "	AND (s.data_class = 1                        ",
  "OR (s.data_class = 2 AND s.quality_flag =',4' ) ",
  "OR (s.data_class = 2 AND s.quality_flag =',1,4') ",
  "OR (s.data_class = 2 AND s.quality_flag =',2,4')) ",
  sep=""
)

chan <- dbConnect(drv = dbDriver("Oracle"), username=uid,  password = pwd,  dbname = "ptran", believeNRows=FALSE)

logs <- dbGetQuery(chan, quer2)

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

# Convert site boundaries to DD #################################################

Latitude <- matrix(unlist(strsplit(as.character(sites$Lat), ";")), nrow = nrow(sites),ncol = 3, byrow = T)

Latitude[,3] <- substr(Latitude[,3], 1, nchar(Latitude[,3])-1)
Latitude <- matrix(as.numeric(Latitude[,]), nrow = nrow(sites), ncol = 3, byrow = F)

GPS.Latitude.DD <- Latitude[,1] + Latitude[,2]/60 + Latitude[,3]/3600

Longitude <- matrix(unlist(strsplit(as.character(sites$Long), ";")), nrow = nrow(sites), 
                    ncol = 3, byrow = T)
Longitude[,3] <- substr(Longitude[,3], 1, nchar(Longitude[,3])-1)
Longitude <- matrix(as.numeric(Longitude[,]), nrow = nrow(sites), ncol = 3, byrow = F)
GPS.Longitude.DD <- Longitude[,1] + Longitude[,2]/60 + Longitude[,3]/3600 #Negative degrees (W)
GPS.Longitude.NDD <- GPS.Longitude.DD * (-1)

rm(Latitude, Longitude)

sites$Latitude <- GPS.Latitude.DD
sites$Longitude <- GPS.Longitude.NDD

# and make polygon
sites.sf <- st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326) |>
  dplyr::select(Site)

sites.sf  <- sites.sf |> 
  group_by(Site) |> 
  dplyr::summarize(m = mean(Site), do_union=FALSE) |> 
  sf::st_cast("POLYGON") |> 
  dplyr::select(Site)

sites.sf  <- sites.sf |>
  st_transform(crs = 4269) |> 
  mutate(Site = as.factor(Site))

sites.sf  <- sites.sf |>
  st_transform(crs = 32620) |> 
  mutate(Site = as.factor(Site))

#Add buffer to sites - 1 nautical mile = 1852 metres: 37 (1) Subject to subsections (2) and (3), the master of a vessel fishing with mobile gear shall maintain a distance of at least one-half nautical mile between his vessel, including any mobile gear attached thereto, and any previously set fishing gear.

#buffer.sites.sf  <-st_buffer(sites.sf, 926)
#st_write(buffer.sites.sf, "Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/SMB_proposed_Aquaculture_sites_half_nm_buffer.shp", driver = "ESRI Shapefile", overwrite = T)

#2km buffer - just for visual
#buffer.sites.sf  <-st_buffer(sites.sf, 2000)
#st_write(buffer.sites.sf, "Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/SMB_proposed_Aquaculture_sites_2km_buffer.shp", driver = "ESRI Shapefile", overwrite = T)

mapview::mapview(sites.sf)#+
 # mapview::mapview(buffer.sites.sf)

# ----Filter out areas for privacy considerations (min 5 trips per area)-------

log.priv <- logs %>%
  group_by(AREA) %>% 
  filter(!n() <=5) %>% #Filter out any areas within the dataset that have less than 5
  ungroup() %>%
  dplyr::select(ID, AREA, DDSlon, DDSlat, DAY_CATCH_KG, EFFORT_HOURS, YEAR, LICENCE_ID)

#Create log and Print out records removed by rule of 5
cat(nrow(logs)-nrow(log.priv), "records are removed - rule of 5\n") 


# Make spatial sf object -----------------------------------------------------

log.priv.sf <- st_as_sf(log.priv, coords = c("DDSlon", "DDSlat"), crs = 4326) %>% 
  st_transform(crs = 4269) #convert projection to match management zone boundaries (NAD83)

#Note any coordinate errors by plotting
#plot(log.priv.sf) 
mapview::mapview(log.priv.sf)


# ----Remove any data points outside the management zone boundaries--------------

log.priv.sf <- log.priv.sf %>% 
  st_crop(mgmt_zone)

# ----Remove any data points on land---------------------------------------------------------------------

log.priv.sf <- st_difference(log.priv.sf, st_combine(land)) #Takes a few minutes

#plot
mapview::mapview(log.priv.sf)+
  mapview::mapview(sites.sf)

#st_write(sites.sf, "Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/SMB_proposed_Aquaculture_sites.shp", driver = "ESRI Shapefile", overwrite = T)

#Crop logs to exposure zone:

log.priv.sf <- log.priv.sf %>% 
  st_intersection(zoi.sf) %>% 
  mutate(CATCH_MT = DAY_CATCH_KG/1000)

st_write(log.priv.sf, "E:/AquacultureReview_shps/SPA1A_2004to22_logbook.shp", driver = "ESRI Shapefile", overwrite = T)


# ----How many records overlap with the site boundaries?---------------------------------------------------------------------

num.rec.in.site <- nrow(log.priv.sf %>% st_crop(sites.sf)) 
cat(num.rec.in.site, "- usable commercial record(s) overlap with the site boundaries")

site.overlap <- log.priv.sf %>% 
  st_intersection(sites.sf)

mapview::mapview(site.overlap)+
  mapview::mapview(sites.sf)

# ----How many licences are reported in the zone of influence---------------------------------------------------------------------
licence.no <- log.priv.sf %>% 
  st_crop(sites.sf)
licence.no <- n_distinct(licence.no$LICENCE_ID)
licence.no 

cat(paste0(licence.no, " out of ", n_distinct(log.priv$LICENCE_ID), " licences from usable records are reported within the site boundaries."))





