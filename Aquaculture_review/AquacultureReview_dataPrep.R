
#This script is built to help provide information for Aquaculture Site Review Requests. This script requires coordinates of the zone of influence or boundaries related to the site (typically provided by the Coastal Oceanography & Ecosystem Research Section (COERS))), and overlap with commercial fishing. The script looks at total catch and effort from usable records over the last 5 years and determines overlap with the proposed Aquaculture site boundary.

#July 16, 2021

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

sf::sf_use_s2(FALSE)

uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

#Specify directory (make new one for each request)
direct <- "Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/GIS_info"
  #"Y:/Admin/Request and Review Tracking/Aquaculture_Reviews/2021/Beaver_Harbour/" #to save logs and read in ll data
last.fishing.yr <- 2022
start.year <- last.fishing.yr - 5 #Get data from latest year back 5 years (6 years inclusive).


# -----READ IN SITE INFO (Zone of Influence) - PROVIDED BY Coastal Oceanography & Ecosystem Research Section (COERS) (Lindsay)-------

site.name <- "SaintMarysBay"
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
  st_transform(crs = 4269)#convert projection to match managment zone boundaries (NAD83)

#BeaverHarbour
#read.table(paste0(dir,"rams_head_pez_pelagic_speed26.6_cms_ttrack_3h.ll"))

#zoi.sf <- st_as_sf(zoi, coords = c("V1", "V2"), crs = 4326) %>% #May need to adjust Lat long headers in coords = c()
#  summarise(do_union = FALSE) %>% 
#  st_cast("LINESTRING") %>% 
#  st_transform(crs = 4269) #convert projection to match managment zone boundaries (NAD83)

mapview::mapview(data.1449)+
  mapview(data.1450)+
  mapview(data.1451)+
  mapview(data.1452)


zoi.sf <- st_union(data.1449, data.1452) #These two zones cover the others, so only need 1449 and 1452
mapview::mapview(zoi.sf)

cruise <- "SPA3" #Set Cruise parameters based on where site is located

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
SPA1A <- st_read(paste0(temp2, "/SPA1A_polygon_NAD83.shp"))
SPA1B <- st_read(paste0(temp2, "/SPA1B_polygon_NAD83.shp"))
SPA2 <- st_read(paste0(temp2, "/SPA2_polygon_NAD83.shp"))
SPA3 <- st_read(paste0(temp2, "/SPA3_polygon_NAD83.shp"))
SPA4 <- st_read(paste0(temp2, "/SPA4_polygon_NAD83.shp"))
SPA5 <- st_read(paste0(temp2, "/SPA5_polygon_NAD83.shp"))
SPA6A <- st_read(paste0(temp2, "/SPA6A_polygon_NAD83.shp")) %>% mutate(ET_ID = "6A")
SPA6B <- st_read(paste0(temp2, "/SPA6B_polygon_NAD83.shp")) %>% mutate(ET_ID = "6B")
SPA6C <- st_read(paste0(temp2, "/SPA6C_polygon_NAD83.shp")) %>% mutate(ET_ID = "6C")
SPA6D <- st_read(paste0(temp2, "/SPA6D_polygon_NAD83.shp")) %>% mutate(ET_ID = "6D")
SPA6_all <- rbind(SPA6A, SPA6B, SPA6C, SPA6D)

SMB <- st_read(paste0(temp2, "/SMB_SurveyStrata.shp"))
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

#Run for determining subareas for data query.
sub.area <- NULL
if(cruise == "SPA1A") {
  sub.area <- paste0("'1A'")
  mgmt_zone <- SPA1A
}
if(cruise == "SPA1B") {
  sub.area <- paste0("'1B'")
  mgmt_zone <- SPA1B
}
if(cruise == "SPA2") {
  sub.area <- paste0("'SPA2'")
  mgmt_zone <- SPA2
}
if(cruise == "SPA3") {
  sub.area <- paste0("'SPA3'")
  mgmt_zone <- SPA3
}
if(cruise == "SPA4") {
  sub.area <- paste0("'SPA4'")
  mgmt_zone <- SPA4
}
if(cruise == "SPA5") {
  sub.area <- paste0("'SPA5'")
  mgmt_zone <- SPA5
}
if(cruise == "SPA6") {
  sub.area <- paste0("'6A','6B','6C','6D'")
  mgmt_zone <- SPA6_all
}

quer2 <- paste(
  "SELECT * 			                             ",
  "FROM scallop.scallop_log_marfis s		         ",
  "WHERE s.assigned_area in (",sub.area,")       ",
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

#logs.spa1a <- logs
#logs.spa3 <- logs
#logs.spa4 <- logs
#logs <- rbind(logs.spa1a, logs.spa3, logs.spa4)

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
  st_transform(crs = 4269) #convert projection to match managment zone boundaries (NAD83)

#Note any coordinate errors by plotting
#plot(log.priv.sf) 
mapview::mapview(log.priv.sf)


# ----Remove any data points outside the management zone boundaries--------------

log.priv.sf <- log.priv.sf %>% 
  st_crop(mgmt_zone)

#plot
mapview::mapview(log.priv.sf)


# ----Remove any data points on land---------------------------------------------------------------------

log.priv.sf <- st_difference(log.priv.sf, st_combine(land)) #Takes a few minutes

mapview::mapview(log.priv.sf)+
  mapview::mapview(zoi.sf)


# print # of records removed
cat(nrow(log.priv)-nrow(log.priv.sf), "record(s) removed due to coordinate errors")

# ----Calculate average distance to shore--------------------------------------------------------------------

#library(osmdata)
#library(geosphere)

#log.dist <- st_transform(log.priv.sf, crs = 4326) %>% 
#  filter(DAY_CATCH_KG != 0)
#land.wgs84 <- st_transform(land, crs = 4326) %>% 
#  st_cast("MULTILINESTRING")

#dist <- geosphere::dist2Line(p = st_coordinates(log.dist), line = st_coordinates(land.wgs84)[,1:2])
#df <- cbind( d1 %>% rename(y=lat,x=long),dist) %>%
#  mutate(miles=distance/1609)



# ----How many records are in the zone of influence?---------------------------------------------------------------------

num.rec.in.site <- nrow(log.priv.sf %>% st_crop(zoi.sf)) 
cat(num.rec.in.site, "- usable commercial record(s) within the zone of influence")


# ----What subareas (if any) overlap with the zone of influence?---------------------------------------------------------------------

spa.site.overlap <- log.priv.sf %>% 
  st_crop(zoi.sf) %>%
  summarise(AREA) %>% 
  unique() %>%
  st_set_geometry(NULL) %>% 
  pull()

cat(spa.site.overlap, "- subarea(s) overlap with the zone of influence")

# ----How many usable records are within the area and subarea?---------------------------------------------------------------------

area_total <- log.priv.sf %>% 
  nrow()
area_total

subarea_total <- log.priv.sf %>% 
  filter(AREA == spa.site.overlap) %>% 
  nrow()
subarea_total

cat(paste0(area_total, " usable records fall within ", cruise, " and of those records ", subarea_total, " fall within subarea ", spa.site.overlap))

# ----How many licences are reported in the zone of influence---------------------------------------------------------------------

licence.no <- log.priv.sf %>% 
  st_crop(zoi.sf)
licence.no <- n_distinct(licence.no$LICENCE_ID)
licence.no 

cat(paste0(licence.no, " out of ", n_distinct(log.priv$LICENCE_ID), " licences from usable records are reported within the zone of influence."))


#### GRID DATA: ####

#hex <- st_make_grid(log.priv.sf , cellsize= 0.015, square=FALSE) #Hexagon grid shape
grid <- st_make_grid(log.priv.sf , cellsize= 0.015, square=TRUE) #Square grid shape

#hex <- st_as_sf(data.table(id_hex=1:length(hex), geom=sf::st_as_text(hex)), wkt='geom', crs = 4326) #id_hex = 1:#of elements in grid
grid <- st_as_sf(data.table(id_grd=1:length(grid), geom=sf::st_as_text(grid)), wkt='geom', crs = 4269)

#plot(st_geometry(hex))
plot(st_geometry(grid))
plot(log.priv.sf, add = TRUE)

#join <- suppressMessages(hex %>% 
  #st_join(log.priv.sf, join=st_contains) %>%
 # group_by(id_hex) %>% 
  #dplyr::summarise(sum_CPUE_KG=sum(CPUE_KG, na.rm=T),
  #                 mean_CPUE_KG=mean(CPUE_KG, na.rm=T)) %>% 
  #filter(sum_CPUE_KG != 0))


# ------ Summarizing catch and effort for whole SPA (catch in kg, effort in hours)-------------------------------

grid.data <- suppressMessages(grid %>% #supresses message - "although coordinates are longitude/latitude, st_union assumes that they are planar"
  st_join(log.priv.sf, join=st_contains) %>%
  group_by(id_grd) %>% 
  dplyr::summarise(sum_CATCH=sum(DAY_CATCH_KG, na.rm=T),
                   sum_EFFORT=sum(EFFORT_HOURS, na.rm=T)) %>% 
  filter(sum_CATCH != 0))

mapview::mapview(grid.data, zcol="sum_CATCH") + 
  mapview::mapview(grid.data, zcol = "sum_EFFORT")+
  mapview::mapview(zoi.sf)

# ----------What is the Total catch (MT) and Effort for all of SPA # ?---------------------------------------------------------------

tot.catch.eff <- grid.data %>% 
  dplyr::summarise(tot_CATCH_MT=sum(sum_CATCH, na.rm=T)/1000, #convert kg to metric tonnes
                   tot_EFFORT=sum(sum_EFFORT, na.rm=T))
tot.catch.eff


# ------ Summarizing catch and effort for Subarea (catch in kg, effort in hours)---------------------

subarea.grid.data <- suppressMessages(grid %>% 
                                st_join(log.priv.sf %>% filter(AREA == spa.site.overlap), join=st_contains) %>%
                                group_by(id_grd) %>% 
                                dplyr::summarise(sum_CATCH=sum(DAY_CATCH_KG, na.rm=T),
                                                 sum_EFFORT=sum(EFFORT_HOURS, na.rm=T)) %>% 
                                filter(sum_CATCH != 0))

mapview::mapview(subarea.grid.data, zcol="sum_CATCH") + 
  mapview::mapview(subarea.grid.data, zcol = "sum_EFFORT")+
  mapview::mapview(zoi.sf)

# ----------What is the Total catch (MT) and Effort for the Subarea (catch in mt) ?---------------------------------------------------------------

subarea.tot.catch.eff <- subarea.grid.data %>% 
  dplyr::summarise(tot_CATCH_MT=sum(sum_CATCH, na.rm=T)/1000, #convert kg to metric tonnes
                   tot_EFFORT=sum(sum_EFFORT, na.rm=T))
subarea.tot.catch.eff


# -----What is the Total catch (MT) and Effort Within zone of influence--------------------------------------------------------------------
#Take zois and make polygon
data.1449 <- data.1449 |> 
  st_cast("POLYGON")
data.1452 <- data.1452 |> 
  st_cast("POLYGON")

zoi.sf <- st_union(data.1449, data.1452)

site.grid <- grid.data %>% 
  st_intersection(zoi.sf) %>% 
  mutate(sum_CATCH_MT = sum_CATCH/1000) #Create sum_CATCH_MT (catch in mt) column to look at each grid cell values

#st_write(site.grid, "Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/SPA4_site_Logbook_grids_CatchEffort.shp", driver = "ESRI Shapefile", overwrite = T)

#What percentage of catch from Year-Year comes from each grid...just exploring ways to look at the data.
#grid.site.prop <- site.grid |> 
#  mutate(CATCH_PROP = sum_CATCH/sum(sum_CATCH)*100) |> 
#  mutate(EFFORT_PROP = sum_EFFORT/sum(sum_EFFORT)*100)

mapview::mapview(zoi.sf)+ #Note cropped grid cells still contain full data.
  mapview::mapview(data.1449)+
  mapview::mapview(data.1450)+
  mapview::mapview(data.1451)+
mapview::mapview(site.grid, zcol="sum_CATCH_MT") + 
  mapview::mapview(site.grid, zcol = "sum_EFFORT")
  #mapview::mapview(grid.site.prop, zcol="CATCH_PROP") + 
  #mapview::mapview(grid.site.prop, zcol = "EFFORT_PROP")+
  
#st_write(site.grid, "Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/Commercial_Grid_logdata_SPA1A-3-4.shp", driver = "ESRI Shapefile", overwrite = T)
#st_write(data.1452, "Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1452.shp", driver = "ESRI Shapefile", overwrite = T)


#Summarize all grid cells for total effort and catch (mt)
site.catch.eff <- site.grid %>% 
  dplyr::summarise(tot_CATCH_MT=sum(sum_CATCH, na.rm=T)/1000, #convert kg to metric tonnes
                   tot_EFFORT=sum(sum_EFFORT, na.rm=T))
site.catch.eff

# ------- Calculate percentage of catch and effort in all of the Scallop Production Area and Subarea(s)---------

perc.catch.spa <- round((site.catch.eff$tot_CATCH_MT/tot.catch.eff$tot_CATCH_MT) *100, digits = 2) #percent catch in zone of influence from within whole SPA

perc.catch.subarea <- round((site.catch.eff$tot_CATCH_MT/subarea.tot.catch.eff$tot_CATCH_MT) *100, digits = 2) #percent catch zone of influence from just the subarea 

perc.effort.spa <- round((site.catch.eff$tot_EFFORT/tot.catch.eff$tot_EFFORT) *100, digits = 2) #percent effort within zone of influence from within whole SPA

perc.effort.subarea <- round((site.catch.eff$tot_EFFORT/subarea.tot.catch.eff$tot_EFFORT) *100, digits = 2) #percent of effort in zone of influence from just the subarea 

tot.catch.eff
site.catch.eff

perc.catch.spa
perc.effort.spa

perc.catch.subarea
perc.effort.subarea

#REPORT THE FOLLOWING INFORMATION:

cat(paste("Summary: The Zone of Influence (ZOI) falls within",cruise, "and overlaps with subarea(s)", spa.site.overlap,".\n From ",start.year, "to", last.fishing.yr,"inclusive (6 years), from usable logs, landings within the ZOI were\n", round(site.catch.eff$tot_CATCH_MT, 2), "metric tonnes corresponding to", perc.catch.spa, "% of landings in", cruise,"(", perc.catch.subarea,"% of landings within subarea", spa.site.overlap,").\n"))


#Save as .txt
cat(paste("Summary: The Zone of Influence (ZOI) falls within",cruise, "and overlaps with subarea(s)", spa.site.overlap,".\n From ",start.year, "to", last.fishing.yr,"inclusive (6 years), from usable logs, landings within the ZOI were\n", round(site.catch.eff$tot_CATCH_MT, 2), "metric tonnes corresponding to", perc.catch.spa, "% of landings in", cruise,"(", perc.catch.subarea,"% of landings within subarea", spa.site.overlap,").\n"), file = paste0(direct,site.name,"_AquacultureReview.log"))


