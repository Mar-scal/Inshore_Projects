#EAFM Presentation figures (Spatial Scale)



#Load Libraries
library(sf)
library(ggplot2)
library(mapview)
library(cowplot)
library(raster)
library(marmap)
library(stars)
library(scales)
library(viridis)
library(tidyverse)
library(rmapshaper)
#library(rcartocolor)
#library(RColorBrewer)
library(ROracle)
library(lubridate)
library(maptools)
library(ggsn)
library(forcats)

#### Import Mar-scal functions 
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
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

############################################
########## LOADING DATA FOR PLOTS ##########
############################################


# -----Land Shapefile and bathy contours --------------------------------------------------------------------
sf::sf_use_s2(FALSE)

#Read in land shapefile - used for filtering out points on land.
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/other_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

land.sf <- st_read(paste0(temp2, "/Atl_region_land.shp"), crs = 4326) %>% 
  st_transform(crs = 32620) %>%
  filter(PROVINCE %in% c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador")) %>% 
  st_make_valid() %>%  #shapefile contains invalid geometry
  dplyr::select(PROVINCE) 

#bathy <- getNOAA.bathy(-68.3, -54.9, 39.9, 47.9, resolution = 1, keep=TRUE) #Used R 4.2.2
bathy <- getNOAA.bathy(-69.3, -62.9, 42.7, 46.4, resolution = 1, keep=TRUE) #Used R 4.2.2
bathy <- marmap::as.raster(bathy)
bathy.cont <- rasterToContour(bathy)
bathy.sf <- bathy.cont %>% st_as_sf()
plot(bathy.sf)
#autoplot.bathy(bathy, geom=c("tile","contour")) +
#  scale_fill_gradient2(low="dodgerblue4", mid="gainsboro", high="darkgreen")


# ----Load Shapefiles from ESS:----------------------

#SFA28 Boundaries
SFA28 <- st_read("Y:/Inshore/Inshore Scallop Fishing Area Map/shp polygons/SFA28_from_OCMD.shp") %>% rename(ET_ID = Name) %>% mutate(Area = ET_ID) %>% dplyr::select(ET_ID, Area) %>% st_transform(crs = 4326)
SFA28.merge <- SFA28 %>%
  group_by(Area) %>%
  summarise() %>% 
  arrange(Area) #For Plot 1.

levels(SFA28.merge$Area)

SFA28 <- SFA28 %>% 
  st_union() %>%
  st_sf() %>% 
  mutate(Area = "SFA28 A-D")
#plot(SFA28)

#SFA28E polygon
#SFA29E <- st_read("Y:/Inshore/Inshore Scallop Fishing Area Map/shp polygons/SFA29E.shp") %>% st_transform(crs = 4326) %>% mutate(Area = "SFA29E") %>% dplyr::select(Area)
#SFA29E <- st_read("Y:/Inshore/Inshore Scallop Fishing Area Map/shp polygons/SFA29_from_OCMD.shp")
new.SFA29E <- st_read("Y:/Inshore/Inshore Scallop Fishing Area Map/Management_Spatial_Scale_Maps/shp/SFA29E_poly_for_fig_NOTofficial.shp") %>% mutate(Area = "SFA29 East") %>% dplyr::select(Area)
#plot(new.SFA29E)

#Georges Bank Scallop boundaries:
Gba.scallop <- st_read("Z:/Projects/OFI/BEcoME/Data/WP3/shp/OFI_GBa_2020.shp") %>%
  dplyr::group_by(brk) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)), .groups = "drop") %>% 
  st_make_valid() %>% 
  dplyr::summarise()
#plot(Gba.scallop)

Gbb.scallop <- st_read("Z:/Projects/OFI/BEcoME/Data/WP3/shp/OFI_GBb_2020.shp") %>%
  dplyr::group_by(brk) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)), .groups = "drop") %>% 
  st_make_valid() %>% 
  dplyr::summarise()
#plot(Gbb.scallop)

Gb.scallop.bounds <- st_union(Gba.scallop, Gbb.scallop) %>% 
  st_simplify(dTolerance = 500) %>% 
  mutate(ID = "Georges Bank") %>% 
  mutate(Area = "Offshore")
#plot(Gb.scallop.bounds)

#st_write(Gb.scallop.bounds, "Y:/Inshore/Inshore Scallop Fishing Area Map/Management_Spatial_Scale_Maps/shp/Gb_poly.shp")

# --- Load shapefiles from Github (Inshore and Offshore) ----------------------

#Read in the BoF and 29W boundaries shapefile
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles and format for plotting

#SFA28
SFA28A <- st_read(paste0(temp2, "/SPA1A_polyline_NAD83.shp")) %>% st_transform(crs = 4326) %>% st_cast("POLYGON") %>% mutate(Area = "SFA28A") %>% dplyr::select(Area)# %>% mutate(ET_ID = "SFA28A") %>% dplyr::select(ET_ID)
SFA28B <- st_read(paste0(temp2, "/SPA1B_28B_polyline_NAD83.shp")) %>% st_transform(crs = 4326) %>% st_cast("POLYGON") %>% mutate(Area = "SFA28B") %>% dplyr::select(Area)#%>% mutate(ET_ID = "SFA28B")  %>% dplyr::select(ET_ID) 
SFA28C <- st_read(paste0(temp2, "/SPA1B_28C_polyline_NAD83.shp")) %>% st_transform(crs = 4326) %>% st_cast("POLYGON") %>% mutate(Area = "SFA28C") %>% dplyr::select(Area)#%>% mutate(ET_ID = "SFA28C") %>% dplyr::select(ET_ID)
SFA28D <- st_read(paste0(temp2, "/SPA1B_28D_polyline_NAD83.shp")) %>% st_transform(crs = 4326) %>% st_cast("POLYGON") %>% mutate(Area = "SFA28D") %>% dplyr::select(Area)#%>% mutate(ET_ID = "SFA28D") %>% dplyr::select(ET_ID)


SPA1A <- st_read(paste0(temp2, "/SPA1A_polygon_NAD83.shp")) %>% st_transform(crs = 4326) %>% mutate(ET_ID = "SPA1A") %>% dplyr::select(ET_ID)
SPA1B <- st_read(paste0(temp2, "/SPA1B_polygon_NAD83.shp")) %>% st_transform(crs = 4326) %>% mutate(ET_ID = "SPA1B") %>% dplyr::select(ET_ID)
SPA2 <- st_read(paste0(temp2, "/SPA2_polygon_NAD83.shp")) %>% st_transform(crs = 4326) %>% mutate(Area = "SPA2") %>% dplyr::select(Area)# %>% mutate(ET_ID = "SPA2") %>% dplyr::select(ET_ID)
SPA3 <- st_read(paste0(temp2, "/SPA3_polygon_NAD83.shp")) %>% st_transform(crs = 4326) %>% mutate(Area = "SPA3") %>% dplyr::select(Area)# %>% mutate(ET_ID = "SPA3") %>% dplyr::select(ET_ID) %>%
SPA4 <- st_read(paste0(temp2, "/SPA4_polygon_NAD83.shp"))  %>% st_transform(crs = 4326) %>% mutate(Area = "SPA4") %>% dplyr::select(Area) # %>% mutate(ET_ID = "SPA4") %>% dplyr::select(ET_ID)
SPA5 <- st_read(paste0(temp2, "/SPA5_polygon_NAD83.shp")) %>% st_transform(crs = 4326) %>% mutate(Area = "SPA5") %>% dplyr::select(Area)

#SPA6 - format
SPA6_comb <- st_read(paste0(temp2, "/SPA6_wgs84.shp")) %>% mutate(Area = "SPA6") %>% dplyr::select(Area) #All SPA6 subareas combined into one shapefile (no lines)
SPA6A <- st_read(paste0(temp2, "/SPA6A_polygon_NAD83.shp")) %>% mutate(ET_ID = "6A")
SPA6B <- st_read(paste0(temp2, "/SPA6B_polygon_NAD83.shp")) %>% mutate(ET_ID = "6B")
SPA6C <- st_read(paste0(temp2, "/SPA6C_polygon_NAD83.shp")) %>% mutate(ET_ID = "6C")
SPA6D <- st_read(paste0(temp2, "/SPA6D_polygon_NAD83.shp")) %>% mutate(ET_ID = "6D")
#Because SPA6A, 6B, 6D overlap - need to cut out (#TO FIX IN COPY ON GITHUB)
SPA6A <- rmapshaper::ms_erase(SPA6A,SPA6B)%>% 
  st_make_valid()
#plot(SPA6A)
SPA6B <- rmapshaper::ms_erase(SPA6B,SPA6D)%>% 
  st_make_valid()
#Now can combine all SPA6 - maintains boundaries for subareas
SPA6 <- rbind(SPA6A, SPA6B, SPA6C, SPA6D) %>% st_transform(crs = 4326) %>% mutate(Area = "SPA6")
rm(SPA6A, SPA6B,SPA6C,SPA6D)#tidy environment

#SFA29 - format
SFA29W <- st_read(paste0(temp2, "/SFA29_subareas_utm19N.shp")) %>% st_transform(crs = 4326) %>%
  mutate(ET_ID = c("SFA29W A", "SFA29W B","SFA29W C","SFA29W D", "SFA29W E")) %>%
  dplyr::select(ET_ID)

SFA29W_nobound <- st_read(paste0(temp2, "/SFA29_BoundariesFollowing12nmDD_NoSubareas_WGS84.shp")) %>% mutate(ET_ID = "SFA29W")  %>% mutate(Area = "SFA29W") %>% dplyr::select(Area)
SFA29E.line <- st_read(paste0(temp2, "/12vm_TerrSea_sfa29E.shp")) %>% mutate(ET_ID = "SFA29E") %>% mutate(Area = "SFA29E")%>% dplyr::select(Area) #line



#Read in OFFSHORE Strata FROM GITHUB
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/offshore/offshore.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles
BBn <- st_read(paste0(temp2, "/BBn.shp")) %>% mutate(ET_ID = "Browns North") %>% dplyr::select(ET_ID)
BBs <- st_read(paste0(temp2, "/BBs.shp")) %>% mutate(ET_ID = "Browns South") %>% dplyr::select(ET_ID)
Ban <- st_read(paste0(temp2, "/Ban.shp")) %>% mutate(ET_ID = "Banquereau") %>% dplyr::select(ET_ID)
GBa <- st_read(paste0(temp2, "/GBa.shp")) %>% mutate(ET_ID = "Georges A") %>% dplyr::select(ET_ID)
GBb <- st_read(paste0(temp2, "/GBb.shp")) %>% mutate(ET_ID = "Georges B") %>% dplyr::select(ET_ID)
Ger <- st_read(paste0(temp2, "/Ger.shp")) %>% mutate(ET_ID = "German Bank") %>% dplyr::select(ET_ID)
ESS <- st_read(paste0(temp2, "/Sab.shp")) %>% mutate(ET_ID = "Eastern Scotian Shelf") %>% dplyr::select(ET_ID)


#Read in OFFSHORE scallop boundaries FROM GITHUB
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/other_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles
#German Bank
ger.scallop.bounds <- st_read(paste0(temp2, "/WGS_84_German.shp")) %>%
  rename(ID = X1) %>%
  mutate(Area = "Offshore") %>%
  st_transform(crs = 4326) %>% 
  st_make_valid() %>% #Cleans it uo a bit
  st_simplify(dTolerance = 500)
#plot(ger.scallop.bounds)

#Sable Bank, Banquereau, Middle Bank, Browns Bank North, Browns South.
scallop.offshore <- st_read(paste0(temp2, "/Sab_old.shp"))

#Sab lines not quite connecting so have to format to make polygon
scallop.sab <- scallop.offshore %>% filter(ID %in% c("Sab_old", "Sab"))
sab <- st_union(scallop.sab[2,], scallop.sab[1,]) %>% 
  st_make_valid() %>% 
  st_polygonize()

sab.scallop.bounds <- st_simplify(sab, dTolerance = 500) %>% #removes line inside polygon
  dplyr::select(ID) %>%
  mutate(Area = "Offshore") 

#plot(sab.scallop.bounds)
#st_write(sab.scallop.bounds, "Y:/Inshore/Inshore Scallop Fishing Area Map/Management_Spatial_Scale_Maps/shp/Sab_old_poly.shp")

ban.scallop.bounds <- scallop.offshore %>%
  filter(ID %in% c("Ban")) %>% 
  st_make_valid() %>% 
  st_polygonize() %>% 
  mutate(ID = "Banquereau Bank") %>% 
  mutate(Area = "Offshore")
#plot(ban.scallop.bounds)

mid.scallop.bounds <- scallop.offshore %>%
  filter(ID %in% c("Mid")) %>% 
  st_make_valid() %>% 
  st_polygonize() %>% 
  mutate(ID = "Middle Bank") %>% 
  mutate(Area = "Offshore")

bbn.scallop.bounds <- scallop.offshore %>%
  filter(ID %in% c("BBn")) %>% 
  st_make_valid() %>% 
  st_cast("POLYGON") %>% 
  mutate(ID = "Browns North") %>% 
  mutate(Area = "Offshore")

bbs.scallop.bounds <- scallop.offshore %>%
  filter(ID %in% c("BBs"))  %>% 
  st_make_valid() %>% 
  st_cast("POLYGON") %>% 
  mutate(ID = "Browns South") %>% 
  mutate(Area = "Offshore")

# ----------Load survey data from Database for species distribution plot ------------

uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')


#BAY OF FUNDY:
quer1 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.scliveres s			",
  " LEFT JOIN                          ",
  " 	(SELECT tow_date, cruise, tow_no     ",
  "	 FROM SCALLSUR.sctows) t             ",
  "on (s.cruise = t.cruise and s.tow_no = t.tow_no)",
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30, 31, 32, 35, 37, 38, 39, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56)    ",
  sep=""
)


ScallopSurv <- dbGetQuery(chan, quer1)
ScallopSurv  <- ScallopSurv[,1:51]

ScallopSurv <- ScallopSurv %>% 
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>% #standardize number per tow to numbers per m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195
  mutate(rec = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0))# Pre-recruit scallop - BIN_ID_0:BIN_ID_60


com.contours <- contour.gen(ScallopSurv %>% 
                              dplyr::select(ID, lon, lat, com),
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

bof.scallop.bounds <- st_union(totCont.poly.sf) %>% 
  st_sf() %>% 
  mutate(ID = "BoF") %>% 
  mutate(Area = "Inshore")
  #st_cast("POLYGON")
#plot(bof.scallop.bounds)

#SFA29 West
quer2 <- paste(
  "SELECT *                                     ",
  "FROM scallsur.SCLIVERES                      ",                                                                                                                                  
  "WHERE strata_id IN (41, 42, 43, 44, 45)      ",
  sep=""
)

#ROracle: 
ScallopSurv.sfa29 <- dbGetQuery(chan, quer2)

ScallopSurv.sfa29 <- ScallopSurv.sfa29 %>%
  mutate(year = as.numeric(substr(ScallopSurv.sfa29$CRUISE,6,9))) %>%
  filter(grepl("SFA29", CRUISE)) %>% #Filter out the BI cruises
  #mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date - works with SFA29
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>%  #add tot for bin 0 to bin 195 and standardize to #/m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com = dplyr::select(., BIN_ID_100:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Commercial scallop - >=100mm; BINS 100 to 195
  mutate(rec = dplyr::select(., BIN_ID_90:BIN_ID_95) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Recruit scallop - 90-99; BINS 90 to 95
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_85) %>% rowSums(na.rm = TRUE) %>% round(0))# Pre-recruit scallop - 0-85 mm; BINS 0 to 85


com.contours <- contour.gen(ScallopSurv.sfa29 %>% dplyr::select(ID, lon, lat, com), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
#sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>%
  mutate(level = unique(CP$PolyData$level))

sfa29.scallop.bounds <- st_union(totCont.poly.sf) %>% 
  st_sf() %>% 
  mutate(ID = "SFA29W") %>% 
  mutate(Area = "Inshore")
#plot(sfa29.scallop.bounds)

#SFA29 East
sfa29E.scallop.bounds <- st_read("Y:/Inshore/Inshore Scallop Fishing Area Map/Management_Spatial_Scale_Maps/shp/sfa29E_scallop_bounds.shp")

#SFA29E shapefile was acquired by running the following:
#set to TRUE if SFA29E needs to be re-run
if (FALSE){

quer3 <- paste(
  "SELECT *                         ",
  "FROM scallop.scallop_log_marfis            ",
  sep=""
)

#ROracle: 
ScallopLogs <- dbGetQuery(chan, quer3)

ScallopLogs.sfa29E <- ScallopLogs %>% 
  mutate(LATITUDE = convert.dd.dddd(LATITUDE/100)) %>% #Convert to DD
  mutate(LONGITUDE = (-1)*convert.dd.dddd(LONGITUDE/100)) %>%
  mutate(lat = LATITUDE) %>% #Duplicate coord columns for use later
  mutate(lon = LONGITUDE) %>%
 filter(!is.na(lon)) %>% 
  filter(ASSIGNED_AREA %in% c("29", "U"))

#create Sf object to "cut" logs to SFA29E area (new.SFA29E)
ScallopLogs.sfa29E.sf <- st_as_sf(ScallopLogs.sfa29E, coords = c("LONGITUDE","LATITUDE"), crs = 4326)

# Remove points on land
ScallopLogs.sfa29E.sf <- st_difference(ScallopLogs.sfa29E.sf, st_combine(land.sf)) #Takes a few minutes
#  Crop to SFA29E
ScallopLogs.sfa29E.sf <- st_intersection(ScallopLogs.sfa29E.sf, new.SFA29E)
#mapview::mapview(ScallopLogs.sfa29E.sf)

#remove geometry
ScallopLogs.sfa29E <- st_set_geometry(ScallopLogs.sfa29E.sf, NULL) %>% 
  mutate(ID = seq(1, nrow(ScallopLogs.sfa29E.sf),1)) %>% 
  filter(!is.na(CPUE_KG))

com.contours <- contour.gen(ScallopLogs.sfa29E %>% dplyr::select(ID, lon, lat, CPUE_KG), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>%
  mutate(level = unique(CP$PolyData$level))

sfa29E.scallop.bounds <- st_union(totCont.poly.sf) %>% 
  st_sf() %>% 
  mutate(ID = "SFA29E") %>% 
  mutate(Area = "Inshore")
#plot(sfa29E.scallop.bounds)
#st_write(sfa29E.scallop.bounds, "Y:/Inshore/Inshore Scallop Fishing Area Map/Management_Spatial_Scale_Maps/shp/sfa29E_scallop_bounds.shp")
}

# Colour palettes ---------------------------------------------------------

#Set Colour palettes

#Map 1 list: SFA25, SFA26, SFA27, SFA28, SFA29W_nobound SFA29E
mycols.1 <- c("cyan4","orangered3","seagreen3", "steelblue4", "magenta4", "lightgoldenrod2",  "darkseagreen3", "sandybrown" , "peachpuff3", "grey44", "tomato4",  "maroon4", "olivedrab3", "aquamarine3") #"#6A5ACD",
barplot(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), col = mycols.1)



#Map 2 list: SFA25, SFA26, SFA27, SFA28C, SFA28B,SFA28D, SFA28A, SFA29W_nobound, new.SFA29E
mycols.2 <- c("cyan4","orangered3","seagreen3", "#00429d", "#4771b2","#2e59a8", "#5d8abd",  "maroon4", "lightgoldenrod2",  "darkseagreen3", "sandybrown" , "peachpuff3", "grey44", "tomato4", "mediumpurple4", "lightpink3", "olivedrab3", "aquamarine3") #"#6A5ACD",
barplot(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), col = mycols.2) 

#Map 2 list: ESS.crop, Ban, GBa, GBb, BBn, BBs, Ger, SPA1A (SFA28A), SPA1B, SFA29W, SFA29E, SPA2, SPA3, SPA4, SPA6 
mycols.3 <- c("cyan4","paleturquoise3", "seagreen4", "seagreen3", "orangered2","orangered3", "sienna3",
              "maroon4", "deeppink", "palevioletred3", "maroon3", "deeppink3",
              "lightgoldenrod2","#00429d", "#4771b2", "#2e59a8",  "#5d8abd", "deepskyblue4","#b3cde0", "steelblue3") #"#6A5ACD",
barplot(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14, 15, 16, 17, 18, 19, 20), col = mycols.3) 

mycols.4 <- c("steelblue3","sandybrown")
mycols.5 <- c("steelblue4","peru")


############################################
###############  PLOTTING  #################
############################################

# Figure 1. C68 related stock groupings ---------------------------

#SFA 25
SFA25 <- ESS %>% mutate(Area = "SFA25") %>% dplyr::select(Area)
#plot(SFA25)

#SFA 26 - group and dissolve
SFA26 <- rbind(BBn,BBs,Ger) %>%
  mutate(Area = "SFA26") %>%
  group_by(Area) %>%
  summarise()
#plot(SFA26)

#SFA 27
SFA27<- rbind(GBa,GBb) %>% 
  mutate(Area = "SFA27") %>%
  group_by(Area) %>%
  summarise()
#plot(SFA27)

C68_boundaries <- rbind(SFA25, SFA26, SFA27, SFA28, SFA29W_nobound, new.SFA29E)
plot(C68_boundaries)

#SFF_FSP_SFA_boundaries <- rbind(SFA26, SFA27, SFA28, SFA29W_nobound)
#plot(SFF_FSP_SFA_boundaries)
#SFF_FSP_SFA_boundaries <- SFF_FSP_SFA_boundaries %>%
#  st_make_valid()
#st_write(SFF_FSP_SFA_boundaries, "Y:/Inshore/Inshore Scallop Fishing Area Map/Management_Spatial_Scale_Maps/shp/SFA_stockdefinitions.shp")

ggplot()+
  geom_sf(data = bathy.sf, colour = "grey", alpha = 0.6)+
  geom_sf(data = C68_boundaries, aes(fill = forcats::fct_inorder(Area)), size = 0.5, colour = "black", alpha = 0.6) +
  geom_sf(data = land.sf)+
  #geom_sf_text(data = SFA29E, aes(label = Area), size = 3, colour = "black", angle = 25, nudge_y = 0.12)+ #SFA29E label
  #geom_sf_text(data = SFA_all %>% filter(Area == "SFA29W"), aes(label = Area), size = 3, colour = "black", nudge_y = -0.12, nudge_x = 0.30)+ #SFA29W label
  #geom_sf_text(data = SFA_all %>% filter(Area == "SFA25"), aes(label = Area), size = 3, colour = "black", nudge_y = -0.5)+ #SFA25 label
  #geom_sf_text(data = SFA_all %>% filter(Area == "SFA26"), aes(label = Area), size = 3, colour = "black")+ #SFA26 label
  #geom_sf_text(data = SFA_all %>% filter(Area == "SFA27"), aes(label = Area), size = 3, colour = "black", angle = -45, nudge_y = 0.7, nudge_x = -0.2)+ #SFA27 label
  #geom_sf_text(data = SFA_all %>% filter(Area == "SFA28A"), aes(label = Area), size = 3, colour = "black", angle = 30, nudge_y = 0.12, nudge_x = 0.2)+ #SFA28A label
  #geom_sf_text(data = SFA_all %>% filter(Area == "SFA28B"), aes(label = Area), size = 3, colour = "black", angle = 30)+ #SFA28B label
  #geom_sf_text(data = SFA_all %>% filter(Area == "SFA28C"), aes(label = Area), size = 3, colour = "black", nudge_x = -0.3) + #SFA28C label
  #geom_sf_text(data = SFA_all %>% filter(Area == "SFA28D"), aes(label = Area), size = 3, colour = "black", nudge_x = 0.6)+ #SFA28D label
  scale_fill_manual(values = mycols.1, name = "National Reporting Areas\n (e.g. FSP, SFF)")+
  ggsn::scalebar(data = C68_boundaries, dist = 150, st.size=4, height=0.009, transform = TRUE, dist_unit = "km", model = 'WGS84', border.size = 0.3, anchor = c(x = -55.6, y = 40.35))+
  ggsn::north(C68_boundaries, symbol = 11, scale = 0.092, anchor = c(x = -55.4, y = 41.50))+
  coord_sf(xlim = c(-68.3,-54.9), ylim = c(39.9,47.9), expand = FALSE)+
  labs(x = "Longitude", y = "Latitude")+
  theme_bw()+
  theme(legend.key.size = unit(5,"mm"), plot.margin = margin(1,1,1,1, "cm"),panel.border = element_rect(colour = "black", fill=NA, size=1)) #legend.position="none",

#ggsave(filename = paste0('Y:/Inshore/Inshore Scallop Fishing Area Map/Management_Spatial_Scale_Maps/Map1_C68_stock_groupings-DRAFT.png'), plot = last_plot(), scale = 2.5, width = 12, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# Figure 2. Management Groupings (SFA Groups)---------------------------

SFA_all <- rbind(SFA25, SFA26, SFA27,
                 SFA28.merge %>% dplyr::select(Area), SFA29W_nobound, new.SFA29E)

ggplot()+
  geom_sf(data = bathy.sf, colour = "grey", alpha = 0.6)+
  geom_sf(data = SFA_all, aes(fill = forcats::fct_inorder(Area)), size = 0.5, colour = "black", alpha = 0.6) +
  geom_sf(data = land.sf)+
  #geom_sf_text(data = SFA29E, aes(label = Area), size = 3, colour = "black", angle = 25, nudge_y = 0.12)+ #SFA29E label
  scale_fill_manual(values = mycols.2, name = "Scallop Fishing Areas")+
  ggsn::scalebar(data = C68_boundaries, dist = 150, st.size=4, height=0.009, transform = TRUE, dist_unit = "km", model = 'WGS84', border.size = 0.3, anchor = c(x = -55.6, y = 40.35))+
  ggsn::north(SFA_all, symbol = 11, scale = 0.092, anchor = c(x = -55.4, y = 41.50))+
  coord_sf(xlim = c(-68.3,-54.9), ylim = c(39.9,47.9), expand = FALSE)+
  labs(x = "Longitude", y = "Latitude")+
  theme_bw()+
  theme(plot.margin = margin(1.5,1,1,1, "cm"),panel.border = element_rect(colour = "black", fill=NA, size=1))#legend.position="none"

#ggsave(filename = paste0('Y:/Inshore/Inshore Scallop Fishing Area Map/Management_Spatial_Scale_Maps/Map2_SFA_Management_Groupings-DRAFT.png'), plot = last_plot(), scale = 2.5,  width = 12, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# Figure 3. Management Groupings (SPAs/Offshore Banks Groups)---------------------------


ESS.crop <- sf::st_difference(ESS, st_union(Ban)) #Cut out overlapping Banquereau from Eastern Scotian Shelf
SFA25 <- rbind(ESS.crop, Ban) %>% mutate(Area = "SFA25")

SPA_Bank_all <- rbind(ESS.crop, Ban, GBa, GBb, BBn, BBs, Ger,SFA29W, 
                      new.SFA29E %>%  mutate(ET_ID = Area) %>% dplyr::select(ET_ID),
                      SPA1A, SPA1B,
                      SPA2 %>% mutate(ET_ID = Area) %>% dplyr::select(ET_ID),
                      SPA3 %>% mutate(ET_ID = Area)%>% dplyr::select(ET_ID),
                      SPA4 %>% mutate(ET_ID = Area) %>% dplyr::select(ET_ID),
                      SPA5 %>% mutate(ET_ID = Area) %>% dplyr::select(ET_ID),
                      SPA6 %>% mutate(ET_ID= "SPA6") %>% dplyr::select(ET_ID)) 

ggplot()+
  geom_sf(data = bathy.sf, colour = "grey", alpha = 0.6)+
  geom_sf(data = SPA_Bank_all, aes(fill = forcats::fct_inorder(ET_ID)), size = 0.5, colour = "black", alpha = 0.6) +
  geom_sf(data = land.sf)+
  #geom_sf_text(data = SFA29E, aes(label = Area), size = 3, colour = "black", angle = 25, nudge_y = 0.12)+ #SFA29E label
  scale_fill_manual(values = mycols.3,name = "Management Areas")+
  ggsn::scalebar(data = SPA_Bank_all, dist = 150, st.size=4, height=0.009, transform = TRUE, dist_unit = "km", model = 'WGS84', border.size = 0.3, anchor = c(x = -55.6, y = 40.35))+
  ggsn::north(SPA_Bank_all, symbol = 11, scale = 0.092, anchor = c(x = -55.4, y = 41.50))+
  coord_sf(xlim = c(-68.3,-54.9), ylim = c(39.9,47.9), expand = FALSE)+
  labs(x = "Longitude", y = "Latitude")+
  theme_bw()+
  theme(plot.margin = margin(1.5,1,1,1, "cm"),panel.border = element_rect(colour = "black", fill=NA, size=1))#legend.position="none",

#ggsave(filename = paste0('Y:/Inshore/Inshore Scallop Fishing Area Map/Management_Spatial_Scale_Maps/Map3_SPA_and_Bank_Management_Groupings-DRAFT.png'), plot = last_plot(), scale = 2.5, width = 12, height = 8, dpi = 300, units = "cm", limitsize = TRUE)



# Figure 3. Biological Population Groupings  ---------------------------

scallop.bounds <- rbind(bof.scallop.bounds, sfa29.scallop.bounds, sfa29E.scallop.bounds, ban.scallop.bounds,
                        mid.scallop.bounds, bbn.scallop.bounds, bbs.scallop.bounds, ger.scallop.bounds,
                        Gb.scallop.bounds, sab.scallop.bounds)

plot(scallop.bounds)

ggplot()+
  geom_sf(data = bathy.sf, colour = "grey", alpha = 0.4)+
  geom_sf(data = scallop.bounds, size = 0.5, aes(fill = Area, colour = Area), alpha = 1) +
  geom_sf(data = SPA_Bank_all, size = 0.5, colour = "grey", fill = NA, alpha = 0.4)+
  geom_sf(data = land.sf)+
  scale_fill_manual(values = mycols.4, name = "Scallop Habitat")+
  scale_colour_manual(values = mycols.5, name = "Scallop Habitat")+
  ggsn::scalebar(data = scallop.bounds, dist = 150, st.size=4, height=0.009, transform = TRUE, dist_unit = "km", model = 'WGS84', border.size = 0.3, anchor = c(x = -55.6, y = 40.35))+
  ggsn::north(scallop.bounds, symbol = 11, scale = 0.092, anchor = c(x = -55.4, y = 41.50))+
  coord_sf(xlim = c(-68.3,-54.9), ylim = c(39.9,47.9), expand = FALSE)+
  labs(x = "Longitude", y = "Latitude")+
  theme_bw()+
  theme(plot.margin = margin(1.5,1,1,1, "cm"),panel.border = element_rect(colour = "black", fill=NA, size=1),legend.key = element_rect(colour = "black", size=0.5)) #legend.position="none"

#ggsave(filename = paste0('Y:/Inshore/Inshore Scallop Fishing Area Map/Management_Spatial_Scale_Maps/Map4_Population_Groupings-DRAFT.png'), plot = last_plot(), scale = 2.5,width = 12, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


