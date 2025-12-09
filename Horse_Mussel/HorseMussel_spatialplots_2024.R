

#Load in Prorated HM datasheets (using R script "HM_prorate_and_compare.R) and plot numbers per tow.

library(tidyverse)
library(ROracle)
require(maptools)
require(sf)
require(raster)
library(mapview)
#require(lubridate)
#library(magrittr)

#ROracle - credentials
#uid <- keyring::key_list("Oracle")[1,2]
#pwd <- keyring::key_get("Oracle", uid)
uid <- un.sameotoj
pwd <- pw.sameotoj


dir <- "Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/"
#dir <- "C:/Users/WILSONB/Documents/1_Projects/HM_project_temp/Prorated/"
survey.year <- 2023
Year <- c(2018:survey.year)
Year <- Year[! Year %in% 2020]

#### Import Source functions####
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r") 
direct <- getwd()
for(fun in funcs) 
{
  temp <- direct
  download.file(fun,destfile = basename(fun))
  source(paste0(direct,"/",basename(fun)))
  file.remove(paste0(direct,"/",basename(fun)))
}


#Read in the inshore strata for plotting
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/inshore_survey_strata.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
strata <- st_read(paste0(temp2, "/PolygonSCSTRATAINFO_rm46-26-57.shp"))
#mapview::mapview(strata)

#Read in inshore boundaries
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
SFA29 <- st_read(paste0(temp2, "/SFA29_subareas_utm19N.shp")) %>% mutate(ID = seq(1,5,1)) %>%  #TO FIX IN COPY ON GITHUB (ET_ID missing so adding it here)
  mutate(ET_ID = case_when(ID == 1 ~ 41, 
                           ID == 2 ~ 42,
                           ID == 3 ~ 43,
                           ID == 4 ~ 44,
                           ID == 5 ~ 45)) %>% 
  dplyr::select(Id = ID, ET_ID) %>% st_transform(crs = 4326)


#Load VMS rasters:
#ss.scallop.vms <- raster("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Other/FishingEffort_Rastor/Fisheries_VMS_TIFF/SS_Scallop_VMS_Percentiles.tif")
#ss.scallop.vms <- projectRaster(ss.scallop.vms, crs = 4326)

#ss.gf.mobile.vms<- raster("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Other/FishingEffort_Rastor/Fisheries_VMS_TIFF/SS_Groundfish_Mobile_VMS_Percentiles.tif")
#ss.gf.mobile.vms<-projectRaster(ss.gf.mobile.vms, crs = 4326)


#Read in Conservation areas
#temp <- tempfile()
# Download this to the temp directory
#download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/Conservation_areas/DFO_EBSA.zip", temp)
# Figure out what this file was saved as
#temp2 <- tempfile()
# Unzip it
#unzip(zipfile=temp, exdir=temp2)
#HM.ebsa <- st_read(paste0(temp2, "/DFO_EBSA.shp")) %>% filter(Name == "Modiolus reefs, NS Shore") %>% st_transform(crs = 4326) %>% dplyr::select(Name)
HM.ebsa <- arcpullr::get_spatial_layer("https://gist.dfo-mpo.gc.ca/arcgis/rest/services/Maritimes/Draft_Conservation_Network_Sites/MapServer/0")  
st_crs(HM.ebsa)

head(HM.ebsa)
unique(HM.ebsa$SiteName_E )
#BI.Closure <- HM.ebsa %>% filter(SiteName_E == "Brier Island") 
BI.Closure <- HM.ebsa[47,]
plot(BI.Closure$geoms)


st_write(BI.Closure, "C:/GISdata/Basemaps/ProtectedAreas/networksites_proposed_OEM_MPA_20240312/BI.Closure.shp")



#Read in Kostelev et al. horse mussel bioherms shapefile
#kostelev.poly <- st_read("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Shapefiles/HorseMussel_shapefiles/HM_polygons_Kostylev2009/HM_polygons.shp")
#kos.mod.poly <- st_read("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Shapefiles/HorseMussel_shapefiles/HM_polygons_Modified_2019/final_mod_polygons.shp")

# Load live horse mussel data (survey) for all years and areas (2018-present) ------------------------------------------

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#Db Query:
quer2 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.sclivehorsemussel_std_vw 			",
  sep=""
)

hm.live <- dbGetQuery(chan, quer2)

hm.live <- hm.live %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE))

hm.pres.tow <- hm.live |> filter(tot != 0) |> 
  mutate(PRESENT = 1) |> 
  mutate(Gear = "Drag") |> 
  dplyr::select("Station" = tow, lon, lat, PRESENT, Gear) 
  
hm.pres.tow$Station  <- as.factor(hm.pres.tow$Station)


#  Set up data for spatial plot ------------------------------------------

hm.live.sf <- st_as_sf(hm.live, coords = c("lon","lat"), crs = 4326)

survey.dat <- hm.live.sf |> 
  mutate(Survey.type = "Tow") |> 
  dplyr::select(Survey.type)


# Plot Horse mussel density -----------------------------------------------
hm.live <- hm.live |> 
  mutate(ID = paste0(CRUISE, "_", tow))

#hm.live <- hm.live |> filter(CRUISE != "GM2023")
#hm.live <- hm.live |> filter(CRUISE != "BI2023")

com.contours <- contour.gen(hm.live %>% 
                              dplyr::select(ID, lon, lat, tot),
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

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGnBu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 1.0), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector



#Plot with Pecjector:

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

spa3.tows <- hm.live.sf %>%  filter(CRUISE %in% c("BF2018", "BF2019", "BF2021","BF2022","BF2023" ,"BI2019","BI2021","BI2022","BI2023")) 
spa3.tows <- spa3.tows %>% filter(STRATA_ID %in% c(22, 23, 24, 56))


p +
  geom_sf(data = spa3.tows, colour = "black", size = 0.5) +
  geom_sf(data = HM.ebsa %>% filter(Name_en_Fr == "Brier Island / L’île Brier "), colour = "deeppink4", fill = NA, linewidth = 2) +
 # coord_sf(xlim = c(-67.50,-65.0), ylim = c(43.10,45.50), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.84,.32), #legend position
        legend.box.background = element_rect(colour = "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8))

dir

#save
ggsave(filename = paste0(dir,'HM_density_ScallopSurvey_tows_2019-2023.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


### LOG BOOK DATA 

#Date range for logs to be selected 
start.date.logs <- "2003-10-01"  #YYYY-MM-DD use Oct 1 
ends.date.logs <- "2023-10-01"  #YYYY-MM-DD use Oct 1 


#### Read files ####
#Polygons for spatial plots
poly.sf <- st_read("Y:/Inshore/BoFBoundaries/SPABoundaries_Redrawn2014/SPA New Polys/shp polygons", layer = "SPA3_polygon_NAD83")
poly.VMS <- st_read("Y:/Inshore/Databases/Scallsur/SPA3", layer = "SPA3_VMS_StrataBrierLurcher")
poly.SMB <- st_read("Y:/Inshore/Databases/Scallsur/SPA3", layer = "SMB")


#### Select data ####

quer2 <- paste(
  "SELECT * 			                             ",
  "FROM scallop.scallop_log_marfis s		         ",
  "WHERE s.assigned_area in ('SPA3', '1A')       ",
  " 	AND  s.date_fished >= to_date('",start.date.logs,"','YYYY-MM-DD') and s.date_fished < to_date('",ends.date.logs,"','YYYY-MM-DD') ",
  "	AND (s.data_class = 1                        ",
  "OR (s.data_class = 2 AND s.quality_flag =',4' ) ",
  "OR (s.data_class = 2 AND s.quality_flag =',1,4') ",
  "OR (s.data_class = 2 AND s.quality_flag =',2,4')) ",
  sep=""
)

chan <- dbConnect(drv = dbDriver("Oracle"), username=uid,  password = pwd,  dbname = "ptran", believeNRows=FALSE)

logs <- dbGetQuery(chan, quer2)
dim(logs)
table(logs$ASSIGNED_AREA)

#Convert coordinates:
logs$lat <- convert.dd.dddd(logs$LATITUDE/100) #coordinates in SCALLOP db are stored without the decimal point, have to divide by 100 to get degrees decimal minutes for convert.dd.dddd
logs$lon <- (-1)*convert.dd.dddd(logs$LONGITUDE/100)

#Add year column:
logs$DATE_FISHED <- as.Date(logs$DATE_FISHED, format="%Y-%m-%d")  
logs$YEAR <- as.numeric(format(logs$DATE_FISHED, "%Y")) 

#Check data
table(logs$YEAR) #check years, fishing season in SPA3 spans 2 calendar years
table(logs$FLEET) #check fleets, SPA3 should only have records from Full Bay

#In the 2021 season after verifying with log scans, there was still a CPUE outlier of 180.92 kg/h. Removing records with CPUE > 180
logs <- (logs[which(logs$CPUE_KG <= 180),]) 
dim(logs)

head(logs)

#make as sf object 
logs.sf <- st_as_sf(logs, coords = c("lon", "lat"), crs = 4326, remove = FALSE)


logs.sf <- logs.sf %>% filter(lat < 44.6)

p +
  #geom_sf(data = spa3.tows, colour = "black", size = 0.5) +
  geom_sf(data = HM.ebsa %>% filter(Name_en_Fr == "Brier Island / L’île Brier "), colour = "deeppink4", fill = NA, linewidth = 2) +
  geom_sf(data = logs.sf,aes( size = 0.1),  color = "blue", alpha = 0.1)
  # coord_sf(xlim = c(-67.50,-65.0), ylim = c(43.10,45.50), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.84,.32), #legend position
        legend.box.background = element_rect(colour = "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8))

  #save
  ggsave(filename = paste0(dir,'HM_density_ScallopLogbook_2003-2023.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)
  




