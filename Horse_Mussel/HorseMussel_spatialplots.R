

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
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

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

ss.scallop.vms <- raster("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Other/FishingEffort_Rastor/Fisheries_VMS_TIFF/SS_Scallop_VMS_Percentiles.tif")
ss.scallop.vms <- projectRaster(ss.scallop.vms, crs = 4326)

ss.gf.mobile.vms<- raster("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Other/FishingEffort_Rastor/Fisheries_VMS_TIFF/SS_Groundfish_Mobile_VMS_Percentiles.tif")
ss.gf.mobile.vms<-projectRaster(ss.gf.mobile.vms, crs = 4326)


#Read in Conservation areas
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/Conservation_areas/DFO_EBSA.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
HM.ebsa <- st_read(paste0(temp2, "/DFO_EBSA.shp")) %>% filter(Name == "Modiolus reefs, NS Shore") %>% st_transform(crs = 4326) %>% dplyr::select(Name)

#Read in Kostelev et al. horse mussel bioherms shapefile

kostelev.poly <- st_read("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Shapefiles/HorseMussel_shapefiles/HM_polygons_Kostylev2009/HM_polygons.shp")

kos.mod.poly <- st_read("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Shapefiles/HorseMussel_shapefiles/HM_polygons_Modified_2019/final_mod_polygons.shp")

# Load Camera survey data (camera)  ------------------------------------------

#camera Survey stations - all stations (i.e. presence and absence)
cam.surv <- st_read("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Shapefiles/AORG_NSCC_Survey_Lines_and_Points/CombinedCameraStation_CtrPts/2017-19_AORG_CameraStns_MERGE_and_CLEANED.shp")

cam.surv <- cam.surv |> 
  mutate(Survey.type = "Cam") |> 
  dplyr::select(Survey.type)

#camera survey horse mussel presence only
hm.pres.cam <- read.csv("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/HMPresence_CameraSurvey.csv") |> 
  mutate(Gear = "Drop Camera")
hm.pres.cam$Station  <- as.factor(hm.pres.cam$Station)

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

#  Merge camera and tow presence data  ------------------------------------------

hm.presence.2023 <- rbind(hm.pres.cam, hm.pres.tow)

write.csv(hm.presence.2023, "Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/HMpresence_drag_and_cam_updated2023.csv")

#  Set up data for spatial plot ------------------------------------------

hm.live.sf <- st_as_sf(hm.live, coords = c("lon","lat"), crs = 4326)

survey.dat <- hm.live.sf |> 
  mutate(Survey.type = "Tow") |> 
  dplyr::select(Survey.type)

survey.dat <- rbind(cam.surv,survey.dat)

# Plot survey tows --------------------------------------------------------

p <- pecjector(area = "inshore",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", scale.bar = c('tl',0.5,-1, -1)))
mycols.4 <-c("sienna3","deepskyblue4")

p+
  geom_sf(data = survey.dat, aes(colour = Survey.type, fill = Survey.type, size = Survey.type, shape = Survey.type))+
  #geom_sf(data = hm.live.sf, colour = "black", size = 1)+
  #geom_sf(data = cam.surv, colour = "firebrick2", size = 2)+
  #geom_sf(data = HM.ebsa, colour = "deeppink4", fill = NA, size = 1)+
  scale_colour_manual(values = mycols.4, labels = c("Camera Survey", "Dredge Survey"), name = "")+
  scale_fill_manual(values = mycols.4, labels = c("Camera Survey", "Dredge Survey"), name = "")+
  scale_size_manual(values = c(2.5,1), labels = c("Camera Survey", "Dredge Survey"), name = "")+
  scale_shape_manual(values = c(8,21), labels = c("Camera Survey", "Dredge Survey"), name = "")+
  coord_sf(xlim = c(-67.50,-64.05), ylim = c(43.10,45.80), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.84,.13), #legend position
        legend.box.background = element_rect(colour = "transparent"), #Legend bkg colour and transparency
        #legend.box.margin = margin(0, 1, 0, 1),
        legend.key.size = unit(15,"mm"), plot.margin = margin(1,0,1,0, "cm"),panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.key=element_rect(colour="black"))

ggsave(filename = "Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/MCT_availabledata_spatialPlot.png", plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#Testing Colour palettes

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
                    
mycols.4 <-c("orangered2","aquamarine3","steelblue4", "cyan4","seagreen3", "magenta4", "lightgoldenrod2",  "darkseagreen3", "sandybrown" , "peachpuff3", "grey44", "tomato4",  "maroon4", "olivedrab3", "aquamarine3")

#mycols.4 <-c("orangered2","aquamarine3")
#mycols.4 <-c("darkseagreen3","lightgoldenrod2")
#mycols.4 <- c("steelblue3","sandybrown")
#mycols.4 <- c("peru","steelblue4")

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

p <- pecjector(area = "inshore",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p+
  #geom_sf(data = hm.live.sf, colour = "black", size = 0.5)+
  geom_sf(data = HM.ebsa, colour = "deeppink4", fill = NA, size = 1)+
  geom_sf(data = kostelev.poly, colour = "yellow", fill = "yellow")+
  coord_sf(xlim = c(-67.50,-64.05), ylim = c(43.10,45.80), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                       axis.title = element_text(size = 12),
                       axis.text = element_text(size = 10),
                       legend.title = element_text(size = 10, face = "bold"), 
                       legend.text = element_text(size = 8),
                       legend.position = c(.84,.32), #legend position
                       legend.box.background = element_rect(colour = "white"), #Legend bkg colour and transparency
                       legend.box.margin = margin(6, 8, 6, 8))


#save
ggsave(filename = paste0(dir,'ContPlot_HM_Density_and_tows_2018-2022.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#Plot with Pecjector - just survey locations:

p <- pecjector(area = "inshore",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)))

p+
  geom_sf(data = hm.live.sf, colour = "black", size = 0.5)+
  #geom_sf(data = HM.ebsa, colour = "deeppink4", fill = NA, size = 1)+
  coord_sf(xlim = c(-67.50,-64.05), ylim = c(43.10,45.80), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.84,.32), #legend position
        legend.box.background = element_rect(colour = "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8))


#save
ggsave(filename = paste0(dir,'ScallopSurvey_tows_2018-2022.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)



