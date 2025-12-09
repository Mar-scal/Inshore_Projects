
library(tidyverse)
library(ROracle)
require(maptools)
require(sf)
require(lubridate)
library(magrittr)

#### Import Source functions####
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/archive/2016/contour.gen.r") 
direct <- getwd()
for(fun in funcs) 
{
  temp <- direct
  download.file(fun,destfile = basename(fun))
  source(paste0(direct,"/",basename(fun)))
  file.remove(paste0(direct,"/",basename(fun)))
}


# LIVE- CAMERA DATA  -------------------------------------------------

hm.dat <- read.csv("Z:/Projects/Horse_Mussel/HM_CameraSurvey/data/HM_camerasurvey.csv")

#Standardize images to #/m^2
hm.dat <- hm.dat %>% 
  filter(!is.na(SurfaceArea.m2)) %>%  
  mutate(TOTAL.STD = Modiolus_modiolus/SurfaceArea.m2) %>%
  mutate(Lat = Latitude) %>% 
  mutate(Long = Longitude) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(32620) %>% 
  mutate(ID = Station)

hm.dat %>% filter(Image.Quality=="Poor" & TOTAL.STD > 0)
nrow(hm.dat %>% filter(TOTAL.STD > 0))


# Load environmental data -------------------------------------------------

direct <- "E:/BOF_envirodataset/Snapped"
BoF_pred_list <- list.files(direct, pattern =".gri$",full.names = TRUE)  #Change path for data resolution
# Put the rasters into a RasterStack:
BoF_predictors <- raster::stack(BoF_pred_list)

mapview::mapview(BoF_predictors[[20]])

#Read in Benthoscape shapefile
benthoscape <- st_read("Z:/Projects/BoF_Mapping_Project/Analysis/Benthoscape_mapping/Unsupervised_Classification/UnsupervisedClassification_shapefiles/BoF_Benthoscape_Unsupervised_objDissolve/BoF_Benthoscape_objDissolve.shp")

#STRATA FILE FROM GITHUB
# Find where tempfiles are stored
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/inshore_survey_strata.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the strata shapefile
strata <- st_read(paste0(temp2, "/PolygonSCSTRATAINFO_rm46-26-57.shp"))%>% 
  st_transform(32620)

# Extract environmental data at image coords: ---------------------------------
enviro.dat <- raster::extract(BoF_predictors, hm.dat, na.rm = FALSE) #from Rasters
benthoscape <- st_intersection(benthoscape, hm.dat)  #from shapefile
strata.hm <- st_intersection(strata, hm.dat)


#Join raster data with horse mussel data
hm.enviro.dat <- cbind(benthoscape %>% dplyr::select(Station, TOTAL.STD, ImageID, Image.Quality, CLASS, Long, Lat), enviro.dat)
hm.enviro.dat <- inner_join(hm.enviro.dat, strata.hm %>% dplyr::select(STRATA_ID, ImageID) %>% st_set_geometry(NULL), by = "ImageID")

# Tidy Data --------------------------------------------------------


hm.enviro.dat <- hm.enviro.dat %>% 
  #mutate(PRESENT.ABSENT = as.factor(PRESENT.ABSENT)) %>%
  rename(Bathy = BOF_ALLBath_2010_50m_adj_dodd_gsc_finalc_z20) %>% 
  rename(BtmSalinity = layer.1) %>% 
  rename(BtmStress = layer.2) %>%
  rename(BtmTemp = layer.3) %>% 
  rename(MLD = layer.4) %>% 
  rename(Backscatter = Band_1.1) %>%
  rename(Easterness = w001001.1) %>%
  rename(Curvature = Fundy50m_Curv) %>%
  rename(Slope = Fundy50m_Slope) %>%
  rename(BBPI = FundyBroadBPI_5_100SnapStan) %>%
  rename(FBPI = FundyFineBPI_1_10SnapStan) %>%
  rename(MeanGrainSize = Band_1.2) %>%
  rename(Northerness = w001001.3) %>%
  rename(RDMV = w001001.4) %>%
  rename(Sed_Mobilization_Freq = Sediment.Mobilization.Freq.Interp) %>%
  rename(StandardDev_Bathy = w001001.5) %>% 
  rename(Wave_Shear_Vel = Wave.Shear.Velocity.Interp) %>% 
  rename(Benthoscape = CLASS)


# Separate by Strata ------------------------------------------------------

hm.enviro.dat <- hm.enviro.dat %>% 
  mutate(SPATIAL_AREA = case_when(STRATA_ID == 22 ~ "SMB", 
                                  STRATA_ID %in% c(23,56) ~ "BI",
                                  STRATA_ID %in% c(41:45) ~ "LURCHER",
                                  STRATA_ID %in% c(30:32) ~ "SPA6",
                                  STRATA_ID %in% c(1:21, 47, 48) ~ "SPA4",
                                  (STRATA_ID %in% c(35, 49, 50, 51, 52) & Long >= -64.9254) ~ "UPPERBAY", 
                                  STRATA_ID %in% c(37:38, 53:55) ~ "INNERBAY",
                                  (STRATA_ID == 49 & Long <= -64.9254) ~ "INNERBAY",
                                  STRATA_ID == 39 ~ "MIDBAYSOUTH")) %>% 
  mutate(SPATIAL_AREA = as.factor(SPATIAL_AREA))

summary(hm.enviro.dat)


hm.enviro.sf <- hm.enviro.dat %>%
  mutate(PRESENT.ABSENT = if_else(TOTAL.STD >= 1, 1, 0)) %>%
  filter(Benthoscape != "Not_Classified") %>% 
  mutate(STRATA_ID = as.factor(STRATA_ID)) %>% 
  mutate(Benthoscape = as.factor(Benthoscape)) 

#hm.enviro.sf <- merge(hm.enviro.sf,hm.enviro.dat, by = "ImageID")

hm.enviro.dat <- hm.enviro.sf %>% st_set_geometry(NULL)

#write.csv(hm.enviro.dat, "Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Documents/DataExploration_report/HM_camdata_andEnviro.csv", row.names = F)


vars.perstation <- hm.enviro.dat %>%
  mutate(PRESENT.ABSENT = as.factor(PRESENT.ABSENT)) %>% 
  group_by(Station, Benthoscape, SPATIAL_AREA,PRESENT.ABSENT) %>%
  summarise(across("Long":"Wave_Shear_Vel", ~ mean(.x, na.rm = TRUE)))

tot.perstation <- hm.enviro.dat %>% 
  group_by(Station, Benthoscape, SPATIAL_AREA,PRESENT.ABSENT) %>%
  summarise(TOTAL.STD = sum(TOTAL.STD))

per.station <- merge(tot.perstation, vars.perstation, by = "Station")

per.station <- per.station %>% 
  dplyr::select(!c(SPATIAL_AREA.y, Benthoscape.y,PRESENT.ABSENT.y)) %>%
  rename(SPATIAL_AREA = SPATIAL_AREA.x) %>% 
  rename(Benthoscape = Benthoscape.x) %>% 
  rename(PRESENT.ABSENT = PRESENT.ABSENT.x)

write.csv(per.station, "Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/HorseM_Cam_EnviroData.csv", row.names = F)

# Contour plots -----------------------------------------------------------

plot.hm.enviro.dat <- hm.enviro.dat %>%
  group_by(Station) %>% 
  summarise(TOTAL.STD = sum(TOTAL.STD), Lat = mean(Lat), Long = mean(Long))%>%
  mutate(PRESENT.ABSENT = if_else(TOTAL.STD >= 1, 1, 0)) # Absent == 0, Present = 1 

plot.hm.enviro.dat$PRESENT.ABSENT <- as.factor(plot.hm.enviro.dat$PRESENT.ABSENT)
  
plot.hm.enviro.dat <- plot.hm.enviro.dat %>% st_set_geometry(NULL)


com.contours <- contour.gen(plot.hm.enviro.dat %>% dplyr::select(Station, Long, Lat, TOTAL.STD),
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
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

#Plot with Pecjector for each area:
p <- pecjector(area = "inshore",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", scale.bar = c('tl',0.5,-1,-1)), # bathy = "ScallopMap"
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

#Plot without contours
p <- pecjector(area = "inshore",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", scale.bar = c('tl',0.5,-1,-1))) # bathy = "ScallopMap"


#PLOT

p + #Plot survey data and format figure.
  geom_spatial_point(data = plot.hm.enviro.dat, aes(Long, Lat, colour = PRESENT.ABSENT, shape = PRESENT.ABSENT), size = 3) + #
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "Live M.modiolus Density"),
  #guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.50,-64.30), ylim = c(43.10,45.80), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.80,.52), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8)) + #Legend bkg margin (top, right, bottom, left)
  guides(shape = guide_legend(override.aes = list(size = 3)))

ggsave(filename = paste0('Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Documents/DataExploration_report/Figures/PresenceAbsence_CameraHM.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)



# Data Exploration --------------------------------------------------------


source("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/HighstatLibV7.R")

#Inspect the file
#What do we have?
names(hm.enviro.dat)

#[1] "TOTAL.STD"     "ImageID"       "Image.Quality" "Benthoscape"  
#[5] "Bathy"         "MLD"           "BBPI"          "FBPI"         
#[9] "BS_Bulkshift"  "Easterness"    "LogSlope"      "MaxVel"       
#[13] "MaxSal"        "MaxTemp"       "MeanGrainsize" "MeanVel"      
#[17] "MeanSal"       "MeanTemp"      "MinSal"        "MinTemp"      
#[21] "Northerness"   "rdmv"          "SMF"           "WSV"          
#[25] "STRATA_ID"

#Are factor factors? - Yes (Benthoscape, SPATIAL_AREA)
str(hm.enviro.dat)

#data.frame':	1369 obs. of  25 variables:
# $ TOTAL.STD    : num  0 0 0 0 0 0 0 0 0 0 ...
# $ ImageID      : chr  "20170412_BoF_Mod1_4K_224850_003.tif" "20170412_BoF_Mod1_4K_224915_004.tif"...
# $ Image.Quality: chr  "Moderate" "Poor" "Poor" "Poor" ...
# $ Benthoscape  : chr  "Sand" "Sand" "Sand" "Sand" ...
# $ Bathy        : num  -95 -95 -95.3 -95.3 -95.3 ...
# $ MLD          : num  14 14 14 14 14 ...
# $ BBPI         : num  -0.133 -0.133 -0.36 -0.36 -0.36 ...
# $ FBPI         : num  0.3694 0.3694 0.0278 0.0278 0.0278 ...
# $ BS_Bulkshift : num  -20.6 -20.6 -18.9 -18.9 -18.9 ...
# $ Easterness   : num  -0.432 -0.432 -0.657 -0.657 -0.657 ...
# $ LogSlope     : num  -1.86 -1.86 -1.26 -1.26 -1.26 ...
# $ MaxVel       : num  0.717 0.717 0.716 0.716 0.716 ...
# $ MaxSal       : num  32.7 32.7 32.7 32.7 32.7 ...
# $ MaxTemp      : num  8.44 8.44 8.44 8.44 8.44 ...
# $ MeanGrainsize: num  2.43 2.43 1.92 1.92 1.92 ...
# $ MeanVel      : num  0.358 0.358 0.358 0.358 0.358 ...
# $ MeanSal      : num  32 32 32 32 32 ...
# $ MeanTemp     : num  7.37 7.37 7.37 7.37 7.37 ...
# $ MinSal       : num  32 32 32 32 32 ...
# $ MinTemp      : num  5.5 5.5 5.5 5.5 5.5 ...
# $ Northerness  : num  -0.756 -0.756 -0.751 -0.751 -0.751 ...
# $ rdmv         : num  0.1638 0.1638 0.0402 0.0402 0.0402 ...
# $ SMF          : num  41.3 41.3 41.2 41.2 41.2 ...
# $ WSV          : num  0.032 0.032 0.032 0.032 0.032 ...
# $ STRATA_ID    : Factor w/ 32 levels "1","2","3","4",..: 18 18 18 18 18 18 18 18 18 13 ...

##########################################################################################


# A Outliers in Y -------------------------------------------------
par(mfrow = c(1, 2))
boxplot(hm.enviro.dat$TOTAL.STD, 
        main = "Abundance")
dotchart(hm.enviro.dat$TOTAL.STD, 
         xlab = "Range of data", 
         ylab = "Values")


# A Outliers in X -------------------------------------------------
par(mfrow = c(4, 5))
dotchart(hm.enviro.dat$Bathy, main = "Depth")
dotchart(hm.enviro.dat$MLD, main = "Mixed Layer Depth")
dotchart(hm.enviro.dat$BBPI, main = "Broad BPI")
dotchart(hm.enviro.dat$FBPI, main = "Fine BPI")
dotchart(hm.enviro.dat$BS_Bulkshift, main = "Backscatter")
dotchart(hm.enviro.dat$Easterness, main = "Easterness")
dotchart(hm.enviro.dat$LogSlope, main = "Log Slope")
dotchart(hm.enviro.dat$MaxVel, main = "Maximum Velocity")
dotchart(hm.enviro.dat$MaxSal, main = "Maximum Salinity")
dotchart(hm.enviro.dat$MaxTemp, main = "Maximum Temp")
dotchart(hm.enviro.dat$MeanGrainsize, main = "Mean Grain size")
dotchart(hm.enviro.dat$MeanVel, main = "Mean Velocity")
dotchart(hm.enviro.dat$MeanSal, main = "Mean Salinity")
dotchart(hm.enviro.dat$MeanTemp, main = "Mean Temp")
dotchart(hm.enviro.dat$MinSal, main = "Minimum Salinity")
dotchart(hm.enviro.dat$MinTemp, main = "Minimum Temp")
dotchart(hm.enviro.dat$Northerness, main = "Norhterness (aspect)")
dotchart(hm.enviro.dat$rdmv, main = "Relative deviation from Mean")
dotchart(hm.enviro.dat$SMF, main = "Sediment Mobility Frequency")
dotchart(hm.enviro.dat$WSV, main = "Wave Shear Stress")
#dotchart(hm.enviro.dat$Benthoscape, main = "Benthoscape class")
#dotchart(hm.enviro.dat$SPATIAL_AREA, main = "Spatial area")


#OR#
#multi-panel dotplot
MyVar <- c("Bathy", "MLD","BBPI" ,"FBPI","BS_Bulkshift","Easterness","LogSlope","MaxVel","MaxSal",       
           "MaxTemp","MeanGrainsize", "MeanVel","MeanSal","MeanTemp","MinSal","MinTemp", "Northerness","rdmv",         
           "SMF","WSV","Benthoscape","SPATIAL_AREA")

Mydotplot(as.matrix(hm.enviro.dat[,MyVar])) 


#Identify the outlier in area.
plot(x = hm.enviro.dat$SMF, 
     y = 1:nrow(hm.enviro.dat),
     xlab = "Value of variable",
     ylab = "Order of the data from text file")

identify(x = hm.enviro.dat$SMF, y = 1:nrow(hm.enviro.dat))


#Apply transformations

# B Collinearity X ------------------------------------------------


pairs(hm.enviro.dat[,c("Bathy", "MLD","BBPI" ,"FBPI","BS_Bulkshift","Easterness","LogSlope","MaxVel","MaxSal",       
                       "MaxTemp","MeanGrainsize", "MeanVel","MeanSal","MeanTemp","MinSal","MinTemp", "Northerness","rdmv",
                       "SMF","WSV")],
      lower.panel = panel.cor)

corvif(hm.enviro.dat[,c("Bathy", "MLD","BBPI" ,"FBPI","BS_Bulkshift","Easterness","LogSlope","MaxVel","MaxSal",       
                        "MaxTemp","MeanGrainsize", "MeanVel","MeanSal","MeanTemp","MinSal","MinTemp", "Northerness","rdmv",
                        "SMF","WSV")])#,"Benthoscape","SPATIAL_AREA")])

#Variance inflation factors

#GVIF
#Bathy          7.138631  *
#MLD            8.195943  *
#MaxVel        16.585184  *
#MaxSal        12.648527  *
#MaxTemp       68.785222  *
#SMF            3.990756  *
#WSV            7.269310  *
#MeanVel       26.090731  *
#MeanSal       18.328741  *
#MeanTemp      77.617174  *
#MinSal        11.307067  *
#MinTemp       29.605638  *

#BBPI           1.944175
#FBPI           2.031477
#BS_Bulkshift   2.340267
#Easterness     1.266851
#LogSlope       1.337724
#Northerness    1.287441
#rdmv           1.328511
#MeanGrainsize  2.994327


#Correlation with Factors:

#Boxplots Loop through all variables
#Benthoscape - Derived from Bathy, Backscatter, Slope, Wave shear stress, Broad Batymetric Position Index
par(mfrow = c(4, 5))
for(i in 10:29){
  factor.cor <- boxplot(hm.enviro.dat[,i] ~ factor(Benthoscape), 
                        data = hm.enviro.dat,
                        varwidth = TRUE,
                        ylab = "Benthoscape",
                        xlab = colnames(hm.enviro.dat[i]),
                        main = "")
  factor.cor
}

#Boxplots Loop through all variables
#SPATIAL AREA
par(mfrow = c(4, 5))
for(i in 10:29){
  factor.cor <- boxplot(hm.enviro.dat[,i] ~ factor(SPATIAL_AREA), 
                        data = hm.enviro.dat,
                        varwidth = TRUE,
                        ylab = "Spatial Area",
                        xlab = colnames(hm.enviro.dat[i]),
                        main = "")
  factor.cor
}

# C Relationships Y vs X --------------------------------------------------

MyVar <- c("TOTAL.STD", "Bathy", "MLD","BBPI" ,"FBPI","BS_Bulkshift","Easterness","LogSlope","MaxVel","MaxSal",       
           "MaxTemp","MeanGrainsize", "MeanVel","MeanSal","MeanTemp","MinSal","MinTemp", "Northerness","rdmv",         
           "SMF","WSV","Benthoscape","SPATIAL_AREA")
pairs(hm.enviro.dat[, MyVar],
      lower.panel = panel.cor)


boxplot(TOTAL.STD ~ factor(Benthoscape), 
        data = hm.enviro.dat,
        varwidth = TRUE,
        ylab = "Horse mussel abundance",
        xlab = "Benthoscape",
        main = "")

boxplot(TOTAL.STD ~ factor(SPATIAL_AREA), 
        data = hm.enviro.dat,
        varwidth = TRUE,
        ylab = "Horse mussel abundance",
        xlab = "Spatial Area",
        main = "")


#Plot every covariate versus Y
MyX  <- c("Bathy", "MLD","BBPI" ,"FBPI","BS_Bulkshift","Easterness","LogSlope","MaxVel","MaxSal",       
          "MaxTemp","MeanGrainsize", "MeanVel","MeanSal","MeanTemp","MinSal","MinTemp", "Northerness","rdmv",         
          "SMF","WSV")
Myxyplot(hm.enviro.dat, MyX, "TOTAL.STD", MyYlab = "Abundance")

# D Spatial/temporal aspects of sampling design -------


# E Interactions (is the quality of the data good enough to include  --------

#Loops to plot all variables for Spatial area
for(i in 7:28){
  
  p1 <- xyplot(TOTAL.STD ~ hm.enviro.dat[i] | factor(SPATIAL_AREA),
               data = hm.enviro.dat, 
               xlab = colnames(hm.enviro.dat[i]),
               ylab = "Abundance",
               strip = function(bg = 'white', ...) 
                 strip.default(bg = 'white', ...),
               scales = list(alternating = TRUE, 
                             x = list(relation = "free"),
                             y = list(relation = "same")),
               panel=function(x,y){
                 panel.grid(h=-1, v= 2)
                 panel.points(x, y, col = 1)
                 #panel.loess(x,y,col=1,lwd=2) #Add smoother
                 panel.abline(lm(y~x))        #Add regression line
               })
  print(p1)
}

#Loops to plot all variables for Benthoscape
for(i in 7:28){
  
  p2 <- xyplot(TOTAL.STD ~ hm.enviro.dat[i] | factor(Benthoscape),
               data = hm.enviro.dat, 
               xlab = colnames(hm.enviro.dat[i]),
               ylab = "Abundance",
               strip = function(bg = 'white', ...) 
                 strip.default(bg = 'white', ...),
               scales = list(alternating = TRUE, 
                             x = list(relation = "free"),
                             y = list(relation = "same")),
               panel=function(x,y){
                 panel.grid(h=-1, v= 2)
                 panel.points(x, y, col = 1)
                 #panel.loess(x,y,col=1,lwd=2) #Add smoother
                 panel.abline(lm(y~x))        #Add regression line
               })
  print(p2)
}


# F Zero inflation Y ------------------------------------------------------

#Frequency plots - # of observations
par(mar = c(4, 4, 3, 2))
plot(table(round(hm.enviro.dat$TOTAL.STD)),
     type = "h",
     xlim = c(0, 100),
     xlab = "Observed values", ylab = "Frequency")



# G Are categorical covariates balanced? ----------------------------------










