library(raster)
library(sf)
library(tidyverse)
library(mapview)


# DATA SET UP - Already done - Skip to next section -----------------------

# 50 meter resolution
# --------------------
# 1 - Bathy
# 2 - BPI Broad - 1000m
# 3 - BPI Fine - 50m
# 4 - Backscatter - bulkshift method by Ben Misiuk
# 5 - Easterness - TASSE toolbox - Lecours et al., 2016
# 6 - Slope - Log slope
# 7 - Mean grain size - Ben Misiuk
# 8 - Northerness - TASSE toolbox - Lecours et al., 2016
# 9 - Relative deviation from mean value (pits and peaks)- TASSE toolbox - Lecours et al., 2016
# 10 - Benthoscape (Shapefile) - Wilson et al., 2020

# 800 meter resolution
# --------------------
# 11 - Mixed Layer Depth - BNAM Wang et al., 2018
# 12 - Bottom Stress - BNAM Wang et al., 2018
# 13 - Bottom Temperature - BNAM Wang et al., 2018
# 14 - Bottom Salinity - BNAM Wang et al., 2018

# 100 meter resolution
# --------------------
# 15 - Sediment mobility frequency - Li et al., 2017
# 16 - Wave Shear Stress -  Li et al., 2017

#direct <- "E:/BOF_envirodataset/Snapped"

#pred_list <- list.files(direct, pattern =".gri$",full.names = TRUE)
#predictors <- lapply(pred_list, raster) 
#predictors <- stack(predictors) #MUST BE SAME EXTENT/RESOLUTION

#benthoscape <- st_read("Z:/Projects/BoF_Mapping_Project/Analysis/Benthoscape_mapping/Unsupervised_Classification/UnsupervisedClassification_shapefiles/BoF_Benthoscape_Unsupervised_objDissolve/BoF_Benthoscape_objDissolve.shp")
#benthoscape <- st_transform(benthoscape, crs = st_crs(4326)) %>%
#  st_transform(st_crs(32620))

#hm.dat <- readRDS("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/Prorated/horsemussellive_prorated.rds") %>%
#  mutate(mid_lon = MID_LONG) %>% #duplicate lat, longs to use after converting to sf
#  mutate(mid_lat = MID_LAT) %>% 
#  st_as_sf(coords = c("MID_LONG", "MID_LAT"), crs = 4326) %>% 
#  st_transform(32620)

#enviro.dat <- raster::extract(predictors, hm.dat, na.rm = FALSE) #from Rasters
#benthoscape <- st_intersection(benthoscape, hm.dat)  #from shapefile

#Join raster data with horse mussel data
#hm.enviro.dat <- cbind(hm.dat %>% dplyr::select(ID, CRUISE, TOW, ABUND.STD, PRESENT.ABSENT, STRATA_ID, START_LONG, START_LAT, mid_lon, mid_lat), enviro.dat)
#join benthoscape classes
#hm.enviro.dat <- st_join(hm.enviro.dat,benthoscape  %>% dplyr::select(c(CLASS, ID)), by = "ID")

#mapview::mapview(hm.enviro.dat)

#names(predictors) 
#c("Bathy","BtmSalinity","BtmStress","BtmTemp","MLD","Backscatter","Easterness","Curvature", "Slope", "BBPI","FBPI","MeanGrainSize", "Northerness", "RDMV", "Sed_Mobilization_Freq","StandardDev_Bathy", "Wave_Shear_Vel") 

#hm.enviro.dat <- hm.enviro.dat %>% 
#  mutate(PRESENT.ABSENT = as.factor(PRESENT.ABSENT)) %>% 
#  rename(ID = ID.x) %>%
#  rename(Bathy = BOF_ALLBath_2010_50m_adj_dodd_gsc_finalc_z20) %>% 
#  rename(BtmSalinity = layer.1) %>% 
#  rename(BtmStress = layer.2) %>%
#  rename(BtmTemp = layer.3) %>% 
#  rename(MLD = layer.4) %>% 
#  rename(Backscatter = Band_1.1) %>%
#  rename(Easterness = w001001.1) %>%
#  rename(Curvature = Fundy50m_Curv) %>%
#  rename(Slope = Fundy50m_Slope) %>%
#  rename(BBPI = FundyBroadBPI_5_100SnapStan) %>%
#  rename(FBPI = FundyFineBPI_1_10SnapStan) %>%
#  rename(MeanGrainSize = Band_1.2) %>%
#  rename(Northerness = w001001.3) %>%
#  rename(RDMV = w001001.4) %>%
#  rename(Sed_Mobilization_Freq = Sediment.Mobilization.Freq.Interp) %>%
#  rename(StandardDev_Bathy = w001001.5) %>% 
#  rename(Wave_Shear_Vel = Wave.Shear.Velocity.Interp) %>% 
#  rename(Benthoscape = CLASS) %>% 
#  dplyr::select(!ID.y)

# Separate by Strata ------------------------------------------------------

#hm.enviro.dat <- hm.enviro.dat %>% 
#  mutate(SPATIAL_AREA = case_when(STRATA_ID == 22 ~ "SMB", 
#                                  STRATA_ID %in% c(23,56) ~ "BI",
#                                  STRATA_ID %in% c(41:45) ~ "LURCHER",
#                                  STRATA_ID %in% c(30:32) ~ "SPA6",
#                                  STRATA_ID %in% c(1:21, 47, 48) ~ "SPA4",
#                                  (STRATA_ID %in% c(35, 49, 50, 51, 52) & START_LONG >= -64.9254) ~ "UPPERBAY", 
#                                  STRATA_ID %in% c(37:38, 53:55) ~ "INNERBAY",
#                                  (STRATA_ID == 49 & START_LONG <= -64.9254) ~ "INNERBAY",
#                                  STRATA_ID == 39 ~ "MIDBAYSOUTH")) %>% 
#  mutate(SPATIAL_AREA = as.factor(SPATIAL_AREA))

#summary(hm.enviro.dat)


#hm.enviro.sf <- hm.enviro.dat %>% 
  #filter(!is.na(Bathy)) %>% 
  #filter(!is.na(MaxSal)) %>% 
  #filter(Benthoscape != "Not_Classified") %>% 
#  mutate(STRATA_ID = as.factor(STRATA_ID))

#hm.enviro.dat <- hm.enviro.sf %>% st_set_geometry(NULL)

#write.csv(hm.enviro.dat, "Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/HorseM_EnviroData.csv", row.names = F)

# ENVIRONMENTAL DATA ---------------------------------------------------------------


library(tidyverse)
library(raster)
library(sf)
library(mgcv)
library(lme4)
library(mapview)
library(gridExtra)
library(gratia)
library(ggspatial)
library(lattice)
library(viridis)
library(ggcorrplot)
library(flextable)
library(GGally)

#ZUUR functions
source("Z:/Projects/GB_time_area_closure_SPERA/scripts/HighstatLibV11.R")

#Load the environmental data
enviro.dat <- read.csv("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/HorseM_EnviroData.csv")

#Table with predictor variable metadata
pred.tab <- read.csv("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/PredictorVariableTable.csv")

enviro.dat.long <- pivot_longer(enviro.dat, cols = Bathy:Wave_Shear_Vel,  names_to = "Pred_Variable", values_to = "Value")

# Outliers in Terrain variables
ggplot(enviro.dat.long , aes(Value, TOW)) +
  geom_point(size = 0.01)+
  scale_y_reverse() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_wrap(vars(Pred_Variable), scales = "free")

ggplot(enviro.dat.long , aes(TOW, Value)) +
  geom_boxplot()+
  scale_y_reverse() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(vars(Pred_Variable), scales = "free")

# Collinearity Terrain Vars
cor <- round(cor(enviro.dat %>% drop_na() %>% dplyr::select(Bathy,Backscatter, Easterness, Curvature,Slope, BBPI, FBPI, MeanGrainSize, Northerness, RDMV,StandardDev_Bathy)), 1)
ggcorrplot(cor, hc.order = TRUE, type = "lower",
           outline.col = "white", lab = TRUE, lab_size = 2, tl.cex = 8)

#GGPAIRS
lowerFn <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(color = 'blue', alpha=0.3, size=1) +
    geom_smooth(color = 'black', method='lm', size=1,...)
  p
}

g <- ggpairs( 
  data = enviro.dat %>% drop_na() %>% dplyr::select(Bathy,Backscatter, Easterness, Curvature,Slope, BBPI, FBPI, MeanGrainSize, Northerness, RDMV,StandardDev_Bathy),
  lower = list(
    continuous =  wrap(lowerFn) #wrap("smooth", alpha = 0.3, color = "blue", lwd=1) 
  ),
  upper = list(continuous = wrap("cor", size = 2)), progress = F
)
g <- g + theme(
  axis.text = element_text(size = 6),
  axis.title = element_text(size = 6),
  legend.background = element_rect(fill = "white"),
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "grey95")
)
print(g, bottomHeightProportion = 0.5, leftWidthProportion = .5)

#GVIFS
corvif(enviro.dat[,c("Bathy","Backscatter", "Easterness", "Curvature","Slope", "BBPI", "FBPI", "MeanGrainSize", "Northerness", "RDMV","StandardDev_Bathy")])#,"Benthoscape","SPATIAL_AREA")])

corvif(enviro.dat[,c("Bathy","Backscatter", "Easterness", "Curvature","BBPI", "FBPI", "MeanGrainSize", "Northerness", "RDMV","StandardDev_Bathy")])#,"Benthoscape","SPATIAL_AREA")])

# Collinearity Oceanographic Vars
cor <- round(cor(enviro.dat %>% drop_na() %>% dplyr::select(BtmSalinity:MLD,Sed_Mobilization_Freq,Wave_Shear_Vel)), 1)
ggcorrplot(cor, hc.order = TRUE, type = "lower",
           outline.col = "white", lab = TRUE, lab_size = 2, tl.cex = 8)

#GGPAIRS
#ggpairs(enviro.dat %>% drop_na() %>% dplyr::select(BtmSalinity:MLD,Sed_Mobilization_Freq,Wave_Shear_Vel),progress = F)
lowerFn <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(color = 'blue', alpha=0.3, size=1) +
    geom_smooth(color = 'black', method='lm', size=1,...)
  p
}

g <- ggpairs( 
  data = enviro.dat %>% drop_na() %>% dplyr::select(BtmSalinity:MLD,Sed_Mobilization_Freq,Wave_Shear_Vel),
  lower = list(
    continuous =  wrap(lowerFn) #wrap("smooth", alpha = 0.3, color = "blue", lwd=1) 
  ),
  upper = list(continuous = wrap("cor", size = 4)), progress = F
)
g <- g + theme(
  axis.text = element_text(size = 6),
  axis.title = element_text(size = 6),
  legend.background = element_rect(fill = "white"),
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "grey95")
)
print(g, bottomHeightProportion = 0.5, leftWidthProportion = .5)


#GVIFS
corvif(enviro.dat[,c("BtmSalinity", "BtmStress", "BtmTemp", "MLD","Sed_Mobilization_Freq","Wave_Shear_Vel")])#,"Benthoscape","SPATIAL_AREA")])

corvif(enviro.dat[,c("BtmStress", "BtmTemp", "MLD","Sed_Mobilization_Freq","Wave_Shear_Vel")])#,"Benthoscape","SPATIAL_AREA")])

corvif(enviro.dat[,c("BtmStress", "BtmTemp","Sed_Mobilization_Freq","Wave_Shear_Vel")])#,"Benthoscape","SPATIAL_AREA")])

#CORPLOTS ALL VARS
cor <- round(cor(enviro.dat %>% drop_na() %>% dplyr::select(Bathy:Wave_Shear_Vel) %>% dplyr::select(!c(MLD,BtmSalinity, Slope))), 1)
ggcorrplot(cor, hc.order = TRUE, type = "lower",
           outline.col = "white", lab = TRUE, lab_size = 2, tl.cex = 8)

#GGPAIRS
lowerFn <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(color = 'blue', alpha=0.3, size=1) +
    geom_smooth(color = 'black', method='lm', size=1,...)
  p
}

g <- ggpairs( 
  data = enviro.dat %>% drop_na() %>% dplyr::select(Bathy:Wave_Shear_Vel) %>% dplyr::select(!c(MLD,BtmSalinity, Slope)),
  lower = list(
    continuous =  wrap(lowerFn) #wrap("smooth", alpha = 0.3, color = "blue", lwd=1) 
  ),
  upper = list(continuous = wrap("cor", size = 2)), progress = F
)
g <- g + theme(
  axis.text = element_text(size = 6),
  axis.title = element_text(size = 6),
  legend.background = element_rect(fill = "white"),
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "grey95")
)
print(g, bottomHeightProportion = 0.5, leftWidthProportion = .5)

#GVIFS ALL
corvif(enviro.dat[,c("Bathy","Backscatter", "Easterness", "Curvature","BBPI", "FBPI", "MeanGrainSize", "Northerness", "RDMV","StandardDev_Bathy","BtmStress", "BtmTemp","Sed_Mobilization_Freq","Wave_Shear_Vel")])#,"Benthoscape","SPATIAL_AREA")])


# Trends between the covariate domain and the observed locations

direct <- "E:/BOF_envirodataset/Snapped"

pred_list <- list.files(direct, pattern =".gri$",full.names = TRUE)
predictors <- lapply(pred_list, raster) 
predictors <- stack(predictors) #MUST BE SAME EXTENT/RESOLUTION

pred.sp <- as(predictors, 'SpatialPixelsDataFrame')

names(pred.sp) <- c("Bathy","BtmSalinity","BtmStress","BtmTemp","MLD","Backscatter","Easterness","Curvature", "Slope", "BBPI","FBPI","MeanGrainSize", "Northerness", "RDMV", "Sed_Mobilization_Freq","StandardDev_Bathy", "Wave_Shear_Vel")

pred.domain <- pred.sp@data %>% dplyr::select(!c(MLD,BtmSalinity, Slope))

pred.obs <- enviro.dat %>% 
  dplyr::select(Bathy:Wave_Shear_Vel) %>% dplyr::select(!c(MLD,BtmSalinity, Slope))

#values at Obervations vs. values across domain.

for(i in 1:14){
  print(ggplot(data=data.frame(pred.obs = pred.obs[,i]),
               aes(x=pred.obs, colour="Observations")) +
          geom_density() +
          geom_density(data=data.frame(pred.domain = pred.domain[,i]),
                       aes(x=pred.domain, colour='Domain')) + xlab(paste0(names(pred.obs[i]))) +
          scale_color_manual(name = "Density", values = c('Observations' = 'black', 'Domain' = 'red')))
}


# ABUNDANCE AND PRESENCE ---------------------------------------------------------------


#Cleavland dot plot
dotchart(enviro.dat$ABUND.STD, 
         xlab = "Range of data", 
         ylab = "Values")

#Abundance
hist(enviro.dat$ABUND.STD , main = paste("Histogram of Abundance"), xlab="Abundance" , col=rgb(0.8,0.8,0.3,0.5) , las=2)


#Presence Absence
enviro.dat <- enviro.dat %>% drop_na() %>%  mutate(PRESENT.ABSENT = as.factor(PRESENT.ABSENT))

pa.count <- enviro.dat %>% group_by(PRESENT.ABSENT) %>% 
  summarise(count = n()) %>% ungroup

#Presence/Absence
ggplot(enviro.dat, aes(x=PRESENT.ABSENT, fill = PRESENT.ABSENT)) +
  geom_histogram(position="dodge", stat = "count")+
  geom_text(aes(y=count, label = count), data = pa.count, vjust = -1)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6)+
  ylim(0,1000)+
  theme(legend.position="top")

pa.count.2 <- enviro.dat %>% group_by(PRESENT.ABSENT, SPATIAL_AREA) %>% 
  summarise(count = n()) %>% ungroup

ggplot(enviro.dat, aes(x=SPATIAL_AREA, fill= PRESENT.ABSENT)) +
  geom_histogram(position="dodge", stat = "count")+
  ylim(0,500)+
  geom_text(aes(y=count, label = count), data = pa.count.2, vjust = -1, position = position_dodge(.9))+
  #geom_text(aes(y=count, label = count), data = pa.count.2)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6)+
  theme(legend.position="top")



#Abundance By Spatial Area
enviro.dat %>%
  ggplot( aes(x=SPATIAL_AREA, y=ABUND.STD, fill=SPATIAL_AREA)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  #theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Abundance by Spatial Area") +
  xlab("")

#Abundance By Benthoscape

enviro.dat %>% filter(Benthoscape != "Not_Classified") %>% 
  ggplot( aes(x=Benthoscape, y=ABUND.STD, fill=Benthoscape)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.5, alpha=0.9) +
  #theme_ipsum() +
  theme(axis.text.x = element_text(size = 4.5),
        legend.position="none",
        plot.title = element_text(size=11)
  ) +
  ggtitle("Abundance and Benthoscape") +
  xlab("")

# Abundance and Continuous Variables
ggplot(enviro.dat.long %>% filter(!Pred_Variable %in% c("BtmSalinity","MLD", "Slope")), aes(Value, ABUND.STD)) +
  geom_point(size = 0.01)+
  geom_smooth()+
  facet_wrap(vars(Pred_Variable), scales = "free")

var.name <- c("Bathy","BtmStress","BtmTemp","Backscatter","Easterness","Curvature", "BBPI","FBPI","MeanGrainSize", "Northerness", "RDMV", "Sed_Mobilization_Freq","StandardDev_Bathy", "Wave_Shear_Vel")


# Abundance and Continuous Variables by Spatial Area
for(i in 1:length(var.name)){
  print(enviro.dat.long %>% drop_na() %>% filter(Pred_Variable == var.name[i]) %>% 
          ggplot(aes(Value, ABUND.STD)) +
          geom_smooth()+
          geom_point(size = 2)+
          xlab(paste0(var.name[i])) +
          facet_wrap(vars(SPATIAL_AREA), scales = "free"))
}

# Presence and Absence with Continuous Variables

enviro.dat.long <- enviro.dat.long %>% mutate(PRESENT.ABSENT = as.factor(PRESENT.ABSENT))

ggplot(enviro.dat.long %>% drop_na() %>%  filter(!Pred_Variable %in% c("BtmSalinity","MLD", "Slope")),
       aes(PRESENT.ABSENT, Value)) +
  geom_boxplot()+
  facet_wrap(vars(Pred_Variable), scales = "free")


# Read in layer(s) to plot ------------------------------------------------
RastList <- list.files("E:/BOF_envirodataset/Snapped", pattern =".grd$",full.names = TRUE) #Change path
rasterList1 <- lapply(RastList, raster)
rast.stack <- stack(rasterList1)

names(rast.stack) <- c("Bathy","BtmSalinity","BtmStress","BtmTemp","MLD","Backscatter","Easterness","Curvature", "Slope", "BBPI","FBPI","MeanGrainSize", "Northerness", "RDMV", "Sed_Mobilization_Freq","StandardDev_Bathy", "Wave_Shear_Vel")                   


image(rast.stack[[1:6]], col = viridis(500,option = "A"), useRaster=TRUE,maxnl=6)
image(rast.stack[[7:12]], col = viridis(500,option = "A"), useRaster=TRUE,maxnl=6)
image(rast.stack[[13:17]], col = viridis(500,option = "A"), useRaster=TRUE,maxnl=6)