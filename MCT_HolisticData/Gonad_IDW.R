# --------------------------------------------------------------------------#
#                     Gonad Stats from Holistic Analyses
#                   (Gonad weight & GSI) + Study Area Figure
#
#                          G. English, Dec 2025
#---------------------------------------------------------------------------#

## |--------------Set up libraries and holistic data -----------------------|####
#load packages
require (CircStats)
require (TeachingDemos)
require (PBSmapping)
require (akima)
require (gstat)
require (fields)
require (splancs)
require (RColorBrewer)
require (spatstat)
require (RPMG)
require (rmapshaper)
require (lubridate)
require (tidyverse)
require (sf)
require(maptools)
require(forcats)
library(ROracle)
require(ggspatial)
require(concaveman)
require(lme4)
require(readxl)
require(terra)

#ROracle
uid <- un.englishg
pwd <- pw.englishg
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#set year and other parameters. Outside of selecting maturity stage for gonad model, only these three lines should need to be changed between area/year
survey.year <- 2026  #survey year
area <- "SFA29" #SPA6, BF, SFA29, SPA2
map.area <- "sfa29" #BoF, spa1, spa1b, spa3, spa4, spa6, or sfa29

# path.directory <- paste0("Y:/Projects/Holistic_sampling_Inshore/Holistic_sampling_",area,"/",survey.year,"/MCP_Reporting/")
path.directory <- paste0("Y:/Projects/Holistic_sampling_Inshore/Holistic_sampling_",area,"/",survey.year,"/")

#set up directory to save plot
saveplot.dir <- paste0(path.directory,"Figures/")

#load holistic data
# data <- read_excel(paste0(path.directory,"dataproducts/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), sheet=2) 
# metadata <- read_excel(paste0(path.directory,"dataproducts/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), sheet=1)
#load holistic data
data <- read_excel(paste0(path.directory,"Data/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), sheet=2) 
metadata <- read_excel(paste0(path.directory,"Data/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), sheet=1)


#### Import Mar-scal shapefiles and functions ####
#### Import Mar-scal functions 
funcs <- c(#"https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
  "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
  #"https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r"
) 

dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

# Read in the shapefiles from GitHub

mgmt.zones.detailed <- st_read("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/Scallop_Strata.shp")

Land <- st_read(paste0("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/Atl_region_land.shp")) %>%
  st_transform(crs = 32620)
# Land <- st_read("Y:/GISdata/Private/AtlanticUSCdnCoast/MAR_Maine.shp") %>%
#   st_transform(crs = 32620)

bathy_sf <- st_read(paste0("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/bathymetry/bathymetry_15m.shp")) 

mgmt.zones <- st_read(paste0("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/scallop_management_zones/ScallopFishingAreas_2024.shp")) %>% 
  filter(str_starts(Area_Name, "SPA")|Area_Name == "SFA29W") %>%  
  st_transform(crs = 32620)

#### |-----------------------MODELLING------------------------------------| ####
####  GONAD WEIGHT  ####
# 1-immature, 2-recovering, 3-ripening, 4-ripe, 5-spawning, 6-spent
data_mat <- data %>% 
  filter(Maturity_stage %in% c(#"3",
                               "4",
                               "5"))  #3, 4, 5 (ripening/ripe/spawning) chose for best model fit

#standardize subsetted data
data_mat$Log.HEIGHT <- log(data_mat$Shell_height_mm)
data_mat$Log.HEIGHT.CTR <- data_mat$Log.HEIGHT - mean(data_mat$Log.HEIGHT)
data_mat$Log.DEPTH <- log(abs(data_mat$ADJ_DEPTH)) #take abs to keep value positive
data_mat$Log.DEPTH.CTR <- data_mat$Log.DEPTH - mean(data_mat$Log.DEPTH)
dim(data_mat)
table(data_mat$Tow)

#GLMM
M2.10 <- glmer(Gonad_weight_g ~ Log.HEIGHT.CTR + Maturity_stage + (Log.HEIGHT.CTR|Tow), data=data_mat, family=Gamma(link=log), na.action = na.omit) #MODEL BEING USED FOR IDW PLOT

summary(M2.10)

####  GSI  ####
# NO SIGNIFICANCE SEEN WITH MODELLING APPROACH. 
# MEAN AND SD GSI PERFORMED IN SECTION BELOW

#### |---------------- Gonad Stats for Spatial Map -----------------------| ####
####  GONAD WEIGHT ####
#gonad weight with ripe/ripening (mat=3/4) dataset, controlled for mat=3 and SH=100
livefreq.gonad.spatial <- data_mat
#adjust depth, must center on same mean as used in original model
livefreq.gonad.spatial$log.ctr.adj_depth <- log(abs(livefreq.gonad.spatial$ADJ_DEPTH)) - mean(data_mat$Log.DEPTH) 
#subset to just those tows that were detailed sampled 
livefreq.gonad.spatial <- livefreq.gonad.spatial[is.element(livefreq.gonad.spatial$Tow, unique(data_mat$Tow)),c("Tow", "DDSlat", "DDSlon", "Maturity_stage", "ADJ_DEPTH","log.ctr.adj_depth")]
#first one is gonad weight with ripe/ripening (mat=3/4) dataset, controlled for mat=3 and SH=100
livefreq.gonad.spatial$GonadWeight <- predict(M2.10,newdata=data.frame(Log.HEIGHT.CTR = rep(log(100) - mean(data_mat$Log.HEIGHT), nrow(livefreq.gonad.spatial)),Log.DEPTH.CTR = livefreq.gonad.spatial$log.ctr.adj_depth, Maturity_stage = "5",Tow = livefreq.gonad.spatial$Tow), re.form=NULL, type="response")
#reducing from 560 GW observations (repeats per tow) to 41 observations (1 per tow)
livefreq.gonad.spatial <- livefreq.gonad.spatial %>% 
  unique() %>% 
  filter(Maturity_stage=="5") %>% #all predictions are based on maturity=3. but "maturity=4" has been kept in dataframe, so I'm removing it 
  mutate("SH"=100) # adding SH column to signify the controlling for 100 mm SH in csv 

#### GSI ####
livefreq.gsi.spatial <- data %>% 
  filter(Maturity_stage %in% c("5")) %>% #change to 3,4,or 5, or combinations of them
  mutate(GSI = (Gonad_weight_g/Whole_weight_g)*100) %>% 
  group_by(Tow, DDSlat, DDSlon) %>% 
  summarize(GSI.mean = mean(GSI),
            GSI.sd = sd(GSI)) 

#Export/load file for mapping ####
write.csv(livefreq.gonad.spatial, paste0(path.directory,"Data/GonadForSpatialPlot_",area,"_",survey.year,".csv"), row.names = FALSE)
gonad.dat<- livefreq.gonad.spatial 
write.csv(livefreq.gsi.spatial, paste0(path.directory,"Data/GSIForSpatialPlot_",area,"_",survey.year,".csv"), row.names = FALSE)
gsi.dat<- livefreq.gsi.spatial 

#Load in dataset if already run
gonad.dat <-read.csv(paste0(path.directory,"Data/GonadForSpatialPlot_",area,"_",survey.year,".csv"))
gsi.dat <- read.csv(paste0(path.directory,"Data/GSIForSpatialPlot_",area,"_",survey.year,".csv"))


#### |----------------------- IDW PLOTTING -------------------------------| #### 
# ----------- Making a custom boundary for IDW analysis based off survey tows ---------------
#Db Query:
quer2 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.scliveres s			",
  " LEFT JOIN                          ",
  " 	(SELECT tow_date, cruise, tow_no     ",
  "	 FROM SCALLSUR.sctows) t             ",
  "on (s.cruise = t.cruise and s.tow_no = t.tow_no)",
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30, 31, 32, 35, 37, 38, 39, 41, 42, 43, 44, 45, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56)    ",
  sep=""
)


#If ROracle: 
ScallopSurv <- dbGetQuery(chan, quer2)
ScallopSurv  <- ScallopSurv[,1:51]

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and commercial, recruit and prerecruit data columns
ScallopSurv <- ScallopSurv %>% 
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>% #standardize number per tow to numbers per m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195
  mutate(rec = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0)) # Pre-recruit scallop - BIN_ID_0:BIN_ID_60


table(ScallopSurv$year)
summary(ScallopSurv) #check data

#create shapefile out of survey tow locations. It will get re-written during IDW, but just a placeholder for function below.
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620) 
mapview::mapview(Surv.sf)
# Define groups so that BoF, SPA3, 29W, and GM can be isolated
group_list <- list(
  BoF = c("1", "4", "5"),
  spa3 = c("3"),
  spa6 = c("6A", "6B", "6C"),
  sfa29 = c("29")
)

# Function to process each group
create_polygon <- function(group_ids, group_name) {
  group_points <- Surv.sf[Surv.sf$MGT_AREA_ID %in% group_ids, ]
  
  if (nrow(group_points) > 2) {  # At least 3 points needed for a polygon
    hull <- concaveman(group_points)  # Create concave hull
    
    if (!is.null(hull) && nrow(hull) > 0) {  # Check if hull was created
      buffered <- st_buffer(hull, dist = 5000)  # Apply 5 km buffer
      
      if (!is.null(buffered) && nrow(buffered) > 0) {  # Ensure valid geometry
        return(st_sf(group = group_name, geometry = st_geometry(buffered)))  # Ensure geometry column
      }
    }
  }
  return(NULL)  # Return NULL if no valid polygon was created
}

# Apply function to each group and remove NULLs
polygons <- lapply(names(group_list), function(name) create_polygon(group_list[[name]], name))
idw_sf<- do.call(rbind, polygons[!sapply(polygons, is.null)]) %>%    # Remove NULLs and combine
  filter(group==map.area) ### ***pick BoF, SPA3, or SPA6 so that it runs faster than running a layer for all SPAs***
# Ensure correct CRS
st_crs(idw_sf) <- 32620

# -------------- Set consistent plot objects/themes ------------------------------

#Set standard layers: bathymetry on bottom layer, and land, survey tows, etc above IDW layer
bathy <-   ggplot() + #goes before IDW layer
  geom_sf(data = bathy_sf, color = "steelblue", alpha = 0.1, size = 0.5) +
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)

# Function to generate map layers to go after IDW layer
p <- function(
    mgmt_zone = NULL,
    land = NULL,
    surv_sf = NULL,
    scale_location = "tl",
    arrow_location = "tl",
    legend_position = NULL
) {
  
  layers <- list()
  
  # Management zone layer (optional)
  if (!is.null(mgmt_zone))
    layers <- append(layers, list(mgmt_zone))
  
  # Land polygons (optional)
  if (!is.null(land))
    layers <- append(layers, list(
      geom_sf(data = land, fill = "grey60", colour = NA)
    ))
  
  # Survey points (optional)
  if (!is.null(surv_sf))
    layers <- append(layers, list(
      geom_sf(
        data = surv_sf,
        color = "white", fill = "black",
        shape = 21, size = 1.5
      )
    ))
  
  # Axes formatting (always included)
  layers <- append(layers, list(
    scale_x_continuous(labels = scales::label_number(accuracy = 0.01)),
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  ))
  
  # Theme (optional legend position)
  layers <- append(layers, list(
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      legend.box.background = element_rect(
        colour = "white",
        fill = alpha("white", 0.7)
      ),
      legend.box.margin = margin(6, 8, 6, 8),
      legend.position = legend_position,
      legend.justification = c(0.5, 0.5)
    )
  ))
  
  # Scale bar (if user wants it)
  if (!is.null(scale_location))
    layers <- append(layers, list(
      annotation_scale(
        location = scale_location, width_hint = 0.5,
        pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")
      )
    ))
  
  # North arrow (if user wants it)
  if (!is.null(arrow_location))
    layers <- append(layers, list(
      annotation_north_arrow(
        location = arrow_location, which_north = "true",
        height = unit(1.25, "cm"), width = unit(1, "cm"),
        pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),
        style = north_arrow_fancy_orienteering
      )
    ))
  
  layers
}


#creating list with legend positions so that you can call for "spa1a" legend position. Any edits to legend positions are done here
legend_positions <- list(
  "BoF"  = c(0.85, 0.35),
  "spa1"  = c(0.85, 0.35),
  "spa1b" = c(0.85, 0.25),
  "spa3"  = c(0.82, 0.44),
  "spa4"  = c(0.88, 0.23),
  "spa6"  = c(0.15, 0.62),
  "sfa29" = c(0.15, 0.25)
)

mgmt_zone_styles <- list(
  "BoF"  = geom_sf(data = mgmt.zones.detailed %>% filter(!STRATA_ID %in% c(26,57,90,91,92,93)), color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid"),
  "spa1"  = geom_sf(data = mgmt.zones.detailed %>% filter(!STRATA_ID %in% c(26,57,90,91,92,93)), color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid"),
  "spa1b"  = geom_sf(data = mgmt.zones.detailed %>% filter(!STRATA_ID %in% c(26,57,90,91,92,93)), color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid"),
  "spa3" = geom_sf(data = mgmt.zones.detailed %>% filter(STRATA_ID %in% c(92,93)), color = "red", linewidth = 0.3, fill = NA, linetype = "dashed"),
  "spa4"  = geom_sf(data = mgmt.zones.detailed %>% filter(!STRATA_ID %in% c(26,57,90,91,92,93)), color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid"),
  "spa6" = geom_sf(data = mgmt.zones.detailed %>% filter(STRATA_ID %in% c(90,91)) %>% st_union() %>% st_as_sf(), color = "red", fill = NA, linewidth = 0.3, linetype = "dashed"),
  "sfa29" = geom_sf(data = mgmt.zones.detailed %>% filter(STRATA_ID %in% c(41,42,43,44,45)) %>% st_as_sf(), color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid"),
  # "spa6in" = geom_sf(data = mgmt.zones.detailed %>%  filter(STRATA_ID %in% 90), color = "red", fill = NA, linewidth = 0.2, linetype = "solid"),
  # "spa6out" = geom_sf(data = mgmt.zones.detailed %>%  filter(STRATA_ID %in% 91), color = "black", fill = NA, linewidth = 0.2, linetype = "solid"),
  "Gen"  = geom_sf(data = mgmt.zones, color = "black", fill = NA, linewidth = 0.3, linetype = "solid")
)

coord_ranges = list(
  "BoF" = list(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80)),
  "spa1" = list(xlim = c(-66.40,-64.70), ylim = c(44.37,45.30)),
  "spa1b" = list(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70)),
  "spa3"  = list(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60)),
  "spa4"  = list(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96)),
  "spa6"  = list(xlim = c(-67.5, -66.35), ylim = c(44.4, 45.2)),
  "sfa29"  = list(xlim = c(-66.8, -65.3), ylim = c(43, 44)),
  "BoFall" = list(xlim = c(-67.28,-64.4), ylim = c(43.60, 45.62))
) 

# ----- GONAD WEIGHT ----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(gonad.dat,coords=c("DDSlon","DDSlat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_gonad <- log(Surv.sf$GonadWeight)
Surv.sf$log_gonad[which(Surv.sf$log_gonad==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_gonad~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$GonadWeight)

#PLOT
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H", name = "Gonad weight (g)", limits = c(min(preds_idw2$prediction),max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Land, Surv.sf, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) +
  coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
  labs(x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'IDWplot_Gonad_',area,"_",survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# #FRENCH
# bathy + #Plot survey data and format figure.
#   geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
#   scale_fill_viridis_c(option = "H", name = "poids des gonades (g)", limits = c(5.25,9.75), oob = scales::squish) + #max(preds_idw2$prediction))) + 
#   p(mgmt_zone = mgmt_zone_styles[["Gen"]], Land, Surv.sf,  scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) +
#   coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
#   labs(x = "Longitude", y = "Latitude")
# 
# #save
# ggsave(filename = paste0(saveplot.dir.fr,'IDWplot_Gonad_',area,"_",survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

### For MCT, To be updated for new directories ####
# Convert sf to terra vector
v <- vect(preds_idw2)

r_template <- rast(ext(v),resolution = 1000,crs = crs(v))

r <- rasterize(v, r_template, field = "prediction")

#checking cells/resolution and making sure it looks similar to IDW sf
res(r)
ncell(r)
plot(r) 

# Write to GeoTIFF
writeRaster(r,paste0(path.directory,"dataproducts/rasters/",area,survey.year,"_GonadWeight.tif"),
  filetype = "GTiff",overwrite = TRUE)

# ----- GSI ----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(gsi.dat,coords=c("DDSlon","DDSlat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_gsi <- log(Surv.sf$GSI.mean)
Surv.sf$log_gsi[which(Surv.sf$log_gsi==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_gsi~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$GSI.mean)

#PLOT
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H", name = "GSI (Mean/Tow)", limits = c(min(preds_idw2$prediction),max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Land, Surv.sf, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) +
  coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
  labs(x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'IDWplot_GSI_',area,"_",survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# #FRENCH
# bathy + #Plot survey data and format figure.
#   geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
#   scale_fill_viridis_c(option = "H", name = "Indice gonado-somatique \n(moyenne/traits de chalut)", limits = c(11,26), oob = scales::squish)+ #max(preds_idw2$prediction))) + 
#   p(mgmt_zone = mgmt_zone_styles[["Gen"]], Land, Surv.sf,  scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) +
#   coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
#   labs(x = "Longitude", y = "Latitude")
# 
# #save
# ggsave(filename = paste0(saveplot.dir.fr,'IDWplot_GSI_',area,"_",survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# Convert sf to terra vector
v <- vect(preds_idw2)

r_template <- rast(ext(v),resolution = 1000,crs = crs(v))

r <- rasterize(v, r_template, field = "prediction")

#checking cells/resolution and making sure it looks similar to IDW sf
res(r)
ncell(r)
plot(r) 

# Write to GeoTIFF
writeRaster(r,paste0(path.directory,"dataproducts/rasters/",area,survey.year,"_GSImean.tif"),
            filetype = "GTiff",overwrite = TRUE)


# ----- GSI VARIANCE/SD  ----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(gsi.dat,coords=c("DDSlon","DDSlat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_gsi.sd <- log(Surv.sf$GSI.sd)
Surv.sf$log_gsi.sd[which(Surv.sf$log_gsi.sd==-Inf)] <- log(0.0001)
Surv.sf <- Surv.sf[!is.na(Surv.sf$log_gsi.sd), ]


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_gsi.sd~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$GSI.sd)

#PLOT
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H", name = "GSI (SD/Tow)", limits = c(min(preds_idw2$prediction), max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]],  Land, Surv.sf, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) +
  coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
  labs(x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'IDWplot_GSI.SD_',area,"_",survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)
# 
# #FRENCH
# bathy + #Plot survey data and format figure.
#   geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
#   scale_fill_viridis_c(option = "H", name = "Indice gonado-somatique \n(L'écart type/traits de chalut)", limits = c(1, max(preds_idw2$prediction))) + 
#   p(mgmt_zone = mgmt_zone_styles[["Gen"]], Land, Surv.sf,  scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) +
#   coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
#   labs(#title = paste(survey.year, "", "SPA6 Condition"), 
#     x = "Longitude", y = "Latitude")
# 
# #save
# ggsave(filename = paste0(saveplot.dir.fr,'IDWplot_GSI.SD_',area,"_",survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

### For MCT, To be updated for new directories ####
# Convert sf to terra vector
v <- vect(preds_idw2)

r_template <- rast(ext(v),resolution = 1000,crs = crs(v))

r <- rasterize(v, r_template, field = "prediction")

#checking cells/resolution and making sure it looks similar to IDW sf
res(r)
ncell(r)
plot(r) 

# Write to GeoTIFF
writeRaster(r,paste0(path.directory,"dataproducts/rasters/",area,survey.year,"_GSI_SD.tif"),
            filetype = "GTiff",overwrite = TRUE)


# # SWNB STUDY AREA FIGURE ####
# Surv.sf <- st_as_sf(data, coords=c("DDSlon","DDSlat"))
# st_crs(Surv.sf) <- 4326
# Surv.sf<-st_transform(Surv.sf,crs = 32620)
# 
# custom_labels <- data.frame(
#   x = c(-66.9, -66.3, -66.3),   # longitude
#   y = c(44.4, 44.5, 45.0),      # latitude
#   label = c("SPA6", "SPA1A", "SPA1B")
# )
# 
# bathy <-   ggplot() + #goes before IDW layer
#   geom_sf(data = bathy_sf, color = "steelblue", alpha = 0.1, size = 0.5) +
#   coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)
# 
# bathy + #Plot survey data and format figure.
#   geom_sf(data = mgmt.zones, fill=NA,  colour = "black", size=0.5) +
#   geom_sf(data = Land, fill = "grey60") +
#   geom_sf(data = Surv.sf, color = "black", fill = "red", shape = 21, size = 2) +
#   # Set theme for the plot
#   theme(plot.title = element_text(size = 14, hjust = 0.5), # Plot title size and position
#         axis.title = element_text(size = 14),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 14, face = "bold"), 
#         legend.text = element_text(size = 12),
#         legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), # Legend background color and transparency
#         legend.box.margin = margin(6, 8, 6, 8),
#         legend.position = c(0.15, 0.62), # Dynamic legend position
#         legend.justification = c(0.5, 0.5)) + # Keep legend within bounds, 
#   scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
#   scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
#   # Add scale bar with selectable location
#   annotation_scale(
#     location = "tl", width_hint = 0.5, 
#     pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")
#   ) +
#   annotation_north_arrow(
#     location = "tl", which_north = "true", 
#     height = unit(1.25, "cm"), width = unit(1, "cm"),
#     pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),
#     style = north_arrow_fancy_orienteering
#   ) +
#   coord_sf(xlim = c(-67.5, -66.2), ylim = c(44.3, 45.4), expand = FALSE) +
#   geom_text(data = custom_labels,
#             aes(x = x, y = y, label = label),
#             size = 4, color = "black", fontface = "bold") +
#   labs(x = "Longitude", y = "Latitude")
# 
# ggsave(filename = paste0(saveplot.dir, area,"map_",survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)
# #------------------------------------- END OF SCRIPT -------------------------------------------------#
