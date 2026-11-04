# --------------------------------------------------------------------------#
#                           Holistic Myco/Grey Meat Plots 
#                     + survey myco/grey meats for all survey years       
#
#                          G. English, Dec 2025
#---------------------------------------------------------------------------#

## |--------------Set up libraries and holistic data ---------------------------|####
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
require(openxlsx)


#ROracle
uid <- un.englishg
pwd <- pw.englishg
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#set year and other parameters. Should be only things that need changing
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


##### |--------------------------- HOLISTIC MYCO --------------------------------------| ####
# -------------------------------Format for Myco plots-----------------------------------------
#If no Myco present, this will error, unlike grey meats below

myco.dat <- data %>% 
  group_by(Cruise, Tow, ID, DDSlat, DDSlon) %>% 
  count(Mycobacteria)

#convert to wide table format
myco.datw <- pivot_wider(myco.dat, 
                         names_from = Mycobacteria,
                         values_from = n,
                         values_fill = 0)

myco.datw <- myco.datw %>%
  mutate(tot = N+Y) %>% 
  mutate(prop = Y/(tot)) #calculates proportion of infected meats

# -------------------------------Format for Discoloured scallops plots-----------------------------------------
# Group and count
greymeat.dat <- data %>% 
  group_by(Cruise, Tow, ID, DDSlat, DDSlon) %>% 
  count(Meat_colour)

# Convert to wide format
greymeat.datw <- pivot_wider(greymeat.dat, 
                             names_from = Meat_colour,
                             values_from = n,
                             values_fill = 0)

# Ensure B and C columns exist
greymeat.datw <- greymeat.datw %>%
  mutate(
    B = ifelse(!"B" %in% colnames(greymeat.datw), 0, B),
    C = ifelse(!"C" %in% colnames(greymeat.datw), 0, C)
  )

# Calculate proportion
greymeat.datw <- greymeat.datw %>%
  mutate(
    prop = (B + C) / (A + B + C),
    NUM_GREYMEAT = B + C
  ) %>%
  mutate(NUM_GREYMEAT = as.numeric(NUM_GREYMEAT))

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
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0))# Pre-recruit scallop - BIN_ID_0:BIN_ID_60

table(ScallopSurv$year)
summary(ScallopSurv) #check data

#create shapefile out of survey tow locations. It will get re-written during IDW, but just a placeholder for function below.
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

# Define groups so that BoF, SPA3, and GM can be isolated
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
  filter(group==map.area) ### ***pick BoF, spa3, sfa29 or spa6 for map.area in first few lines of script so that it runs faster than running a layer for all SPAs***
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

# -------------- MYCOBACTERIUM ------------------------------------------------------------
# Proportion of Myco ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(myco.datw,coords=c("DDSlon","DDSlat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop <- log(Surv.sf$prop) 
Surv.sf$log_prop[which(Surv.sf$log_prop==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop)

#PLOT
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,0.25), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[[map.area]]),  Land, Surv.sf, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) + coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
  labs(x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'IDWplot_MycoProportion_',area,"_",survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# # #Myco per Tow  ------------------------------------------------------
# 
# #IDW for whole BoF. Specific changes for each area will be done during plotting
# Surv.sf<-st_as_sf(myco.datw,coords=c("DDSlon","DDSlat"))
# st_crs(Surv.sf) <- 4326
# Surv.sf<-st_transform(Surv.sf,crs = 32620)
# 
# #log transforming for better idw() display. Transformed back later for reader comprehension
# Surv.sf$log_Y <- log(Surv.sf$Y) 
# Surv.sf$log_Y[which(Surv.sf$log_Y==-Inf)] <- log(0.0001)
# 
# 
# #Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
# grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
# #Doing the inverse distance weighted interpolation
# preds_idw2 <- gstat::idw(log_Y~1,Surv.sf,grid,idp=3)
# 
# preds_idw2$prediction <- exp(preds_idw2$var1.pred)
# summary(preds_idw2$prediction)
# summary(Surv.sf$Y)
# 
# #PLOT
# bathy + #Plot survey data and format figure.
#   geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
#   scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/Tow)", limits = c(0,2)) + 
#   p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[[map.area]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) +
#   coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
#   labs(x = "Longitude", y = "Latitude")
# 
# #save
# ggsave(filename = paste0(saveplot.dir,'IDWplot_MycoPerTow_',area,"_",survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE) # figure not included in report
# 
# #FRENCH
# bathy + #Plot survey data and format figure.
#   geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
#   scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/traits de chalut)", limits = c(0,2)) + 
#   p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[[map.area]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) +
#   coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
#   labs(x = "Longitude", y = "Latitude")
# 
# #save
# ggsave(filename = paste0(saveplot.dir,'IDWplot_MycoPerTow_',area,"_",survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# -------------- DISCOLOURED SCALLOPS ------------------------------------------------------------

# Proportion of Discoloured scallops ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(greymeat.datw,coords=c("DDSlon","DDSlat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop <- log(Surv.sf$prop) 
Surv.sf$log_prop[which(Surv.sf$log_prop==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop)

#PLOT
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured scallop \n(proportion)", limits = c(0,0.5), oob = scales::squish) +
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[[map.area]]), Land, Surv.sf,  scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) +
  coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
  labs(x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'IDWplot_GreyMeatProp_',area,"_",survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# #FRENCH
# bathy + #Plot survey data and format figure.
#   geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
#   scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(proportion)", limits = c(0,0.5), oob = scales::squish) +
#   p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[[map.area]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) +
#   coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
#   labs(x = "Longitude", y = "Latitude")
# 
# #save
# ggsave(filename = paste0(saveplot.dir,'IDWplot_GreyMeatProp_',area,"_",survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# Number of Discoloured scallops (moderate + severe) per Tow  ------------------------------------------------------
# 
# #IDW for whole BoF. Specific changes for each area will be done during plotting
# Surv.sf<-st_as_sf(greymeat.datw,coords=c("DDSlon","DDSlat"))
# st_crs(Surv.sf) <- 4326
# Surv.sf<-st_transform(Surv.sf,crs = 32620)
# 
# #log transforming for better idw() display. Transformed back later for reader comprehension
# Surv.sf$log_grey <- log(Surv.sf$NUM_GREYMEAT) 
# Surv.sf$log_grey[which(Surv.sf$log_grey==-Inf)] <- log(0.0001)
# 
# 
# #Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
# grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
# #Doing the inverse distance weighted interpolation
# preds_idw2 <- gstat::idw(log_grey~1,Surv.sf,grid,idp=3)
# 
# preds_idw2$prediction <- exp(preds_idw2$var1.pred)
# summary(preds_idw2$prediction)
# summary(Surv.sf$NUM_GREYMEAT)
# 
# #PLOT
# bathy + #Plot survey data and format figure.
#   geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
#   scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured scallop \n(N/Tow)", limits = c(0,2)) + 
#   p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[[map.area]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) +
#   coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
#   labs(x = "Longitude", y = "Latitude")
# 
# #save
# ggsave(filename = paste0(saveplot.dir,'IDWplot_GreyMeatPerTow_',area,"_",survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE) #figure not included in report
# 
# #FRENCH
# bathy + #Plot survey data and format figure.
#   geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
#   scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(N/traits de chalut)", limits = c(0,2)) + 
#   p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[[map.area]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[[map.area]]) +
#   coord_sf(xlim = coord_ranges[[map.area]]$xlim, ylim = coord_ranges[[map.area]]$ylim, expand = FALSE) +
#   labs(x = "Longitude", y = "Latitude")
# 
# #save
# ggsave(filename = paste0(saveplot.dir,'IDWplot_GreyMeatPerTow_',area,"_",survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


