# --------------------------------------------------------------------------#
#                           Holistic Condition Plot 
#                     + survey condition for all survey years       
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

#ROracle
uid <- un.englishg
pwd <- pw.englishg
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#set year 
survey.year <- 2026  #survey year
area <- "SPA6"
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


##### |--------------------------- HOLISTIC CONDITION --------------------------------------| ####
#---- Meat weight shell height modelling ----
liveweightYYYY <- data
dim(liveweightYYYY)

## unique tows & observations 
table(liveweightYYYY$Tow)

#create dataset for model
test.data <- subset(liveweightYYYY, Shell_height_mm>40) #ensuring we only model with above 40mm scallops like in assessment
test.data$Log.HEIGHT <- log(test.data$Shell_height_mm)
test.data$Log.HEIGHT.CTR <- test.data$Log.HEIGHT - mean(test.data$Log.HEIGHT)
test.data$Log.DEPTH <- log(abs(test.data$ADJ_DEPTH)) #take abs to keep value positive
test.data$Log.DEPTH.CTR <- test.data$Log.DEPTH - mean(test.data$Log.DEPTH)
summary(test.data)

# run model (GLMM)
MWSH2.YYYY <- glmer(Meat_weight_g~Log.HEIGHT.CTR+Log.DEPTH.CTR+(Log.HEIGHT.CTR|Tow),data=test.data,
                    family=Gamma(link=log), na.action = na.omit)
summary(MWSH2.YYYY)
plot(MWSH2.YYYY)
# the above code of (Log.HEIGHT.CTR|TOW_NO)  shows SH is embedded in Tow where tow is the grouping variable for the random effect. This is a random intercept and slope model
# Random intercept only model would be using (1|TOW_NO)

#For tows that were just those that had samples (ie all match to the random effect) can use re.form=NULL # 

# reform:specify which random effects to condition on when predicting. If NULL, include all random effects; if NA or ~0, include no random effects.


# ----  Condition for Spatial Map ----
# Only calculates the point condition for the single year you modelled above (i.e. surveyyear that's set above) 

livefreq.condition.spatial <- data
#adjust depth, must center on same mean as used in original model
livefreq.condition.spatial$log.ctr.adj_depth <- log(abs(livefreq.condition.spatial$ADJ_DEPTH)) - mean(test.data$Log.DEPTH) 
#subset to just those tows that were detailed sampled 
livefreq.condition.spatial <- livefreq.condition.spatial[is.element(livefreq.condition.spatial$Tow, unique(test.data$Tow)),c("Tow","DDSlat", "DDSlon", "ADJ_DEPTH","log.ctr.adj_depth")]

#GLMM
livefreq.condition.spatial$Condition <- predict(MWSH2.YYYY,newdata=data.frame(Log.HEIGHT.CTR=rep(log(100), dim(livefreq.condition.spatial)[1])-mean(test.data$Log.HEIGHT),Log.DEPTH.CTR=livefreq.condition.spatial$log.ctr.adj_depth, Tow=livefreq.condition.spatial$Tow), re.form=NULL, type="response")

head(livefreq.condition.spatial)
livefreq.condition.spatial <- livefreq.condition.spatial %>% 
  unique() #get rid of duplicated rows/predictions for each tow

#export for spatial plot later 
write.csv(livefreq.condition.spatial, paste0(path.directory,"dataproducts/ConditionForSpatialPlot_",area,"_",survey.year,".csv"), row.names = FALSE)
con.dat<- livefreq.condition.spatial

#Load csv for mapping ####
con.dat <- read.csv(paste0(path.directory, "dataproducts/ConditionForSpatialPlot_",area,"_",survey.year,".csv"))

#### IDW PLOTTING ####
# ----------- Making a custom boundary for IDW analysis based off survey tows ---------------
#THIS STEP REQUIRES ACCES TO SCALLOP SQL DATABASE. This could be re-run with spatial holistic data above, but the shape of IDW outline would change
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
  SPA3 = c("3"),
  SPA6 = c("6A", "6B", "6C"),
  SFA29 = c("29")
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
  filter(group=="SPA6") ### ***pick BoF, SPA3, or SPA6 so that it runs faster than running a layer for all SPAs***
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
  "spa1"  = c(0.85, 0.35),
  "spa1b" = c(0.85, 0.25),
  "spa3"  = c(0.82, 0.44),
  "spa4"  = c(0.88, 0.23),
  "spa6"  = c(0.15, 0.62),
  "sfa29" = c(0.15, 0.62)
)

mgmt_zone_styles <- list(
  "BoF"  = geom_sf(data = mgmt.zones.detailed %>% filter(!STRATA_ID %in% c(26,57,90,91,92,93)), color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid"),
  "spa3" = geom_sf(data = mgmt.zones.detailed %>% filter(STRATA_ID %in% c(92,93)), color = "red", linewidth = 0.3, fill = NA, linetype = "dashed"),
  "spa6" = geom_sf(data = mgmt.zones.detailed %>% filter(STRATA_ID %in% c(90,91)) %>% st_union() %>% st_as_sf(), color = "red", fill = NA, linewidth = 0.3, linetype = "dashed"),
  "sfa29" = geom_sf(data = mgmt.zones.detailed %>% filter(STRATA_ID %in% c(41,42,43,44,45)) %>% st_union() %>% st_as_sf(), color = "red", fill = NA, linewidth = 0.3, linetype = "dashed"),
  # "spa6in" = geom_sf(data = mgmt.zones.detailed %>%  filter(STRATA_ID %in% 90), color = "red", fill = NA, linewidth = 0.2, linetype = "solid"),
  # "spa6out" = geom_sf(data = mgmt.zones.detailed %>%  filter(STRATA_ID %in% 91), color = "black", fill = NA, linewidth = 0.2, linetype = "solid"),
  "Gen"  = geom_sf(data = mgmt.zones, color = "black", fill = NA, linewidth = 0.3, linetype = "solid")
)

coord_ranges = list(
  "BoF" = list(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80)),
  "spa1a" = list(xlim = c(-66.40,-64.70), ylim = c(44.37,45.30)),
  "spa1b" = list(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70)),
  "spa3"  = list(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60)),
  "spa4"  = list(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96)),
  "spa6"  = list(xlim = c(-67.5, -66.35), ylim = c(44.4, 45.2)),
  "sfa29"  = list(xlim = c(-66.5, -65.5), ylim = c(43.1, 43.8)),
  "BoFall" = list(xlim = c(-67.28,-64.4), ylim = c(43.60, 45.62))
) 

#### Plot Condition ####
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(con.dat,coords=c("DDSlon","DDSlat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_condition <- log(Surv.sf$Condition)
Surv.sf$log_condition[which(Surv.sf$log_condition==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_condition~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$Condition)

#PLOT
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Condition (g)", limits = c(min(preds_idw2$prediction),max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Land, Surv.sf,  scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'IDWplot_Condition_',area,"_",survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# Convert sf to terra vector
v <- vect(preds_idw2)

r_template <- rast(ext(v),resolution = 1000,crs = crs(v))

r <- rasterize(v, r_template, field = "prediction")

#checking cells/resolution and making sure it looks similar to IDW sf
res(r)
ncell(r)
plot(r) 

# Write to GeoTIFF
writeRaster(r,paste0(path.directory,"dataproducts/rasters/",area,survey.year,"_Condition.tif"),
            filetype = "GTiff",overwrite = TRUE)


#NOT SURE IF WE NEED TO INCLUDE CODE BELOW FOR COMPARING TO OTHER YEARS? WE DID SHOW THIS FIGURE IN THE PRESENTATION

# #### |------------------------ SURVEY CONDITION -------------------------------------| #####
# #load "condition for spatial plot" files for last few years and combine after giving them all a year column
# con.dat16 <- read.csv("Y:/Inshore/BoF/2022/Assessment/Data/SurveyIndices/SPA6/GMConditionforMap2016.csv")
# con.dat17 <- read.csv("Y:/Inshore/BoF/2022/Assessment/Data/SurveyIndices/SPA6/GMConditionforMap2017.csv")
# con.dat18 <- read.csv("Y:/Inshore/BoF/2022/Assessment/Data/SurveyIndices/SPA6/GMConditionforMap2018.csv")
# con.dat19 <- read.csv("Y:/Inshore/BoF/2022/Assessment/Data/SurveyIndices/SPA6/GMConditionforMap2019.csv")
# con.dat21 <- read.csv("Y:/Inshore/BoF/2022/Assessment/Data/SurveyIndices/SPA6/GMConditionforMap2021.csv")
# con.dat22 <- read.csv("Y:/Inshore/BoF/2022/Assessment/Data/SurveyIndices/SPA6/GMConditionforMap2022.csv")
# con.dat23 <- read.csv("Y:/Inshore/BoF/2023/Assessment/Data/SurveyIndices/SPA6/GMConditionforMap2023.csv")
# con.dat24 <- read.csv("Y:/Inshore/BoF/2024/Assessment/Data/SurveyIndices/SPA6/GMConditionforMap2024.csv")
# 
# con.dat.allyrs <- rbind(con.dat16,con.dat17,con.dat18,con.dat19,con.dat21,con.dat22,con.dat23,con.dat24) %>% 
#   mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
#   mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
#   rename(year = YEAR) #changing to lowercase so function below works 
# 
# #define years data was collected
# years <- sort(unique(con.dat.allyrs$year))
# 
# # Condition ------------------------------------------------------
# #same process for IDW layer creation used in single figures, just made into a function to run multiple years and save as one file for easier plotting
# idw_for_year <- function(y) {
#   
#   Surv.sf <- st_as_sf(
#     subset(con.dat.allyrs, year == y), 
#     coords = c("lon", "lat")
#   ) |>
#     st_set_crs(4326) |>
#     st_transform(32620)
#   
#   Surv.sf$log_condition <- log(Surv.sf$Condition)
#   Surv.sf$log_condition[which(Surv.sf$log_condition==-Inf)] <- log(0.0001)
#   
#   grid <- st_make_grid(idw_sf, cellsize = 1000) |> 
#     st_intersection(idw_sf)
#   
#   preds <- gstat::idw(log_condition ~ 1, Surv.sf, grid, idp = 3)
#   preds$prediction <- exp(preds$var1.pred)
#   preds$year <- y
#   Surv.sf$year <- y
#   
#   list(preds = preds, points = Surv.sf)
# }
# 
# res <- map(years, idw_for_year)
# 
# preds_all_years  <- bind_rows(lapply(res, `[[`, "preds"))
# points_all_years <- bind_rows(lapply(res, `[[`, "points"))
# 
# 
# ggplot() +
#   geom_sf(data = bathy_sf, color = "steelblue", alpha = 0.1, size = 0.5) +
#   geom_sf(data = preds_all_years, aes(fill = prediction), colour = NA) +
#   geom_sf(data = points_all_years, color = "white", fill="black", shape=21, size = 2) + 
#   geom_sf(data = Land, fill = "grey60", colour = NA) +
#   scale_fill_viridis_c(
#     option = "H",
#     trans = "sqrt",
#     name = "Condition (g)",
#     limits = c(9.5, 16.5),
#     oob = scales::squish
#   ) +
#   p(mgmt_zone = mgmt_zone_styles[["Gen"]], 
#     scale_location = "tl",
#     arrow_location = "tl") +
#   coord_sf(
#     xlim = coord_ranges[["spa6"]]$xlim,
#     ylim = coord_ranges[["spa6"]]$ylim,
#     expand = FALSE
#   ) +
#   labs(x = "Longitude", y = "Latitude") +
#   facet_wrap(~year)
# 
# #save
# ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_Condition_allyrs.png'), plot = last_plot(), scale = 2.5, width = 12, height = 12, dpi = 300, units = "cm", limitsize = TRUE)
