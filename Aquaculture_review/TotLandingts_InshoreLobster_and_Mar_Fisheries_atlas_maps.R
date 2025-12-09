

library(arcgisbinding)
library(raster)
library(sf)
library(stringr)
library(dplyr)
library(mapedit)
library(mapview)
library(units)
library(RColorBrewer)
library(stringr)

arc.check_product()
sf::sf_use_s2(FALSE)

#Read in land shapefile - used for filtering out points on land.
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/other_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

land <- st_read(paste0(temp2, "/Atl_region_land.shp"), crs = 4326) %>% 
  st_transform(crs = 32620) %>%
  filter(PROVINCE %in% c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador")) %>% 
  st_make_valid() %>%  #shapefile contains invalid geometry
  dplyr::select(PROVINCE)

####################################################################################################################

# Read in Geodatabase Layers ----------------------------------------------
#Layers were extracted from https://open.canada.ca/data/en/dataset/c44a8574-9f7d-45b7-afda-27802353a04c
#Geodatabases contain monthly averages from 1990 to 2015 (monthly mean climatology) for depths: 
#surface, 110 m, 156 m, 222 m, 318 m, 541 m, 1062 m, and bottom. Spatial resolution is 1/12 degree.

setwd("E:/AquacultureReview_shps")
mar.fish <- arc.open("Maritimes_Fisheries_Atlas_2014_2018.gdb") #Feature Classes
#mar.fish <- arc.open("CommercialBySpecies.gdb") #Feature Classes

###################################################################################################################

# Extracting ALL *shapefiles* from geodatabase --------------------------

ShpList <- mar.fish@children$FeatureClass

fish <- list() #This takes a while, only use if extracting all layers at all depths
for(i in 1:length(ShpList)){
  fish[[i]] <- arc.data2sf(arc.select(arc.open(paste0("Maritimes_Fisheries_Atlas_2014_2018.gdb/", ShpList[[i]]))))  
}

fish.name <- list() #This takes a while, only use if extracting all layers at all depths
for(i in 1:length(fish)){
  fish.name[[i]] <- fish[[i]]$speciesName[1]
}

mapview(fish[[43]]) #confirm this is the layer we want

#Extract scallop data:
scal <- fish[[43]]
str(scal)

scal <- st_transform(scal, crs = 32620)

mapviewOptions(fgb = FALSE)
mapview(scal)

scal.pts <- scal |> 
  group_by(OBJECTID) |> 
st_centroid()

mapview(scal.pts)

#Extract lobster data
#mapview(fish[[48]])
#lobster <- fish[[48]]
#str(lobster)
#lobster <- st_transform(lobster, crs = 32620)
#mapviewOptions(fgb = FALSE)
#mapview(lobster)
#lobster.pts <- lobster |> 
#  group_by(GRID_ID) |> 
#  st_centroid()
#mapview(lobster.pts)

##################################################################################################################

#Read in lobster data
lobs <- readRDS("Z:/GISdata/Private/InshoreLobsterLandings/LandingsbyGrid2014-2018.rds") |> 
  st_transform(crs = 32620)
#Make Ploygon to crop lobster layer:
#east.shore <- (mapview(lobs)) %>%
#  editMap("east.shore")

#plot(east.shore$drawn)
#east.shore <- st_transform(east.shore$drawn, crs = 4269)
#st_write(east.shore,"Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/east.shore.shp")
east.shore <- st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/east.shore.shp")|> 
  st_transform(crs = 32620)

lobs <- st_difference(lobs,st_union(east.shore))
plot(lobs)

##################################################################################################################
#crop scallop landings to same area as lobster grid

scal.pts <- st_intersection(scal.pts,st_union(lobs))

#Combine scallop (pts) into lobster grid

comb.dat <- lobs %>% 
  st_join(scal.pts, join=st_contains) %>%
  group_by(GRID_NO, land) %>%
  dplyr::summarise(scal_landings_kg=sum(roundWeight_kg, na.rm=T)) |> 
  mutate(tot_landings_kg = scal_landings_kg + land)

mapview(comb.dat)

mapview(comb.dat, zcol = "tot_landings_kg")+
  mapview(lobs, zcol = "land")+
  mapview(scal.pts, zcol = "roundWeight_kg")

#test Grid_no 81 - making sure the landings are adding properly.
#lob.81 <- lobs |> filter(GRID_NO == 81)
#all.spec.pts.81 <- st_intersection(all.spec.pts,st_union(lob.81))
#mapview(lob.81, zcol = "land")+
#  mapview(all.spec.pts.81, zcol = "Weight")

#(sum(all.spec.pts.81$Weight) + lob.81$land)
#comb.dat |> filter(GRID_NO == 81) |> select(tot_landings_kg)



##################################################################################################################
#Area Calculations:

#Crop to land
comb.dat <- st_difference(comb.dat,st_union(land))
mapview(comb.dat, zcol = "tot_landings_kg")


comb.dat <- comb.dat |> 
  mutate(area_m2 = st_area(comb.dat)) |> #Calculate area in m2
  mutate(area_km2 = set_units(area_m2, km^2)) #Convert to km2


tot.area <- sum(comb.dat$area_km2)

#comb.dat <- comb.dat |> 
  #mutate(area.proportion = drop_units(area_km2/tot.area))

comb.dat <- comb.dat |> 
  mutate(landings.std = drop_units(tot_landings_kg/area_km2))
  
mapview(comb.dat, zcol = "landings.std")
plot(comb.dat[8])

#Something wrong with the cropping - polygon used to crop didn't line up exactly so it left some artifacts:

#Some checks

lob.301 <- lobs |> filter(GRID_NO == 301)
mapview(lob.301)
lob.469 <- lobs |> filter(GRID_NO == 469)
mapview(lob.469)
lob.485 <- lobs |> filter(GRID_NO == 485)
mapview(lob.485)

#Remove from comb.dat:
comb.dat <- comb.dat |> 
  filter(!GRID_NO %in% c(301,469,485))

#comb.dat |> filter(GRID_NO %in% c(301,469,485))

#####################################################################################################################

#PLOT

#Read in the inshore boundaries shapefile
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

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

comb.dat <- comb.dat |>
  st_transform(crs = 4326)

col <- rev(brewer.pal(9,"RdYlBu"))

#Remove grids that are not in extent of plot:
comb.dat <- comb.dat |> filter(!GRID_NO %in% c(132:224))

landings.brks <- cut(comb.dat$landings.std,c(1,500,1000,2000,5000,10000,20000,30000,40000,50000,60000,70000), right = FALSE, dig.lab = 5)
comb.dat <- cbind(comb.dat, landings.brks)


#cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(N,tow)), limits = labels)
#breaks <- c(1,50,150,200,250,300,350,400,450)

p <- pecjector(area =list(x=c(-67.08,-64.4), y=c(45.62, 43.70), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('tl',0.5,-1,-1)))


p + #Plot survey data and format figure
  geom_sf(data = comb.dat, aes(fill = landings.brks)) +
  scale_fill_manual(values = alpha(col, 0.9), na.value = 'transparent', name = bquote('Scallop and\n Lobster\n Landings'~(kg/km^2)))+
  geom_sf(data = land, colour = "black", size = 1, fill = "grey") +
  labs(x = "Longitude", y = "Latitude") + #title = paste("1991-2022 ", "SPA3 Adult Scallop Density (> 65 mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .9))) + #Legend transparency
  coord_sf(xlim = c(-67.08,-64.4), ylim = c(43.70, 45.62), expand = FALSE)+
  theme(plot.title = element_text(size = 14), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 9, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.83,.35), #legend position
        legend.box.background = element_rect(colour = 'black', fill= "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(12, 8, 12, 8),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#ggsave(filename = paste0("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/exploratory_figures/Scallop_lobster_totLandings_2014-2018.png"), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##################################################################################################################
# Extracting ALL *shapefiles* from geodatabase --------------------------

mar.fish <- arc.open("Commercial_Fishery_2009to2018.gdb") #Feature Classes
ShpList <- mar.fish@children$FeatureClass

fish <- list() #This takes a while, only use if extracting all layers at all depths
for(i in 1:length(ShpList)){
  fish[[i]] <- arc.data2sf(arc.select(arc.open(paste0("Commercial_Fishery_2009to2018.gdb/", ShpList[[i]]))))  
}

all.spec <- fish[[1]]
str(all.spec)

all.spec <- st_transform(all.spec, crs = 32620)

#mapviewOptions(fgb = FALSE)
mapview(all.spec)

#Convert hexagons to points
all.spec.pts <- all.spec |> 
  group_by(GRID_ID) |> 
  st_centroid()

mapview(all.spec.pts)

##################################################################################################################

#Read in lobster data
lobs <- readRDS("Z:/GISdata/Private/InshoreLobsterLandings/LandingsbyGrid2009-2018.rds") |> 
  st_transform(crs = 32620)
#Make Ploygon to crop lobster layer:
#east.shore <- (mapview(lobs)) %>%
#  editMap("east.shore")

#plot(east.shore$drawn)
#east.shore <- st_transform(east.shore$drawn, crs = 4269)
#st_write(east.shore,"Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/east.shore.shp")
east.shore <- st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/east.shore.shp")|> 
  st_transform(crs = 32620)

lobs <- st_difference(lobs,st_union(east.shore))
plot(lobs)

##################################################################################################################
#crop all species to same area as lobster grid

all.spec.pts <- st_intersection(all.spec.pts,st_union(lobs))

#Combine all species (pts) into lobster grid
comb.dat <- lobs %>% 
  st_join(all.spec.pts, join=st_contains) %>%
  group_by(GRID_NUM, land) %>%
  dplyr::summarise(spec_landings_kg=sum(Weight, na.rm=T)) |> 
  mutate(tot_landings_kg = spec_landings_kg + land)

mapview(comb.dat)

mapview(comb.dat, zcol = "tot_landings_kg")+
  mapview(lobs, zcol = "land")+
  mapview(all.spec.pts, zcol = "Weight")


#test Grid_no 81 - making sure the landings are adding properly.
#lob.81 <- lobs |> filter(GRID_NUM == 81)
#all.spec.pts.81 <- st_intersection(all.spec.pts,st_union(lob.81))
#mapview(lob.81, zcol = "land")+
#  mapview(all.spec.pts.81, zcol = "Weight")

#(sum(all.spec.pts.81$Weight) + lob.81$land)
#comb.dat |> filter(GRID_NUM == 81) |> select(tot_landings_kg)

##################################################################################################################
#Area Calculations:

#Crop to land
comb.dat <- st_difference(comb.dat,st_union(land))
mapview(comb.dat, zcol = "tot_landings_kg")


comb.dat <- comb.dat |> 
  mutate(area_m2 = st_area(comb.dat)) |> #Calculate area in m2
  mutate(area_km2 = set_units(area_m2, km^2)) #Convert to km2


#tot.area <- sum(comb.dat$area_km2)

#comb.dat <- comb.dat |> 
#mutate(area.proportion = drop_units(area_km2/tot.area))

comb.dat <- comb.dat |> 
  mutate(landings.std = drop_units(tot_landings_kg/area_km2))

mapview(comb.dat, zcol = "landings.std")
plot(comb.dat[8])

#Something wrong with the cropping - polygon used to crop didn't line up exactly so it left some artifacts:

#Some checks

lob.301 <- lobs |> filter(GRID_NUM == 301)
mapview(lob.301)
lob.469 <- lobs |> filter(GRID_NUM == 469)
mapview(lob.469)
lob.485 <- lobs |> filter(GRID_NUM == 485)
mapview(lob.485)

#Remove from comb.dat:
comb.dat <- comb.dat |> 
  filter(!GRID_NUM %in% c(301,469,485))

#comb.dat |> filter(GRID_NO %in% c(301,469,485))

#####################################################################################################################
#PLOT

#Read in the inshore boundaries shapefile
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

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

comb.dat <- comb.dat |>
  st_transform(crs = 4326)

#Remove grids that are not in extent of plot:
comb.dat <- comb.dat |> filter(!GRID_NUM %in% c(132:224))

col <- rev(brewer.pal(5,"RdYlBu"))

range(comb.dat$landings.std)
landings.brks <- cut(comb.dat$landings.std,c(1,20000,40000,60000,80000,100000,150000), right = FALSE, dig.lab = 7)
comb.dat <- cbind(comb.dat, landings.brks)
#c(1,20000,40000,60000,80000,100000,150000,200000,250000,300000,350000,400000)

#cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(N,tow)), limits = labels)
#breaks <- c(1,50,150,200,250,300,350,400,450)

p <- pecjector(area =list(x=c(-67.08,-64.4), y=c(45.62, 43.70), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('tl',0.5,-1,-1)))


p + #Plot survey data and format figure
  geom_sf(data = comb.dat, aes(fill = landings.brks)) +
  scale_fill_manual(values = alpha(col, 0.9), na.value = 'transparent', name = bquote('All Species\n Landings'~(kg/km^2)))+
  geom_sf(data = land, colour = "black", size = 1, fill = "grey") +
  labs(x = "Longitude", y = "Latitude") + #title = paste("1991-2022 ", "SPA3 Adult Scallop Density (> 65 mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .9))) + #Legend transparency
  coord_sf(xlim = c(-67.08,-64.4), ylim = c(43.70, 45.62), expand = FALSE)+
  theme(plot.title = element_text(size = 14), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 9, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.83,.25), #legend position
        legend.box.background = element_rect(colour = 'black', fill= "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(12, 8, 12, 8),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#ggsave(filename = paste0("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/exploratory_figures/AllSpecies_totLandings_2009-2018.png"), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)