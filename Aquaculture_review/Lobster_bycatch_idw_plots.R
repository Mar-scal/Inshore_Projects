#LOBSTER BYCATCH survey idw plots

library(sf)

#Functions:
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
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

#Data:

#LobsterSurv <- read.csv("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/data/lobcom_dataset.csv")
#LobsterSurv <- read.csv("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/data/lobrec_dataset.csv")
LobsterSurv <- read.csv("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/data/lobpre_dataset.csv") 

LobsterSurv <- LobsterSurv|> 
  mutate(ID = 1:nrow(LobsterSurv)) |> 
  filter(year >= 1991) # To match scallop data temporal scale

LobsterSurv.sf <- LobsterSurv |>
  st_as_sf(coords = c("SLONG","SLAT"), crs = 4326) |> 
  st_transform(4269)

sites.sf <- st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/SMB_proposed_Aquaculture_sites.shp")
pezs <- rbind(st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1449.shp") |> st_cast("LINESTRING"),
              st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1450.shp"),
              st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1451.shp"),
              st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1452.shp")|> st_cast("LINESTRING"))

pez.1449 <- st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1449.shp")
pez.1452 <- st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1452.shp")
pez.poly <- st_union(pez.1449, pez.1452)

poly.crop <- st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/polytocropto.shp")
LobsterSurv.sf <- st_intersection(LobsterSurv.sf, st_union(pez.poly))

#### Import Mar-scal shapefiles

# Find where tempfiles are stored
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles

#Management zones
mgmt.zones <- rbind(st_read(paste0(temp2, "/SPA1A_polygon_NAD83.shp")) %>% mutate(ET_ID = "1A"), st_read(paste0(temp2, "/SPA1B_polygon_NAD83.shp")) %>% mutate(ET_ID = "1B"), st_read(paste0(temp2, "/SPA2_polygon_NAD83.shp"))%>% mutate(ET_ID = "2"), st_read(paste0(temp2, "/SPA3_polygon_NAD83.shp"))%>% mutate(ET_ID = "3"), st_read(paste0(temp2, "/SPA4_polygon_NAD83.shp"))%>% mutate(ET_ID = "4"), st_read(paste0(temp2, "/SPA5_polygon_NAD83.shp"))%>% mutate(ET_ID = "5"), st_read(paste0(temp2, "/SPA6A_polygon_NAD83.shp")) %>% mutate(ET_ID = "6A"), st_read(paste0(temp2, "/SPA6B_polygon_NAD83.shp")) %>% mutate(ET_ID = "6B"), st_read(paste0(temp2, "/SPA6C_polygon_NAD83.shp")) %>% mutate(ET_ID = "6C"),  st_read(paste0(temp2, "/SPA6D_polygon_NAD83.shp")) %>% mutate(ET_ID = "6D")) %>% 
  st_transform(crs= 4269)


land.chunk <- st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/mainland.chunk.shp")

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


####################################################################################################################
#Setting up for plot:

#SPA3
com.contours <- contour.gen(LobsterSurv %>% filter(STRATA_ID %in% c(22, 23, 24)) %>% #only SPA3
                              dplyr::select(ID, SLONG, SLAT, ABUN_STD), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls <- c(0,2,4,6,8,10,12,14,16,18)
#lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4269) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#plot(totCont.poly.sf)
#mapview::mapview(totCont.poly.sf) +
#  mapview::mapview(sites.sf, col.regions = "red")

#st_write(totCont.poly.sf, "Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/1991-2022_prerecDensity.shp", driver = "ESRI Shapefile", overwrite = T)
#unique(totCont.poly.sf$level)
labels <- c("1-2", "2-4", "4-6", "6-8", "8-10", "10-12","12-14","14-16","16-18")
#labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- rev(brewer.pal(length(lvls),"RdYlBu")) #RdBu, Spectral set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(N,tow)), limits = labels)


#SPA 4 and SPA 1A
com.contours.2 <- contour.gen(LobsterSurv %>% 
                                filter(!STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>%
                                dplyr::select(ID, SLONG, SLAT, ABUN_STD), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


#lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL.2 <- contourLines(com.contours.2$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP.2 <- convCP(CL.2)
totCont.poly.2 <- CP.2$PolySet

##Convert pbsmapping object to sf
totCont.poly.2 <- as.PolySet(totCont.poly.2,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly.2 <- PolySet2SpatialLines(totCont.poly.2) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf.2 <- st_as_sf(totCont.poly.2) %>%
  st_transform(crs = 4269) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP.2$PolyData$level))

contours <- rbind(totCont.poly.sf, totCont.poly.sf.2, sf_column_name = NULL) |> 
  group_by(level) %>%
  summarise()

#########################

#Now plot it.

p <- pecjector(area ="spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('tl',0.5,-1,-1)), add_custom = list(obj = contours %>% arrange(level) %>% mutate(brk = labels[1:length(unique(contours$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk) %>% st_intersection(st_union(pez.poly)), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_sf(data = LobsterSurv.sf %>% 
            st_intersection(st_union(pez.poly)), colour = "black", size = 0.5) +
  geom_sf(data = pezs, colour = "red", size = 1, alpha = 0.7, fill = NA) +
  geom_sf(data = land, colour = "black", size = 1, fill = "grey") +
  geom_sf(data = mgmt.zones, colour = "black", size = 1, fill = NA, linetype = "dashed") +
  geom_sf(data = land.chunk, colour = "grey", size = 1, fill = "grey") +
  geom_sf_text(data = mgmt.zones |> filter(ET_ID == 3), aes(label = "SPA 3"), size = 4, colour = "black")+
  geom_sf_text(data = mgmt.zones |> filter(ET_ID == "1A"), aes(label = "SPA 1A"), size = 4, colour = "black", nudge_y = -0.22, nudge_x = -0.2)+
  geom_sf_text(data = mgmt.zones |> filter(ET_ID == "4"), aes(label = "SPA 4"), size = 4, colour = "black", nudge_y = -0.15, nudge_x = -0.25)+
  geom_sf(data = sites.sf, colour = "black", size = 1.25, fill = alpha("white", 0.2)) + #alpha = 0.7,
  geom_sf_text(data = sites.sf, aes(label = Site), size = 3, colour = "black", nudge_x = 0.015, nudge_y = 0.009)+
  #geom_sf(data = pezs, colour = "red", size = 1, alpha = 0.7, fill = NA) +
  labs(title = paste("1991-2022 ", "SPA3 Pre-recruit size Lobster (< = 69mm CW)"), x = "Longitude", y = "Latitude") + #
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.52,-65.80), ylim = c(44.1,44.60), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.87,.34), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#ggsave(filename = paste0("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/exploratory_figures/1991-2022_PrerecruitLobster_Density.png"), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


#####################################################################################################################################3

#Number per tow plot - per size class, and SPA3 strata

Lobster.com <- read.csv("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/data/lobcom_dataset.csv") |> 
  mutate(size_class = "Commercial")
Lobster.rec <- read.csv("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/data/lobrec_dataset.csv")|> 
  mutate(size_class = "Recruit")
Lobster.prerec <- read.csv("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/data/lobpre_dataset.csv") |> 
  mutate(size_class = "Pre-Recruit")

Lobster <- rbind(Lobster.com, Lobster.rec, Lobster.prerec)

#Lobster.sf <- Lobster |>
#  st_as_sf(coords = c("SLONG","SLAT"), crs = 4326) |> 
#  st_transform(4269)

Lobster.spa3 <- Lobster %>% filter(STRATA_ID %in% c(22, 23, 24))

#plot(Lobster.spa3 |> dplyr::select(STRATA_ID))

#Lobster.spa3 <- Lobster.spa3 |> 
#  mutate(STRATA_ID = case_when(STRATA_ID == 22 ~ "St.Marys", 
 #                              STRATA_ID == 23 ~ "Brier Island",
 #                              STRATA_ID == 24 ~ "Lurcher"))

Lobster.spa3 <- Lobster.spa3 |> 
  mutate(Spatial_area = case_when(STRATA_ID == 22 ~ "St.Marys", 
                               STRATA_ID == 23 ~ "Outside",
                               STRATA_ID == 24 ~ "Outside"))

Lobster.spa3 <- Lobster.spa3 |> 
  filter(year >= 2010) |> 
  group_by(Spatial_area, year, size_class) |> 
  summarise(ABUN_STD_mean = mean(ABUN_STD))

median.by.size <- Lobster.spa3 |>
  group_by(size_class, Spatial_area) |> 
  summarise(ABUN_STD_mean = median(ABUN_STD_mean))

#write.csv(median.by.size, "Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/data/median.numpertow.by.size.spatialarea.csv", row.names = FALSE)
Lobster.spa3$year <- as.numeric(Lobster.spa3$year)

ggplot(data = Lobster.spa3,aes(x=year, y=ABUN_STD_mean, col=size_class), group = 1) + 
  geom_point() + 
  geom_line()+
  geom_hline(data = median.by.size, aes(yintercept=ABUN_STD_mean,col=size_class),size = 1, linetype = "dotted") +
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.88)) + 
  scale_color_manual(values=c("sienna3","seagreen3","maroon4"))+
  scale_x_continuous(breaks = seq(2010,2022,2))+
  facet_wrap(~Spatial_area)

