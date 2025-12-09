#KERNAL DENSITY - EFFORT AND POINTS Plots
library(tidyverse)
library(stars)
library(sf)
library(RColorBrewer)
library(cowplot)
sf::sf_use_s2(FALSE)

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

# ----Import Data----------------------------------------------

sites.sf <- st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/SMB_proposed_Aquaculture_sites.shp")
pezs <- rbind(st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1449.shp") |> st_cast("LINESTRING"),
              st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1450.shp"),
              st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1451.shp"),
              st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1452.shp")|> st_cast("LINESTRING"))

pezs <- pezs |> 
  mutate(PEZ_type = "feces")

pez.1449 <- st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1449.shp")
pez.1452 <- st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/proposed_site_data.1452.shp")
pez.poly <- st_union(pez.1449, pez.1452) |> 
  st_transform(32620)

pezs.feed <- rbind(st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/PEZs_SMB/PEZ_feed_1449.shp") |> st_cast("LINESTRING"),
              st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/PEZs_SMB/PEZ_feed_1450.shp"),
              st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/PEZs_SMB/PEZ_feed_1451.shp"),
              st_read("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/shp/PEZs_SMB/PEZ_feed_1452.shp")|> st_cast("LINESTRING"))

pezs.feed <- pezs.feed |> 
  mutate(PEZ_type = "feed")

pezs <- rbind(pezs, pezs.feed)

#logbook.pts <- st_read("E:/AquacultureReview_shps/2004to22_logbook_utm.shp")

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
  st_transform(crs = 32620) %>%
  filter(PROVINCE %in% c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador")) %>% 
  st_make_valid() %>%  #shapefile contains invalid geometry
  dplyr::select(PROVINCE)

######################################################################################################################

## Load KDE Layers - Effort ##

#SPA1A, 3 and 4
kde.effort <-read_stars("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/data/KernelDensity/KernelD_ALL_Effort_utm.tif", NA_value = NA)
kde.effort <- st_warp(kde.effort, crs = 32620)%>% 
  st_crop(st_union(pez.poly))

kde.effort[[1]][kde.effort[[1]] < 1] <- NA
plot(kde.effort)

kde.effort <- cut(kde.effort,c(1,50,100,200,300,400,500,600,700), right = FALSE)#
#kde.effort <- cut(kde.effort,c(1,50,150,200,250,300,400,500,600,700), right = FALSE)#
plot(kde.effort)

kde.effort <- st_transform(kde.effort,crs = 4269)
#logbook.pts <-st_transform(logbook.pts, crs = 4269)
pez.poly <-st_transform(pez.poly, crs = 4269)

labels <- c("1-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500-600", "600-700", "700+")
col <-c("#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FEE090", "#FDAE61", "#F46D43", "#D73027", "firebrick4")
#col <- alpha(rev(brewer.pal(10,"RdYlBu")), 0.8)
#cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(N,tow)), limits = labels)
#breaks <- c(1,50,150,200,250,300,350,400,450)

p <- pecjector(area ="spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('tl',0.5,-1,-1)))


p + #Plot survey data and format figure
  geom_stars(data = kde.effort, aes(fill = KernelD_ALL_Effort_utm.tif))+
  scale_fill_manual(values = alpha(col, 0.9), na.value = 'transparent', labels = labels, name = bquote('Kernel Density\n Fishing Effort'~(h/km^2)))+
  #geom_sf(data = logbook.pts %>% st_intersection(st_union(pez.poly)), colour = "black", size = 0.25) +
  geom_sf(data = pezs, aes(linetype = PEZ_type), colour = "red",  size = 1, alpha = 0.7, fill = NA) +
  #geom_sf(data = pezs.feed, aes(colour = "red"), size = 1.5, fill = NA, alpha = 0.7) +
  scale_linetype_manual(values = c("dashed","solid"), name = "Benthic PEZs", labels = c("Feces - sink speed 0.3 cm/s","Feed - sink speed 5.3 cm/s"))+
  #scale_colour_manual(values = c("red","red"), name = "PEZ", labels = c("Feed - sink rate 5.3 cm/s", "Feces - sink rate 0.3 cm/s"))+
  geom_sf(data = land, colour = "black", size = 1, fill = "grey") +
  geom_sf(data = mgmt.zones, colour = "black", size = 1, fill = NA, linetype = "dashed") +
  geom_sf(data = land.chunk, colour = "grey", size = 1, fill = "grey") +
  geom_sf_text(data = mgmt.zones |> filter(ET_ID == 3), aes(label = "SPA 3"), size = 4, colour = "black")+
  geom_sf_text(data = mgmt.zones |> filter(ET_ID == "1A"), aes(label = "SPA 1A"), size = 4, colour = "black", nudge_y = -0.22, nudge_x = -0.2)+
  geom_sf_text(data = mgmt.zones |> filter(ET_ID == "4"), aes(label = "SPA 4"), size = 4, colour = "black", nudge_y = -0.15, nudge_x = -0.25)+
  geom_sf(data = sites.sf, colour = "black", size = 1, fill = alpha("white", 0.2)) + #alpha = 0.7,
  #geom_sf_label(data = sites.sf, aes(label = Site), size = 3, colour = "black", nudge_x = 0.015, nudge_y = 0.009)+
  geom_sf_text(data = sites.sf, aes(label = Site), size = 4, colour = "black", nudge_x = 0.017, nudge_y = 0.009)+
  labs(x = "Longitude", y = "Latitude") + #title = paste("1991-2022 ", "SPA3 Adult Scallop Density (> 65 mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .9))) + #Legend transparency
  coord_sf(xlim = c(-66.52,-65.80), ylim = c(44.1,44.60), expand = FALSE)+
  theme(plot.title = element_text(size = 14), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 9, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.83,.32), #legend position
        legend.box.background = element_rect(colour = "black", fill= "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(8, 8, 12, 12),
        legend.key = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#ggsave(filename = paste0("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/exploratory_figures/2004-22_logbook_kde_effort_2.png"), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


#####WITH INSETS################################################################################################################

p <- pecjector(area=list(x=c(-66.52,-65.7), y=c(44.60, 44.1), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('tl',0.5,-1,-1)))

map1 <- p + #Plot survey data and format figure
  geom_stars(data = kde.effort, aes(fill = KernelD_ALL_Effort_utm.tif))+
  scale_fill_manual(values = alpha(col, 0.8), na.value = 'transparent', name = bquote('Kernel Density\n Fishing Effort'~(h/km^2)))+
  #geom_sf(data = logbook.pts %>% st_intersection(st_union(pez.poly)), colour = "black", size = 0.25) +
  geom_sf(data = pezs, colour = "red", size = 1, alpha = 0.7, fill = NA) +
  geom_sf(data = land, colour = "black", size = 1, fill = "grey") +
  geom_sf(data = mgmt.zones, colour = "black", size = 1, fill = NA, linetype = "dashed") +
  geom_sf(data = land.chunk, colour = "grey", size = 1, fill = "grey") +
  geom_sf_text(data = mgmt.zones |> filter(ET_ID == 3), aes(label = "SPA 3"), size = 4, colour = "black")+
  geom_sf_text(data = mgmt.zones |> filter(ET_ID == "1A"), aes(label = "SPA 1A"), size = 4, colour = "black", nudge_y = -0.22, nudge_x = -0.2)+
  geom_sf_text(data = mgmt.zones |> filter(ET_ID == "4"), aes(label = "SPA 4"), size = 4, colour = "black", nudge_y = -0.15, nudge_x = -0.25)+
  geom_sf(data = sites.sf, colour = "black", size = 1.25, fill = alpha("white", 0.2)) + #alpha = 0.7,
  #geom_sf_label(data = sites.sf, aes(label = Site), size = 3, colour = "black", nudge_x = 0.015, nudge_y = 0.009)+
  geom_sf_text(data = sites.sf, aes(label = Site), size = 3, colour = "black", nudge_x = 0.017, nudge_y = 0.009)+
  labs(x = "Longitude", y = "Latitude") + #title = paste("1991-2022 ", "SPA3 Adult Scallop Density (> 65 mm)"), 
  guides(fill = guide_legend(ncol = 2, override.aes= list(alpha = .9))) + #Legend transparency
  coord_sf(xlim = c(-66.52,-65.70), ylim = c(44.1,44.60), expand = FALSE)+
  theme(plot.title = element_text(size = 14), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 9, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.85,.84), #legend position
        legend.box.background = element_rect(colour = "black", fill= "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(8, 8, 12, 12),
        panel.border = element_rect(colour = "black", fill=NA, size=1))


coord.1449.1450 <- coord_sf(xlim = c(-66.133,-66.047), ylim = c(44.476,44.416), expand = FALSE)
#coord.1450 <- coord_sf(xlim = c(-66.135,-66.088), ylim = c(44.452,44.419), expand = FALSE)
coord.1451.1452 <- coord_sf(xlim = c(-66.282,-66.127), ylim = c(44.404,44.295), expand = FALSE)
#coord.1452 <- coord_sf(xlim = c(-66.259,-66.211), ylim = c(44.342,44.30), expand = FALSE)

#Plot main with insets:
map2 <- map1 %>% 
  ggdraw() +
  draw_plot(
    {
      ggplot()+
        geom_stars(data = kde.effort, aes(fill = KernelD_ALL_Effort_utm.tif))+
        scale_fill_manual(values = alpha(col, 0.8), na.value = 'transparent', name = bquote('Kernel Density\n Fishing Effort'~(h/km^2)))+
        geom_sf(data = land, colour = "black")+
        geom_sf(data = sites.sf, colour = "black", size = 1.25, fill = alpha("white", 0.2)) +
        geom_sf(data = pezs.feed, colour = "red", size = 1.25, fill = NA) +
        geom_sf_text(data = sites.sf |> filter(Site %in% c(1449, 1450)), aes(label = Site), size = 3, colour = "black", nudge_x = 0.017, nudge_y = 0.009)+ 
        coord.1449.1450+
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_rect(fill = "grey", colour = "grey"),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.x=element_blank(),axis.title.x = element_blank(),axis.ticks.y=element_blank(),
              axis.text.x = element_blank(), legend.position="none",plot.margin=unit(c(0,-1,-1,0),"mm"))
      
    },
    x = 0.67, 
    y = 0.40,
    width = 0.25, 
    height = 0.25) 

#Add second inset
map3 <- map2 %>% 
  ggdraw() +
  draw_plot(
    {
      ggplot()+
        geom_stars(data = kde.effort, aes(fill = KernelD_ALL_Effort_utm.tif))+
        scale_fill_manual(values = alpha(col, 0.8), na.value = 'transparent', name = bquote('Kernel Density\n Fishing Effort'~(h/km^2)))+
        geom_sf(data = land, colour = "black")+
        geom_sf(data = sites.sf, colour = "black", size = 1.25, fill = alpha("white", 0.2)) +
        geom_sf(data = pezs.feed, colour = "red", size = 1.25, fill = NA) +
        geom_sf_text(data = sites.sf |> filter(Site %in% c(1451, 1452)), aes(label = Site), size = 3, colour = "black", nudge_x = 0.017, nudge_y = 0.009)+
        coord.1451.1452+
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_rect(fill = "grey", colour = "grey"),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.x=element_blank(),axis.title.x = element_blank(),axis.ticks.y=element_blank(),
              axis.text.x = element_blank(), legend.position="none",plot.margin=unit(c(0,0,0,0),"mm"))
      
    },
    x = 0.67, 
    y = 0.10,
    width = 0.25, 
    height = 0.25) 

map3

#ggsave(filename = paste0("E:/AquacultureReview_shps/2004-22_logbook_kde_effort_with_inset.png"), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

######################################################################################################################

## Load KDE Layers - point density ##

pez.poly <- st_union(pez.1449, pez.1452) |> 
  st_transform(32620)

#SPA1A, 3 and 4
kde.pts <-read_stars("E:/AquacultureReview_shps/KernelD_ALL_pts_utm.tif", NA_value = NA)
kde.pts <- st_warp(kde.pts, crs = 32620)%>% 
  st_crop(st_union(pez.poly))

kde.pts[[1]][kde.pts[[1]] < 1] <- NA
plot(kde.pts)

kde.pts <- cut(kde.pts,c(1,5,10,20,30,40,50,60), right = FALSE)#
plot(kde.pts)

kde.pts <- st_transform(kde.pts,crs = 4269)
pez.poly <-st_transform(pez.poly, crs = 4269)

col <- rev(brewer.pal(7,"RdYlBu"))

p <- pecjector(area ="spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('tl',0.5,-1,-1)))


p + #Plot survey data and format figure
  geom_stars(data = kde.pts, aes(fill = KernelD_ALL_pts_utm.tif))+
  scale_fill_manual(values = alpha(col, 0.7), na.value = 'transparent', name = "Kernel Density\n Fishing logs")+
  geom_sf(data = logbook.pts %>% 
            st_intersection(st_union(pez.poly)), colour = "black", size = 0.25) +
  geom_sf(data = pezs, colour = "red", size = 1, alpha = 0.7, fill = NA) +
  geom_sf(data = land, colour = "black", size = 1, fill = "grey") +
  geom_sf(data = mgmt.zones, colour = "black", size = 1, fill = NA, linetype = "dashed") +
  geom_sf(data = land.chunk, colour = "grey", size = 1, fill = "grey") +
  geom_sf_text(data = mgmt.zones |> filter(ET_ID == 3), aes(label = "SPA 3"), size = 4, colour = "black")+
  geom_sf_text(data = mgmt.zones |> filter(ET_ID == "1A"), aes(label = "SPA 1A"), size = 4, colour = "black", nudge_y = -0.22, nudge_x = -0.2)+
  geom_sf_text(data = mgmt.zones |> filter(ET_ID == "4"), aes(label = "SPA 4"), size = 4, colour = "black", nudge_y = -0.15, nudge_x = -0.25)+
  geom_sf(data = sites.sf, colour = "black", size = 1.25, fill = alpha("white", 0.2)) + #alpha = 0.7,
  #geom_sf_label(data = sites.sf, aes(label = Site), size = 3, colour = "black", nudge_x = 0.015, nudge_y = 0.009)+
  geom_sf_text(data = sites.sf, aes(label = Site), size = 3, colour = "black", nudge_x = 0.017, nudge_y = 0.009)+
  labs(x = "Longitude", y = "Latitude") + #title = paste("1991-2022 ", "SPA3 Adult Scallop Density (> 65 mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .9))) + #Legend transparency
  coord_sf(xlim = c(-66.52,-65.80), ylim = c(44.1,44.60), expand = FALSE)+
  theme(plot.title = element_text(size = 14), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 9, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.80,.34), #legend position
        legend.box.background = element_rect(colour = "white", fill= "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(8, 8, 12, 12),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#ggsave(filename = paste0("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/exploratory_figures/2004-22_logbook_kde_pts.png"), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


######################################################################################################################

## Load KDE Layers - Catch ##

pez.poly <- st_union(pez.1449, pez.1452) |> 
  st_transform(32620)

#SPA1A, 3 and 4
kde.catch <-read_stars("E:/AquacultureReview_shps/KernelD_ALL_catchmt_utm.tif", NA_value = NA)
kde.catch <- st_warp(kde.catch, crs = 32620)%>% 
  st_crop(st_union(pez.poly))

kde.catch[[1]][kde.catch[[1]] < 1] <- NA
plot(kde.catch)

kde.catch <- cut(kde.catch,c(1,2,3,4,5,6,7,8,9), right = FALSE)#
plot(kde.catch)

kde.catch <- st_transform(kde.catch,crs = 4269)
pez.poly <-st_transform(pez.poly, crs = 4269)

col <- rev(brewer.pal(8,"RdYlBu"))

p <- pecjector(area ="spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('tl',0.5,-1,-1)))


p + #Plot survey data and format figure
  geom_stars(data = kde.catch, aes(fill = KernelD_ALL_catchmt_utm.tif))+
  scale_fill_manual(values = alpha(col, 0.7), na.value = 'transparent', name = "Kernel Density\n Catch (mt)")+
  geom_sf(data = logbook.pts %>% 
            st_intersection(st_union(pez.poly)), colour = "black", size = 0.25) +
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
  labs(x = "Longitude", y = "Latitude") + #title = paste("1991-2022 ", "SPA3 Adult Scallop Density (> 65 mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .9))) + #Legend transparency
  coord_sf(xlim = c(-66.52,-65.80), ylim = c(44.1,44.60), expand = FALSE)+
  theme(plot.title = element_text(size = 14), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 9, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.80,.34), #legend position
        legend.box.background = element_rect(colour = "white", fill= "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(8, 8, 12, 12),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#ggsave(filename = paste0("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/exploratory_figures/2004-22_logbook_kde_pts.png"), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


