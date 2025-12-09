
#Shell Length Frequency data set-up and plots are modified from Scallop SHF scripts


library(ROracle)

uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

surveyyear <- 2021
years <-c(2018:2019, 2021:surveyyear)

#### Import Mar-scal functions 
funcs <- "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
# Note: uses older contour.gen.r version (working on alternative to contour.gen altogether).
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}


#Load standardized horsemussel data:

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#Db Query:
quer2 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.sclivehorsemussel_std_vw 			",
  sep=""
)

livefreq.hm <- dbGetQuery(chan, quer2)

#livefreq.hm <- readRDS("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/Prorated/horsemussellive_prorated.rds") %>% filter(SAMPLE.METHOD != 1)#remove tows where no SHF data was collected.

# add YEAR column to data
livefreq.hm$YEAR <- ifelse(grepl("SFA29", livefreq.hm$CRUISE), as.numeric(substr(livefreq.hm$CRUISE,6,9)), as.numeric(substr(livefreq.hm$CRUISE,3,6)))

table(livefreq.hm$YEAR)

livefreq.hm %>% filter(is.na(YEAR))


# ------ CALCULATE SHF FOR EACH YEAR BY STRATA AND PLOT SLF-------------------------------------------------------------------


# --- 1. Saint Mary's Bay - STRATA ID 22 ----------------------------------------------------------------------

livefreq.SMB <- livefreq.hm %>% 
  filter(STRATA_ID==22) 

#check.spatial <- st_as_sf(livefreq.SMB, coords = c("START_LONG", "START_LAT"), crs = 4326)
#mapview::mapview(check.spatial)


SMB.SLFmeans <- sapply(split(livefreq.SMB[c(4:42)], livefreq.SMB$YEAR), function(x){apply(x,2,mean)})
round (SMB.SLFmeans,2)
# matrix to dataframe
SMB.SLFmeans <- data.frame(SMB.SLFmeans)

SMB.SLFmeans.for.plot <- data.frame(bin.label = row.names(SMB.SLFmeans), SMB.SLFmeans)
SMB.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(SMB.SLFmeans.for.plot)
SMB.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)


SMB.SLFmeans.for.plot <- pivot_longer(SMB.SLFmeans.for.plot, 
                                      cols = starts_with("X"),
                                      names_to = "year",
                                      names_prefix = "X",
                                      values_to = "SL",
                                      values_drop_na = FALSE)
SMB.SLFmeans.for.plot$year <- as.numeric(SMB.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
SMB.SLFmeans.for.plot$SL <- round(SMB.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF
plot.SMB.SLF <- ggplot() + geom_col(data = SMB.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SL)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SMB.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_SMB_strata22_SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SMB.SLF)
dev.off()

# --- 2. Brier Island - STRATA ID 23 ----------------------------------------------------------------------

livefreq.BI <- livefreq.hm %>% 
  filter(STRATA_ID %in% c(23, 56))

#check.spatial <- st_as_sf(livefreq.BI, coords = c("START_LONG", "START_LAT"), crs = 4326)
#mapview::mapview(check.spatial)


BI.SLFmeans <- sapply(split(livefreq.BI[c(4:42)], livefreq.BI$YEAR), function(x){apply(x,2,mean)})
round (BI.SLFmeans,2)
# matrix to dataframe
BI.SLFmeans <- data.frame(BI.SLFmeans)

BI.SLFmeans.for.plot <- data.frame(bin.label = row.names(BI.SLFmeans), BI.SLFmeans)
BI.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(BI.SLFmeans.for.plot)
BI.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)


BI.SLFmeans.for.plot <- pivot_longer(BI.SLFmeans.for.plot, 
                                      cols = starts_with("X"),
                                      names_to = "year",
                                      names_prefix = "X",
                                      values_to = "SL",
                                      values_drop_na = FALSE)
BI.SLFmeans.for.plot$year <- as.numeric(BI.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
BI.SLFmeans.for.plot$SL <- round(BI.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF
plot.BI.SLF <- ggplot() + geom_col(data = BI.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SL)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.BI.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_BI_strata23_56_SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.BI.SLF)
dev.off()


# --- 3. Lurcher - STRATA ID 24 ----------------------------------------------------------------------

livefreq.LURCH <- livefreq.hm %>% 
  filter(STRATA_ID==24) 

#check.spatial <- st_as_sf(livefreq.LURCH, coords = c("START_LONG", "START_LAT"), crs = 4326)
#mapview::mapview(check.spatial)

LURCH.SLFmeans <- sapply(split(livefreq.LURCH[c(4:42)], livefreq.LURCH$YEAR), function(x){apply(x,2,mean)})
round (LURCH.SLFmeans,2)
# matrix to dataframe
LURCH.SLFmeans <- data.frame(LURCH.SLFmeans)

LURCH.SLFmeans.for.plot <- data.frame(bin.label = row.names(LURCH.SLFmeans), LURCH.SLFmeans)
LURCH.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(LURCH.SLFmeans.for.plot)
LURCH.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)


LURCH.SLFmeans.for.plot <- pivot_longer(LURCH.SLFmeans.for.plot, 
                                     cols = starts_with("X"),
                                     names_to = "year",
                                     names_prefix = "X",
                                     values_to = "SL",
                                     values_drop_na = FALSE)
LURCH.SLFmeans.for.plot$year <- as.numeric(LURCH.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
LURCH.SLFmeans.for.plot$SL <- round(LURCH.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF
plot.LURCH.SLF <- ggplot() + geom_col(data = LURCH.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SL)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.LURCH.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_LURCH_strata24_SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.LURCH.SLF)
dev.off()


# --- 4. SFA29 - STRATA ID 41-45 ----------------------------------------------------------------------

livefreq.SFA29 <- livefreq.hm %>% 
  filter(STRATA_ID %in% c(41:45))

#check.spatial <- st_as_sf(livefreq.SFA29, coords = c("START_LONG", "START_LAT"), crs = 4326)
#mapview::mapview(check.spatial)

SFA29.SLFmeans <- sapply(split(livefreq.SFA29[c(4:42)], livefreq.SFA29$YEAR), function(x){apply(x,2,mean)})
round (SFA29.SLFmeans,2)
# matrix to dataframe
SFA29.SLFmeans <- data.frame(SFA29.SLFmeans)

SFA29.SLFmeans.for.plot <- data.frame(bin.label = row.names(SFA29.SLFmeans), SFA29.SLFmeans)
SFA29.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(SFA29.SLFmeans.for.plot)
SFA29.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)


SFA29.SLFmeans.for.plot <- pivot_longer(SFA29.SLFmeans.for.plot, 
                                        cols = starts_with("X"),
                                        names_to = "year",
                                        names_prefix = "X",
                                        values_to = "SL",
                                        values_drop_na = FALSE)
SFA29.SLFmeans.for.plot$year <- as.numeric(SFA29.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
SFA29.SLFmeans.for.plot$SL <- round(SFA29.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF
plot.SFA29.SLF <- ggplot() + geom_col(data = SFA29.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SL)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SFA29.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_SFA29_SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SFA29.SLF)
dev.off()

# --- 5. SPA6 - STRATA ID 30, 31, 32 ----------------------------------------------------------------------

livefreq.GM <- livefreq.hm %>% 
  filter(STRATA_ID %in% c(30:32))

#check.spatial <- st_as_sf(livefreq.GM, coords = c("START_LONG", "START_LAT"), crs = 4326)
#mapview::mapview(check.spatial)

GM.SLFmeans <- sapply(split(livefreq.GM[c(4:42)], livefreq.GM$YEAR), function(x){apply(x,2,mean)})
round (GM.SLFmeans,2)
# matrix to dataframe
GM.SLFmeans <- data.frame(GM.SLFmeans)

GM.SLFmeans.for.plot <- data.frame(bin.label = row.names(GM.SLFmeans), GM.SLFmeans)
GM.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(GM.SLFmeans.for.plot)
GM.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)


GM.SLFmeans.for.plot <- pivot_longer(GM.SLFmeans.for.plot, 
                                        cols = starts_with("X"),
                                        names_to = "year",
                                        names_prefix = "X",
                                        values_to = "SL",
                                        values_drop_na = FALSE)
GM.SLFmeans.for.plot$year <- as.numeric(GM.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
GM.SLFmeans.for.plot$SL <- round(GM.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF
plot.GM.SLF <- ggplot() + geom_col(data = GM.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SL)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.GM.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_GM_strata_30-32_SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.GM.SLF)
dev.off()


# --- 6. SPA 4 and 5 - STRATA ID 1:20, 47, 48, 21 ----------------------------------------------------------------------

livefreq.SPA4 <- livefreq.hm %>% 
  filter(STRATA_ID %in% c(1:21, 47, 48))

#check.spatial <- st_as_sf(livefreq.SPA4, coords = c("START_LONG", "START_LAT"), crs = 4326)
#mapview::mapview(check.spatial)

SPA4.SLFmeans <- sapply(split(livefreq.SPA4[c(4:42)], livefreq.SPA4$YEAR), function(x){apply(x,2,mean)})
round (SPA4.SLFmeans,2)
# matrix to dataframe
SPA4.SLFmeans <- data.frame(SPA4.SLFmeans)

SPA4.SLFmeans.for.plot <- data.frame(bin.label = row.names(SPA4.SLFmeans), SPA4.SLFmeans)
SPA4.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(SPA4.SLFmeans.for.plot)
SPA4.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)


SPA4.SLFmeans.for.plot <- pivot_longer(SPA4.SLFmeans.for.plot, 
                                     cols = starts_with("X"),
                                     names_to = "year",
                                     names_prefix = "X",
                                     values_to = "SL",
                                     values_drop_na = FALSE)
SPA4.SLFmeans.for.plot$year <- as.numeric(SPA4.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
SPA4.SLFmeans.for.plot$SL <- round(SPA4.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF
plot.SPA4.SLF <- ggplot() + geom_col(data = SPA4.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SL)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA4.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_SPA4_strata_1-21_47-48_SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA4.SLF)
dev.off()


# --- 6. Upper Bay - STRATA ID 35, 49 (East of longitude -64.92542), 50, 51, 52 ----------------------------------------------------------------------

livefreq.Upper.Bay <- livefreq.hm %>% 
  filter(STRATA_ID %in% c(35, 49, 50, 51, 52)) %>% 
  filter(START_LONG >= -64.9254)

#check.spatial <- st_as_sf(livefreq.Upper.Bay, coords = c("START_LONG", "START_LAT"), crs = 4326)
#mapview::mapview(check.spatial)

Upper.Bay.SLFmeans <- sapply(split(livefreq.Upper.Bay[c(4:42)], livefreq.Upper.Bay$YEAR), function(x){apply(x,2,mean)})
round (Upper.Bay.SLFmeans,2)
# matrix to dataframe
Upper.Bay.SLFmeans <- data.frame(Upper.Bay.SLFmeans)

Upper.Bay.SLFmeans.for.plot <- data.frame(bin.label = row.names(Upper.Bay.SLFmeans), Upper.Bay.SLFmeans)
Upper.Bay.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(Upper.Bay.SLFmeans.for.plot)
Upper.Bay.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)


Upper.Bay.SLFmeans.for.plot <- pivot_longer(Upper.Bay.SLFmeans.for.plot, 
                                       cols = starts_with("X"),
                                       names_to = "year",
                                       names_prefix = "X",
                                       values_to = "SL",
                                       values_drop_na = FALSE)
Upper.Bay.SLFmeans.for.plot$year <- as.numeric(Upper.Bay.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
Upper.Bay.SLFmeans.for.plot$SL <- round(Upper.Bay.SLFmeans.for.plot$SL,3)

ylimits <- c(0,40)
xlimits <- c(0,195)

# plot SHF
plot.Upper.Bay.SLF <- ggplot() + geom_col(data = Upper.Bay.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SL)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.Upper.Bay.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_UpperBay_strata_35_49Eof-64.92_50-52_SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.Upper.Bay.SLF)
dev.off()

# --- 7. Inner Bay - STRATA ID 37, 38, 49 (West of longitude -64.92542), 53-56 ----------------------------------------------------------------------

livefreq.Inner.Bay <- livefreq.hm %>% 
  filter(STRATA_ID %in% c(37:38, 53:55))
         
livefreq.Inner.Bay <- rbind(livefreq.Inner.Bay, livefreq.hm %>% 
                              filter(STRATA_ID == 49 & START_LONG <= -64.9254))

#check.spatial <- st_as_sf(livefreq.Inner.Bay, coords = c("START_LONG", "START_LAT"), crs = 4326)
#mapview::mapview(check.spatial)

Inner.Bay.SLFmeans <- sapply(split(livefreq.Inner.Bay[c(4:42)], livefreq.Inner.Bay$YEAR), function(x){apply(x,2,mean)})
round (Inner.Bay.SLFmeans,2)
# matrix to dataframe
Inner.Bay.SLFmeans <- data.frame(Inner.Bay.SLFmeans)

Inner.Bay.SLFmeans.for.plot <- data.frame(bin.label = row.names(Inner.Bay.SLFmeans), Inner.Bay.SLFmeans)
Inner.Bay.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(Inner.Bay.SLFmeans.for.plot)
Inner.Bay.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)


Inner.Bay.SLFmeans.for.plot <- pivot_longer(Inner.Bay.SLFmeans.for.plot, 
                                            cols = starts_with("X"),
                                            names_to = "year",
                                            names_prefix = "X",
                                            values_to = "SL",
                                            values_drop_na = FALSE)
Inner.Bay.SLFmeans.for.plot$year <- as.numeric(Inner.Bay.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
Inner.Bay.SLFmeans.for.plot$SL <- round(Inner.Bay.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF
plot.Inner.Bay.SLF <- ggplot() + geom_col(data = Inner.Bay.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SL)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.Inner.Bay.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_InnerBay_strata_37-38_53-55_49Wof-64.92_SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.Inner.Bay.SLF)
dev.off()

# --- 7. Mid Bay South - STRATA ID 39 ----------------------------------------------------------------------

livefreq.MBS <- livefreq.hm %>% 
  filter(STRATA_ID == 39)

#check.spatial <- st_as_sf(livefreq.MBS, coords = c("START_LONG", "START_LAT"), crs = 4326)
#mapview::mapview(check.spatial)

MBS.SLFmeans <- sapply(split(livefreq.MBS[c(4:42)], livefreq.MBS$YEAR), function(x){apply(x,2,mean)})
round (MBS.SLFmeans,2)
# matrix to dataframe
MBS.SLFmeans <- data.frame(MBS.SLFmeans)

MBS.SLFmeans.for.plot <- data.frame(bin.label = row.names(MBS.SLFmeans), MBS.SLFmeans)
MBS.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(MBS.SLFmeans.for.plot)
MBS.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)


MBS.SLFmeans.for.plot <- pivot_longer(MBS.SLFmeans.for.plot, 
                                            cols = starts_with("X"),
                                            names_to = "year",
                                            names_prefix = "X",
                                            values_to = "SL",
                                            values_drop_na = FALSE)
MBS.SLFmeans.for.plot$year <- as.numeric(MBS.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
MBS.SLFmeans.for.plot$SL <- round(MBS.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF
plot.MBS.SLF <- ggplot() + geom_col(data = MBS.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SL)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.MBS.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_MBS_strata_39_SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.MBS.SLF)
dev.off()