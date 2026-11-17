
#Incremental Ageing
#Most of this code is modified from Offshore (https://github.com/Mar-scal/Framework/blob/master/DataInputs/Ageing/Ageing.Rmd)

#potential resource for plotting multiple area curves once final models are decided: https://derekogle.com/fishR/2020-01-02-ggplot-vonB-fitPlot-2

library(tidyverse)
library(openxlsx)
library(ROracle)
library(sf)

#Load functions:
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r") 

dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

#Shapefiles for plotting
#SFA29extent <- st_read("Y:/Projects/Condition Project/AZMP/2025/GabrielaVieiraLopes_Honours/GIS_data/SFA29_Extent.shp")
Land <- st_read(paste0("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/Atl_region_land.shp")) %>% 
  st_transform(crs = 4326) %>% 
  filter(PROVINCE %in% c("Maine", "Nova Scotia", "New Brunswick"))
inshore_strata <- st_read(paste0("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/PolygonSCSTRATAINFO_rm46-26-57.shp"))

# Define:
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

# ---- Obtain data ---- 

#SQL query 1:
quer1 <- paste0("SELECT *
FROM (
  SELECT cruise, tow_no, start_lat, start_long, strata_id, 0 gear_id, shell_no , 
  age_ring, hgt_at_ring, tot_hgt , cond_id, ager_id
  FROM SCALLSUR.SCTOWS, SCALLSUR.SCAGEANALYSIS 
  WHERE sctows.tow_seq = scageanalysis.tow_seq)")

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle
#detailed sampling data
age.dat <- dbGetQuery(chan, quer1)

summary(age.dat)

# ---- Format data ---- 

age.dat <- age.dat[!is.na(age.dat$AGE_RING),]
age.dat <- age.dat[!is.na(age.dat$HGT_AT_RING),]

unique(age.dat$CRUISE)
#[1] "BF1993"    "BI1991"    "BI1992"    "BI1993"    "BI1994"    "BI1995"   
#[7] "BF1982"    "BF1984"    "BF1985"    "BF1986"    "BF1987"    "BF1988"   
#[13] "BF1989"    "BF1983"    "BF2016"    "SFA292016" "GM2016"    "BI2019"   
#[19] "BI2016"    "GM2024"    "BI2024"   

age.dat <- age.dat %>% 
  mutate(SPA = case_when(STRATA_ID %in% c(6,  7, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 39) ~ "SPA1A",
                         STRATA_ID %in% c(35, 37, 38, 49, 51, 52, 53, 54) ~ "SPA1B",
                         STRATA_ID %in% c(26,57) ~ "SPA2",
                         STRATA_ID %in% c(22,23,24) ~ "SPA3",
                         STRATA_ID %in% c(1,  2 , 3,  4,  5,  6,  7,  8,  9, 21, 47) ~ "SPA4and5",
                         STRATA_ID %in% c(30,31,32) ~ "SPA6",
                         STRATA_ID %in% c(41,42,43,44,45) ~ "SFA29W")) %>% 
  mutate(Year = as.numeric(str_sub(CRUISE, -4, -1))) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>%
  mutate(lon = convert.dd.dddd(START_LONG)) %>% 
  mutate(ID = paste0(CRUISE, ".", TOW_NO))
  
 #for spatial checks:
 age.sf <- st_as_sf(age.dat, coords = c("lon","lat"), crs = 4326)
 
 #check strata 46
 #mapview::mapview(age.sf %>% filter(STRATA_ID == 46))+
#   mapview::mapview(SFA29extent)
#BI1992, BI1994, BI1995 - Offshore Georges Bank - 6 tows? - remove Strata ID 46 for now.
age.dat <- age.dat %>% filter(STRATA_ID != 46)
 
summary(age.dat)
#table(age.dat$SPA)
#table(age.dat %>% 
#  filter(is.na(SPA)) %>% select(STRATA_ID))

#Note the NAs in TOT_HGT, not sure if these will need to be removed for further analysis.
age.dat %>% filter(is.na(TOT_HGT))
unique(age.dat %>% filter(is.na(TOT_HGT)) %>% select(CRUISE))
#CRUISE
#1    BF1983
#9    BF2016
#3963 BI2016

# ---- Modelling ---- 

source("Y:/Projects/Holistic_sampling_Inshore/Holistic_sampling_SPA2/2024/Ageing/Hubley_VonB_functions.R")

# For selecting years and SPA
years <- 1995:2024
Area <- "SPA3"

#Format age.dat for modelling with Hubley_VonB_functions

AGE.dat <- age.dat %>% rename("HEIGHT" = HGT_AT_RING, "AGE" = AGE_RING, "year" = Year) %>% 
  filter(SPA == Area) %>% 
  filter(year %in% c(years)) %>% 
  mutate(SHELL_NO = as.factor(SHELL_NO)) %>% 
  mutate(ID = as.factor(ID)) %>% 
  mutate(shell.ID = as.factor(paste0(ID, ".", SHELL_NO)))

#Starting values for the optimization algorithm
require(FSA)
f.starts <- findGrowthStarts(HEIGHT~AGE,data=AGE.dat)
f.starts

#SPA3
#Linf           K          t0 
#151.3771893   0.1663421   0.2788779 

########################################################################################################

#From Hubley VonB:

#M1 <- lvb.nlme(AGE.dat = AGE.dat, random.effect="SHELL_NO") #note warning: In nlme.formula(model = HEIGHT ~ linf * (1 - exp(-k * (AGE - tzero))),  :
#Iteration 2, LME step: nlminb() did not converge (code = 1). PORT message: false convergence (8)
#There will be multiple shell numbers, so need to use shell ID or include CRUISE/Tow.
#lvb.plt(M1)
#lvb.plt1(M1) #Single plot (red - individual shells)

#M2 <- lvb.nlme(AGE.dat = AGE.dat, random.effect="ID")
#lvb.plt(M2)
#lvb.plt1(M2)

#M3 <- lvb.nlme(AGE.dat=AGE.dat, ran.par="both", k.par="estimate")
#lvb.plt(M3)
#lvb.plt1(M3)

########################################################################################################
#From Offshore Aging.Rmd:

#non-Linear least squares
M.1 <- nls(HEIGHT ~ linf * (1 - exp(-k * (AGE - tzero))), data=AGE.dat, start=c(linf=151, k=0.2, tzero=0.3))

preds <- data.frame(AGE = seq(min(AGE.dat$AGE), max(AGE.dat$AGE), 0.1))
preds$vonB <- predict(M.1, newdata=preds)
# from https://derekogle.com/fishR/2019-12-31-ggplot-vonB-fitPlot-1
boots <- car::Boot(M.1)
vonB_se <- confint(boots) #Note warning: BCa method fails for this problem. Using 'perc' instead
preds$vonB_se_low <- vonB_se[1,1] * (1 - exp(-vonB_se[2,1] * (preds$AGE - vonB_se[3,1])))
preds$vonB_se_up <- vonB_se[1,2] * (1 - exp(-vonB_se[2,2] * (preds$AGE - vonB_se[3,2])))

resids <- dplyr::select(AGE.dat, AGE, HEIGHT, ID, SHELL_NO)
resids$vonB <- coef(M.1)[["linf"]] * (1 - exp(-coef(M.1)[["k"]] * (resids$AGE - coef(M.1)[["tzero"]])))
resids$vonB <- resids$HEIGHT-resids$vonB

AGE.dat.max <- AGE.dat %>%
  group_by(CRUISE, year, SPA, TOW_NO, SHELL_NO, ID) %>%
  summarize(HEIGHT = max(HEIGHT))

AGE.dat.max <- left_join(AGE.dat.max, AGE.dat)

#Max Height
M.2 <- nls(HEIGHT ~ linf * (1 - exp(-k * (AGE - tzero))), data=AGE.dat.max, start=c(linf=151, k=0.2, tzero=0.3))

preds$vonB2 <- predict(M.2, newdata=preds)
# from https://derekogle.com/fishR/2019-12-31-ggplot-vonB-fitPlot-1
boots <- car::Boot(M.2) 
vonB_se <- confint(boots)
preds$vonB_se_low2 <- vonB_se[1,1] * (1 - exp(-vonB_se[2,1] * (preds$AGE - vonB_se[3,1])))
preds$vonB_se_up2 <- vonB_se[1,2] * (1 - exp(-vonB_se[2,2] * (preds$AGE - vonB_se[3,2])))

resids$vonB2 <- coef(M.2)[["linf"]] * (1 - exp(-coef(M.2)[["k"]] * (resids$AGE - coef(M.2)[["tzero"]])))
resids$vonB2 <- resids$HEIGHT-resids$vonB2


###############################################################################################

require(mgcv)

M.3 <- gam(data=AGE.dat, HEIGHT ~ s(AGE, bs="cs", k=3))
preds$gam_k3 <- predict(M.3, newdata = preds, se.fit = T)$fit
preds$gam_k3_se <- predict(M.3, newdata = preds, se.fit = T)$se.fit
resids$gam_k3 <- predict(M.3, newdata=resids)
resids$gam_k3 <- resids$HEIGHT-resids$gam_k3

plot(M.3, residuals = TRUE)

M.4 <- gam(data=AGE.dat, HEIGHT ~ s(AGE, bs="cs", k=4))
preds$gam_k4 <- predict(M.4, newdata = preds, se.fit = T)$fit
preds$gam_k4_se <- predict(M.4, newdata = preds, se.fit = T)$se.fit
resids$gam_k4 <- predict(M.4, newdata=resids)
resids$gam_k4 <- resids$HEIGHT-resids$gam_k4

plot(M.4, residuals = TRUE)

M.5 <- gam(data=AGE.dat, HEIGHT ~ s(AGE, bs="cs", k=5))
preds$gam_k5 <- predict(M.5, newdata = preds, se.fit = T)$fit
preds$gam_k5_se <- predict(M.5, newdata = preds, se.fit = T)$se.fit
resids$gam_k5 <- predict(M.5, newdata=resids)
resids$gam_k5 <- resids$HEIGHT-resids$gam_k5

plot(M.5, residuals = TRUE)

#M.6 <- gam(data=AGE.dat, HEIGHT ~ s(AGE, bs="cs", k=6))
#preds$gam_k6 <- predict(M.6, newdata = preds, se.fit = T)$fit
#preds$gam_k6_se <- predict(M.6, newdata = preds, se.fit = T)$se.fit
#resids$gam_k6 <- predict(M.6, newdata=resids)
#resids$gam_k6 <- resids$HEIGHT-resids$gam_k6
#plot(M.6, residuals = TRUE)

#M.7 <- gam(data=AGE.dat, HEIGHT ~ s(AGE, bs="cs", k=7))
#preds$gam_k7 <- predict(M.7, newdata = preds, se.fit = T)$fit
#preds$gam_k7_se <- predict(M.7, newdata = preds, se.fit = T)$se.fit
#resids$gam_k7 <- predict(M.7, newdata=resids)
#resids$gam_k7 <- resids$HEIGHT-resids$gam_k7
#plot(M.7, residuals = TRUE)

#Set up for Comparison plotting
preds_long <- pivot_longer(preds, cols=names(preds)[!names(preds) %in% "AGE"])
preds_se <- preds_long[grep(x=preds_long$name, pattern="_se"),]
preds_se$name <- gsub(x=preds_se$name, pattern="_se", replacement="")
names(preds_se) <- c("AGE", "name", "se")
preds_vonB <- preds_se[preds_se$name %in% c("vonB_low", "vonB_up"),]
preds_vonB2 <- preds_se[preds_se$name %in% c("vonB_low2", "vonB_up2"),]
preds_vonB <- pivot_wider(data = preds_vonB, names_from = "name", values_from = "se")
preds_vonB2 <- pivot_wider(data = preds_vonB2, names_from = "name", values_from = "se")
preds_vonB$name <- "vonB"
preds_vonB2$name <- "vonB2"
preds_se <- preds_se[which(grepl(x=preds_se$name, pattern="vonB")==F),]
preds_long <- preds_long[which(grepl(x=preds_long$name, pattern="_se")==F),]
preds_long <- left_join(preds_long, preds_se)
preds_long <- left_join(preds_long, preds_vonB)
preds_long <- left_join(preds_long, preds_vonB2)

resids_long <- pivot_longer(resids, cols=names(resids)[!names(resids) %in% c("AGE", "HEIGHT", "ID", "SHELL_NO")])

#Comparison Plot (VonB, VonB2, gam_k3-5)
ggplot() + geom_point(data=AGE.dat, aes(AGE, HEIGHT), alpha=0.25) +
  geom_line(data=preds_long, aes(AGE, value, colour=name)) +
  geom_ribbon(data=preds_long, aes(AGE, ymin=value-1.96*se, ymax=value+1.96*se, fill=name), alpha=0.2)+
  geom_ribbon(data=preds_long, aes(AGE, ymin=vonB_low, ymax=vonB_up, fill=name), alpha=0.2)+
  geom_text(data=preds_long[preds_long$AGE==max(preds_long$AGE),], aes(AGE+0.1, value, label=name, colour=name), hjust=0) +
  ggtitle("predictions") +
  theme_bw() +
  xlim(2,13)


#Comparison Plot (VonB, VonB2)
ggplot() + geom_point(data=AGE.dat, aes(AGE, HEIGHT), alpha=0.25) +
  geom_line(data=preds_long %>% filter(name %in% c("vonB", "vonB2")), aes(AGE, value, colour=name)) +
  geom_ribbon(data=preds_long %>% filter(name %in% c("vonB", "vonB2")), aes(AGE, ymin=value-1.96*se, ymax=value+1.96*se, fill=name), alpha=0.2)+
  geom_ribbon(data=preds_long %>% filter(name %in% c("vonB", "vonB2")), aes(AGE, ymin=vonB_low, ymax=vonB_up, fill=name), alpha=0.2)+
  #geom_text(data=preds_long[preds_long$AGE==max(preds_long$AGE),], aes(AGE+0.1, value, label=name, colour=name), hjust=0) +
  ggtitle("predictions") +
  theme_bw() +
  xlim(2,13)

###############################################################################################

#With Random Effects
#remotes::install_github("m-clark/gammit")

#add shell ID
#AGE.dat$shell.ID <- as.factor(paste0(AGE.dat$ID, ".", AGE.dat$SHELL_NO))

#3 knots
M.3.re <- gam(data=AGE.dat, method="REML", HEIGHT ~ s(AGE, k=3) + s(ID, bs="re") + s(SHELL_NO, bs="re"))
#coef(M.3.re)
gratia::variance_comp(M.3.re)
summary(M.3.re)
preds_exp <- expand.grid(AGE=preds$AGE, ID=unique(AGE.dat$ID))
preds_exp <- left_join(preds_exp, unique(dplyr::select(AGE.dat, ID, SHELL_NO)))
preds_exp$gam_re_3 <- predict(object = M.3.re, newdata = preds_exp)
preds_exp$gam_re_3_se <- predict(object = M.3.re, newdata = preds_exp, se.fit=T)$se.fit
preds_exp$gam_re_3f <- gammit::predict_gamm(M.3.re, newdata=preds_exp, exclude = c("ID", "SHELL_NO"), re_form=NA)$prediction
preds_exp$gam_re_3f_se <- gammit::predict_gamm(M.3.re, newdata=preds_exp, exclude = c("ID", "SHELL_NO"), re_form=NA, se=T)$se
resids$gam_re_3 <- predict(M.3.re, newdata=resids)
resids$gam_re_3 <- resids$HEIGHT-resids$gam_re_3
#M.3.re

ggplot() + geom_point(data=AGE.dat, aes(AGE, HEIGHT, colour=ID)) +
  geom_line(data=preds_exp, aes(AGE, gam_re_3, colour=ID,group=SHELL_NO)) +
  theme(legend.position = "none")+
  facet_wrap(~ID)

#SHELL ID?
#preds_exp$shell.ID <- as.factor(paste0(preds_exp$ID, ".", preds_exp$SHELL_NO))
#resids$shell.ID <- as.factor(paste0(resids$ID, ".", resids$SHELL_NO))

#M.3.2.re <- gam(data=AGE.dat, method="REML", HEIGHT ~ s(AGE, k=3) + s(shell.ID, bs="re"))
#coef(M.3.2.re)
#gratia::variance_comp(M.3.2.re)
#summary(M.3.2.re)
#preds_exp$gam_re_3.2 <- predict(object = M.3.2.re, newdata = preds_exp)
#preds_exp$gam_re_3.2_se <- predict(object = M.3.2.re, newdata = preds_exp, se.fit=T)$se.fit
#preds_exp$gam_re_3.2f <- gammit::predict_gamm(M.3.2.re, newdata=preds_exp, exclude = c("shell.ID"), re_form=NA)$prediction
#preds_exp$gam_re_3.2f_se <- gammit::predict_gamm(M.3.2.re, newdata=preds_exp, exclude = c("shell.ID"), re_form=NA, se=T)$se
#resids$gam_re_3.2<- predict(M.3.2.re, newdata=resids)
#resids$gam_re_3.2<- resids$HEIGHT-resids$gam_re_3.2
#M.3.2.re

#ggplot() + geom_point(data=AGE.dat, aes(AGE, HEIGHT, colour=ID)) +
#   geom_line(data=preds_exp, aes(AGE, gam_re_3.2, colour=ID,group=SHELL_NO)) +
#  theme(legend.position = "none")+
#   facet_wrap(~ID)


#SHELL_NO and ID nested random effects
require(gamm4)

M.3.3.re <- gamm4(data=AGE.dat, HEIGHT ~ s(AGE, bs="cs", k=3), random = ~(1|SHELL_NO/ID))
preds_exp$gam_re_3.3 <- gammit::predict_gamm(model = M.3.3.re$gam, newdata = preds_exp)$prediction
preds_exp$gam_re_3.3_se<- gammit::predict_gamm(model = M.3.3.re$gam, newdata = preds_exp, se=T)$se
preds_exp$gam_re_3.3f <- gammit::predict_gamm(model = M.3.3.re$gam, newdata = preds_exp, re_form=NA)$prediction
preds_exp$gam_re_3.3f_se<- gammit::predict_gamm(model = M.3.3.re$gam, newdata = preds_exp, se=T, re_form=NA)$se
resids$gam_re_3.3f<- gammit::predict_gamm(model = M.3.3.re$gam, newdata = resids, re_form=NA)$prediction
resids$gam_re_3.3f<- resids$HEIGHT-resids$gam_re_3.3f

ggplot() + geom_point(data=AGE.dat, aes(AGE, HEIGHT, colour=ID)) +
  geom_line(data=preds_exp, aes(AGE, gam_re_3.3, colour=ID,group=SHELL_NO)) +
  theme(legend.position = "none")+
  facet_wrap(~ID)


#5 knots
M.5.re <- gam(data=AGE.dat, method="REML", HEIGHT ~ s(AGE, k=5) + s(ID, bs="re") + s(SHELL_NO, bs="re"))
#coef(M.5.re)
gratia::variance_comp(M.5.re)
summary(M.5.re)
preds_exp$gam_re_5 <- predict(object = M.5.re, newdata = preds_exp)
preds_exp$gam_re_5_se <- predict(object = M.5.re, newdata = preds_exp, se.fit=T)$se.fit
preds_exp$gam_re_5f <- gammit::predict_gamm(M.5.re, newdata=preds_exp, exclude = c("ID", "SHELL_NO"), re_form=NA)$prediction
preds_exp$gam_re_5f_se <- gammit::predict_gamm(M.5.re, newdata=preds_exp, exclude = c("ID", "SHELL_NO"), re_form=NA, se=T)$se
resids$gam_re_5 <- predict(M.5.re, newdata=resids)
resids$gam_re_5 <- resids$HEIGHT-resids$gam_re_5
#M.5.re

ggplot() + geom_point(data=AGE.dat, aes(AGE, HEIGHT, colour=ID)) +
  geom_line(data=preds_exp, aes(AGE, gam_re_5, colour=ID,group=SHELL_NO)) +
  theme(legend.position = "none")+
  facet_wrap(~ID)


#SHELL_NO and ID nested random effects
#require(gamm4)

M.5.2.re <- gamm4(data=AGE.dat, HEIGHT ~ s(AGE, bs="cs", k=5), random = ~(1|SHELL_NO/ID))
preds_exp$gam_re_5.2 <- gammit::predict_gamm(model = M.5.2.re$gam, newdata = preds_exp)$prediction
preds_exp$gam_re_5.2_se<- gammit::predict_gamm(model = M.5.2.re$gam, newdata = preds_exp, se=T)$se
preds_exp$gam_re_5.2f <- gammit::predict_gamm(model = M.5.2.re$gam, newdata = preds_exp, re_form=NA)$prediction
preds_exp$gam_re_5.2f_se<- gammit::predict_gamm(model = M.5.2.re$gam, newdata = preds_exp, se=T, re_form=NA)$se
resids$gam_re_5.2f<- gammit::predict_gamm(model = M.5.2.re$gam, newdata = resids, re_form=NA)$prediction
resids$gam_re_5.2f<- resids$HEIGHT-resids$gam_re_5.2f

ggplot() + geom_point(data=AGE.dat, aes(AGE, HEIGHT, colour=ID)) +
  geom_line(data=preds_exp, aes(AGE, gam_re_5.2, colour=ID,group=SHELL_NO)) +
  theme(legend.position = "none")+
  facet_wrap(~ID)


# get ready for plotting
preds_simple <- dplyr::select(preds_exp, 
                              -gam_re_3, -gam_re_3_se, 
                              # -gam_re_3.2, -gam_re_3.2_se, 
                              -gam_re_5, -gam_re_5_se, 
                              # -gam_re_5.2,-gam_re_5.2_se, 
                              -ID, -SHELL_NO)
preds_simple <- unique(pivot_longer(preds_simple, cols=names(preds_simple)[!names(preds_simple) %in% c("AGE")]))
preds_se <- preds_simple[grep(x=preds_simple$name, pattern="_se"),]
preds_se$name <- gsub(x=preds_se$name, pattern="_se", replacement="")
names(preds_se) <- c("AGE", "name", "se")
preds_simple <- preds_simple[which(grepl(x=preds_simple$name, pattern="_se")==F),]
preds_simple <- left_join(preds_simple, preds_se)

preds_exp <- pivot_longer(preds_exp, cols=names(preds_exp)[!names(preds_exp) %in% c("AGE", "ID", "SHELL_NO")])
preds_se <- preds_exp[grep(x=preds_exp$name, pattern="_se"),]
preds_se$name <- gsub(x=preds_se$name, pattern="_se", replacement="")
names(preds_se) <- c("AGE", "ID", "SHELL_NO", "name", "se")
preds_exp <- preds_exp[which(grepl(x=preds_exp$name, pattern="_se")==F),]
preds_exp <- left_join(preds_exp, preds_se)

preds_long <- full_join(preds_exp, preds_long)
preds_long <- full_join(preds_long, preds_simple)

vonB <- preds_long[preds_long$name %in% c("vonB", "vonB2"),]
names(vonB)[names(vonB)=="name"] <- "ref"


#SPA3
labels <- data.frame(AGE = c(rep(10, 4)), 
                     #rep(11.5,3), 
                     #rep(11,3)), 
                     name=c(#"gam_k5", 
                       "vonB", 
                       "vonB2",
                       #"gam_k3", 
                       "gam_re_3.3f", 
                       "gam_re_5.2f"#, 
                       #"gam_re_3.2f", 
                       #"gam_re_5.2f", 
                       #"gam_re_3.3f"
                     ))


labels <- left_join(labels, unique(dplyr::select(preds_long, AGE, name, value)))
labels$long[labels$name=="gam_re_5.2f"] <- "GAMM, 5 knots" 
labels$long[labels$name=="gam_re_3.3f"] <- "GAMM, 3 knots"
labels$long[labels$name=="vonB"] <- "von B"
labels$long[labels$name=="vonB2"] <- "von B (shell)"
preds_long$long[preds_long$name=="gam_re_5.2f"] <- "GAMM, 5 knots" 
preds_long$long[preds_long$name=="gam_re_3.3f"] <- "GAMM, 3 knots"
preds_long$long[preds_long$name=="vonB"] <- "von B"
preds_long$long[preds_long$name=="vonB2"] <- "von B (shell)"

require(ggrepel)
all <- ggplot() + 
  geom_point(data=AGE.dat, aes(AGE, HEIGHT), size=1)+
  geom_line(data=preds_long[preds_long$name %in% c("gam_re_3.3f", "gam_re_5.2f", "vonB", "vonB2"),], 
            aes(AGE, value, group=name), show.legend=F) + 
  geom_ribbon(data=preds_long[preds_long$name %in% c("gam_re_3.3f", "gam_re_5.2f", "vonB", "vonB2"),], 
              aes(AGE, ymin=value-1.96*se, ymax=value+1.96*se, group=name), alpha=0.2, show.legend=F)+
  geom_ribbon(data=preds_long[preds_long$name %in% c("gam_re_3.3f", "gam_re_5.2f", "vonB", "vonB2"),], 
              aes(AGE, ymin=vonB_low, ymax=vonB_up, group=name), alpha=0.2, show.legend=F)+
   geom_ribbon(data=preds_long[preds_long$name %in% c("gam_re_3.2f", "gam_re_5.2f", "vonB", "vonB2"),], 
               aes(AGE, ymin=vonB_low2, ymax=vonB_up2, group=name), alpha=0.2, show.legend=F)+
  geom_text(data=labels, aes(AGE, value, group=name, label=long), hjust=0)+
   geom_text_repel(data=labels,
                   aes(AGE, value, group=long, label=long),
                   show.legend=F,
                   max.overlaps=Inf,
                   hjust="left",
                   direction="y",
                   box.padding=1,
                   nudge_x=c(2,2,2),
                   nudge_y=c(1,1,1),
                   arrow=arrow(length = unit(0.015, "npc"))) +
  xlim(2,12) +
  scale_x_continuous(breaks=seq(2,12,2), limits=c(xlim=c(2,12)))+
  #ylim(50,170) +
  theme_bw() + theme(panel.grid=element_blank())+
  ylab("Shell height (mm)") +
  xlab("Age")
print(all)


#####################################################################################################


# Model evalutation -------------------------------------------------------

require(stats)

df <- NULL

#VonB
preds_sub <- preds_long[preds_long$name=="vonB",]
mod <- M.1

row <- data.frame(Model="VonB",
                  name=deparse(formula(mod)), 
                  AIC=AIC(mod), 
                  r.sq = NA,
                  scale.est=NA,
                  dev=NA,
                  H10=predict(mod, newdata = data.frame(AGE=10)),
                  H5=predict(mod, newdata = data.frame(AGE=5)),
                  H2=predict(mod, newdata = data.frame(AGE=2)),
                  #A_avgFR=preds_sub$AGE[which.min(abs(preds_sub$value-l.bar))],
                  A_maxR=preds_sub$AGE[which.min(abs(90-preds_sub$value))],
                  A_minR=preds_sub$AGE[which.min(abs(75-preds_sub$value))])

row$K_maxR <- preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_maxR)))] - preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_maxR-1)))]
#row$K_avgFR <- preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_avgFR)))] - preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_avgFR-1)))]
row$K_2_to_5 <- row$H5 - row$H2

df <- rbind(df, row) #add to df

#VonB2 (max height)

preds_sub <- preds_long[preds_long$name=="vonB2",]
mod <- M.2

row <- data.frame(Model="VonB2",
                  name=deparse(formula(mod)), 
                  AIC=AIC(mod), 
                  r.sq = NA,
                  scale.est=NA,
                  dev=NA,
                  H10=predict(mod, newdata = data.frame(AGE=10)),
                  H5=predict(mod, newdata = data.frame(AGE=5)),
                  H2=predict(mod, newdata = data.frame(AGE=2)),
                  #A_avgFR=preds_sub$AGE[which.min(abs(preds_sub$value-l.bar))],
                  A_maxR=preds_sub$AGE[which.min(abs(90-preds_sub$value))],
                  A_minR=preds_sub$AGE[which.min(abs(75-preds_sub$value))])
row$K_maxR <- preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_maxR)))] - preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_maxR-1)))]
#row$K_avgFR <- preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_avgFR)))] - preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_avgFR-1)))]
row$K_2_to_5 <- row$H5 - row$H2

df <- full_join(df, row) #add to df



#GAMM Knot 3

preds_sub <- preds_long[preds_long$name=="gam_re_3.3",]
mod <- M.3.3.re

row <- data.frame(Model="M.3.3.re",
                  name=paste0(deparse(formula(mod$gam))), 
                  AIC=AIC(mod$mer), 
                  r.sq = summary(mod$gam)$r.sq,
                  scale.est=summary(mod$gam)$scale,
                  dev=summary(mod$gam)$dev,
                  H10=gammit::predict_gamm(mod$gam, newdata = data.frame(AGE=10, ID=1, SHELL_NO=1), re_form=NA)$prediction,
                  H5=gammit::predict_gamm(mod$gam, newdata = data.frame(AGE=5, ID=1, SHELL_NO=1), re_form=NA)$prediction,
                  H2=gammit::predict_gamm(mod$gam, newdata = data.frame(AGE=2, ID=1, SHELL_NO=1), re_form=NA)$prediction,
                  #A_avgFR=preds_sub$AGE[which.min(abs(preds_sub$value-l.bar))],
                  A_maxR=preds_sub$AGE[which.min(abs(90-preds_sub$value))],
                  A_minR=preds_sub$AGE[which.min(abs(75-preds_sub$value))])
row$K_maxR <- preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_maxR)))] - preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_maxR-1)))]
#row$K_avgFR <- preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_avgFR)))] - preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_avgFR-1)))]
row$K_2_to_5 <- row$H5 - row$H2

df <- full_join(df, row) #add to df




df <- NULL
#FOR SPA3
l.bar <- read.csv('Y:/Inshore/Assessment/BoF/2026/Assessment/Data/Growth/SPA3/SPA3.lbar.to2026.csv') %>% select(years, SPA3.SHactual.Com)
for(mod in c("vonB", "vonB2", "mod_re_3.3f", 
             #"mod_re_3.2", "mod_re_3.3", 
             "mod_re_5.2f"#, 
             #"mod_re_5.2"
)){
  print(nrow(df))
  
  if(!mod %in% c("vonB", "vonB2")){
    if(mod=="mod_re_3.4") pred <- "gam_re_3.4f"
    if(mod=="mod_re_5.4") pred <- "gam_re_5.4f"
    
    preds_sub <- preds_simple[preds_simple$name==pred,]
    
    if(!any(class(get(mod))=="list")) {
      row <- data.frame(Model=mod,
                        name=deparse(formula(get(mod))), 
                        AIC=AIC(get(mod)), 
                        r.sq = summary(get(mod))$r.sq,
                        scale.est=summary(get(mod))$scale,
                        dev=summary(get(mod))$dev,
                        H10=gammit::predict_gamm(get(mod), newdata = data.frame(AGE=10, ID=1, Scall.no=1), re_form=NA)$prediction,
                        H5=gammit::predict_gamm(get(mod), newdata = data.frame(AGE=5, ID=1, Scall.no=1), re_form=NA)$prediction,
                        H2=gammit::predict_gamm(get(mod), newdata = data.frame(AGE=2, ID=1, Scall.no=1), re_form=NA)$prediction,
                        A_avgFR=preds_sub$AGE[which.min(abs(preds_sub$value-l.bar))],
                        A_maxR=preds_sub$AGE[which.min(abs(90-preds_sub$value))],
                        A_minR=preds_sub$AGE[which.min(abs(75-preds_sub$value))])
      
      row$K_maxR <- preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_maxR)))] - preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_maxR-1)))]
      row$K_avgFR <- preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_avgFR)))] - preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_avgFR-1)))]
      row$K_2_to_5 <- row$H5 - row$H2
      
      
    }
    if(any(class(get(mod))=="list")) {
      row <- data.frame(Model=mod,
                        name=paste0(deparse(formula(get(mod)$gam)), ", ",  deparse(formula(get(mod)$mer))), 
                        AIC=AIC(get(mod)$mer), 
                        r.sq = summary(get(mod)$gam)$r.sq,
                        scale.est = summary(get(mod)$gam)$scale,
                        dev = NA,
                        H10=gammit::predict_gamm(get(mod)$gam, newdata = data.frame(AGE=10, ID=1, Scall.no=1), re_form=NA)$prediction,
                        H5=gammit::predict_gamm(get(mod)$gam, newdata = data.frame(AGE=5, ID=1, Scall.no=1), re_form=NA)$prediction,
                        H2=gammit::predict_gamm(get(mod)$gam, newdata = data.frame(AGE=2, ID=1, Scall.no=1), re_form=NA)$prediction,
                        A_avgFR=preds_sub$AGE[which.min(abs(preds_sub$value-l.bar))],
                        A_maxR=preds_sub$AGE[which.min(abs(90-preds_sub$value))],
                        A_minR=preds_sub$AGE[which.min(abs(75-preds_sub$value))])
      
      row$K_maxR <- preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_maxR)))] - preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_maxR-1)))]
      row$K_avgFR <- preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_avgFR)))] - preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_avgFR-1)))]
      row$K_2_to_5 <- row$H5 - row$H2
    }
  }
  
  if(mod %in% c("vonB", "vonB2")){
    if (mod =="vonB") {
      mod <- "cheat"
      preds_sub <- preds_long[preds_long$name=="vonB",]
    }
    if (mod =="vonB2") {
      mod <- "cheat2"
      preds_sub <- preds_long[preds_long$name=="vonB2",]
    }
    
    row <- data.frame(Model=mod,
                      name=deparse(formula(get(mod))), 
                      AIC=AIC(get(mod)), 
                      r.sq = NA,
                      scale.est=NA,
                      dev=NA,
                      H10=predict(get(mod), newdata = data.frame(AGE=10)),
                      H5=predict(get(mod), newdata = data.frame(AGE=5)),
                      H2=predict(get(mod), newdata = data.frame(AGE=2)),
                      A_avgFR=preds_sub$AGE[which.min(abs(preds_sub$value-l.bar))],
                      A_maxR=preds_sub$AGE[which.min(abs(90-preds_sub$value))],
                      A_minR=preds_sub$AGE[which.min(abs(75-preds_sub$value))])
    
    row$K_maxR <- preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_maxR)))] - preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_maxR-1)))]
    row$K_avgFR <- preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_avgFR)))] - preds_sub$value[which.min(abs(preds_sub$AGE-(row$A_avgFR-1)))]
    row$K_2_to_5 <- row$H5 - row$H2
  }
  
  if(is.null(df)) df <- rbind(df, row)
  if(!is.null(df)) df <- full_join(df, row)
}

print(df)

df$Model[grep(x=df$Model, pattern = "3")] <- "GAMM, 3 knots"
df$Model[grep(x=df$Model, pattern = "5")] <- "GAMM, 5 knots"
df$Model[grep(x=df$Model, pattern = "cheat")] <- "von B"
df$Model[grep(x=df$Model, pattern = "cheat2")] <- "von B (shell)"

names(df)[2] <- "Formula"

write.csv(x = df, paste0(plotsGo, "/", bank, "/summary_stats.csv"))


