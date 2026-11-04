
#Max Age
#Most of this code is modified from Offshore (https://github.com/Mar-scal/Framework/blob/master/DataInputs/Ageing/Ageing.Rmd)

library(tidyverse)
library(openxlsx)
library(ROracle)
library(sf)
require(Polychrome)

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
FROM SCALLSUR.SCWGT_HGT_AGE")

# ROracle
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle
#detailed sampling data
age.dat <- dbGetQuery(chan, quer1)

summary(age.dat)

#Combine AGE and AGEI into one column:
#AGE - Shell age in years. Held in SCSHELLDETAILS table
#AGEI - Shell age from increment ageing, data is the max age ring. Held in SCAGEANALYSIS table

age.dat <- age.dat %>% filter(!is.na(AGEI) | !is.na(AGE))
age.dat$AGE_COMB <- coalesce(age.dat$AGEI, age.dat$AGE)

summary(age.dat)
unique(age.dat$CRUISE)

#Add SPA names, extract Year from CRUISE, convert coords, and create unique ID..
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

#spatial checks:
age.sf <- st_as_sf(age.dat, coords = c("lon","lat"), crs = 4326)

#check strata 46
age.sf %>% filter(STRATA_ID == 46)
#mapview::mapview(age.sf %>% filter(STRATA_ID == 46))+
#   mapview::mapview(SFA29extent)
#BI1992, BI1994, BI1995 - remove Strata ID 46.
age.dat <- age.dat %>% filter(STRATA_ID != 46)

summary(age.dat)
#table(age.dat$SPA)
#table(age.dat %>% 
#  filter(is.na(SPA)) %>% select(STRATA_ID))

#Note 1 NA in HEIGHT.. remove.
age.dat %>% filter(is.na(HEIGHT))
#CRUISE   TOW_NO
#BF1983    85
age.dat <- age.dat %>% filter(!is.na(HEIGHT))


# ---- Set up for modelling ---- 

# For selecting years and SPA
years <- 1996:2024
Area <- "SPA2"

#Format data for modelling
AGE.dat <- age.dat %>% 
  filter(SPA == Area) %>% 
  filter(Year %in% c(years)) %>%
  mutate(Year = as.factor(Year)) %>% 
  mutate(SHELL_NO = as.factor(SHELL_NO)) %>% 
  mutate(ID = as.factor(ID)) %>% 
  mutate(shell.ID = as.factor(paste0(ID, ".", SHELL_NO)))

#Starting values for the optimization algorithm
require(FSA)
f.starts <- findGrowthStarts(HEIGHT~AGE_COMB,data=AGE.dat)
f.starts

#SPA2
#Linf           K          t0 
#140.7263553   0.2174404   0.1959549

#SPA3
#Linf           K          t0 
#167.4524776   0.1132219  -2.1014951 


# ---- MODELLING ----

# ---- #Non-Linear least squares ----

#Max Height
M.1 <- nls(HEIGHT ~ linf * (1 - exp(-k * (AGE_COMB - tzero))), data=AGE.dat, start=c(linf=140.73, k=0.22, tzero=0.20))
preds <- data.frame(AGE_COMB = seq(min(AGE.dat$AGE_COMB), max(AGE.dat$AGE_COMB), 0.1))
preds$vonB <- predict(M.1, newdata=preds)
# from https://derekogle.com/fishR/2019-12-31-ggplot-vonB-fitPlot-1
boots <- car::Boot(M.1) 
vonB_se <- confint(boots)
preds$vonB_se_low <- vonB_se[1,1] * (1 - exp(-vonB_se[2,1] * (preds$AGE - vonB_se[3,1])))
preds$vonB_se_up <- vonB_se[1,2] * (1 - exp(-vonB_se[2,2] * (preds$AGE - vonB_se[3,2])))

resids <- dplyr::select(AGE.dat, AGE_COMB, HEIGHT, ID)
resids$vonB <- coef(M.1)[["linf"]] * (1 - exp(-coef(M.1)[["k"]] * (resids$AGE_COMB - coef(M.1)[["tzero"]])))
resids$vonB <- resids$HEIGHT-resids$vonB

# ---- General additive models ----

#Knot 3
require(mgcv)
M.2 <- gam(data=AGE.dat, HEIGHT ~ s(AGE_COMB, bs="cs", k=3))
preds$gam_k3 <- predict(M.2, newdata = preds, se.fit = T)$fit
preds$gam_k3_se <- predict(M.2, newdata = preds, se.fit = T)$se.fit
resids$gam_k3 <- predict(M.2, newdata=resids)
resids$gam_k3 <- resids$HEIGHT-resids$gam_k3

plot(M.2, residuals = TRUE)

#Knot 5
M.3 <- gam(data=AGE.dat, HEIGHT ~ s(AGE_COMB, bs="cs", k=5))
preds$gam_k5 <- predict(M.3, newdata = preds, se.fit = T)$fit
preds$gam_k5_se <- predict(M.3, newdata = preds, se.fit = T)$se.fit
resids$gam_k5 <- predict(M.3, newdata=resids)
resids$gam_k5 <- resids$HEIGHT-resids$gam_k5

plot(M.3, residuals = TRUE)

#Knot 6
M.4 <- gam(data=AGE.dat, HEIGHT ~ s(AGE_COMB, bs="cs", k=6))
preds$gam_k6 <- predict(M.4, newdata = preds, se.fit = T)$fit
preds$gam_k6_se <- predict(M.4, newdata = preds, se.fit = T)$se.fit
resids$gam_k6 <- predict(M.4, newdata=resids)
resids$gam_k6 <- resids$HEIGHT-resids$gam_k6

plot(M.4, residuals = TRUE)

#Set up for Comparison plotting
preds_long <- pivot_longer(preds, cols=names(preds)[!names(preds) %in% "AGE_COMB"])
preds_se <- preds_long[grep(x=preds_long$name, pattern="_se"),]
preds_se$name <- gsub(x=preds_se$name, pattern="_se", replacement="")
names(preds_se) <- c("AGE_COMB", "name", "se")
preds_vonB <- preds_se[preds_se$name %in% c("vonB_low", "vonB_up"),]
preds_vonB <- pivot_wider(data = preds_vonB, names_from = "name", values_from = "se")
preds_vonB$name <- "vonB"
preds_se <- preds_se[which(grepl(x=preds_se$name, pattern="vonB")==F),]
preds_long <- preds_long[which(grepl(x=preds_long$name, pattern="_se")==F),]
preds_long <- left_join(preds_long, preds_se)
preds_long <- left_join(preds_long, preds_vonB)

resids_long <- pivot_longer(resids, cols=names(resids)[!names(resids) %in% c("AGE_COMB", "HEIGHT", "ID", "SHELL_NO")])

#Comparison Plot (VonB, VonB2, gam_k3-5)
ggplot() + geom_point(data=AGE.dat, aes(AGE_COMB, HEIGHT), alpha=0.25) +
  geom_line(data=preds_long, aes(AGE_COMB, value, colour=name)) +
  geom_ribbon(data=preds_long, aes(AGE_COMB, ymin=value-1.96*se, ymax=value+1.96*se, fill=name), alpha=0.2)+
  geom_ribbon(data=preds_long, aes(AGE_COMB, ymin=vonB_low, ymax=vonB_up, fill=name), alpha=0.2)+
  geom_text(data=preds_long[preds_long$AGE_COMB==max(preds_long$AGE_COMB),], aes(AGE_COMB+0.1, value, label=name, colour=name), hjust=0) +
  ggtitle("predictions") +
  theme_bw() +
  xlim(2,13)

# ---- GAM with Random Effects ----

#3 knots
M.3.re <- gam(data=AGE.dat, method="REML", HEIGHT ~ s(AGE_COMB, k=3) + s(ID, bs="re"))
#coef(M.3.re)
gratia::variance_comp(M.3.re)
summary(M.3.re)
preds_exp <- expand.grid(AGE_COMB=preds$AGE_COMB, ID=unique(AGE.dat$ID))
preds_exp <- left_join(preds_exp, unique(dplyr::select(AGE.dat, ID)))
preds_exp$gam_re_3 <- predict(object = M.3.re, newdata = preds_exp)
preds_exp$gam_re_3_se <- predict(object = M.3.re, newdata = preds_exp, se.fit=T)$se.fit
preds_exp$gam_re_3f <- gammit::predict_gamm(M.3.re, newdata=preds_exp, exclude = c("ID", "SHELL_NO"), re_form=NA)$prediction
preds_exp$gam_re_3f_se <- gammit::predict_gamm(M.3.re, newdata=preds_exp, exclude = c("ID", "SHELL_NO"), re_form=NA, se=T)$se
resids$gam_re_3 <- predict(M.3.re, newdata=resids)
resids$gam_re_3 <- resids$HEIGHT-resids$gam_re_3
#M.3.re

ggplot() + geom_point(data=AGE.dat, aes(AGE_COMB, HEIGHT, colour=ID)) +
  geom_line(data=preds_exp, aes(AGE_COMB, gam_re_3, colour=ID)) + 
  theme(legend.position = "none")+
  facet_wrap(~ID)

#5 knots
M.5.re <- gam(data=AGE.dat, method="REML", HEIGHT ~ s(AGE_COMB, k=5) + s(ID, bs="re"))
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

ggplot() + geom_point(data=AGE.dat, aes(AGE_COMB, HEIGHT, colour=ID)) +
  geom_line(data=preds_exp, aes(AGE_COMB, gam_re_5, colour=ID)) +
  theme(legend.position = "none")+
  facet_wrap(~ID)

# get ready for plotting
preds_simple <- dplyr::select(preds_exp, 
                              -gam_re_3, -gam_re_3_se, 
                              -gam_re_5, -gam_re_5_se, 
                              -ID)
preds_simple <- unique(pivot_longer(preds_simple, cols=names(preds_simple)[!names(preds_simple) %in% c("AGE_COMB")]))
preds_se <- preds_simple[grep(x=preds_simple$name, pattern="_se"),]
preds_se$name <- gsub(x=preds_se$name, pattern="_se", replacement="")
names(preds_se) <- c("AGE_COMB", "name", "se")
preds_simple <- preds_simple[which(grepl(x=preds_simple$name, pattern="_se")==F),]
preds_simple <- left_join(preds_simple, preds_se)

preds_exp <- pivot_longer(preds_exp, cols=names(preds_exp)[!names(preds_exp) %in% c("AGE_COMB", "ID")])
preds_se <- preds_exp[grep(x=preds_exp$name, pattern="_se"),]
preds_se$name <- gsub(x=preds_se$name, pattern="_se", replacement="")
names(preds_se) <- c("AGE_COMB", "ID", "name", "se")
preds_exp <- preds_exp[which(grepl(x=preds_exp$name, pattern="_se")==F),]
preds_exp <- left_join(preds_exp, preds_se)

preds_long <- full_join(preds_exp, preds_long)
preds_long <- full_join(preds_long, preds_simple)

vonB <- preds_long[preds_long$name %in% c("vonB", "vonB2"),]
names(vonB)[names(vonB)=="name"] <- "ref"


#SPA3
labels <- data.frame(AGE_COMB = c(rep(10, 3)), 
                     name=c( 
                       "vonB",
                       "gam_re_3f", 
                       "gam_re_5f"
                     ))


labels <- left_join(labels, unique(dplyr::select(preds_long, AGE_COMB, name, value)))
labels$long[labels$name=="gam_re_5f"] <- "GAMM, 5 knots" 
labels$long[labels$name=="gam_re_3f"] <- "GAMM, 3 knots"
labels$long[labels$name=="vonB"] <- "von B"
preds_long$long[preds_long$name=="gam_re_5f"] <- "GAMM, 5 knots" 
preds_long$long[preds_long$name=="gam_re_3f"] <- "GAMM, 3 knots"
preds_long$long[preds_long$name=="vonB"] <- "von B"

require(ggrepel)
all <- ggplot() + 
  geom_point(data=AGE.dat, aes(AGE_COMB, HEIGHT), size=1)+
  geom_line(data=preds_long[preds_long$name %in% c("gam_re_3f", "gam_re_5f", "vonB"),], 
            aes(AGE_COMB, value, group=name), show.legend=F) + 
  geom_ribbon(data=preds_long[preds_long$name %in% c("gam_re_3f", "gam_re_5f", "vonB"),], 
              aes(AGE_COMB, ymin=value-1.96*se, ymax=value+1.96*se, group=name), alpha=0.2, show.legend=F)+
  geom_ribbon(data=preds_long[preds_long$name %in% c("gam_re_3f", "gam_re_5f", "vonB"),], 
              aes(AGE_COMB, ymin=vonB_low, ymax=vonB_up, group=name), alpha=0.2, show.legend=F)+
  #geom_text(data=labels, aes(AGE_COMB, value, group=name, label=long), hjust=0)+
  geom_text_repel(data=labels,
                  aes(AGE_COMB, value, group=long, label=long),
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
                  H10=predict(mod, newdata = data.frame(AGE_COMB=10)),
                  H5=predict(mod, newdata = data.frame(AGE_COMB=5)),
                  H2=predict(mod, newdata = data.frame(AGE_COMB=2)),
                  A_maxR=preds_sub$AGE_COMB[which.min(abs(90-preds_sub$value))],
                  A_minR=preds_sub$AGE_COMB[which.min(abs(75-preds_sub$value))])

row$K_maxR <- preds_sub$value[which.min(abs(preds_sub$AGE_COMB-(row$A_maxR)))] - preds_sub$value[which.min(abs(preds_sub$AGE_COMB-(row$A_maxR-1)))]
row$K_2_to_5 <- row$H5 - row$H2

df <- rbind(df, row) #add to df

#GAMM Knot 3

preds_sub <- preds_long[preds_long$name=="gam_re_3",]
mod <- M.3.re

row <- data.frame(Model="M.3.re",
                  name=deparse(formula(mod)), 
                  AIC=AIC(mod), 
                  r.sq = summary(mod)$r.sq,
                  scale.est=summary(mod)$scale,
                  dev=summary(mod)$dev,
                  H10=gammit::predict_gamm(mod, newdata = data.frame(AGE_COMB=10, ID=1, SHELL_NO=1), re_form=NA)$prediction,
                  H5=gammit::predict_gamm(mod, newdata = data.frame(AGE_COMB=5, ID=1, SHELL_NO=1), re_form=NA)$prediction,
                  H2=gammit::predict_gamm(mod, newdata = data.frame(AGE_COMB=2, ID=1, SHELL_NO=1), re_form=NA)$prediction,
                  A_maxR=preds_sub$AGE_COMB[which.min(abs(90-preds_sub$value))],
                  A_minR=preds_sub$AGE_COMB[which.min(abs(75-preds_sub$value))])
row$K_maxR <- preds_sub$value[which.min(abs(preds_sub$AGE_COMB-(row$A_maxR)))] - preds_sub$value[which.min(abs(preds_sub$AGE_COMB-(row$A_maxR-1)))]
row$K_2_to_5 <- row$H5 - row$H2

df <- full_join(df, row) #add to df


#GAMM Knot 5

preds_sub <- preds_long[preds_long$name=="gam_re_5",]
mod <- M.5.re

row <- data.frame(Model="M.5.re",
                  name=deparse(formula(mod)), 
                  AIC=AIC(mod), 
                  r.sq = summary(mod)$r.sq,
                  scale.est=summary(mod)$scale,
                  dev=summary(mod)$dev,
                  H10=gammit::predict_gamm(mod, newdata = data.frame(AGE_COMB=10, ID=1, SHELL_NO=1), re_form=NA)$prediction,
                  H5=gammit::predict_gamm(mod, newdata = data.frame(AGE_COMB=5, ID=1, SHELL_NO=1), re_form=NA)$prediction,
                  H2=gammit::predict_gamm(mod, newdata = data.frame(AGE_COMB=2, ID=1, SHELL_NO=1), re_form=NA)$prediction,
                  A_maxR=preds_sub$AGE_COMB[which.min(abs(90-preds_sub$value))],
                  A_minR=preds_sub$AGE_COMB[which.min(abs(75-preds_sub$value))])
row$K_maxR <- preds_sub$value[which.min(abs(preds_sub$AGE_COMB-(row$A_maxR)))] - preds_sub$value[which.min(abs(preds_sub$AGE_COMB-(row$A_maxR-1)))]
row$K_2_to_5 <- row$H5 - row$H2

df <- full_join(df, row)

df$Model[grep(x=df$Model, pattern = "3")] <- "GAMM, 3 knots"
df$Model[grep(x=df$Model, pattern = "5")] <- "GAMM, 5 knots"
df$Model[grep(x=df$Model, pattern = "VonB")] <- "von B"

names(df)[2] <- "Formula"

write.csv(df, paste0("Y:/Projects/Inshore_Ageing/data/",area,"_summary_stats.csv")