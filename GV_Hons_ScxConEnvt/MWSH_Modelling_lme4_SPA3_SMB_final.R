## Modelling of Scallop MW-SH data to derive condition index 
## J.Sameoto & B.Wilson Sept 2026 
## For Gabriela V Honours project (DAL)

library(lattice)
library(lme4)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(performance)
library(DHARMa)
library(sf)
library(terra)
source(file = "Y:/Courses/GAMM_and_GAM_updated2026/AllDataGAMMFreqV3/AllData/HighstatLibV15.R") 
source("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r")

# Load files ---------------------------------------------------------

#dir <- "Y:/Projects/Condition Project/AZMP/2025/GabrielaVieiraLopes_Honours"
dir <- "D:/Projects/GabrielaVieiraLopes_Honours"

##MWSH data (after data exploration and finalizing dataset)
detail.dat <- read.csv(paste0(dir,"/Data/SMB_MWSH_2000-2025_Jun1-Aug15.csv"))
  
##Shapefiles
extent <- st_read(paste0(dir,"/GIS_data/SPA3_SMB_Extent.shp"))

# Formatting for Modelling ---------------------------------------------------------

#Set shell height for prediction:
SH.for.pred <- 100

#Calculate mean depth for each area for predictions:
#bathy.BoF <- rast("Y:/Inshore/Assessment/StandardDepth/ScotianShelfDEM_Olex/mdem_olex/w001001.adf") %>% project("epsg:4326")
#bathy.29W <- rast("Y:/Inshore/Assessment/StandardDepth/ScotianShelfDEM_Olex/mdem_olex/w001001.adf") %>% project("epsg:4326")

#BF.avg.depth <- round(bathy.BoF %>% crop(BFextent, mask = TRUE) %>% global(fun = "mean", na.rm = TRUE),1) #-78.0
#SPA3.avg.depth <- round(bathy.BoF %>% crop(BIextent, mask = TRUE) %>% global(fun = "mean", na.rm = TRUE),1) #-69.4
#SMB.avg.depth <- round(bathy.BoF %>% crop(smb.sf, mask = TRUE) %>% global(fun = "mean", na.rm = TRUE),1) #-24.1
#BILU.avg.depth <- round(bathy.BoF %>% crop(brLur.sf, mask = TRUE) %>% global(fun = "mean", na.rm = TRUE),1) #-77.8
#SPA6.avg.depth <- round(bathy.BoF %>% crop(GMextent, mask = TRUE) %>% global(fun = "mean", na.rm = TRUE),1) #-62.1
#SFA29W.avg.depth <- round(bathy.29W %>% crop(SFA29extent, mask = TRUE) %>% global(fun = "mean", na.rm = TRUE),1) #-52.6

#BF.avg.depth <- -78.0
#SPA3.avg.depth <- -69.4
#SMB.avg.depth <- -24.1
#BILU.avg.depth <- -77.8
#SPA6.avg.depth <- -62.1
#SFA29W.avg.depth <- -52.6

avg.depth <- -24.1

#Double check data what expected 
str(detail.dat)
table(detail.dat$month)
table(detail.dat$year)
min(detail.dat$HEIGHT) #should be 50 
max(detail.dat$DayofYear) #should be no greater than 227 or 228 - except if dealing with 29W 

test <- detail.dat
test$lat <- convert.dd.dddd(test$START_LAT)
test$lon <- convert.dd.dddd(test$START_LONG)

test.sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)
mapview::mapview(test.sf)

ggplot() + geom_sf(data = test.sf)
  
 
#### create dataset for model ####
test.data <- detail.dat %>%
  mutate(Log.HEIGHT = log(HEIGHT)) %>% 
  mutate(Log.HEIGHT.CTR = Log.HEIGHT - mean(Log.HEIGHT)) %>% 
 # mutate(Log.WET.MEAT.WGT = log(WET_MEAT_WGT)) %>% 
 # mutate(Log.WET.MEAT.WGT.CTR = Log.WET.MEAT.WGT - mean(Log.WET.MEAT.WGT)) %>% 
  mutate(Log.DEPTH = log(abs(ADJ_DEPTH))) %>%
  mutate(Log.DEPTH.CTR = Log.DEPTH - mean(Log.DEPTH)) %>% 
  mutate(ID = as.factor(ID)) %>% 
  mutate(Year.ID = paste0(year,".",ID)) %>% 
  mutate(Year.ID = as.factor(Year.ID)) %>% 
  mutate(year.f = as.factor(year))

summary(test.data)
str(test.data)


#---- Models----

### GLMM model - random slope and intercept ####
MWTSHBF.1 <- glmer(WET_MEAT_WGT~0+Log.HEIGHT.CTR+Log.DEPTH.CTR+year.f+(Log.HEIGHT.CTR|ID),
                   data=test.data, family=Gamma(link=log), na.action = na.omit, control= glmerControl(optimizer = c("bobyqa")))
#Note - Model failed to converge with control= glmerControl(optimizer = c("Nelder_Mead"))

# View the summary
summary(MWTSHBF.1)
AIC(MWTSHBF.1)
# 26008.63
BIC(MWTSHBF.1)
# 26192.02

#diagnostics
latt.1 <- data.frame(test.data, res=residuals(MWTSHBF.1,"pearson"),fit=fitted(MWTSHBF.1))

#Residuals vs fitted
ggplot(latt.1, aes(x = fit, y = res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")+
  facet_wrap(~year)
ggsave(paste0(dir,"/ScxConditionModels/SPA3_SMB/SPA3_SMB.model.1.resids.tiff"), width = 15, height = 10, units = "cm", dpi = 300)

#Plot of fitted values 
ggplot(latt.1, aes(x = fit, y = WET_MEAT_WGT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")+
  facet_wrap(~year)
ggsave(paste0(dir,"/ScxConditionModels/SPA3_SMB/SPA3_SMB.model.1.fit.tiff"), width = 15, height = 10, units = "cm", dpi = 300)


#Predictions at tow level
nested.grps <- unique(test.data[, c("year.f", "ID")])

fixed_ef <- expand.grid(
  Log.HEIGHT.CTR=log(SH.for.pred)-mean(test.data$Log.HEIGHT),
  Log.DEPTH.CTR=log(abs(avg.depth)) - mean(test.data$Log.DEPTH),
  year.f = as.factor(unique(test.data$year))
)

unique(fixed_ef$Log.HEIGHT.CTR)
unique(fixed_ef$Log.DEPTH.CTR)

## Predict - only use fixed effects; re.form=~0 or NA gives population level effects , re.form = NULL uses random effects 

##If you wanted to include random effects for the prediction - but we don't (but could and look at spatial residuals, etc)
#To include random effects, then prediction data needs ID (Cruise.TowNo field)
#newdata <- merge(fixed_ef, nested.grps, by = "year.f", all.y = TRUE)
#newdata$Condition <- NA
#head(newdata)
#prediction now using random effects: 
#newdata$Condition <- predict(MWTSHBF.3, newdata = newdata, re.form = NULL, type = "response")
#head(newdata)

#Include only fixed effects
#To only used fixed effects, prediction data only needs the fixed effects fields (height, depth, year)
newdata.1 <- fixed_ef
newdata.1$Condition <- NA
newdata.1$model <- NA 
newdata.1$year <- as.numeric(as.character(newdata.1$year.f))
newdata.1$model <- "GLMM_MWTSHBF.1"
head(newdata.1)
str(newdata.1)
#prediction now using random effects: 
newdata.1$Condition <- predict(MWTSHBF.1, newdata = newdata.1, re.form = ~0, type = "response")
head(newdata.1)
View(newdata.1)

ggplot(data = newdata.1, aes(x = year.f, y = Condition)) + geom_point() + geom_line()



### GLMM model - random intercept ####
#failed to converge with default Nelder_Mead: failure to converge in 10000 evaluations
MWTSHBF.2 <- glmer(WET_MEAT_WGT~ 0 + Log.HEIGHT.CTR+Log.DEPTH.CTR+year.f+(1|ID), 
                   data=test.data, family=Gamma(link=log), na.action = na.omit, control= glmerControl(optimizer = c("bobyqa")))

summary(MWTSHBF.2)
AIC(MWTSHBF.2)
#26339.64
BIC(MWTSHBF.2)
#26509.93

#diagnostics
latt.2 <- data.frame(test.data, res=residuals(MWTSHBF.2,"pearson"),fit=fitted(MWTSHBF.2))

#Residuals vs fitted
ggplot(latt.2, aes(x = fit, y = res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")+
  facet_wrap(~year)

#Plot of fitted values 
ggplot(latt.2, aes(x = fit, y = WET_MEAT_WGT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")+
  facet_wrap(~year)


#Predictions at tow level
nested.grps <- unique(test.data[, c("year.f", "ID")])

fixed_ef <- expand.grid(
  Log.HEIGHT.CTR=log(SH.for.pred)-mean(test.data$Log.HEIGHT),
  Log.DEPTH.CTR=log(abs(avg.depth)) - mean(test.data$Log.DEPTH),
  year.f = as.factor(unique(test.data$year))
)

unique(fixed_ef$Log.HEIGHT.CTR)
unique(fixed_ef$Log.DEPTH.CTR)

## Predict - only use fixed effects; re.form=~0 or NA gives population level effects , re.form = NULL uses random effects 

##If you wanted to include random effects for the prediction - but we don't (but could and look at spatial residuals, etc)
##To include random effects, then prediction data needs ID (Cruise.TowNo field)
#newdata.2 <- merge(fixed_ef, nested.grps, by = "year.f", all.y = TRUE)
#newdata.2$Condition <- NA
#head(newdata.2)
##prediction now using random effects: 
#newdata.2$Condition <- predict(MWTSHBF.2, newdata = newdata.2, re.form = NULL, #type = "response")
#head(newdata.2)

#Include only fixed effects
#To only used fixed effects, prediction data only needs the fixed effects fields (height, depth, year)
newdata.2 <- fixed_ef
newdata.2$Condition <- NA
newdata.2$model <- NA 
newdata.2$year <- as.numeric(as.character(newdata.2$year.f))
newdata.2$model <- "GLMM_MWTSHBF.2"
head(newdata.2)
str(newdata.2)
#prediction now using random effects: 
newdata.2$Condition <- predict(MWTSHBF.2, newdata = newdata.2, re.form = ~0, type = "response")
head(newdata.2)
View(newdata.2)

ggplot(data = newdata.2, aes(x = year.f, y = Condition)) + geom_point() + geom_line()



### Compare GLMM models: ####
AIC(MWTSHBF.1,MWTSHBF.2)
BIC(MWTSHBF.1,MWTSHBF.2)
anova(MWTSHBF.1,MWTSHBF.2)

# MWTSHBF.1 best on all metrics 

#if wanted to look at difference in prediction (condition)
aa <- newdata.1 %>% select(year, Condition, model)
bb <- newdata.2 %>% select(year, Condition, model)

xx <- rbind(aa,bb)
head(xx)

ggplot(data = xx, aes(x = year, y = Condition, group = as.factor(model), color = as.factor(model))) + geom_point() + geom_line()

#select final model; DEFINE  
Final.Model <- MWTSHBF.1
Final.Data <- newdata.1
Final.Data$Area <- "SPA3_SMB"

#--- Export final model and final model condition dataset ---- 

#output of final model 
#Save summary to txt file
sink(paste0(dir,"/ScxConditionModels/SPA3_SMB_ModelSummary.txt"))
print(summary(Final.Model))
sink()

#Condition dataset 
write.csv(Final.Data, paste0(dir, "/ScxConditionModels/SPA3_SMB_ConditionTS.csv"), row.names = FALSE)

### END #### 

### GLM - Fixed effect model only to compare to GLMMs ####
MWTSHBF.glm <- glm(WET_MEAT_WGT~0+Log.HEIGHT.CTR+Log.DEPTH.CTR+as.factor(year), 
                 data=test.data, family=Gamma(link=log), na.action = na.omit)
summary(MWTSHBF.glm)

## Predict
fixed_ef <- expand.grid(
  Log.HEIGHT.CTR=log(SH.for.pred)-mean(test.data$Log.HEIGHT),
  Log.DEPTH.CTR=log(abs(avg.depth)) - mean(test.data$Log.DEPTH),
  year = as.factor(unique(test.data$year))
)
newdata.glm <- fixed_ef
newdata.glm$Condition <- NA
newdata.glm$model <- NA
newdata.glm$model <- "GLM_fixedonly"
head(newdata.glm)
#prediction
newdata.glm$Condition <- predict(MWTSHBF.glm, newdata = newdata.glm, type = "response")
head(newdata.glm)
ggplot(data = newdata.glm,aes(x = year, y = Condition, group = 1)) + 
  geom_point(color = "red", size = 3) +
  geom_line(color = "blue", linewidth = 1)



