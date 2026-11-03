# --------------------------------------------------------------------------#
#                     ADDITIONAL MEAT WEIGHT, WHOLE WEIGHT
#                            & SHELL HEIGHT PLOTS      
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
saveplot.dir.fr <- paste0(saveplot.dir,"FrenchFigures_indicies/")

#load holistic data
# data <- read_excel(paste0(path.directory,"dataproducts/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), sheet=2) 
# metadata <- read_excel(paste0(path.directory,"dataproducts/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), sheet=1)
#load holistic data
data <- read_excel(paste0(path.directory,"Data/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), sheet=2) 
metadata <- read_excel(paste0(path.directory,"Data/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), sheet=1)

# MEAT WEIGHT VS SHELL HEIGHT ####
dim(data)
table(data$Tow)

#create dataset for model
test.data <- subset(data, Shell_height_mm>40) #ensuring we only model with above 40mm scallops like in assessment
test.data$Log.HEIGHT <- log(test.data$Shell_height_mm)
test.data$Log.HEIGHT.CTR <- test.data$Log.HEIGHT - mean(test.data$Log.HEIGHT)
test.data$Log.DEPTH <- log(abs(test.data$ADJ_DEPTH)) #take abs to keep value positive
test.data$Log.DEPTH.CTR <- test.data$Log.DEPTH - mean(test.data$Log.DEPTH)
summary(test.data)

# run model (GLMM)
MWSH.mod <- glmer(Meat_weight_g~Log.HEIGHT.CTR+Log.DEPTH.CTR+(Log.HEIGHT.CTR|Tow),data=test.data,
                    family=Gamma(link=log), na.action = na.omit)
summary(MWSH.mod)

#1 Predict meat weight for range of heights
Mod.newdata <- expand.grid(Log.HEIGHT.CTR = rep(seq(from  = range(test.data$Log.HEIGHT.CTR, na.rm = TRUE)[1],
                                                   to = range(test.data$Log.HEIGHT.CTR, na.rm = TRUE)[2],
                                                   length = 25)),Log.DEPTH.CTR = test.data$Log.DEPTH.CTR)

Mod.newdata$predicted.MW <- predict(MWSH.mod, newdata = Mod.newdata, allow.new.levels = TRUE, re.form=~0, type="response")

#Back Standardize height
Mod.newdata$Log.Height <- Mod.newdata$Log.HEIGHT.CTR + mean(test.data$Log.HEIGHT)
Mod.newdata$Height <- exp(Mod.newdata$Log.Height)
Mod.newdata$Log.Depth <- Mod.newdata$Log.DEPTH.CTR + mean(test.data$Log.DEPTH)
Mod.newdata$Depth <- exp(Mod.newdata$Log.Depth)

# Create the plot
p1 <- ggplot() + theme_bw() +
  geom_point(aes(x = Shell_height_mm, y = Meat_weight_g), data = data, color = 'black', size = 1) + 
  geom_smooth(aes(x = Height, y = predicted.MW), data = Mod.newdata, color = 'red') +
  xlab("Shell Height (mm)") + ylab("Meat Weight (g)") +
  theme(text = element_text(size=14), 
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size=6, lineheight = 1.0), 
        panel.spacing.y = unit(.5, "lines"))

print(p1)


# WHOLE WEIGHT VS SHELL HEIGHT ####
dim(data)
table(data$Tow)

#create dataset for model
test.data <- subset(data, Shell_height_mm>40) #ensuring we only model with above 40mm scallops like in assessment
test.data$Log.HEIGHT <- log(test.data$Shell_height_mm)
test.data$Log.HEIGHT.CTR <- test.data$Log.HEIGHT - mean(test.data$Log.HEIGHT)
test.data$Log.DEPTH <- log(abs(test.data$ADJ_DEPTH)) #take abs to keep value positive
test.data$Log.DEPTH.CTR <- test.data$Log.DEPTH - mean(test.data$Log.DEPTH)
summary(test.data)

# run model (GLMM)
MWSH.mod <- glmer(Whole_weight_g~Log.HEIGHT.CTR+Log.DEPTH.CTR+(Log.HEIGHT.CTR|Tow),data=test.data,
                  family=Gamma(link=log), na.action = na.omit)
summary(MWSH.mod)

#1 Predict meat weight for range of heights
Mod.newdata <- expand.grid(Log.HEIGHT.CTR = rep(seq(from  = range(test.data$Log.HEIGHT.CTR, na.rm = TRUE)[1],
                                                    to = range(test.data$Log.HEIGHT.CTR, na.rm = TRUE)[2],
                                                    length = 25)),Log.DEPTH.CTR = test.data$Log.DEPTH.CTR)

Mod.newdata$predicted.WW <- predict(MWSH.mod, newdata = Mod.newdata, allow.new.levels = TRUE, re.form=~0, type="response")

#Back Standardize height
Mod.newdata$Log.Height <- Mod.newdata$Log.HEIGHT.CTR + mean(test.data$Log.HEIGHT)
Mod.newdata$Height <- exp(Mod.newdata$Log.Height)
Mod.newdata$Log.Depth <- Mod.newdata$Log.DEPTH.CTR + mean(test.data$Log.DEPTH)
Mod.newdata$Depth <- exp(Mod.newdata$Log.Depth)

# Create the plot
p2 <- ggplot() + theme_bw() +
  geom_point(aes(x = Shell_height_mm, y = Whole_weight_g), data = data, color = 'black', size = 1) + 
  geom_smooth(aes(x = Height, y = predicted.WW), data = Mod.newdata, color = 'red') +
  xlab("Shell Height (mm)") + ylab("Whole Weight (g)") +
  theme(text = element_text(size=14), 
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size=6, lineheight = 1.0), 
        panel.spacing.y = unit(.5, "lines"))

print(p2)


# MEAT WEIGHT VS WHOLE WEIGHT ####
dim(data)
table(data$Tow)

#create dataset for model
test.data <- subset(data, Shell_height_mm>40) #ensuring we only model with above 40mm scallops like in assessment
test.data$Log.WEIGHT <- log(test.data$Whole_weight_g)
test.data$Log.WEIGHT.CTR <- test.data$Log.WEIGHT - mean(test.data$Log.WEIGHT)
test.data$Log.DEPTH <- log(abs(test.data$ADJ_DEPTH)) #take abs to keep value positive
test.data$Log.DEPTH.CTR <- test.data$Log.DEPTH - mean(test.data$Log.DEPTH)
summary(test.data)

# run model (GLM)
MWSH.mod <- glmer(Meat_weight_g~Log.WEIGHT.CTR+Log.DEPTH.CTR+(Log.WEIGHT.CTR|Tow),data=test.data,
                  family=Gamma(link=log), na.action = na.omit)
summary(MWSH.mod)

#1 Predict meat weight for range of heights
Mod.newdata <- expand.grid(Log.WEIGHT.CTR = rep(seq(from  = range(test.data$Log.WEIGHT.CTR, na.rm = TRUE)[1],
                                                    to = range(test.data$Log.WEIGHT.CTR, na.rm = TRUE)[2],
                                                    length = 25)),Log.DEPTH.CTR = test.data$Log.DEPTH.CTR)

Mod.newdata$predicted.MW <- predict(MWSH.mod, newdata = Mod.newdata, allow.new.levels = TRUE, re.form=~0, type="response")

#Back Standardize height
Mod.newdata$Log.Weight <- Mod.newdata$Log.WEIGHT.CTR + mean(test.data$Log.WEIGHT)
Mod.newdata$Weight <- exp(Mod.newdata$Log.Weight)
Mod.newdata$Log.Depth <- Mod.newdata$Log.DEPTH.CTR + mean(test.data$Log.DEPTH)
Mod.newdata$Depth <- exp(Mod.newdata$Log.Depth)

# Create the plot
p3 <- ggplot() + theme_bw() +
  geom_point(aes(x = Whole_weight_g, y = Meat_weight_g), data = data, color = 'black', size = 1) + 
  geom_smooth(aes(x = Weight, y = predicted.MW), data = Mod.newdata, color = 'red') +
  xlab("Whole Weight (g)") + ylab("Meat Weight (g)") +
  theme(text = element_text(size=14), 
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size=6, lineheight = 1.0), 
        panel.spacing.y = unit(.5, "lines"))

print(p3)

cowplot::plot_grid(p1,p2,p3, ncol=3)

#save
ggsave(filename = paste0(saveplot.dir,'MW_WW_SH_Plots',area,"_",survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 12, height = 6, dpi = 300, units = "cm", limitsize = TRUE)
