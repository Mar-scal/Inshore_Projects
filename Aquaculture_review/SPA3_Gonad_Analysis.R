
###### SPA3 GONAD ANALYSIS #######


# Load libraries and data ---------------------------------------------------
library(mgcv)
library(lme4)
library(nlme)
library(gridExtra)
library(gratia)
library(ggspatial)
library(lattice)
library(viridis)
library(flextable)
library(GGally)
library(RColorBrewer)
library(png)
library(grid)
library(glmmTMB)
library(performance)
library(sf)
require (lubridate)
require (PBSmapping)
require(maptools)
library(ggcorrplot)
library(scales)


source("Z:/Projects/Holistic_sampling_with_HGS/TechReport_SPA3/Pr_Frac_function.R") #Plotting residuals - found on Sky
source("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/HighstatLibV13.R")

data <- read_csv("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/data/SPA3_scallop_data.csv", show_col_types = FALSE) 

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


sf::sf_use_s2(FALSE)



# Data formatting  --------------------------------------------------------

names(data)
str(data)
#colSums(is.na(data)) 

#format data - factors, numeric
cols.factor <- c("Tow_Sample.ID","Tow", "Strata", "SPA", "Cruise", "Strata_ID", "Sample.ID",  "Alb", "Epifauna", "Sex", "Maturity", "Meat.Colour", "Mycobacterium", "Sand","Boring.Upper", "Boring.Lower", "Pearl.Location.edit", "Spatial_Area")

cols.numeric <- c("Age","Pearls","Nodules","Height", "Width", "DEPTH", "GSI")

data[,cols.factor] <- lapply(data[,cols.factor],as.factor)
data[,cols.numeric] <- lapply(data[,cols.numeric], function(x){ as.numeric(x)})

data <- data |> 
  dplyr::rename(Viscera = Viscera.Wet.Corrected) |> 
  dplyr::rename(Gonad = Gonad.Wet.Corrected) |> 
  dplyr::rename(Muscle = Muscle.Wet.Corrected)

#Standardize height and depth

data$Log.Height <- log(data$Height)
data$Log.Height.std <- data$Log.Height - mean(data$Log.Height)

data$Muscle.std <- MyStd(data$Muscle)
data$Depth.std <- MyStd(data$DEPTH)

data <- data |> 
  dplyr::select(Gonad, Height, Log.Height.std, Log.Height, Depth.std, Tow, Sex, Spatial_Area, GSI, DDSlat, DDSlon, Muscle, Maturity, Age) |> 
  drop_na()


# Data Exploration  --------------------------------------------------------

# Spatial:
data.sf <- data |> 
  st_as_sf(coords = c("DDSlon","DDSlat"), crs = 4326)

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar=c('tl',0.5,-1,-1)))

mycols.1 <- c("lightgoldenrod2","seagreen3","maroon4")

p +
  geom_sf(data = data.sf, aes(colour = Spatial_Area, size = 1))+
  scale_colour_manual(values = mycols.1)


# Number of observations per tow:
table(data$Tow)

# Number of observations per Spatial_Area:
table(data$Spatial_Area)

# Number of observations per Sex:
table(data$Sex)

# Number of observations per Sex:
table(data$Sex, data$Spatial_Area)

# Outliers  --------------------------------------------------------

MyVar <- c("Height", "Gonad", "GSI", "Depth.std", "DDSlat", "DDSlon", "Muscle")
Mydotplot(data[, MyVar])

# Colinearity  --------------------------------------------------------

# Make boxplots of the tow conditional on Meat Colour. 
MyVar <- c("Height", "Gonad", "GSI", "Depth.std", "DDSlat", "DDSlon", "Muscle")
#Mypairs(data[,MyVar])

#ggpairs(data %>% drop_na() %>% dplyr::select(MyVar),progress = F)
lowerFn <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(color = 'blue', alpha=0.3, size=1) +
    geom_smooth(color = 'black', method='lm', size=1,...)
  p
}

g <- ggpairs( 
  data = data %>% drop_na() %>% dplyr::select(MyVar),
  lower = list(
    continuous =  wrap(lowerFn) #wrap("smooth", alpha = 0.3, color = "blue", lwd=1) 
  ),
  upper = list(continuous = wrap("cor", size = 4)), progress = F
)
g <- g + theme(
  axis.text = element_text(size = 6),
  axis.title = element_text(size = 6),
  legend.background = element_rect(fill = "white"),
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "grey95")
)
print(g, bottomHeightProportion = 0.5, leftWidthProportion = .5)


# Exploratory plots  --------------------------------------------------------

# Depth versus Spatial area
ggplot() + 
  geom_point(data = data, aes(y = Depth.std, x = Spatial_Area)) +
  geom_boxplot(data = data, aes(y = Depth.std, x = Spatial_Area))+
  xlab("Spatial Area") + ylab("Depth")+
  theme(text = element_text(size=12), legend.position="none")  
#Use spatial area for model

ggplot()+
  geom_point(data = data, aes(y = Height, x = Maturity)) +
  geom_boxplot(data = data, aes(y = Height, x = Maturity))+
  xlab("Maturity") + ylab("Shell Height")+
  theme(text = element_text(size=12), legend.position="none")

# Boxplot by Spatial Area and Gonad weight
ggplot() + geom_boxplot(data = data, 
                        aes(y = Gonad, x = Spatial_Area))+
  xlab("Spatial area") + ylab("Gonad weight (g)")+
  theme(text = element_text(size=15)) 

# Boxplot by sex and Gonad weight by spatial area
ggplot() + geom_boxplot(data = data, 
                        aes(y = Gonad, x = Sex))+
  xlab("Sex") + ylab("Gonad weight (g)")+
  theme(text = element_text(size=15))+
  facet_wrap(~Spatial_Area)

#Gonad Weight and Height
ggplot() + geom_point(data = data, aes(y = Gonad, x = Height))+
  xlab("Shell Height") + ylab("Gonad weight (g)")+
  theme(text = element_text(size=15))

#Gonad Weight and Height by tow
ggplot() + geom_point(data = data, aes(y = Gonad, x = Height))+
  xlab("Shell Height") + ylab("Gonad weight (g)")+
  theme(text = element_text(size=15))+
  geom_smooth(data = data, aes(y = Gonad, x = Height),
              method = "lm",
              se = FALSE)+
  facet_wrap(~Tow)

#Gonad Weight and Muscle weight
ggplot() + geom_point(data = data, aes(y = Gonad, x = Muscle))+
  xlab("Muscle weight (g)") + ylab("Gonad weight (g)")+
  theme(text = element_text(size=15))

#Gonad Weight and Maturity by spatial area
ggplot()+
  geom_point(data = data, aes(y = Gonad, x = Maturity)) +
  geom_boxplot(data = data, aes(y = Gonad, x = Maturity))+
  xlab("Maturity") + ylab("Gonad Weight")+
  theme(text = element_text(size=12), legend.position="none", axis.text.x=element_text(angle=90,vjust=-0.025)) +
  facet_wrap(~Spatial_Area)

#Gonad Weight and spatial area by maturity
ggplot()+
  geom_point(data = data, aes(y = Gonad, x = Spatial_Area)) +
  geom_boxplot(data = data, aes(y = Gonad, x = Spatial_Area))+
  xlab("Spatial Area") + ylab("Gonad Weight")+
  theme(text = element_text(size=12), legend.position="none") +
  facet_wrap(~Maturity)

#Gonad weight and shell height by trends tow
#ggplot()+
#  geom_point(data = data, 
#             aes(y = Gonad, x = Height),
#             shape = 1, 
#             size = 1)+
#  xlab("Shell Height") + ylab("Gonad weight (g)")+
#  theme(text = element_text(size=15))+
#  geom_smooth(data = data, aes(y = Gonad, x = Height, group = Tow), method = "lm",se = FALSE)



# ----Maturity ratio By Sex--------------------------------------------------------

cols <- brewer.pal(name = "BuPu", n = 6)
names <- c("Immature","Recovering", "Ripening", "Ripe", "Spawning", "Spent")

data_mat <- data %>% 
  dplyr::select(Tow, Spatial_Area, Sex, Gonad, Maturity) %>% 
  group_by(Maturity, Spatial_Area) %>%
  filter(!is.na(Sex)) %>% 
  dplyr::summarise(n = n())

ggplot(data_mat, aes(fill=Maturity, y=n, x=Spatial_Area)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(labels = names, values = cols)+
  labs(x="Spatial Area", y="Proportion of samples at maturity level",  title = "Maturity ratio by Spatial Area") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank())

#REMOVE SPENT SAMPLES HERE
data <- data |> 
  filter(Maturity != "Spent") #|> 
# filter(Sex == Female)

#Re-plot
data_mat <- data %>% 
  dplyr::select(Tow, Spatial_Area, Sex, Gonad, Maturity) %>% 
  group_by(Maturity, Spatial_Area) %>%
  filter(!is.na(Sex)) %>% 
  dplyr::summarise(n = n())

ggplot(data_mat, aes(fill=Maturity, y=n, x=Spatial_Area)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(labels = names, values = cols)+
  labs(x="Spatial Area", y="Proportion of samples at maturity level",  title = "Maturity ratio by Spatial Area") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank())

# ----Model formulation--------------------------------------------------------


M1 <- lme(Gonad ~ Log.Height.std + Spatial_Area,
          random =~ 1 | Tow,
          data = data)

mu  <- fitted(M1)           
E1  <- resid(M1)             
E1n <- resid(M1, type = "n")

summary(M1)

#resids vs fitted plot
par(mfrow = c(1,1), cex.lab = 1.5, mar = c(5,5,2,2))
plot(x = mu, 
     y = E1n, 
     xlab = "Fitted values", 
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2, col = 2)

#Fitted values plotting below zero, and patterns in residuals. Try GLM.

#Gonad weight and Spatial Area - GLM
M2 <- glm(Gonad ~ Spatial_Area + Log.Height.std + Sex + Maturity,  data=data, family=Gamma(link=log))
summary(M2)
rsq <- (M2$null.deviance-M2$deviance)/M2$null.deviance
rsq

#Save model residuals and fitted values to dataframe
data <- data.frame(data, M2.res=residuals(M2),  M2.fit=fitted(M2))

#Plotting residuals for each tow
Res.M2 <-ggplot(data)+
  geom_point(aes(x=M2.fit,y=M2.res))+theme_bw(22)+
  geom_hline(yintercept=0, linetype='dashed', col='red')+
  ylab("Residuals")+xlab("Fitted values") +
  #facet_wrap(~ Tow, nrow = 5) +
  theme(text = element_text(size=8), strip.placement = "outside",
        strip.background = element_blank(), strip.text = element_text(face="bold", 
                                                                      size=6, lineheight = 1.0), panel.spacing.y = unit(.5, "lines"),
        axis.text.x = element_text(angle=90))

Res.M2

#Residuals versus covariates
grid.arrange(pr.fac(M2, data$Log.Height.std, factorname = "logHeight"),
             pr.fac(M2, as.factor(data$Sex), factorname = "Sex"),
             pr.fac(M2, as.factor(data$Spatial_Area), factorname = "Spatial Area"), ncol = 2)

grid.arrange(pr.fac(M2, data$DDSlon, factorname = "Longitude"),
             pr.fac(M2, data$DDSlat, factorname = "Latitude"),
             pr.fac(M2, data$Muscle, factorname = "Muscle Weight"),
             pr.fac(M2, as.factor(data$Sex), factorname = "Sex"),
             pr.fac(M2, as.factor(data$Maturity), factorname = "Maturity"), ncol = 4)



# Predictions -------------------------------------------------------------

#1 Predict gonad weight for range of heights of female, ripe scallop
M2.newdata <- expand.grid(Log.Height.std = rep(seq(from  = range(data$Log.Height.std, na.rm = TRUE)[1],
                                                   to = range(data$Log.Height.std, na.rm = TRUE)[2], 
                                                   length = 25)), Spatial_Area = levels(data$Spatial_Area),Sex = "Female", Maturity = "Ripe")

M2.newdata$predicted.Gonad <- predict(M2, newdata = M2.newdata, allow.new.levels = TRUE, re.form=~0, type="response") 

#Back Standardize height
M2.newdata$Log.Height <- M2.newdata$Log.Height.std + mean(data$Log.Height)  
M2.newdata$Height <- exp(M2.newdata$Log.Height)

p1 <- ggplot() + theme_bw() +
  geom_point(aes(x = Height, y = Gonad, colour = Spatial_Area), data = data) +
  geom_line(aes(x = Height, y = predicted.Gonad, colour = Spatial_Area), data = M2.newdata) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "blue") +
  xlab("Shell Height") + ylab("Gonad weight") +
  scale_colour_manual(values=mycols.1, name='Spatial Area')+
  #facet_wrap(~ Spatial_Area, scales = "fixed", nrow = 3) +
  theme(text = element_text(size=8), strip.placement = "outside",
        strip.background = element_blank(), strip.text = element_text(face="bold", 
                                                                      size=6, lineheight = 1.0), panel.spacing.y = unit(.5, "lines"))
p1

p2 <- ggplot(M2.newdata)+
  geom_line(aes(x=Height, y=predicted.Gonad, col=Spatial_Area), size=1)+
  xlab("Shell Height (mm)") + ylab("Gonad weight (g)") +
  scale_colour_manual(values= mycols.1, name='Spatial Area')+
  facet_wrap(~Sex)
p2

#2 Predict Gonad weight for Shell height = 100 mm, Maturity = Ripe, for each spatial area: 
newdata.100.StMar <- data.frame(Log.Height.std=log(100)-mean(data$Log.Height), Spatial_Area = "St.Marys", Sex = levels(data$Sex), Maturity = "Ripe")
newdata.100.StMar$predicted.Gonad <- predict(M2, newdata = newdata.100.StMar, allow.new.levels = TRUE, re.form=~0, type="response")
newdata.100.StMar

newdata.100.off <- data.frame(Log.Height.std=log(100)-mean(data$Log.Height), Spatial_Area = "Offshelf", Sex = levels(data$Sex), Maturity = "Ripe")
newdata.100.off$predicted.Gonad <- predict(M2, newdata = newdata.100.off, allow.new.levels = TRUE, re.form=~0, type="response")
newdata.100.off

newdata.100.on <- data.frame(Log.Height.std=log(100)-mean(data$Log.Height), Spatial_Area = "Onshelf", Sex = levels(data$Sex), Maturity = "Ripe")
newdata.100.on$predicted.Gonad <- predict(M2, newdata = newdata.100.on, allow.new.levels = TRUE, re.form=~0, type="response")
newdata.100.on

#3 Predict Gonad weight for Shell height = 100 mm, Sex = Female, Maturity = Ripe, for each spatial area:
newdata.100 <- data.frame(Log.Height.std=log(100)-mean(data$Log.Height), Spatial_Area = levels(data$Spatial_Area), Sex = "Female", Maturity = "Ripe")
newdata.100$predicted.Gonad <- predict(M2, newdata = newdata.100, allow.new.levels = TRUE, re.form=~0, type="response")

#St.Marys
(newdata.100 |> 
    filter(Spatial_Area == "St.Marys") |> dplyr::select(predicted.Gonad) |> pull())[1]

#Onshelf
(newdata.100 |> 
    filter(Spatial_Area == "Onshelf") |> dplyr::select(predicted.Gonad) |> pull())[1]

#Offshelf
(newdata.100 |> 
    filter(Spatial_Area == "Offshelf") |> dplyr::select(predicted.Gonad) |> pull())[1]
  

# Egg calculations - Barber et al., 1988 ----------------------------------

newdata.100  |> 
  group_by(Spatial_Area) |> 
  summarize(mean_gonad = mean(predicted.Gonad)) |> 
  mutate(num_eggs = mean_gonad/(1.6*10^-7)) #Barber et al., 1988

((5.458581 + 5.318328)/2)/(1.6*10^-7) #eggs produced outside smb
7.805962/(1.6*10^-7)#eggs produced inside smb
48787263-33677841 #Difference between inside and outside
