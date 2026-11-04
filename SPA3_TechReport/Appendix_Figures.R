# DRY WEIGHT VS WET WEIGHT - SPA3 DATA
# To be used as appenidx for holistic protocol report

#load packages
library(tidyverse)
library(ggplot2)
require(ggspatial)
library(ggplot2)
library(scales) 
library(patchwork)


saveplot.dir <- "Y:/Projects/Holistic_sampling_Inshore/TechReport/Figures/"

SPA3.dat <- read.csv("Y:/Projects/Holistic_sampling_Inshore/Holistic_sampling_with_HGS/TechReport_SPA3/Data/SPA3_scallop_data.csv") %>% 
  mutate(GSI.Dry = (Gonad.Dry.Corrected/Full.Dry.Corrected)*100) %>% 
  mutate(GSI.Wet = (Gonad.Wet.Corrected/Full.Wet.Corrected)*100) %>% 
  filter(Gonad.Dry.Corrected != 0) #a couple gonads were to small for scale, throws off analysis if kept in

  

#### STATISTICAL TESTS ####
#WHOLE BODY
mod <- lm(log(Full.Dry.Corrected) ~ log(Full.Wet.Corrected), data = SPA3.dat, 
          na.action = na.exclude)
summary(mod) #high correlation

cor.test(SPA3.dat$Full.Wet.Corrected, SPA3.dat$Full.Dry.Corrected, 
         method = "spearman", use = "complete.obs", exact = FALSE) #high correlation

#check for size bias - no major patterns
SPA3.dat$resid <- residuals(mod)

ggplot(SPA3.dat, aes(x = log(Full.Wet.Corrected), y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_classic()


#VISCERA
mod <- lm(log(Viscera.Dry.Corrected) ~ log(Viscera.Wet.Corrected), data = SPA3.dat, 
          na.action = na.exclude)
summary(mod) #high correlation

cor.test(SPA3.dat$Viscera.Wet.Corrected, SPA3.dat$Viscera.Dry.Corrected, 
         method = "spearman", use = "complete.obs", exact = FALSE) #high correlation

#check for size bias - no major patterns
SPA3.dat$resid <- residuals(mod)

p.vis<-ggplot(SPA3.dat, aes(x = log(Viscera.Wet.Corrected), y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_classic(base_size=14) + labs(
    x = "", #"log(Wet viscera weight)",
    y = "Residuals")

#MEAT
mod <- lm(log(Muscle.Dry.Corrected) ~ log(Muscle.Wet.Corrected), data = SPA3.dat, 
          na.action = na.exclude)
summary(mod) #high correlation

cor.test(SPA3.dat$Muscle.Wet.Corrected, SPA3.dat$Muscle.Dry.Corrected, 
         method = "spearman", use = "complete.obs", exact = FALSE) #high correlation

#check for size bias - no major patterns
SPA3.dat$resid <- residuals(mod)

p.meat<-ggplot(SPA3.dat, aes(x = log(Muscle.Wet.Corrected), y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_classic(base_size=14) + labs(
    x = "", #"log(Wet meat weight)",
    y = "Residuals")

#GONAD
mod <- lm(log(Gonad.Dry.Corrected +0.0001) ~ log(Gonad.Wet.Corrected + 0.0001), data = SPA3.dat, 
          na.action = na.exclude)
summary(mod) #high correlation

cor.test(SPA3.dat$Gonad.Wet.Corrected, SPA3.dat$Gonad.Dry.Corrected, 
         method = "spearman", use = "complete.obs", exact = FALSE) #high correlation

#check for size bias - no major patterns
SPA3.dat$resid <- residuals(mod)

p.gon<-ggplot(SPA3.dat, aes(x = log(Gonad.Wet.Corrected), y = resid)) + geom_point() +
  geom_hline(yintercept = 0, linetype = 2) + theme_classic(base_size=14) + labs(
    x = "", #"log(Wet gonad weight)",
    y = "Residuals")

#GSI
mod <- lm(log(GSI.Dry + 0.0001) ~ log(GSI.Wet + 0.0001), data = SPA3.dat, 
          na.action = na.exclude)
summary(mod) #high correlation

cor.test(SPA3.dat$GSI.Wet, SPA3.dat$GSI.Dry, 
         method = "spearman", use = "complete.obs", exact = FALSE) #high correlation

#check for size bias - no major patterns
SPA3.dat$resid <- residuals(mod)

ggplot(SPA3.dat, aes(x = log(GSI.Wet), y = resid)) + geom_point() +
  geom_hline(yintercept = 0, linetype = 2) + theme_classic()


#### PLOTS FOR VISUAL CONFIRMATION ####
# SIZE AND MATURITY DISTRIBUTION
ggplot(data=SPA3.dat, aes(x=Height, fill= factor(Maturity, levels = 1:6, 
  labels = c("Immature", "Recovering","Ripening", "Ripe", "Spawning", "Spent")))) + 
  geom_histogram(binwidth = 5) +
  scale_fill_manual(name= "Maturity stage", values = c("lightblue1", "deepskyblue", "steelblue3", "dodgerblue4","blue4", "slateblue"),na.value = "grey80" ) + 
  theme_classic() + 
  scale_x_continuous(limits = c(0, 160), expand = F) +
  scale_y_continuous(limits = c(0, 45), expand = F) + 
  xlab("Shell height (mm)") + ylab("Frequency")

#save
ggsave(filename = paste0(saveplot.dir,'Appendix_SHFandMat.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# LOG-TRANSFORMED PLOT OF ALL TISSUES (with Spearman's coefficient)
# Create plot_df 
plot_df <- tibble(
  Tissue = rep(c("Remaining Viscera", "Meat", "Gonad", "Whole Body"), each = nrow(SPA3.dat)),
  Wet = c(
    SPA3.dat$Viscera.Wet.Corrected,
    SPA3.dat$Muscle.Wet.Corrected,
    SPA3.dat$Gonad.Wet.Corrected,
    SPA3.dat$Full.Wet.Corrected
  ),
  Dry = c(
    SPA3.dat$Viscera.Dry.Corrected,
    SPA3.dat$Muscle.Dry.Corrected,
    SPA3.dat$Gonad.Dry.Corrected,
    SPA3.dat$Full.Dry.Corrected
  )
) %>%
  filter(!is.na(Wet) & !is.na(Dry))

# Compute Spearman rho per tissue
rho_df <- plot_df %>%
  group_by(Tissue) %>%
  summarise(rho = cor(Wet, Dry, method = "spearman"))

p1<-ggplot(plot_df, aes(x = log(Wet), y = log(Dry))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~Tissue, scales = "free", ncol=4) +
  theme_classic(base_size = 14) +
  labs(
    x = "", #"log(Wet weight)",
    y = "log(Dry weight)",
    #title = "Relationship between wet and dry weights across tissues"
  ) +
  geom_text(
    data = rho_df,
    aes(
      x = -Inf, y = Inf,
      label = paste0("ρ = ", round(rho, 2))
    ),
    hjust = -0.1, vjust = 1.1,
    inherit.aes = FALSE,
    size = 4.5,
    fontface = "bold"
  )

#residuals
# Create plot_df 
plot_df2 <- tibble(
  Tissue = rep(c("Remaining Viscera", "Meat", "Gonad", "Whole Body"), each = nrow(SPA3.dat)),
  Wet = c(
    SPA3.dat$Viscera.Wet.Corrected,
    SPA3.dat$Muscle.Wet.Corrected,
    SPA3.dat$Gonad.Wet.Corrected,
    SPA3.dat$Full.Wet.Corrected
  ),
  resid = c(SPA3.dat$resid,
            SPA3.dat$resid,
            SPA3.dat$resid,
            SPA3.dat$resid
  )
) %>%
  filter(!is.na(Wet) & !is.na(resid))


p2<-ggplot(plot_df2, aes(x = log(Wet), y = resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype="dashed", colour="black")+
  facet_wrap(~Tissue, scales = "free", ncol=4) +
  theme_classic(base_size = 14) +
  labs(
    x = "log(Wet weight)",
    y = "Residuals"
  ) 

cowplot::plot_grid(p1,p2,nrow=2)
#save
ggsave(filename = paste0(saveplot.dir,'Appendix_LogWgt_AllTissues.png'), plot = last_plot(), scale = 2.5, width = 12, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#GSI
ggplot(data=SPA3.dat, aes(x=GSI.Wet, y=GSI.Dry)) + geom_point() + geom_abline(slope=1,size=1, colour="red")+ theme_classic(base_size = 14)+
  geom_text(aes(
      x = -Inf, y = Inf,
      label = paste0("ρ = 0.95 ")
    ),
    hjust = -0.1, vjust = 1.1,
    inherit.aes = FALSE,
    size = 4.5,
    fontface = "bold"
  ) +
  xlab("GSI using wet weights (%)") + ylab("GSI using dry weights (%)") 
#save
ggsave(filename = paste0(saveplot.dir,'Appendix_GSI.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


#### SEPARATE FIGURES WITH RAW DATA ####
#WHOLE BODY
ggplot(data=SPA3.dat, aes(x=Full.Wet.Corrected, y=Full.Dry.Corrected)) + geom_point() +
  geom_smooth() + xlab("Whole body wet weight (g)") + ylab("Whole body dry weight (g)")
#save
ggsave(filename = paste0(saveplot.dir,'Appendix_wholebody.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


#VISCERA
ggplot(data=SPA3.dat, aes(x=Viscera.Wet.Corrected, y=Viscera.Dry.Corrected)) + geom_point() +
  geom_smooth() + xlab("Viscera wet weight (g)") + ylab("Viscera dry weight (g)")
#save
ggsave(filename = paste0(saveplot.dir,'Appendix_viscera.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


#MEAT
ggplot(data=SPA3.dat, aes(x=Muscle.Wet.Corrected, y=Muscle.Dry.Corrected)) + geom_point() +
  geom_smooth() + xlab("Meat wet weight (g)") + ylab("Meat dry weight (g)") 
#save
ggsave(filename = paste0(saveplot.dir,'Appendix_meat.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


#GONAD
ggplot(data=SPA3.dat, aes(x=Gonad.Wet.Corrected, y=Gonad.Dry.Corrected)) + geom_point() +
  geom_smooth() + xlab("Gonad wet weight (g)") + ylab("Gonad dry weight (g)") 
#save
ggsave(filename = paste0(saveplot.dir,'Appendix_gonad.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


