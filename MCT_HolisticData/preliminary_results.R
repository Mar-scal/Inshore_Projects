#First load required packages

library(tidyverse)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(readxl)

# save path 
#set year 
survey.year <- 2026  #survey year
area <- "BF" #SPA6, BF, SFA29, SPA2
# path.directory <- paste0("Y:/Projects/Holistic_sampling_Inshore/Holistic_sampling_",area,"/",survey.year,"/MCP_Reporting/")
path.directory <- paste0("Y:/Projects/Holistic_sampling_Inshore/Holistic_sampling_",area,"/",survey.year,"/")

#set up directory to save plot
saveplot.dir <- paste0(path.directory,"Figures/")

#load holistic data
# data <- read_excel(paste0(path.directory,"dataproducts/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), sheet=2) 
# metadata <- read_excel(paste0(path.directory,"dataproducts/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), sheet=1)
#load holistic data
data <- read_excel(paste0(path.directory,"Data/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), sheet=2) 
metadata <- read_excel(paste0(path.directory,"Data/",area,"_",survey.year,"_HolisticSamples_spatial.xlsx"), sheet=1)

#Change Maturity and Sex code to associated names 

Data_Master <- data %>% 
  mutate(Maturity_stage = case_when(Maturity_stage == "0"~ "Unknown",
                                    Maturity_stage =="1"~ "Immature",
                                    Maturity_stage =="2"~ "Recovering",
                                    Maturity_stage =="3"~ "Ripening",
                                    Maturity_stage =="4"~ "Ripe",
                                    Maturity_stage =="5"~ "Spawning",
                                    Maturity_stage =="6"~ "Spent",
                                    TRUE ~ as.character(NA))) %>% 
  mutate(Sex = case_when(Sex == "M" ~ "Male",
                         Sex == "F" ~ "Female",
                         Sex == "U" ~ "Unknown"))

# Shell height frequency plot ----

Data_Master <- Data_Master %>%
  mutate(SH_bin = floor(Shell_height_mm/ 5) * 5) 

ggplot(data = as_tibble(Data_Master), aes(x = SH_bin)) + 
  geom_bar(fill = "black", color = "white", alpha = 0.7) +
  geom_text(stat = "count", 
            aes(label = ..count..), vjust = -0.5) +
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 130, by = 10), expand = c(0,0)) +
  #scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 5)) +
  labs(x = 'Shell Height (mm)', y = 'Frequency of Scallop') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))

# Save plot to Folder
ggsave(filename = "shell_freq.png", path = saveplot.dir, width = 8, height = 4, units = "in", device = "png")

#SEX RATIO ----

Data_sex <- Data_Master %>% 
  group_by(Sex)  %>% 
  count(Maturity_stage,Sex)

totals <- Data_sex %>%
  group_by(Maturity_stage) %>%
  summarise(total = sum(n), .groups = "drop")

Data_sex$Maturity_stage <- factor(Data_sex$Maturity_stage,
                                  levels = c("Recovering","Ripening", "Ripe", "Spawning", "Spent"))

ggplot(Data_sex, aes(fill = Sex, y = n, x = Maturity_stage)) + 
  geom_bar(position = "fill", stat = "identity") +
  geom_text(data = totals, aes(x = Maturity_stage, y = 1.05, label = total), inherit.aes = FALSE) +
  scale_fill_manual(values = c("black", "lightgrey", "grey40")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Maturity", y = "Sex ratio") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  )

# Save plot to folder
ggsave(filename = "sex_ratio.png", path = saveplot.dir, width = 4, height = 4, units = "in", device = "png")


# Shell height by maturity -----    
Data_Master <- Data_Master %>%
  mutate(SH_bin = floor(Shell_height_mm/ 5) * 5) 


Data_maturity <- Data_Master %>% 
  count(SH_bin, Maturity_stage, Sex)


Data_maturity <- Data_maturity %>%
  group_by(SH_bin) %>%
  mutate(ratio = n / sum(n)) %>%
  ungroup()

totals <- Data_maturity %>%
  group_by(SH_bin) %>%
  summarise(total = sum(n), .groups = "drop")

Data_maturity$Maturity_stage <- factor (Data_maturity$Maturity_stage, levels = c("Recovering", "Ripening", "Ripe", "Spawning", "Spent"))

cols <- brewer.pal(name = "BuPu", n = 6)

ggplot(Data_maturity, aes(fill = Maturity_stage, y = ratio, x = SH_bin)) + 
  geom_bar(position = "fill", stat = "identity") +
  geom_text(data = totals, 
            aes(x = SH_bin, y = 1.05, label = total),
            inherit.aes = FALSE) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  scale_x_continuous(breaks = seq(min(Data_maturity$SH_bin),
                                  max(Data_maturity$SH_bin),
                                  by = 5)) +
  labs(x = " Shell height", y = "Maturity ratio") +
  theme_classic() +
  theme (
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.25)
  )


# save plot to folder
ggsave(filename = "Shell_ht_maturity.png", path = saveplot.dir, width = 7, height = 5, units = "in", device = "png")


##GSI and Sex ----

Data_Master <- Data_Master %>% 
  mutate(GSI = (Gonad_weight_g/Whole_weight_g) * 100)

data.gsi <- Data_Master %>%
  group_by(Sex, Maturity_stage) %>%
  summarise(
    mean_value = mean(GSI),
    sd_value = sd(GSI),
    n_value = n(),
    se_value = sd_value / sqrt(n_value)
  ) %>%
  mutate(Maturity_stage = factor(Maturity_stage,
                                 levels = c("Recovering", "Ripening", "Ripe", "Spawning", "Spent")))

ggplot(data.gsi, aes(x = Sex, y=mean_value, fill=Maturity_stage)) + 
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(
    aes(label = n_value, 
        y = mean_value / 2), 
    position = position_dodge(width = 0.9),
    vjust = 0.5,
    color = "Black", 
    size = 3) +
  geom_errorbar(aes(ymin=mean_value-sd_value, 
                    ymax=mean_value+sd_value), 
                width=.2,
                position=position_dodge(.9)) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  scale_fill_manual(values = cols) +
  theme_classic() +
  labs(x="Sex", y="GSI (%)", fill = "Maturity")

# save plot to folder
ggsave(filename = "GSI_SEX.png", path = saveplot.dir, width = 7, height = 5, units = "in", device = "png")


# GSI and shell height - GSI (Y axis) and SH(x axis) fill = maturity stage ----

ggplot(Data_Master, aes(x = Shell_height_mm, y = GSI, fill = Maturity_stage)) +
  geom_point(shape = 21, size = 2, color = "black", alpha = 0.7) +
  labs(
    x = "Shell Height (mm)",
    y = "Gonadosomatic Index (GSI)",
    fill = "Maturity Stage"
  ) +
  theme_classic()

# Save plot to folder
ggsave(filename = "GSI_shell_height.png", path = saveplot.dir, width = 7, height =5  , units = "in", device = "png")

#with regression lines 
ggplot(Data_Master, aes(x = Shell_height_mm, y = GSI, fill = Maturity_stage)) +
  geom_jitter(shape = 21, size = 2.0, color = "black", alpha = 0.5, width = 1, height = 0) +
  geom_smooth(aes(color = Maturity_stage), method = "lm", se = TRUE, alpha = 0.3) +
  labs(
    x = "Shell Height (mm)",
    y = "Gonadosomatic Index (GSI)",
    fill = "Maturity Stage",
    color = "Maturity Stage"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

# Save plot to folder
ggsave(filename = "GSI_shell_height_lm.png", path = saveplot.dir, width = 7, height = 5, units = "in", device = "png")


# Gonad weight and meat weight, fill = maturity 

ggplot(Data_Master, aes(x = Meat_weight_g, y = Gonad_weight_g)) +
  geom_jitter(aes(fill = Maturity_stage), shape = 21, size = 2.0, color = "black", 
              alpha = 0.5, width = 1, height = 0) +
  # Single regression line across all data
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3, aes(group = 1)) +
  labs(
    x = "Meat Weight (g)",
    y = "Gonad Weight (g)",
    fill = "Maturity Stage"
  ) +
  scale_fill_manual(values = c("blue", "yellow", "green", "red","cyan")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

ggsave(filename = "meat_gonad_wt.png", path = saveplot.dir, width = 7, height = 5, units = "in", device = "png")



#### ADDITIONAL QAQC FIGURES ####
#whole vs meat wts
A.<- ggplot(Data_Master, aes(x=Whole_weight_g ,y=Meat_weight_g)) + geom_text(label=Data_Master$Tow) + geom_smooth(method=lm, se=FALSE) + 
  labs(x="Whole Body Wet Weight (g)", y="Meat Wet Weight (g)", title = "Meat vs whole body wet weights")

#whole wt vs shell ht
B.<-ggplot(Data_Master, aes(x=Whole_weight_g,y=Shell_height_mm)) + geom_text(label=Data_Master$Tow) + geom_smooth(method=lm, se=FALSE) + 
  labs(x="Whole Body Wet Weight (g)", y="Shell height (mm)", title = "Shell height vs whole body wet weight")

#gonad wt vs shell ht
C.<-ggplot(Data_Master, aes(x=Gonad_weight_g ,y=Shell_height_mm)) + geom_text(label=Data_Master$Tow) + geom_smooth(method=lm, se=FALSE) + 
  labs(x="Gonad Wet Weight (g)", y="Shell height (mm)", title = "Shell height vs gonad wet weight")

#meat wt vs shell ht
D.<-ggplot(Data_Master, aes(x=Meat_weight_g ,y=Shell_height_mm)) + geom_text(label=Data_Master$Tow) + geom_smooth(method=lm, se=FALSE) + 
  labs(x="Meat Wet Weight (g)", y="Shell height (mm)", title = "Shell height vs meat wet weight")

#GSI vs shell ht
E.<-ggplot(Data_Master, aes(y= (Gonad_weight_g/Whole_weight_g)*100 ,x=Shell_height_mm)) + geom_text(label=Data_Master$Tow) + geom_smooth(method=lm, se=FALSE) + 
  labs(y="GSI (%)", x="Shell height (mm)", title = "GSI and shell height")

#shell ht vs shell wt
F.<-ggplot(Data_Master, aes(y= Shell_height_mm ,x=Shell_weight_g)) + geom_text(label=Data_Master$Tow) + geom_smooth() + 
  labs(x="shell weight (g)", y="Shell height (mm)", title = "shell height and weight")

#plot and save
cowplot::plot_grid(A.,B.,C.,D.,E.,F.)
ggsave(filename = "Additional.QAQC.png", path = saveplot.dir, width = 14, height = 10, units = "in", device = "png")
