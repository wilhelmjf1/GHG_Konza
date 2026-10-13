#### Data and statistical analysis and visualization for GHG data collected at SFKC Konza Prairie in 2021 ####

# Associated with the publication: Network Dry-down Effects on Dissolved Gases in a Non-Perennial Prairie Stream, Konza Prairie, KS #

# Load in necessary packages
library(ggplot2)
library(gridExtra)
library(cowplot)
library(viridis)
library(tidyverse)
library(ggtext)

#Read in all the data

#setwd("insert your working directory here")

GASS_ENVI_GP=read.csv("konzaSynoptic2021_GASS_ENVI_GP.csv", header=T)

GASS_ENVI_GP$month <- factor(GASS_ENVI_GP$month,levels = c("June", "July", "August"))

GASS_ENVI_GP$flow_state <- factor(GASS_ENVI_GP$flow_state,levels = c("flowing", "pool", "dry"))

seeps=read.csv("Konza_Springs_Units.csv", header=T)

#distance_from_outlet #elevation #flow_state #month

#ncol()

GASS_ENVI_GP <- GASS_ENVI_GP[ -c(56, 58, 59) ] 

###

p <- ggplot(GASS_ENVI_GP, aes(x = distance_from_outlet, y = elevation, shape=flow_state)) +
  geom_point(size=2) +
  scale_shape_manual(values=c(16,15,1)) +
  facet_wrap("month") +
  labs(shape="flow state") +
  xlab("distance from outlet (km)") + ylab("elevation (m)") + 
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(strip.text = element_text(
    size = 10))

p + 
  geom_point(data=seeps, aes(x = distance_from_outlet, y = elevation), shape = 4, color='black') + 
  theme(legend.position = "right") 
