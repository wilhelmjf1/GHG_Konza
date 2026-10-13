#### Data and statistical analysis and visualization for GHG data collected at SFKC Konza Prairie in 2021 ####

# Associated with the publication: Network Dry-down Effects on Dissolved Gases in a Non-Perennial Prairie Stream, Konza Prairie, KS #

# Load in necessary packages
library(ggplot2)
library(gridExtra)
library(cowplot)

#Read in all the data
#setwd("insert your working directory here")

GASS_ENVI_GP=read.csv("GASS_GP_Approach3andStretch_20210604_20210812_V1.csv", header=T)

# I reorder the groups order : I change the order of the factor data$names
GASS_ENVI_GP$month <- factor(GASS_ENVI_GP$month, levels = c("June", "July", "August"))

# grouped boxplot
dodge <- position_dodge(width = 1)

plot <- ggplot(GASS_ENVI_GP, aes(x=factor(month), y=CH4.CO2, color = factor(flow_state)))+
  geom_boxplot(position = dodge, outlier.shape = NA) +
  geom_point(position = dodge, size = 1.5, alpha = 0.7) +
  xlab("") + 
  ylab(expression(CH[4] : CO[2])) +
  theme(axis.text.y =element_text(size=30)) +
  theme(axis.text.x =element_text(size=30)) +
  theme_bw() +
  scale_color_manual(values=c("blue2", "red3")) +
  coord_cartesian(ylim = c(0, .08)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
plot
plot + theme(legend.box.background = element_rect(colour = "black"),legend.key.size = unit(1, 'cm'), legend.position = c(0.2, 0.8))

dodge <- position_dodge(width = 1)
p <- ggplot(GASS_ENVI_GP, aes(x=factor(month), y=dissolvedCO2_umolL_avg, color = factor(flow_state)))+
  geom_boxplot(position = dodge, outlier.shape = NA) +
  geom_point(position = dodge, size = 1.5, alpha = 0.7) +
  xlab("") + 
  ylab(expression(CO[2]*''~(μmol~L^-1))) +
  theme(axis.text.y =element_text(size=30)) +
  theme_bw() +
  scale_color_manual(values=c("blue2", "red3")) +
  coord_cartesian(ylim = c(0, 1200)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
p

p1 <- ggplot(GASS_ENVI_GP, aes(x=factor(month), y=dissolvedN2O_umolL_avg, color = factor(flow_state)))+
  geom_boxplot(position = dodge, outlier.shape = NA) +
  geom_point(position = dodge, size = 1.5, alpha = 0.7) +
  xlab("") + 
  ylab(expression(N[2]*'O'~(μmol~L^-1))) +
  theme_bw() +
  scale_color_manual(values=c("blue2", "red3")) +
  coord_cartesian(ylim = c(0, .05)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
p1

p2 <- ggplot(GASS_ENVI_GP, aes(x=factor(month), y=dissolvedCH4_umolL_avg, color = factor(flow_state)))+
  geom_boxplot(position = dodge, outlier.shape = NA) +
  geom_point(position = dodge, size = 1.5, alpha = 0.7) +
  xlab("") + 
  ylab(expression(CH[4]~(μmol~L^-1))) +
  theme_bw() +
  scale_color_manual(values=c("blue2", "red3")) +
  coord_cartesian(ylim = c(0, 15)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
p2

p3 <- ggplot(GASS_ENVI_GP, aes(x=factor(month), y=N2_umolL_avg, color = factor(flow_state)))+
  geom_boxplot(position = dodge, outlier.shape = NA) +
  geom_point(position = dodge, size = 1.5, alpha = 0.7) +
  xlab("") + 
  ylab(expression(N[2]~(μmol~L^-1))) +
  theme_bw() +
  scale_color_manual(values=c("blue2", "red3")) +
  coord_cartesian(ylim = c(0, 800)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
      axis.text.x=element_blank())
p3

p4 <- ggplot(GASS_ENVI_GP, aes(x=factor(month), y=O2_umolL_avg, color = factor(flow_state)))+
  geom_boxplot(position = dodge, outlier.shape = NA) +
  geom_point(position = dodge, size = 1.5, alpha = 0.7) +
  xlab("") + 
  ylab(expression(O[2]~(μmol~L^-1))) +
  theme_bw() +
  scale_color_manual(values=c("blue2", "red3")) +
  coord_cartesian(ylim = c(0, 500)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
p4

plot <- ggplot(GASS_ENVI_GP, aes(x=factor(month), y=CH4.CO2, color = factor(flow_state)))+
  geom_boxplot(position = dodge, outlier.shape = NA) +
  geom_point(position = dodge, size = 1.5, alpha = 0.7) +
  xlab("") + 
  ylab(expression(CH[4] : CO[2])) +
  theme(axis.text.y =element_text(size=30)) +
  theme(axis.text.x =element_text(size=30)) +
  theme_bw() +
  scale_color_manual(values=c("blue2", "red3")) +
  coord_cartesian(ylim = c(0, .08)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
plot
plot2 <- plot + theme(legend.box.background = element_rect(colour = "black"),legend.key.size = unit(1, 'cm'), legend.position = c(0.3, 0.77))

###Create multi-panel plot of all 6 graphs

plot_grid(p, p1, p2, p3, p4, plot, ncol=2, align="v")

