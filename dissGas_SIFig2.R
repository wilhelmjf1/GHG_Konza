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

doc <- ggplot(GASS_ENVI_GP, aes(x=factor(month), y=NPOCmgL, color = factor(flow_state)))+
  geom_boxplot(position = dodge, outlier.shape = NA) +
  geom_point(position = dodge, size = 1.5, alpha = 0.7) +
  xlab("") + 
  ylab(expression(DOC*''~(mg~L^-1))) +
  theme_bw() +
  theme(
    axis.text.y =element_text(size=10),
    axis.text.x = element_blank(),
    legend.position = "none"
  ) +
  scale_color_manual(values=c("blue2", "red3")) +
  coord_cartesian(ylim = c(0, 30))
doc

no3 <- ggplot(GASS_ENVI_GP, aes(x=factor(month), y=NO3_mgL, color = factor(flow_state)))+
  geom_boxplot(position = dodge, outlier.shape = NA) +
  geom_point(position = dodge, size = 1.5, alpha = 0.7) +
  xlab("") + 
  ylab(expression(NO[3]*''~(μmol~L^-1))) +
  theme_bw() +
  theme(
    axis.text.y =element_text(size=10),
    axis.text.x = element_blank(),
    legend.position = "none"
  ) +
  scale_color_manual(values=c("blue2", "red3")) +
  coord_cartesian(ylim = c(0, 2))
no3

mg.ca <- ggplot(GASS_ENVI_GP,
                aes(x = factor(month),
                    y = MG.CA,
                    color = factor(flow_state))) +
  geom_boxplot(position = dodge, outlier.shape = NA) +
  geom_point(position = dodge, size = 1.5, alpha = 0.7) +
  xlab("") +
  ylab(expression(Mg : Ca)) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("blue2", "red3")) +
  coord_cartesian(ylim = c(0, 0.30))

print(mg.ca)

#combine the plots 

arrange <- plot_grid(
  doc,
  no3,
  mg.ca,
  NULL,
  ncol = 2,
  labels = c("A", "B", "C", ""),
  label_size = 12
)

print(arrange)