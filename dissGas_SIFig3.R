#### Data and statistical analysis and visualization for GHG data collected at SFKC Konza Prairie in 2021 ####

# Associated with the publication: Network Dry-down Effects on Dissolved Gases in a Non-Perennial Prairie Stream, Konza Prairie, KS #

# Load in necessary packages
library(ggplot2)
library(gridExtra)
library(cowplot)

#Read in all the data
#setwd("insert your working directory here")

dissGas=read.csv("dissGas_comparison.csv", header=T)

#AIMS DelVec GRiMe MethDB
dissGas$project <- factor(dissGas$project, levels = c("AIMS", "DelVec", "GRiMe","Colt", "Weiss"))

# grouped boxplot
plot <- ggplot(dissGas, aes(x=factor(project), y=dissolvedCO2_umolL_avg, color = factor(project)))+
  geom_boxplot(outlier.shape = NA) +
  xlab("") + 
  ylab(expression(CO[2]*''~(μmol~L^-1))) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_blank(),    
    axis.title.x = element_blank(),
    panel.border = element_rect(color = "black", size = 0.25),
    legend.position = "none"          
  ) +
  scale_color_manual(values = c("blue", "#009E73", "#E69F00", "#56B4E9", "#CC79A7")) +
  scale_y_log10(
    labels = scales::label_number(accuracy = 1, big.mark = ""),
    expand = expansion(mult = c(0.08, 0.05))  # add a small buffer at bottom and top
  ) +
  coord_cartesian(clip = "off") 
plot

plot2 <- ggplot(dissGas, aes(x=factor(project), y=dissolvedN2O_umolL_avg, color = factor(project)))+
  geom_boxplot()+
  geom_point(position = position_jitterdodge(jitter.width = 0.15), size=1.5, alpha=0.5) +
  xlab("") + 
  ylab(expression(N[2]*'O'~(μmol~L^-1))) +
  theme(axis.text.y =element_text(size=30)) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_blank(),    
    axis.title.x = element_blank(),  
    panel.border = element_rect(color = "black", size = 0.25),
    legend.position = "none"         
  ) +
  scale_color_manual(values = c("blue", "#009E73", "#E69F00", "#56B4E9", "#CC79A7")) +
  scale_y_log10(
    labels = scales::label_number(accuracy = 1, big.mark = ""),
    expand = expansion(mult = c(0.08, 0.05))  # add a small buffer at bottom and top
  ) +
  coord_cartesian(clip = "off") 
plot2

plot3 <- ggplot(dissGas, aes(x=factor(project), y=dissolvedCH4_umolL_avg, color = factor(project)))+
  geom_boxplot()+
  geom_point(position = position_jitterdodge(jitter.width = 0.15), size=1.5, alpha=0.5) +
  xlab("") + 
  ylab(expression(CH[4]~(μmol~L^-1))) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_blank(),    
    axis.title.x = element_blank(),   
    panel.border = element_rect(color = "black", size = 0.25),
    legend.position = "none"          
  ) +
  scale_color_manual(values = c("blue", "#009E73", "#E69F00", "#56B4E9", "#CC79A7")) +
  scale_y_log10(
    labels = scales::label_number(accuracy = 1, big.mark = ""),
    expand = expansion(mult = c(0.08, 0.05))  # add a small buffer at bottom and top
  ) +
  coord_cartesian(clip = "off") 
plot3

plot4 <- ggplot(dissGas, aes(x=factor(project), y=N2_umolL_avg, color = factor(project)))+
  geom_boxplot()+
  geom_point(position = position_jitterdodge(jitter.width = 0.15), size=1.5, alpha=0.5) +
  xlab("") + 
  ylab(expression(N[2]~(μmol~L^-1))) +
  theme_bw() +
  scale_color_manual(values=c("blue", "#56B4E9", "#CC79A7", "#009E73", "#E69F00")) +
  coord_cartesian(ylim = c(0, 900)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_rect(color = "black", size = 0.25))
plot4

plot5 <- ggplot(dissGas, aes(x=factor(project), y=O2_umolL_avg, color = factor(project)))+
  geom_boxplot()+
  geom_point(position = position_jitterdodge(jitter.width = 0.15), size=1.5, alpha=0.5) +
  xlab("") + 
  theme_bw() +
  scale_color_manual(values=c("blue", "#56B4E9", "#CC79A7", "#009E73", "#E69F00")) +
  ylab(expression(O[2]~(μmol~L^-1))) +
  coord_cartesian(ylim = c(0, 500)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      panel.border = element_rect(color = "black", size = 0.25))
plot5

#####
grid.arrange(plot, plot2, plot3, plot4, plot5, ncol = 2, nrow = 3)

library(patchwork)
install.packages('patchwork')
(plot / plot2 / plot3) | (plot4 / plot5)