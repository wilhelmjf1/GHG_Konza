#### Data and statistical analysis and visualization for GHG data collected at SFKC Konza Prairie in 2021 ####

# Associated with the publication: Network Dry-down Effects on Dissolved Gases in a Non-Perennial Prairie Stream, Konza Prairie, KS #

# Load in necessary packages
library(ggplot2)

#Read in all the data
#setwd("insert your working directory here")


GASS_ENVI_GP=read.csv("GASS_GP_Approach3andStretch_20210604_20210812_V1.csv", header=T)

GASS_ENVI_GP$month <- factor(GASS_ENVI_GP$month, levels = c("June", "July", "August"))

GASS_ENVI_GP$month <- as.factor(GASS_ENVI_GP$month)

str(GASS_ENVI_GP)

###
###
#MG.CA #NO3_mgL #NPOCmgL #month
#O2_umolL_avg #N2_umolL_avg #dissolvedN2O_umolL_avg
#dissolvedCH4_umolL_avg #dissolvedCO2_umolL_avg

###

anova_CO2 <- aov(dissolvedCO2_umolL_avg ~ month, data = GASS_ENVI_GP)
residuals_CO2 <- residuals(anova_CO2)
qqnorm(residuals_CO2)
qqline(residuals_CO2)
shapiro.test(residuals_CO2)

anova_CH4 <- aov(dissolvedCH4_umolL_avg ~ month, data = GASS_ENVI_GP)
residuals_CH4 <- residuals(anova_CH4)
qqnorm(residuals_CH4)
qqline(residuals_CH4)
shapiro.test(residuals_CH4)

anova_N2O <- aov(dissolvedN2O_umolL_avg ~ month, data = GASS_ENVI_GP)
residuals_N2O <- residuals(anova_N2O)
qqnorm(residuals_N2O)
qqline(residuals_N2O)
shapiro.test(residuals_N2O)

anova_N2 <- aov(N2_umolL_avg ~ month, data = GASS_ENVI_GP)
residuals_N2 <- residuals(anova_N2)
qqnorm(residuals_N2)
qqline(residuals_N2)
shapiro.test(residuals_N2)

anova_O2 <- aov(O2_umolL_avg ~ month, data = GASS_ENVI_GP)
residuals_O2 <- residuals(anova_O2)
qqnorm(residuals_O2)
qqline(residuals_O2)
shapiro.test(residuals_O2)

anova_NO3 <- aov(NO3_mgL ~ month, data = GASS_ENVI_GP)
residuals_NO3 <- residuals(anova_NO3)
qqnorm(residuals_NO3)
qqline(residuals_NO3)
shapiro.test(residuals_NO3)

anova_MG.CA <- aov(MG.CA ~ month, data = GASS_ENVI_GP)
residuals_MG.CA <- residuals(anova_MG.CA)
qqnorm(residuals_MG.CA)
qqline(residuals_MG.CA)
shapiro.test(residuals_MG.CA)

anova_NPOC <- aov(NPOCmgL ~ month, data = GASS_ENVI_GP)
residuals_NPOC <- residuals(anova_NPOC)
qqnorm(residuals_NPOC)
qqline(residuals_NPOC)
shapiro.test(residuals_NPOC)

###

#MG.CA #NO3_mgL #NPOCmgL #month
#O2_umolL_avg #N2_umolL_avg #dissolvedN2O_umolL_avg
#dissolvedCH4_umolL_avg #dissolvedCO2_umolL_avg
GASS_ENVI_GP$month <- as.factor(GASS_ENVI_GP$month)
GASS_ENVI_GP$flow_state <- as.factor(GASS_ENVI_GP$flow_state)

kruskal.test(dissolvedCO2_umolL_avg ~ month, data = GASS_ENVI_GP)
kruskal.test(dissolvedCH4_umolL_avg ~ month, data = GASS_ENVI_GP)
kruskal.test(dissolvedN2O_umolL_avg ~ month, data = GASS_ENVI_GP)
kruskal.test(N2_umolL_avg ~ month, data = GASS_ENVI_GP)
kruskal.test(O2_umolL_avg ~ month, data = GASS_ENVI_GP)

#MG.CA #NO3_mgL #NPOCmgL #month
kruskal.test(MG.CA ~ month, data = GASS_ENVI_GP)
kruskal.test(NO3_mgL ~ month, data = GASS_ENVI_GP)
kruskal.test(NPOCmgL ~ month, data = GASS_ENVI_GP)

###
###

anova_CO2 <- aov(dissolvedCO2_umolL_avg ~ flow_state, data = GASS_ENVI_GP)
residuals_CO2 <- residuals(anova_CO2)
qqnorm(residuals_CO2)
qqline(residuals_CO2)
shapiro.test(residuals_CO2)

anova_CH4 <- aov(dissolvedCH4_umolL_avg ~ flow_state, data = GASS_ENVI_GP)
residuals_CH4 <- residuals(anova_CH4)
qqnorm(residuals_CH4)
qqline(residuals_CH4)
shapiro.test(residuals_CH4)

anova_N2O <- aov(dissolvedN2O_umolL_avg ~ flow_state, data = GASS_ENVI_GP)
residuals_N2O <- residuals(anova_N2O)
qqnorm(residuals_N2O)
qqline(residuals_N2O)
shapiro.test(residuals_N2O)

anova_N2 <- aov(N2_umolL_avg ~ flow_state, data = GASS_ENVI_GP)
residuals_N2 <- residuals(anova_N2)
qqnorm(residuals_N2)
qqline(residuals_N2)
shapiro.test(residuals_N2)

anova_O2 <- aov(O2_umolL_avg ~ flow_state, data = GASS_ENVI_GP)
residuals_O2 <- residuals(anova_O2)
qqnorm(residuals_O2)
qqline(residuals_O2)
shapiro.test(residuals_O2)

anova_NO3 <- aov(NO3_mgL ~ flow_state, data = GASS_ENVI_GP)
residuals_NO3 <- residuals(anova_NO3)
qqnorm(residuals_NO3)
qqline(residuals_NO3)
shapiro.test(residuals_NO3)

anova_MG.CA <- aov(MG.CA ~ flow_state, data = GASS_ENVI_GP)
residuals_MG.CA <- residuals(anova_MG.CA)
qqnorm(residuals_MG.CA)
qqline(residuals_MG.CA)
shapiro.test(residuals_MG.CA)

anova_NPOC <- aov(NPOCmgL ~ flow_state, data = GASS_ENVI_GP)
residuals_NPOC <- residuals(anova_NPOC)
qqnorm(residuals_NPOC)
qqline(residuals_NPOC)
shapiro.test(residuals_NPOC)

kruskal.test(dissolvedCO2_umolL_avg ~ flow_state, data = GASS_ENVI_GP)
kruskal.test(dissolvedCH4_umolL_avg ~ flow_state, data = GASS_ENVI_GP)
kruskal.test(dissolvedN2O_umolL_avg ~ flow_state, data = GASS_ENVI_GP)
kruskal.test(N2_umolL_avg ~ flow_state, data = GASS_ENVI_GP)
kruskal.test(O2_umolL_avg ~ flow_state, data = GASS_ENVI_GP)

#MG.CA #NO3_mgL #NPOCmgL #flow_state
kruskal.test(MG.CA ~ flow_state, data = GASS_ENVI_GP)
kruskal.test(NO3_mgL ~ flow_state, data = GASS_ENVI_GP)
kruskal.test(NPOCmgL ~ flow_state, data = GASS_ENVI_GP)
