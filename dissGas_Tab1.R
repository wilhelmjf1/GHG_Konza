#### Data and statistical analysis and visualization for GHG data collected at SFKC Konza Prairie in 2021 ####

# Associated with the publication: Network Dry-down Effects on Dissolved Gases in a Non-Perennial Prairie Stream, Konza Prairie, KS #

# Load in necessary packages
library(lme4)
library(MuMIn)
library(dplyr)
library(tibble)

#Read in all the data

#setwd("insert your working directory here")

GHG=read.csv("GASS_GP_Approach3andStretch_20210604_20210812_V1.csv", header=T)

# Create factors
GHG$factor_flow_state <- as.factor(GHG$flow_state)
GHG$factor_month <- as.factor(GHG$month)
GHG$siteID <- as.factor(GHG$siteID)

#CO2

CO2_1 <- lmer(log(dissolvedCO2_umolL_avg) ~
                scale(O2_umolL_avg) + scale(NPOCmgL) +
                (1|siteID), data = GHG)

CO2_2 <- lmer(log(dissolvedCO2_umolL_avg) ~
                scale(O2_umolL_avg) + scale(MG.CA) +
                (1|siteID), data = GHG)

summary(CO2_1)
AIC(CO2_1)
r.squaredGLMM(CO2_1)

summary(CO2_2)
AIC(CO2_2)
r.squaredGLMM(CO2_2)

#N2O

N2O_1 <- lmer(log(dissolvedN2O_umolL_avg) ~
                scale(O2_umolL_avg) + factor_flow_state +
                (1|siteID), data = GHG)

N2O_2 <- lmer(log(dissolvedN2O_umolL_avg) ~
                scale(O2_umolL_avg) + scale(MG.CA) +
                (1|siteID), data = GHG)

N2O_3 <- lmer(log(dissolvedN2O_umolL_avg) ~
                scale(NO3_mgL) + scale(MG.CA) +
                (1|siteID), data = GHG)

summary(N2O_1)
AIC(N2O_1)
r.squaredGLMM(N2O_1)

summary(N2O_2)
AIC(N2O_2)
r.squaredGLMM(N2O_2)

summary(N2O_3)
AIC(N2O_3)
r.squaredGLMM(N2O_3)

#CH4

CH4_1 <- lmer(log(dissolvedCH4_umolL_avg) ~
                scale(NO3_mgL) + scale(O2_umolL_avg) +
                scale(MG.CA) + (1|factor_month), data = GHG)

CH4_2 <- lmer(log(dissolvedCH4_umolL_avg) ~
                scale(O2_umolL_avg) + scale(MG.CA) +
                (1|factor_month), data = GHG)

CH4_3 <- lmer(log(dissolvedCH4_umolL_avg) ~
                scale(O2_umolL_avg) + factor_flow_state +
                (1|factor_month), data = GHG)

summary(CH4_1)
AIC(CH4_1)
r.squaredGLMM(CH4_1)

summary(CH4_2)
AIC(CH4_2)
r.squaredGLMM(CH4_2)

summary(CH4_3)
AIC(CH4_3)
r.squaredGLMM(CH4_3)

#N2

N2_1 <- lmer(log(N2_umolL_avg) ~
               scale(O2_umolL_avg) + scale(NO3_mgL) +
               factor_flow_state + (1|siteID), data = GHG)

N2_2 <- lmer(log(N2_umolL_avg) ~
               scale(NO3_mgL) + factor_flow_state +
               (1|siteID), data = GHG)

N2_3 <- lmer(log(N2_umolL_avg) ~
               scale(O2_umolL_avg) + factor_flow_state +
               (1|siteID), data = GHG)

summary(N2_1)
AIC(N2_1)
r.squaredGLMM(N2_1)

summary(N2_2)
AIC(N2_2)
r.squaredGLMM(N2_2)

summary(N2_3)
AIC(N2_3)
r.squaredGLMM(N2_3)

#O2

O2_1 <- lmer(log(O2_umolL_avg) ~
               scale(NPOCmgL) + factor_flow_state +
               (1|siteID), data = GHG)

O2_2 <- lmer(log(O2_umolL_avg) ~
               scale(NO3_mgL) + factor_flow_state +
               (1|siteID), data = GHG)

O2_3 <- lmer(log(O2_umolL_avg) ~
               scale(NO3_mgL) + scale(NPOCmgL) +
               factor_flow_state + (1|siteID), data = GHG)

summary(O2_1)
AIC(O2_1)
r.squaredGLMM(O2_1)

summary(O2_2)
AIC(O2_2)
r.squaredGLMM(O2_2)

summary(O2_3)
AIC(O2_3)
r.squaredGLMM(O2_3)

#CH4:CO2

Ratio_1 <- lmer(log(CH4.CO2) ~
                  scale(O2_umolL_avg) + scale(MG.CA) +
                  (1|factor_month), data = GHG)

Ratio_2 <- lmer(log(CH4.CO2) ~
                  scale(O2_umolL_avg) + factor_flow_state +
                  (1|factor_month), data = GHG)

Ratio_3 <- lmer(log(CH4.CO2) ~
                  scale(NPOCmgL) + scale(MG.CA) +
                  (1|factor_month), data = GHG)

summary(Ratio_1)
AIC(Ratio_1)
r.squaredGLMM(Ratio_1)

summary(Ratio_2)
AIC(Ratio_2)
r.squaredGLMM(Ratio_2)

summary(Ratio_3)
AIC(Ratio_3)
r.squaredGLMM(Ratio_3)

#Combine into table 2

all_models <- list(
  CO2_1 = CO2_1,
  CO2_2 = CO2_2,
  N2O_1 = N2O_1,
  N2O_2 = N2O_2,
  N2O_3 = N2O_3,
  CH4_1 = CH4_1,
  CH4_2 = CH4_2,
  CH4_3 = CH4_3,
  N2_1 = N2_1,
  N2_2 = N2_2,
  N2_3 = N2_3,
  O2_1 = O2_1,
  O2_2 = O2_2,
  O2_3 = O2_3,
  Ratio_1 = Ratio_1,
  Ratio_2 = Ratio_2,
  Ratio_3 = Ratio_3
)

Table2 <- do.call(rbind, lapply(names(all_models), function(x) {
  
  model <- all_models[[x]]
  r2 <- r.squaredGLMM(model)
  
  data.frame(
    Model = x,
    N = nobs(model),
    Marg_R2 = round(r2[1, "R2m"], 2),
    Cond_R2 = round(r2[1, "R2c"], 2),
    AIC = round(AIC(model), 2)
  )
}))

print(Table2)

write.csv(Table2, "Table2_reproduced.csv",
          row.names = FALSE)