#### Data and statistical analysis and visualization for GHG data collected at SFKC Konza Prairie in 2021 ####

# Associated with the publication: Network Dry-down Effects on Dissolved Gases in a Non-Perennial Prairie Stream, Konza Prairie, KS #

# Load in necessary packages
library(ggplot2)
library(gridExtra)
library(cowplot)
library(dplyr)
library(tidyr)

#Read in all the data
#setwd("insert your working directory here")

GASS_ENVI_GP=read.csv("GASS_GP_Approach3andStretch_20210604_20210812_V1.csv", header=T)

GASS_ENVI_GP$month <- factor(GASS_ENVI_GP$month, levels = c("June", "July", "August"))

GASS_ENVI_GP_long <- GASS_ENVI_GP %>%
  pivot_longer(
    cols = c(dissolvedCO2_umolL_avg, dissolvedCH4_umolL_avg, 
             dissolvedN2O_umolL_avg, N2_umolL_avg, O2_umolL_avg),
    names_to = "Gas",
    values_to = "Concentration"
  )

###
GASS_ENVI_GP_long <- GASS_ENVI_GP_long %>%
  mutate(Gas = case_when(
    Gas == "dissolvedCO2_umolL_avg" ~ "CO2",
    Gas == "dissolvedCH4_umolL_avg" ~ "CH4",
    Gas == "dissolvedN2O_umolL_avg" ~ "N2O",
    Gas == "N2_umolL_avg" ~ "N2",
    Gas == "O2_umolL_avg" ~ "O2",
    TRUE ~ Gas
  ))

###

iqr_table <- GASS_ENVI_GP_long %>%
  group_by(Gas, month) %>%
  summarise(
    IQR = IQR(Concentration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = month, values_from = IQR) %>%
  mutate(
    `Δ IQR (Aug - June)` = August - June,
    Trend = case_when(
      `Δ IQR (Aug - June)` > 0 ~ "↑",
      `Δ IQR (Aug - June)` < 0 ~ "↓",
      TRUE ~ "="
    )
  )

print(iqr_table)

###
###GASS_ENVI_GP

GASS_ENVI_GP_long <- GASS_ENVI_GP %>%
  pivot_longer(
    cols = c(dissolvedCO2_umolL_avg, dissolvedCH4_umolL_avg,
             dissolvedN2O_umolL_avg, N2_umolL_avg, O2_umolL_avg),
    names_to = "Gas_raw",
    values_to = "Concentration"
  ) %>%
  mutate(Gas = case_when(
    Gas_raw == "dissolvedCO2_umolL_avg" ~ "CO2",
    Gas_raw == "dissolvedCH4_umolL_avg" ~ "CH4",
    Gas_raw == "dissolvedN2O_umolL_avg" ~ "N2O",
    Gas_raw == "N2_umolL_avg" ~ "N2",
    Gas_raw == "O2_umolL_avg" ~ "O2",
    TRUE ~ NA_character_
  )) %>%
  select(month, flow_state, Gas, Concentration)

###
iqr_by_gas_month_flow <- GASS_ENVI_GP_long %>%
  group_by(Gas, month, flow_state) %>%
  summarise(
    IQR = IQR(Concentration, na.rm = TRUE),
    .groups = "drop"
  )

###
iqr_comparison <- iqr_by_gas_month_flow %>%
  pivot_wider(
    names_from = flow_state,
    values_from = IQR
  ) %>%
  arrange(Gas, month)

###
print(iqr_comparison)

###

GASS_ENVI_GP_long %>% filter(Gas == "N2", month == "June") %>%
  summarise(IQR_all = IQR(Concentration, na.rm = TRUE))

# IQR split by flow_state
GASS_ENVI_GP_long %>% filter(Gas == "N2", month == "June") %>%
  group_by(flow_state) %>%
  summarise(IQR = IQR(Concentration, na.rm = TRUE))
