#### Data and statistical analysis and visualization for GHG data collected at SFKC Konza Prairie in 2021 ####

# Associated with the publication: Network Dry-down Effects on Dissolved Gases in a Non-Perennial Prairie Stream, Konza Prairie, KS #

# Load in necessary packages
library(lme4)
library(ggeffects)
library(ggplot2)
library(patchwork)

#Read in all the data
#setwd("insert your working directory here")

theme_set(theme_classic(base_size = 11))

# LOAD DATA
GASS_ENVI_GP = read.csv("GASS_GP_Approach3andStretch_20210604_20210812_V1.csv", header = TRUE)

GASS_ENVI_GP$month <- factor(GASS_ENVI_GP$month, levels = c("June", "July", "August"))
GASS_ENVI_GP$flow_state_n <- as.factor(GASS_ENVI_GP$flow_state_n)

# SCALE VARIABLES
GASS_ENVI_GP <- GASS_ENVI_GP |>
  transform(
    O2_s   = as.numeric(scale(O2_umolL_avg)),
    MGCA_s = as.numeric(scale(MG.CA)),
    NO3_s  = as.numeric(scale(NO3_mgL)),
    DOC_s  = as.numeric(scale(NPOCmgL))
  )

#month
#MG.CA, NO3_mgL, flow_state, flow_state_n, NPOCmgL
#dissolvedN2O_umolL_avg, dissolvedCH4_umolL_avg, dissolvedCO2_umolL_avg
#O2_umolL_avg, N2_umolL_avg

mod_co2  <- lmer(dissolvedCO2_umolL_avg ~ O2_s + MGCA_s + (1 | siteID), data = GASS_ENVI_GP)
mod_n2o  <- lmer(dissolvedN2O_umolL_avg ~ O2_s + MGCA_s + (1 | siteID), data = GASS_ENVI_GP)
mod_ch4  <- lmer(dissolvedCH4_umolL_avg ~ O2_s + MGCA_s + (1 | month), data = GASS_ENVI_GP)
mod_n2   <- lmer(N2_umolL_avg ~ NO3_s + flow_state_n + (1 | siteID), data = GASS_ENVI_GP)
mod_o2   <- lmer(O2_umolL_avg ~ DOC_s + flow_state_n + (1 | siteID), data = GASS_ENVI_GP)
mod_ratio <- lmer(CH4.CO2 ~ O2_s + MGCA_s + (1 | month), data = GASS_ENVI_GP)

plot_effect <- function(model, term, response_label, x_label) {
  eff <- ggpredict(model, terms = term)
  
  p <- plot(eff) +
    labs(
      x = x_label,
      y = response_label,
      title = NULL
    ) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 8.2),
      axis.text  = element_text(size = 7),
      plot.title = element_blank(),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    scale_color_manual(values = "#1f78b4") +
    scale_fill_manual(values = "#a6cee3")
  
  # 🔥 FIX FLOW STATE LABELS HERE
  if (term == "flow_state_n") {
    p <- p + scale_x_discrete(
      limits = c("1", "2"),
      labels = c("1" = "Flowing", "2" = "Pooling")
    )
  }
  
  return(p)
}

# CO2
p_co2_o2 <- plot_effect(
  mod_co2, "O2_s",
  expression(CO[2]~"(µmol L"^{-1}*")"),
  expression(O[2]~"(µmol L"^{-1}*")")
)

p_co2_mgca <- plot_effect(
  mod_co2, "MGCA_s",
  expression(CO[2]~"(µmol L"^{-1}*")"),
  expression(Mg:Ca)
)

# N2O
p_n2o_o2 <- plot_effect(
  mod_n2o, "O2_s",
  expression(N[2]*O~"(µmol L"^{-1}*")"),
  expression(O[2]~"(µmol L"^{-1}*")")
)

p_n2o_mgca <- plot_effect(
  mod_n2o, "MGCA_s",
  expression(N[2]*O~"(µmol L"^{-1}*")"),
  expression(Mg:Ca)
)

# CH4
p_ch4_o2 <- plot_effect(
  mod_ch4, "O2_s",
  expression(CH[4]~"(µmol L"^{-1}*")"),
  expression(O[2]~"(µmol L"^{-1}*")")
)

p_ch4_mgca <- plot_effect(
  mod_ch4, "MGCA_s",
  expression(CH[4]~"(µmol L"^{-1}*")"),
  expression(Mg:Ca)
)

# N2
p_n2_no3 <- plot_effect(
  mod_n2, "NO3_s",
  expression(N[2]~"(µmol L"^{-1}*")"),
  expression(NO[3]~"(mg L"^{-1}*")")
)

p_n2_flow <- plot_effect(
  mod_n2, "flow_state_n",
  expression(N[2]~"(µmol L"^{-1}*")"),
  expression(Flow~state)
)

# O2
p_o2_doc <- plot_effect(
  mod_o2, "DOC_s",
  expression(O[2]~"(µmol L"^{-1}*")"),
  expression(DOC~"(mg L"^{-1}*")")
)

p_o2_flow <- plot_effect(
  mod_o2, "flow_state_n",
  expression(O[2]~"(µmol L"^{-1}*")"),
  expression(Flow~state)
)

# CH4:CO2 ratio
p_ratio_o2 <- plot_effect(
  mod_ratio, "O2_s",
  expression(CH[4]:CO[2]~"(mol:mol)"),
  expression(O[2]~"(µmol L"^{-1}*")")
)

p_ratio_mgca <- plot_effect(
  mod_ratio, "MGCA_s",
  expression(CH[4]:CO[2]~"(mol:mol)"),
  expression(Mg:Ca)
)

final_plot <- wrap_plots(
  p_co2_o2, p_co2_mgca,
  p_n2o_o2, p_n2o_mgca,
  p_ch4_o2, p_ch4_mgca,
  p_n2_no3, p_n2_flow,
  p_o2_doc, p_o2_flow,
  p_ratio_o2, p_ratio_mgca,
  ncol = 2
)

final_plot
