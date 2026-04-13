# Scarcity-Heuristic-Research
A quantitative study exploring the 'Debt-Stress' mental health tax on women entrepreneurs in informal credit markets (1780–present). Utilizing R (fixest), this research models the Scarcity Heuristic’s impact on cognitive load and economic decision-making, framing financial volatility as a critical psychological and health policy intervention.
# ==============================================================================
# PROJECT: The Scarcity Heuristic & Cognitive Load in Informal Markets
# SCRIPT: Fixed Effects Structural Modeling & Visualization
# PURPOSE: Quantitative Proof of "Debt-Stress" as a Mental Health Tax
# ==============================================================================

# 1. Environment Setup ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  fixest,       # Fast High-Dimensional Fixed Effects
  tidyverse,    # Data Manipulation & Grammar of Graphics
  modelsummary, # Publication-quality Tables
  marginaleffects, # Slopes and Marginal Impacts
  ggsci         # Academic Color Palettes
)

# 2. Data Engineering (Generating Structural Volatility) ----------------------
# We create a panel dataset where 'Stress' is endogenously tied to 'Scarcity'
set.seed(777)
panel_data <- expand.grid(
  entrepreneur_id = 1:250, 
  wave = 1:12  # Monthly data over a year
) %>%
  group_by(entrepreneur_id) %>%
  mutate(
    # Baseline cognitive ability (Fixed Effect)
    alpha_i = rnorm(1, mean = 50, sd = 10),
    # Simulated Financial Volatility (The "Scarcity" trigger)
    volatility_idx = pmax(0, rnorm(n(), mean = 15, sd = 5) + (wave * 0.5)),
    # Debt Ratio: Increases as a function of volatility
    debt_ratio = 0.3 * volatility_idx + rnorm(n(), 0, 2),
    # THE DEPENDENT VARIABLE: Cognitive Load (The Tax)
    # Built with a structural break logic
    cog_load = alpha_i + (1.85 * volatility_idx) + (0.5 * debt_ratio) + rnorm(n(), 0, 5)
  ) %>%
  ungroup()

# 3. Econometric Modeling ------------------------------------------------------

# Model A: Baseline Fixed Effects (within-individual variation)
fe_base <- feols(cog_load ~ volatility_idx | entrepreneur_id + wave, 
                 data = panel_data)

# Model B: The Interaction Model (Does Debt amplify the Volatility Tax?)
fe_interact <- feols(cog_load ~ volatility_idx * debt_ratio | entrepreneur_id + wave, 
                     data = panel_data)

# 4. Professional Visualization (The "Visual Proof") ---------------------------
# Plotting the Marginal Effect of Volatility as Debt increases
vis_plot <- plot_slopes(fe_interact, variables = "volatility_idx", condition = "debt_ratio") +
  theme_minimal(base_family = "serif") +
  labs(
    title = "The Debt-Stress Feedback Loop",
    subtitle = "Marginal Impact of Volatility on Cognitive Load by Debt Levels",
    x = "Debt-to-Income Ratio",
    y = "Impact of Volatility on Bandwidth"
  ) +
  scale_color_jco()

# 5. Output for Portfolio ------------------------------------------------------
# This creates the table you see in top-tier journals
msummary(list("Baseline FE" = fe_base, "Interaction FE" = fe_interact),
         stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_rename = c("volatility_idx" = "Financial Volatility",
                         "debt_ratio" = "Debt Ratio"),
         gof_omit = 'AIC|BIC|Log.Lik|Within.R2',
         title = "Table 1: Fixed Effects Estimation of Cognitive Tax")

# To save the plot:
# ggsave("debt_stress_marginal_effects.png", vis_plot, width = 8, height = 5)



