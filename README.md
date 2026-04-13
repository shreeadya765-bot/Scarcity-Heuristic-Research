# Scarcity-Heuristic-Research
A quantitative study exploring the 'Debt-Stress' mental health tax on women entrepreneurs in informal credit markets (1780–present). Utilizing R (fixest), this research models the Scarcity Heuristic’s impact on cognitive load and economic decision-making, framing financial volatility as a critical psychological and health policy intervention.
# Title: Modeling the "Debt-Stress" Tax in Informal Credit Markets
# Focus: Scarcity Heuristic, Cognitive Load, and Financial Volatility
# Author: [Your Name]
# Methodology: Fixed Effects (fixest) for High-Dimensional Data

# 1. Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(fixest, tidyverse, modelsummary, sandwich)

# 2. Data Simulation (Representing Historical & Modern Entrepreneurial Data)
set.seed(42)
n_individuals <- 200
n_years <- 10

data <- expand.grid(id = 1:n_individuals, year = 1800:1809) %>%
  mutate(
    # Independent Variable: Cash flow volatility (The Stressor)
    volatility = rnorm(n(), mean = 20, sd = 10),
    # Dependent Variable: Cognitive Load (The Tax)
    # Influenced by volatility + individual fixed effects + random noise
    cognitive_load = 0.45 * volatility + rnorm(n(), mean = 5, sd = 2),
    debt_ratio = runif(n(), 0.1, 0.8),
    market_type = sample(c("Barter", "Micro-credit"), n(), replace = TRUE)
  )

# 3. The Model: Fixed Effects OLS (feols)
# Formula: Y ~ X | Fixed Effects (Individual + Year)
# This isolates the "Debt-Stress" tax from time-invariant individual traits.

debt_stress_model <- feols(
  cognitive_load ~ volatility + debt_ratio | id + year, 
  data = data,
  cluster = ~id
)

# 4. Summary of Results
summary(debt_stress_model)

# 5. Exporting for Research Documentation
# This creates a clean table suitable for a research paper appendix
modelsummary(debt_stress_model, 
             title = "The Impact of Financial Volatility on Cognitive Load",
             stars = TRUE)
