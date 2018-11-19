# Professional Skills R Session: Model selection, 12 Nov 
# From the second worksheet: Model Fit 

# Introductory things ---- 

library(tidyverse)

# Exercise 1: Comparing AIC values of diff linear models ---- 

soils <- read_csv("02-multiple-predictors/peru_soil_data.csv")

View(soils)

## Creating linear models

lm_pH_habitat <- lm(Soil_pH ~ Habitat, data = soils)
lm_pH_tbs <- lm(Soil_pH ~ Total_Base_Saturation, data = soils)
lm_pH_habitat_tbs <- lm(Soil_pH ~ Habitat + Total_Base_Saturation, data = soils)
lm_pH_habitat_tbs_interaction <- lm(Soil_pH ~ Habitat * Total_Base_Saturation, data = soils)

## Compare AIC values 

AIC(lm_pH_habitat, lm_pH_tbs, lm_pH_habitat_tbs, lm_pH_habitat_tbs_interaction)

# AIC of lm_pH_habitat_tbs is the lowest, meaning that it is the best model fit! 

