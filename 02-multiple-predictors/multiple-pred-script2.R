# Professional Skills R Session: Model selection, 12 Nov 
# From the second worksheet: model fit

# Introductory things ---- 

library(dplyr)

soils <- read_csv("02-multiple-predictors/peru_soil_data.csv")

# Comparing AIC values of diff linear models ---- 

## Creating linear models

lm_pH_habitat <- lm(Soil_pH ~ Habitat, data = soils)
lm_pH_tbs <- lm(Soil_pH ~ Total_Base_Saturation, data = soils)
lm_pH_habitat_tbs <- lm(Soil_pH ~ Habitat + Total_Base_Saturation, data = soils)
lm_pH_habitat_tbs_interaction <- lm(Soil_pH ~ Habitat * Total_Base_Saturation, data = soils)

## Compare AIC values 

AIC(lm_pH_habitat, lm_pH_tbs, lm_pH_habitat_tbs, lm_pH_habitat_tbs_interaction)

# AIC of lm_pH_habitat_tbs is the lowest, meaning that it is the best model fit! 
