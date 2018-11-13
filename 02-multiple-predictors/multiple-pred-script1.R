# Professional Skills R Session: Multiple predictors, 12 Nov 
# From last week's linear model workbook, exercise 6.3 and 6.4

# Introductory things ---- 

# Load library 

library(tidyverse)

# Loading data 

soils <- read_csv("02-multiple-predictors/peru_soil_data.csv")

View(soils)

head(soils)

# (6.3) Multiple explanatory variables (1 cat, 1 cont - Hab type, TBS) ---- 

## With no interaction

lm_pH_habitat_tbs <- lm(Soil_pH ~ Habitat + Total_Base_Saturation, data = soils)
plot(lm_pH_habitat_tbs)

# Scale-location is poor (heteroscedastic)
# Residuals-leverage is also poor (points with high leverage)

## With interaction

lm_pH_habitat_tbs_interaction <- lm(Soil_pH ~ Habitat * Total_Base_Saturation, data = soils)
plot(lm_pH_habitat_tbs_interaction)

# Scale-location is still poor (heteroscedastic) 
# Residuals-leverage is also still poor (points with high leverage)
# Model is slightly better, but the interaction term is not actually significant 
# So we might want to just remove it! 

## Checking model fit, by using the R-squared
 
# The higher the R2 value, the more variation the model is explaining 

lm_pH_habitat <- lm(Soil_pH ~ Habitat, data = soils)
lm_pH_tbs <- lm(Soil_pH ~ Total_Base_Saturation, data = soils)

summary(lm_pH_habitat)                    # R2 = 0.4251 
summary(lm_pH_tbs)                        # R2 = 0.895 
summary(lm_pH_habitat_tbs)                # R2 = 0.9217 
summary(lm_pH_habitat_tbs_interaction)    # R2 = 0.922 

# We can see that the 3rd model has high R2, explains most of the variation
# And the interaction term is only marginally increasing the R2, unnecessary! 

# (6.3) Multiple explanatory variables (2 cat - River basin, Hab type) ----

## Preparing the data for analysis

# The colon is telling me the intersection between the "river_basin" and "habitat"
# and summary is telling R to count the number of data points! 

summary(as.factor(soils$River_Basin):as.factor(soils$Habitat))

# There are some things that we need to exclude from the dataset
# Madre de Deios river basin has only 1 habitat type 
# Tambopata and Las Piedras has only 1 data point in both habitat types 

## Preparing the data for analysis: only including relevant data 

soils_trim <- soils %>%
  filter(River_Basin == 'Manu' | River_Basin == 'Los_Amigos')

## Creating some models studying phosphorous and comparing model fit 

lm_p_habitat <- lm(Phosphorus ~ Habitat, data = soils_trim)
lm_p_basin <- lm(Phosphorus ~ River_Basin, data = soils_trim)
lm_p_habitat_basin <- lm(Phosphorus ~ Habitat + River_Basin, data = soils_trim)
lm_p_habitat_basin_int <- lm(Phosphorus ~ Habitat * River_Basin, data = soils_trim)

summary(lm_p_habitat)           # R2 = 0.1740 (Habitat sig)
summary(lm_p_basin)             # R2 = 0.2008 (Basin sig)
summary(lm_p_habitat_basin)     # R2 = 0.3758 (Habitat, Basin sig)
summary(lm_p_habitat_basin_int) # R2 = 0.5016 (Basin, interaction sig)

## Checking model assumptions for the interaction one 

plot(lm_p_habitat_basin_int)

lm_p_habitat_basin_int_resids <- resid(lm_p_habitat_basin_int)
shapiro.test(lm_p_habitat_basin_int_resids)                                      # p = 0.0007494
bartlett.test(Phosphorus ~ interaction(Habitat, River_Basin), data = soils_trim) # p = 8.961e-08

# Problems with assumptions - try transformations 

lm_p_h_b_int_x <- lm(sqrt(Phosphorus) ~ Habitat * River_Basin, data = soils_trim)

plot(lm_p_h_b_int_x)   # Arguably, better than above! 

summary(lm_p_h_b_int_x)
