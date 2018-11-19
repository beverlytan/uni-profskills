
# Beverly Tan 
# Testing the difference between predict(), fitted() and ggpredict() 
# Under different types of models

# Introductory ---- 

library(tidyverse)
library(ggeffects)

soils <- read_csv("02-multiple-predictors/peru_soil_data.csv")
inga <- read_csv("03-glm/inga_abundances.csv")
combo <- full_join(soils, inga, by = "Survey")

# Poisson model, 2 explanatory fixed effects, no interaction ---- 

mod1 <- glm(thibaudiana ~ Habitat + Soil_pH, data = combo, family = poisson)
summary(mod1)
as.data.frame(fitted(mod1))
as.data.frame(predict(mod1))
ggpredict(mod1, terms = "Habitat")
ggpredict(mod1, terms = "Soil_pH")

# Poisson model, 2 explanatory fixed effects, with interaction ---- 

mod2 <- lm(thibaudiana ~ Habitat * Soil_pH, data = combo)
summary(mod2)
as.data.frame(fitted(mod2))
as.data.frame(predict(mod2))
ggpredict(mod2, terms = "Soil_pH")




