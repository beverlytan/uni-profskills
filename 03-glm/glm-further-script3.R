
# Beverly Tan 
# Testing the difference between predict(), fitted() and ggpredict() 
# Under different types of models

# Introductory ---- 

library(tidyverse)
library(ggeffects)
data(efc)
View(efc)
head(efc)

soils <- read_csv("02-multiple-predictors/peru_soil_data.csv")
inga <- read_csv("03-glm/inga_abundances.csv")
combo <- full_join(soils, inga, by = "Survey")

# TESTING ONE Poisson model, 2 explanatory fixed effects, no interaction ---- 

mod1 <- glm(thibaudiana ~ Habitat + Soil_pH, data = combo, family = poisson)
summary(mod1)

## USING GGPREDICT()

gg_preds <- ggpredict(mod1, terms = c("Soil_pH", "Habitat"))
View(gg_preds)

# Note here that the first term is grouped by the levels of the second
# and if you have a third, and the third
# meaning the second and the third are GROUPS! Third = facet in the table
# This is useful https://strengejacke.github.io/ggeffects/reference/ggpredict.html 

## USING FITTED()

fit_preds <- as.data.frame(fitted(mod1))
combo_mod1 <- cbind(fit_preds, combo)
View(combo_mod1)

# If I use fitted(mod1) and merge that to my original dataframe 
# I realise that it's actually the same!! O M G fit_preds = gg_preds!!!
# What I was doing BEFORE THIS was ggpredict(mod1, terms = "Soil_pH")
# or ggpredict(mod1, terms = "Habitat") and that just looks at 
# ONE explanatory variable only!!

## ASIDE: DIFFERENCE BT FITTED() AND PREDICT()

as.data.frame(predict(mod1))
exp(-1.710389) # 0.1807954 Shows that if you exp(predict_values) = fitted_values

# TESTING TWO Poisson model, 2 explanatory fixed effects, with interaction ---- 

mod2 <- lm(thibaudiana ~ Habitat * Soil_pH, data = combo)
summary(mod2)
as.data.frame(fitted(mod2))
as.data.frame(predict(mod2))
ggpredict(mod2, terms = "Soil_pH")




