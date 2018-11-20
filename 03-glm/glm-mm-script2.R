# Professional Skills R Session: Model selection, 19 Nov 
# From the second worksheet: Mixed models 

# Introductory things, loading packages and datasets ----

library(tidyverse)
library(lme4)

soils <- read_csv("02-multiple-predictors/peru_soil_data.csv")

View(combo)

# Creating and evaluating mixed effect models ----

## Creating basic models: How does P acc to habitat, controlling for river basin?

P_mod1 <- lm(log(Phosphorus) ~ Habitat, data = soils)
P_mod2 <- lm(log(Phosphorus) ~ Habitat + River_Basin, data = soils)
P_mod3 <- lm(log(Phosphorus) ~ Habitat * River_Basin, data = soils) 

## Creating mixed effect models

P_mod4 <- lmer(log(Phosphorus) ~ Habitat + (1 | River_Basin), data = soils, REML = FALSE)

summary(P_mod4)

# Compare model 4 to null model 

P_null <- lmer(log(Phosphorus) ~ 1 + (1 | River_Basin), data = soils, REML = FALSE)

AIC(P_null, P_mod4)

# Evaluating my mixed effect models with graphs (no viz of mixed model) ----

## For model 1: lm(log(Phosphorus) ~ Habitat)

fit_1 <- as.data.frame(fitted(P_mod1))
View(fit_1)
soils_fit1 <- cbind (fit_1, soils)
View(soils_fit1)

# Evaluating model 

(plot_fit_1 <- ggplot(soils_fit1, aes(x = log(Phosphorus), y = fitted(P_mod1), colour = River_Basin)) + 
    geom_point() + 
    geom_abline() +
    theme_bw())

## For model 4: lmer(log(Phosphorus) ~ Habitat + (1 | River_Basin))

fit_4 <- as.data.frame(fitted(P_mod4))
View(fit_4)
soils_fit4 <- cbind (fit_4, soils)
View(soils_fit4)

# Evaluating the model 

(plot_fit_4 <- ggplot(soils_fit4, aes(x = log(Phosphorus), y = fitted(P_mod4), colour = River_Basin)) + 
    geom_point() + 
    geom_abline() +
    theme_bw())

