# Professional Skills R Session: Model selection, 19 Nov 
# From the first worksheet: Generalized Linear Models

# Introductory things, loading packages and datasets ----

library(tidyverse)

soils <- read_csv("02-multiple-predictors/peru_soil_data.csv")
inga <- read_csv("03-glm/inga_abundances.csv")

combo <- full_join(soils, inga, by = "Survey")

View(combo)

# Exercise 2: Generalised linear models, Poisson ---- 

### MODEL 1 = How does abd of diff species change with habitat type? 

mod1 <- glm(thibaudiana ~ Habitat, data = combo, family = poisson)
summary(mod1)

# Our model coefficients should not be negative, they present themselves as
# negative now, that's because its actually log-ed! So we should 
# exponentiate that value to get the correct estiamtes

exp(-1.946)        # 0.1428443
exp(-1.946+4.518)  # 13.09198

## Evaluating our model - testing for overdispersion

# METHOD 1: residual deviance / residual df 

48.67 / 23         # 2.116087 meaning that there is model overdispersion (overfit)

# METHOD 2: comparing our model with null-model with AICs

mod_null <- glm(thibaudiana ~ 1, data = combo, family = poisson)

AIC(mod_null, mod1)   

### MODEL 2 = How does abd of diff species change with soil pH? Evaluating model.

mod2 <- glm(thibaudiana ~ Soil_pH, data = combo, family = poisson)
summary(mod2)

AIC(mod_null, mod1, mod2)   

### MODELS 3/4 = How does abd of diff species change with a) habitat and b) pH? 

mod3 <- glm(thibaudiana ~ Habitat + Soil_pH, data = combo, family = poisson)
mod4 <- glm(thibaudiana ~ Habitat * Soil_pH, data = combo, family = poisson)

AIC(mod_null, mod1, mod2, mod3, mod4)   

# We can see that mod3 (with multiple predictors) is the best! 

### SO LET US LOOK AT MODEL 3! 

summary(mod3)

## Test for overdispersion: ratio of residual deviance to residual df 

35.775 / 22     # 1.626136 meaning its better but theres still overdispersion 

## Plot predicted vs observed values to see how well they match 

plot(combo$thibaudiana, fitted(mod3), xlab="Observed", ylab="predicted")
abline(0,1)

# If the model predicted perfectly, then it should lie perfectly on the abline 

combo$Habitat <- as.factor(combo$Habitat)

## Boxplot of residuals-cat-predictor & residuals-v-cont-predictor

plot(resid(mod3) ~ Habitat, data = combo)
plot(resid(mod3) ~ Soil_pH, data = combo)

# when we look at the residuals, we're basically talking about equal variances
# so we want the extent of the variance between the two habitat to be the same
# i.e. the EXTENT of the boxplot to be the same
# So if they're both like the floodplain, tight around the mean (little variability
# around the predicted the mean), that's ok. 
# But if we have both like the upland, widely spread around the mean (high var)
# that's also ok! It's about the comparison between the two habitats! 

# In this case, this means that we're missing something: there's overdispersion
# there is something else affecting the abundance in upland, besides soil pH! 
# So we should plot out our raw data and see how our predicted values compare!

# Plottig raw data with values from fitted()

fit_val <- as.data.frame(fitted(mod3))
fit_val

combo_mod3 <- cbind(fit_val, combo)

(plot_mod3 <- ggplot(combo_mod3, aes(x = Soil_pH, y = thibaudiana)) + 
    geom_point(aes(colour = Habitat, shape = Habitat)) + 
    geom_point(aes(y = fitted(mod3)), shape = 1) + 
    theme_bw())

# Plottig raw data with values from ggpredict() = refer to further-script.R
# fit_val and ggpred_val predictions are the same!! Great. 

ggpred_val <- ggpredict(mod3, terms = c("Soil_pH", "Habitat"))
View(ggpred_val)

# Default plotting with plot() from ggpredict
plot(ggpred_val) 

# Plotting using ggplot! So this new graph is the model that has the two expl variables!
ggplot(ggpred_val, aes(x, predicted)) + 
  geom_line(aes(colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) + 
  geom_point(data = combo_mod3, aes(Soil_pH, thibaudiana, colour = Habitat)) + 
  labs(x = "Soil pH", y = "Predicted abundance of thibaudiana") +
  theme_bw() + 
  theme(panel.grid = element_blank())


# Exercise 3: Generalised linear models, Binomial response ---- 

## Basic binomial model 

auristellae_PA <- combo$auristellae

auristellae_PA[auristellae_PA>0] <- 1

mod1b <- glm(auristellae_PA ~ Soil_pH, data = combo, family = binomial)
summary(mod1b)

# Evaluate the model against null models 

mod_null_b <- glm(auristellae_PA~1,data=combo,family=binomial)
AIC(mod_null_b, mod1b)

## Binomial models with other variables 

mod2b <- glm(auristellae_PA ~ Habitat, data = combo, family = binomial)
mod3b <- glm(auristellae_PA ~ Soil_pH + Habitat, data = combo, family = binomial)
mod4b <- glm(auristellae_PA ~ Soil_pH * Habitat, data = combo, family = binomial)

AIC(mod_null_b, mod1b, mod2b, mod3b, mod4b)

# We can see that the AICs are all quite similar, so lets just go with the 
# one that is the lowest i.e. mod1b the soil pH one! 

# Visualizing model 1b with fitted() 
# mod1b <- glm(auristellae_PA ~ Soil_pH, data = combo, family = binomial)

fit_val_1b <- as.data.frame(fitted(mod1b))
View(fit_val_1b)

combo_mod1b <- cbind(fit_val_1b, combo)
View(combo_mod1b)

(plot_mod1b <- ggplot(combo_mod1b, aes(x = Soil_pH, y = auristellae_PA)) + 
    geom_point() + 
    geom_point(aes(y = fitted(mod1b)), shape = 1) + 
    theme_bw())

# Visualizing model 1b with ggpredict(), ggpred_val_1b has same preds as fit_val_1b

ggpred_val_1b <- ggpredict(mod1b, terms = "Soil_pH")
View(ggpred_val_1b)

# Default plotting with plot() from ggpredict
plot(ggpred_val_1b) 

# Plotting using ggplot! So this new graph is the model that has the two expl variables!
ggplot(ggpred_val_1b, aes(x, predicted)) + 
  geom_line() +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), alpha = 0.2) + 
  geom_point(data = combo_mod1b, aes(Soil_pH, auristellae_PA)) + 
  labs(x = "Soil pH", y = "Predicted probabilities of Auristellae") +
  theme_bw() + 
  theme(panel.grid = element_blank())


