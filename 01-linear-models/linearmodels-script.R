# Professional Skills R Session: Linear models, 5 Nov 

# Introductory things ---- 

# Load library 

library(dplyr)
library(ggplot2)

# Loading data 

soils <- read.csv("01-linear-models/peru_soil_data.csv")

# Quick check of the data 

dim(soils)
names(soils)
head(soils)
summary(soils)

# Running ANOVA: pH varying with habitat ---- 

lm_pH <- lm(Soil_pH ~ Habitat, data = soils)

## Testing assumptions 

# (1) Test with shapiro normality / bartlett homoscedasticity test 
# Recall that we want the p-values for the tests to be greater than 0.05
# That means that it is NOT sig diff from being normally distributed i.e. normal
# And NOT sig diff from having different variances i.e. equal 
# (2) Or you can test with plot(lm_pH) to see the 4 types of plots
# note that plot(lm_pH) shows you the 4 plot, while plot(Soil_pH ~ Cont_var, data = soils)
# shows you just the plot of soil pH against another continuous variable, refer below
# (3) Or we can manually plot histograms with hist() and
# box plots with boxplots() or plot() to check as well 

lm_pH_resids <- resid(lm_pH)
shapiro.test(lm_pH_resids)                         # p-value = 0.06178
bartlett.test(Soil_pH ~ Habitat, data = soils)     # p-value = 0.9735

## Since assumptions are met, run the ANOVA 

anova(lm_pH)                                       # p-value = 0.0002474

# Running ANOVA: soil potassium varying with habitat ---- 

lm_K <- lm(Potassium ~ Habitat, data = soils)

## Testing assumptions 

lm_K_resids <- resid(lm_K)
shapiro.test(lm_K_resids)                          # p-value = 0.7301
bartlett.test(Potassium ~ Habitat, data = soils)   # p-value = 0.004101

# Here, the p-value for bartlett < 0.05 meaning variances are different! 
# Which is problematic because we want equal variances, so we must transform

## Transforming the data 

soils$logK <- log(soils$Potassium)                 # Creates new column of logK

lm_logK <- lm(logK ~ Habitat, data = soils)        # New linear model

## Testing new assumptions 

lm_logK_resids <- resid(lm_logK)
shapiro.test(lm_logK_resids)                       # p-value = 0.1231
bartlett.test(logK ~ Habitat, data = soils)        # p-value = 0.3472

## Assumptions are met and now you can run the ANOVA

anova(lm_logK)                                     # p-value = 9.956e-05










# Linear regression: pH with amount of clay ---- 

lm_pH_clay <- lm(Soil_pH ~ Clay, data = soils)

## Visualizing the data, examine the relationships before running model

# With base R 
plot(Soil_pH ~ Clay, data = soils)
text(Soil_pH ~ Clay, data = soils, labels = soils$Site)
abline(lm_pH_clay, col = "red")

# With ggplot 
(ggplot(soils, aes(x = Clay, y = Soil_pH)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  theme_bw())

# Side note, if we're going to do pH comparisons with amount of clay / sand / silt
# instead of specifying and running each pair, we can use pairs(soils[,c(5,18,19,20)])  
# which will plot ALL the pairwise relationships at once

## Testing normality assumption with Shapiro test 

# For linear regression (e.g. between pH and clay content) - CONTINUOUS VARIABLES
# when you're testing for equal variances, we're testing for whether the variance 
# between pH & its mean //// IS DIFFERENT FROM //// claycontent & its mean 

# But for anovas (e.g. pH between habitat types) - CATEGORICAL VARIABLES
# when you're testing for equal variances, we're testing for whether the variance 
# between pH in habitat A //// IS DIFFERENT FROM //// pH in habitat B

lm_pH_clay_resids <- resid(lm_pH_clay)
shapiro.test(lm_pH_clay_resids)                    # p-value = 0.2897

## Testing homoscedasticity / heteroscedasticity assumptions with NCV test 

# For linear regression (e.g. between pH and clay content) - CONTINUOUS VARIABLES
# when you're testing for equal variances, we're testing for whether the variance 
# between pH & its mean //// IS DIFFERENT FROM //// claycontent & its mean 

# But for anovas (e.g. pH between habitat types) - CATEGORICAL VARIABLES
# when you're testing for equal variances, we're testing for whether the variance 
# between pH in habitat A //// IS DIFFERENT FROM //// pH in habitat B

# So therefore bartlett.test will not work for linear regressions
# But you can use car::ncvTest(lm_whatever) to test for hetero/homo!

car::ncvTest(lm_pH_clay)                           # p-value = 0.59723

## Assumptions are met and now you can run the linear regression 

anova(lm_pH_clay)                                  # p-value = 0.05227
summary(lm_pH_clay)                                # This is the same! 

## Testing assumptions with plot(lm object) to see the 4 types of plots 

plot(lm_pH_clay) 

# Looking at the last plot of residuals vs leverage, we can see that the last point
# is so far away from the other plots, signifying that it has alot of influence
# on the relationship, i.e. high leverage - which is problematic 
# so we will have to remove that point that is causing issues!

## Removing the problematic point 

# With base R 
soils_new <- soils[-1,] 

# With the filter function from dplyr 
soils_new <- filter(soils, Survey != "Blanquillo_floodplain")

## Running the linear model again 

lm_pH_clay_new <- lm(Soil_pH ~ Clay, data = soils_new)
lm_pH_clay_new_resids <- resid(lm_pH_clay_new)
shapiro.test(lm_pH_clay_new_resids)                # p-value = 0.5618
car::ncvTest(lm_pH_clay_new)                       # p-value = 0.76221

## Checking the assumptions with plot(), 4 graphs: 

plot(lm_pH_clay_new)   

# Note how the points in the last graph are all closer to 0

## Plotting the graph! 

# With base R 
plot(Soil_pH ~ Clay, data = soils_new)
text(Soil_pH ~ Clay, data = soils_new, labels = soils$Site)
abline(lm_pH_clay, col = "red")

# With ggplot 
(ggplot(soils_new, aes(x = Clay, y = Soil_pH)) + 
    geom_point() + 
    geom_smooth(method = lm) + 
    theme_bw())















# Non-parametric: Kruskal-Wallis (Anova) ---- 

lm_na <- lm(Sodium ~ Habitat, data = soils)

## Testing assumptions

lm_na_resids <- resid(lm_na)
shapiro.test(lm_na_resids)                         # p-value = 3.155e-09
bartlett.test(Sodium ~ Habitat, data = soils)      # p-value = 2.322e-10

## Transformation, and testing assumptions again

lm_na_x <- lm(log(Sodium) ~ Habitat, data = soils)

lm_na_x_resids <- resid(lm_na_x)
shapiro.test(lm_na_x_resids)                       # p-value = 2.32e-06
bartlett.test(log(Sodium) ~ Habitat, data = soils) # p-value = 0.006953

## Still doesnt fulfil the assumptions, so have to run non-parametric 

kruskal.test(Sodium ~ Habitat, data = soils)       # p-value = 0.0002655

# Non-parametric: Spearman (linear regression) ---- 

lm_sand_tbs <- lm(Total_Base_Saturation ~ Sand, data = soils)

## Testing assumptions

lm_sand_tbs_resids <- resid(lm_sand_tbs)
shapiro.test(lm_sand_tbs_resids)                   # p-value = 0.1435
car::ncvTest(lm_sand_tbs)                          # p-value = 0.42564

# In this case, the shapiro test / ncv test actually show us that the model 
# assumptions ARE met, but assuming that they are not, we can run the non-
# parametric equivalent instead! 

cor.test(soils$Sand, soils$Total_Base_Saturation,  # p-value = 0.002412
         data = soils, method = "spearman")




