# Professional Skills R Session: Model selection, 19 Nov 
# From the second worksheet: Mixed models 

# Introductory things, loading packages and datasets ----

library(tidyverse)

soils <- read_csv("02-multiple-predictors/peru_soil_data.csv")
inga <- read_csv("03-glm/inga_abundances.csv")

combo <- full_join(soils, inga, by = "Survey")

View(combo)

