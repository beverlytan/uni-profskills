## Diagnostic plots

Helpful link: https://data.library.virginia.edu/diagnostic-plots/

### OTHER NOTES

- Histograms: both of the residuals and the data points 

- General vs generalized linear  models 
  - Generalized linear models distribution could be poisson, or any other type 
    - i.e. just not normal distribution 
  - General linear models = normal distribution
  - In R, generalized linear models are specified by glm(), you can specify which distribution to follow. If you don't specify, the glm will just run like an lm i.e. assumes that the distribution is normal 

### QQ PLOTS

- QQ tells you goodness of fit: how well your data fits the model 
  - Points are the raw data, you want them to be perfectly aligned with the straight line 
  
- Highlights any major outliers
  - But note that generally in ecological data, they will deviate at the two tails cuz theres less information being fed

### RESIDUALS - FITTED

- If its straight it means its normal! 

### SCALE - LOCATION 

If its a straight line with points that are both above and below it, and the same number of points above and below, and equal distances = then you do have equal variances between the categories 

Another way to test the same thing (e.g. pH~habitat), make two box plots of pH in habitat A vs pH in habitat B, we want the extent of the two boxplots (the variances, i.e. the line) to be the same between the two boxplots. The mean could be different, and the frequency of records could be different = but EQUAL variances 

### RESIDUALS - LEVERAGE

- Aiming for a straight line again 

- Shows you the ratio of what the residual is for a given data point to how  much it influences the model (the leverage) 
  - E.g. if you have points with pH 2.5, 2.4 and 7 
    - The point with pH 7 has very high leverage, it will skew that model fit as the model tries to capture that point! 