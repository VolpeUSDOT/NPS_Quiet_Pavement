# Manual check calculations of predvals

# To demonstrate how we calculate predicted values using the coefficients.

rootdir <- '//vntscex.local/DFS/Projects/PROJ-VXP700/_J1J3_Death Valley Quieter Pavement/Analysis/For Dan/Passby_Model_Output'
setwd(rootdir)

# Read in coefs and predvals
coefs <- read.csv(file.path(rootdir, 'LZFeq_ANOVA_Coefficients.csv'))
predvals <- read.csv(file.path(rootdir, 'LZFeq_ANOVA_PredictedVals.csv'))

# Calculate baseline predvals at standard speed and pavement temperature

sp = 50
pt = 90

pred_baseline = with(coefs,
                     Baseline + speed * sp + pavetemp * pt + speed.pavetemp * sp * pt)

plot(pred_baseline, predvals$baseline_chipseal,
     pch = 16, col = 'midnightblue',
     xlab = 'Manual calculation of predicted SIL',
     ylab = '`predict()` calculation of SIL',
     main = 'Baseline sound intensity levels \n at constant speed and pavement temperature')
abline(a = 0, b = 1, lty = 2)

# Will return TRUE if these two vectors are identical
summary(lm(pred_baseline ~ predvals$baseline_chipseal))$r.squared == 1


# Calculate one of the treatment predvals at the same speed and temperature

pred_TypeII = with(coefs,
                     Baseline + treatmentType.II.Microsurfacing +
                     speed * sp + pavetemp * pt + speed.pavetemp * sp * pt +
                     treatmentType.II.Microsurfacing.speed * sp)

plot(pred_TypeII, predvals$Type.II.Microsurfacing,
     pch = 16, col = 'midnightblue',
     xlab = 'Manual calculation of predicted SIL',
     ylab = '`predict()` calculation of SIL',
     main = 'Type II Microsurfacing sound intensity levels \n at constant speed and pavement temperature')
abline(a = 0, b = 1, lty = 2)

summary(lm(pred_TypeII ~ predvals$Type.II.Microsurfacing))$r.squared == 1



# 2019 baseline calcs ---- 
coefs <- read.csv(file.path(rootdir, 'LZFeq_ANOVA_Coefficients_2019.csv'))
predvals <- read.csv(file.path(rootdir, 'LZFeq_ANOVA_PredictedVals_2019.csv'))

pred_baseline_2019 = with(coefs,
                     Baseline + speed * sp + pavetemp * pt + speed.pavetemp * sp * pt)

plot(pred_baseline_2019, predvals$baseline_chipseal,
     pch = 16, col = 'midnightblue',
     xlab = 'Manual calculation of predicted SIL',
     ylab = '`predict()` calculation of SIL',
     main = '2019 Baseline sound intensity levels \n at constant speed and pavement temperature')
abline(a = 0, b = 1, lty = 2)

# Will return TRUE if these two vectors are identical
summary(lm(pred_baseline ~ predvals$baseline_chipseal))$r.squared == 1

# 2019 vs 2018 calcs of baseline compared


plot(pred_baseline_2019, pred_baseline,
     pch = 16, col = 'midnightblue',
     xlab = 'Baseline in 2016 predicted when compared to 2019',
     ylab = 'Baseline in 2016 predicted when compared to 2018',
     main = 'Baseline sound intensity levels \n at constant speed and pavement temperature')
abline(a = 0, b = 1, lty = 2)

r2 = summary(lm(pred_baseline ~ predvals$baseline_chipseal))$r.squared
legend('topleft',
       legend = paste('R2 =', round(r2, 4)))
