# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Script to fit linear model in R
# First version 2022-07-18
# --------------------------------------------------#

# loading packages
library(ggplot2)

# reading data
cat <- read.csv("data/raw/crawley_regression.csv")

# Do leaf chemical compounds affect the growth of caterpillars? ----------------

# the response variable
boxplot(cat$growth, col = "darkgreen")

# the predictor variable
unique(cat$tannin)

# creating the lm
mod_cat <- lm(growth ~ tannin, data = cat)

summary(mod_cat) # Residual standard error: 1.693 on 7 degrees of freedom


## ----lm-plot------------------------------------------------------------------
plot(growth ~ tannin, data = cat, bty = 'l', pch = 19) # bty argument is a character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box.
abline(mod_cat, col = "red", lwd = 2)


## ----lm-ggplot----------------------------------------------------------------
ggplot(data = cat, mapping = aes(x = tannin, y = growth)) +
  geom_point() +
  geom_smooth(method = lm) + #add argument se=F to get rid of confidence intervals.
  theme_classic()
# We can't get the parameters from the linear model from this. Do not build your lms in ggplot!

## AOV table
summary.aov(mod_cat)


## fitted values
predict(mod_cat)
cat$fitted <- predict(mod_cat) #add the fitted values as a collumn to cat

# Comparing fitted vs. observed values
ggplot(data = cat) +
  geom_point(aes(x = growth, y = fitted)) +
  geom_abline(aes(slope = 1,  intercept = 0)) +
  theme_classic()


## Model diagnostics -----------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_cat) # creates 4 plots for model diagnosis: Residuals x Fitted, checking if residuals are close to our fitted values, expected horizontal curve, used to see homogeneity of variance.
#          Normal Q-Q, shows if the residuals form a normal distribution. expected; residuals close to dashed line
#          Scale-Location, shows us how residuals variance is distributed. expected horizontal line. In this example the line goes down, which tells us that this is probably not a good model.
#          Residuals vs Leverage, tells us Cook's Distance, used estimate of the influence of a data point when performing a least-squares regression analysis.[1] In a practical ordinary least squares analysis, Cook's distance can be used in several ways: to indicate influential data points that are particularly worth checking for validity; or to indicate regions of the design space where it would be good to be able to obtain more data points . Expected: all values to be between -2<x<2
par(mfrow = c(1, 1))


# Comparing statistical distributions ------------------------------------------
library(fitdistrplus) # to find which distribution best fit the data. The examples here are extracted from Delignette-Muller & Dutang 2015.

data("groundbeef")
?groundbeef # Serving sizes collected in a French survey, for ground beef patties consumed by children under 5 years old. groundbeef is a data frame with 1 column (serving: serving sizes in grams)
str(groundbeef)


plotdist(groundbeef$serving, histo = TRUE, demp = TRUE) # Plots an empirical distribution (non-censored data) with a theoretical one if specified.


# Confronting different distributions

# First, let’s describe the distribution using descriptive statistics such as median, mean, standard deviation, skewness (asymmetry) and kurtosis (“taildness”).

descdist(groundbeef$serving, boot = 1000) # descdist Computes descriptive parameters of an empirical distribution for non-censored data and provides a skewness-kurtosis plot.

# Checking if the Weibull distribution fits to data
fw <- fitdist(groundbeef$serving, "weibull") # Fit of univariate distributions to non-censored data by maximum likelihood (mle), moment matching (mme), quantile matching (qme) or maximizing goodness-of-fit estimation (mge). Weibull distribution is a continuous probability distribution.

summary(fw) # look at the likelihood of your data beint fit to the distribution assigned in last line fitdist()

fg <- fitdist(groundbeef$serving, "gamma") # data, distribution
fln <- fitdist(groundbeef$serving, "lnorm") # data, distribution

par(mfrow = c(2, 2))
plot_legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot_legend) # denscomp: histogram of the data with theorerical densities of different distributions from fw fln and fg
qqcomp(list(fw, fln, fg), legendtext = plot_legend) # qq plot - see line 58
cdfcomp(list(fw, fln, fg), legendtext = plot_legend) # empirical and theoretical Cummulative Density Functions, theoreticals come from fw, fln and fg
ppcomp(list(fw, fln, fg), legendtext = plot_legend) # P-P plot: theoretical probabilities X empirical probabilites

# Apparently the three empirical distributions fit very well to the data. What then?


gofstat(list(fw, fln, fg)) # Computes goodness-of-fit statistics for parametric distributions fitted to a same non-censored data set. Arguments: (fitdist object or list of fitdist object,several other parameters see help)

# The lowest AIC or BIC values from gofstat() are the best ones, and that's the distribution that's closest to your data.
