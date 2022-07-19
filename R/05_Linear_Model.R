# Loading Packages #
library(ggplot2)

# Reading Data #
cat <- read.csv("data/raw/crawley_regression.csv")

# Plot the Growth #
boxplot(cat$growth, col = "darkgreen")
unique(cat$tannin)

# Creating the Linar Model #
mod_cat <- lm(growth ~ tannin, data = cat)

# Summary #

summary(mod_cat)

## Adjust the model to data ##
plot(growth ~ tannin, data = cat, bty = 'l', pch = 19)
abline(mod_cat, col = "red", lwd = 2)

# Fitting the Model #
ggplot(data = cat, mapping = aes(x = tannin, y = growth)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic()

## Anova Model ##
summary.aov(mod_cat)

## Obtain Predicted Values ##
predict(mod_cat)

## Fitted vs Observed Values ##

cat$fitted <- predict(mod_cat)

ggplot(data = cat) +
  geom_point(aes(x = growth, y = fitted)) +
  geom_abline(aes(slope = 1,  intercept = 0)) +
  theme_classic()

## Model Diagnostics ##
par(mfrow = c(2, 2))
plot(mod_cat)
par(mfrow = c(1, 1))


## Fitting Distributions to Data ##

# Load Packages #
library(fitdistrplus)

# Load Data #
data("groundbeef")

# Plot Density and Cumulative #
plotdist(groundbeef$serving, histo = TRUE, demp = TRUE)
par(mfrow = c(1, 1))

# Plot the Culen and Frey graph #
descdist(groundbeef$serving, boot = 1000)

# Fit the data with maximum likelihood #
fw <- fitdist(groundbeef$serving, "weibull") # WeiBull #
summary(fw)


# Compare with others distributions #
fg <- fitdist(groundbeef$serving, "gamma") # Gamma #
fln <- fitdist(groundbeef$serving, "lnorm") #LogNormal #

# Plot the Results #
par(mfrow = c(2, 2))
plot_legend <- c("Weibull", "lognormal", "gamma")

# Histogram Plot #
denscomp(list(fw, fln, fg), legendtext = plot_legend)
# Q-Q Plot #
qqcomp(list(fw, fln, fg), legendtext = plot_legend)
# CDF Empirical and Theoretical #
cdfcomp(list(fw, fln, fg), legendtext = plot_legend)
# P-P Plot #
ppcomp(list(fw, fln, fg), legendtext = plot_legend)

# Summary of Results #
gofstat(list(fw, fln, fg))
