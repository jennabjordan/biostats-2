#' ---
#' title: Lab 7
#' author: Jenna Jordan

#+ message = FALSE, warning = FALSE

library(mgcv)
library(nlme)
?gam()


# Example of 'Why GAM' ----------------------------------------------------

X = seq(0, 7.5, by = 0.05)
Y = jitter(sin(X), amount = 1) # compute sin(X), but add random noise
plot(x = X, y = Y)

test.mod <- lm(Y ~ X) # Construct a simple linear model
plot(x = X, y = Y) # Plot the raw data
abline(test.mod, col = 'red4', lwd = 3, lty = 2) # Plot the model fit

par(mfrow = c(2,2))
plot(m0)?????
  

# Fitting GAMs in mgcv ----------------------------------------------------

g0 <- gam(Y ~ s(X, k = 10), method = 'REML') # More on k = 10 later...
plot(x = X, y = Y) # the observed data
lines(X, g0$fitted.values, lwd = 4, col = 'grey80') # Extract and plot fitted GAM values against original predictor

par(mfrow=c(2,2))
gam.check(g0, col = "orange3") #gam check to check for normality

# Run models for small k
g.k3 <- gam(Y ~ s(X, k = 3), method = 'REML')
g.k4 <- gam(Y ~ s(X, k = 4), method = 'REML')
g.k5 <- gam(Y ~ s(X, k = 5), method = 'REML')
g.k25 <- gam(Y ~ s(X, k = 25), method = 'REML') # kinda large k
g.k50 <- gam(Y ~ s(X, k = 50), method = 'REML') # very large k

# Plot data and our original GAM
plot(x = X, y = Y)
lines(X, g0$fitted.values, lwd = 3, col = 'gray50')

# Plot the different-k GAMs
lines(X, g.k3$fitted.values, lwd = 3, col = 'yellow', lty = 2)
lines(X, g.k4$fitted.values, lwd = 3, col = 'gold', lty = 2)
lines(X, g.k5$fitted.values, lwd = 3, col = 'orange2', lty = 2)
lines(X, g.k25$fitted.values, lwd = 3, col = 'firebrick', lty = 2)

# Plot how quickly optimal "k" is reached vs. different starting values
k.set <- c(3, 4, 5, 10, 25, 50)
k.optimal <- c(
  g.k3$df.null-g.k3$df.resid, 
  g.k4$df.null-g.k4$df.resid, 
  g.k5$df.null-g.k5$df.resid,
  g0$df.null-g0$df.resid,
  g.k25$df.null-g.k25$df.resid,
  g.k50$df.null-g.k50$df.resid
)

plot(k.set, k.optimal, type = "b", col = "orange3", pch = 16)


# GAMs vs. GLMs with polynomials ------------------------------------------

# Polynomial terms are specified with the poly() argument the following way:
g.poly2 <- glm(Y ~ poly(X, degree = 2, raw = T), family = gaussian()) # the degree is the basis expansion
g.poly3 <- glm(Y ~ poly(X, degree = 3, raw = T), family = gaussian())
g.poly5 <- glm(Y ~ poly(X, degree = 5, raw = T), family = gaussian())

# Plot data and original GAM
plot(x = X, y = Y)
lines(X, g0$fitted.values, lwd = 3, col = 'gray50')

# Plot the polynomial GLMs
lines(X, g.poly2$fitted.values, lwd = 3, col = 'blue', lty = 2)
lines(X, g.poly3$fitted.values, lwd = 3, col = 'navyblue', lty = 2)
lines(X, g.poly5$fitted.values, lwd = 3, col = 'purple3', lty = 2)
#notice that even the best polynomial curve can have poor fit at the ends of the data range, which should be evident above.


# THIS WEEK'S ASSIGNMENT --------------------------------------------------

beav <- read.csv("/Users/jennajordan/Documents/GitHub/biostats-2/data/Lab 7/beaver1_day346.csv")
head(beav)

plot(beav$time, beav$temp, pch = 15, col = "turquoise") # quick look at data distribution

#1. Construct GAM relating beaver body temperature to time of day (continuous variable)

g1 <- gam(temp ~ s(time, k = 10), method = 'REML', data = beav)
summary(g1)

capture.output(summary(g1), file = "Jordan_lab7_table1.txt")

plot(x = beav$time, y = beav$temp)
lines(beav$time, g1$fitted.values, lwd = 5, col = 'black')

#2. Plot the GAM fit and SE, with empirical data points

#pred <- predict(g1)
#pred

pred.se <- predict(g1, se.fit = TRUE)
pred.se

length(pred)
length(pred.se$se.fit)
length(beav$time)

plot(x = beav$time, y = beav$temp, xlab = "Time", ylab = "Beaver Body Temperature", main = "GAM and SE Plot")
  lines(beav$time, g1$fitted.values, lwd = 2, col = 'royalblue')
  lines(beav$time, pred.se$fit - pred.se$se.fit, lty = 2, lwd = 3, col = "royalblue")
  lines(beav$time, pred.se$fit + pred.se$se.fit, lty = 2, lwd = 3, col = "royalblue")
  #legend("topleft", inset = .05, legend = c("Without SE", "GAM Fit", "With SE"), col = c("royalblue", "royalblue", "royalblue"), lty = c(2, 1, 3), cex = 1)

#3. Examine effects of adding the activity variable 
  
g2 <- gam(temp ~ activ + s(time, k = 10), method = 'REML', data = beav)
summary(g2)
capture.output(summary(g2), file = "Jordan_lab7_table2.txt")

#4: Compare the full GAM of beaver temp to linear models that include polynomial terms for time

activity <- beav$activ
Time <- beav$time
Temp <- beav$temp

g.poly3rd <- lm(Temp ~ activity + poly(Time, degree = 3, raw = T), family = gaussian()) # the degree is the basis expansion
g.poly5th <- lm(Temp ~ activity + poly(Time, degree = 5, raw = T), family = gaussian())
g.poly7th <- lm(Temp ~ activity + poly(Time, degree = 7, raw = T), family = gaussian())

AIC(g.poly3rd, g.poly5th, g.poly7th)
capture.output(AIC(g.poly3rd, g.poly5th, g.poly7th), file = "Jordan_lab7_table2.txt")

plot(x = beav$time, y = beav$temp)
lines(beav$time, g.poly3rd$fitted.values, lwd = 5, col = 'yellow')
lines(beav$time, g.poly5th$fitted.values, lwd = 5, col = 'orange')
lines(beav$time, g.poly7th$fitted.values, lwd = 5, col = 'red')

#5: Plot original GAM and the top-ranked LM with empirical data

pred.df <- data.frame(beav, activity = 0)

pred.g1 <- predict(g1, newdata = pred.df, type = "response", se.fit = TRUE)
pred.poly7 <- predict(g.poly7th, newdata = pred.df, type = "response", se.fit = TRUE)

plot(x = pred.df$time, y = pred.df$temp, xlab = "Time", ylab = "Beaver Body Temperature", main = "GAM and Top-Ranked LM - with SE")
  lines(pred.df$time, g1$fitted.values, lwd = 5, col = 'turquoise')
  lines(pred.df$time, pred.g1$fit - pred.g1$se.fit, lty = 2, lwd = 3, col = "turquoise")
  lines(pred.df$time, pred.g1$fit + pred.g1$se.fit, lty = 2, lwd = 3, col = "turquoise")
  lines(pred.df$time, pred.poly7$fit, lwd = 3, col = 'orange3')
  lines(pred.df$time, pred.poly7$fit - pred.poly7$se.fit, lty = 4, lwd = 3, col = 'orange3')
  lines(pred.df$time, pred.poly7$fit + pred.poly7$se.fit, lty = 4, lwd = 3, col = 'orange3')
  legend("topleft", inset = .05, legend = c("GAM", "LM (Poly 7)"), col = c("turquoise", "orange3"), lty = c(1, 1), cex = 1)
