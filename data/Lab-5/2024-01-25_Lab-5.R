#Lab 5

library(Matrix)
library(lme4)
library(ggplot2)
library(lattice)

data(sleepstudy)
head(sleepstudy)

hist(sleepstudy$Reaction)

# pipe "|" used to specify random effects
m0 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
summary(m0)

m0.ranef <- ranef(m0) #get subject specific slopes
m0.ranef

dotplot(m0.ranef) #caterpillar plot - get estimated values from highest to lowest

##Visualizing Plots of subject-specific effects

pred.Reaction <- predict(m0, newdata = sleepstudy, type = "response") #predicted values for Subjects
pred.df <- data.frame(sleepstudy, pred.Reaction) #bind predictions to original data frame
ggplot(data = pred.df, aes(Days, Reaction)) +
  geom_point() +
  geom_line(aes(Days, pred.Reaction), col = "darkolivegreen") +
  facet_wrap(vars(Subject))

library(datasets)
data("ChickWeight")
head(ChickWeight)
hist(ChickWeight$weight)

plot(ChickWeight) #visualize trends per-chick


# This Week's Assignment --------------------------------------------------

#1. Contruct GLMM relating chick weight to two predictors - time of day and diet type

#a.& b.
m1 <- lm(weight ~ poly(Time) * Diet, data = ChickWeight)
m1

#c. 
m2 <- lmer(weight ~ poly(Time, 2) * Diet + (Time | Chick), data = ChickWeight)
m2

capture.output(summary(m2), file = "Jordan_lab5_table1.txt")

#2. Extract and plot the point estimates of random slopes and intercepts for the chicks, plut their SE

m1.ranef <- ranef(m2)
dotplot(m1.ranef)

#3. Construct simpler GLMM but force chick growth curves to have identical intercepts

m3 <- lmer(weight ~ poly(Time, 2) * Diet + (0 + Time | Chick), data = ChickWeight)
m3

capture.output(summary(m3), file = "Jordan_lab5_table2.txt")

#4. Compare two mixed models using analysis of deviance

aov <- anova(m2, m3)
aov
capture.output(summary(aov), file = "Jordan_lab5_table3.txt")

#5. Produce chick-specific summary plot for top-ranked of these two GLMM, fixed w random effects and random chick variation

pred.Chick <- predict(m2, newdata = ChickWeight, type = "response")
pred.chick.df <- data.frame(ChickWeight, pred.Chick)

ggplot(data = pred.chick.df, aes(Time, weight)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(Time, pred.Chick), col = "springgreen4", linewidth = 1) +
  labs(title = "Chick-Specific Growth Curves through Time", x = "Time", y = "Weight") +
  facet_wrap(vars(Chick))
