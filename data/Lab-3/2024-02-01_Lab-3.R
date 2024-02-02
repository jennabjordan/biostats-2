#' ---
#' title: "Lab 3"
#' date: 2024-02-01

library("statmod")
wells <- read.delim("/Users/jennajordan/Downloads/wells.txt", sep = " ")
head(wells)
hist(wells$switch) #switch = (response variable) household w/unsafe wells switch to nearby community
table(wells$switch)
plot(wells, col = "seagreen") #2 predictors are binomial (yes/no) data

# Binomial Regression -----------------------------------------------------

m1 <- glm(switch ~ arsenic, family = binomial(link = "logit"), 
          data = wells)
summary(m1) #arsenic is a significant predictor for house switching

##Comparison with lm()
m0 <- glm(switch ~ arsenic, data = wells, family = gaussian(link = "identity"))
summary(m0) #check model diagnotics to see something is wrong

par(mfrow = c(2,2))

# plotting residuals versus fitted values to examine violation of linear assumptions
plot(predict(m0, type = 'response'), qresid(m0), col = 'seagreen')
abline(a = 0, b = 0, lty = 2, lwd = 2)
plot(predict(m1, type = 'response'), qresid(m1), col = 'red')
abline(a = 0, b = 0, lty = 2, lwd = 2)

# Q-Q plots to assess specification of wrong error distribution
qqnorm(qresid(m0), col = 'seagreen'); qqline(qresid(m0), lwd = 2)
qqnorm(qresid(m1), col = 'red'); qqline(qresid(m1), lwd = 2)

# Visualiing Linearized Predictions
fake.arsenic <- seq(min(wells$arsenic), max(wells$arsenic), by = 0.1) # New predictor (X), spanning from min to max of empirical data
fake.switches <- predict(m1, list(arsenic = fake.arsenic), type = 'link') # Predicted Y
plot(wells$arsenic, wells$switch, xlim = c(0,10)) #shows log odds ratio
points(fake.arsenic, fake.switches, col = 'red', type = 'l')

#Visualizing Response Predictions
fake.switches <- predict(m1, list(arsenic = fake.arsenic), type = 'response') #shows raw response
plot(wells$arsenic, wells$switch, xlim = c(0,10))
points(fake.arsenic, fake.switches, col = 'red', type = 'l')


# Poisson Regression for Count Data ------------------------------------------------------

gala <- read.delim("/Users/jennajordan/Downloads/gala.txt", sep = ' ')
head(gala)
plot(gala, col = "seagreen3")
hist(gala$Species, breaks = 15, col = "seagreen3")

#Can numbers of plants be predicted based on area and elevation range? Use Poisson to test

m2 <- glm(Species ~ Area * Elevation, data = gala, family = quasipoisson(link = "log"))
summary(m2) #Area has biggest effect (largest estimate or slope and t-value)

par(mfrow = c(1,2))

# plotting residuals versus fitted values to examine violation of linear assumptions
plot(predict(m2, type = 'response'), qresid(m2), col = 'seagreen', main = "Residuals vs. Fitted")
abline(a = 0, b = 0, lty = 2, lwd = 2)

# Q-Q plot to assess specification of wrong error distribution
qqnorm(qresid(m2), col = 'seagreen'); qqline(qresid(m2), lwd = 2)


# This Week's Assignment --------------------------------------------------

#1:

head(gala)
m3 <- glm(Species ~ Area * Elevation + Nearest * Adjacent, data = gala, family = quasipoisson(link = log))
summary(m3)

#2:
capture.output(summary(m3), file = "Jordan_table1.txt")

#3:
fake.elevation <- seq(min(gala$Elevation), max(gala$Elevation), by = 1) #new X value
length(fake.elevation)

N <- mean(gala$Nearest)
fake.nearest <- rep(N, 1683)

A <- mean(gala$Area)
fake.area <- rep(A, 1683)

AD <- mean(gala$Adjacent)
fake.adjacent <- rep(AD, 1683)


df <- data.frame(Elevation = fake.elevation, Nearest = fake.nearest, Area = fake.area, Adjacent = fake.adjacent) #new predicted Y value
head(df)

fake.species <- predict(m3, list(Elevation = df$Elevation, Nearest = df$Nearest, Area = df$Area, Adjacent = df$Adjacent), type = 'response')
head(fake.species)

plot(gala$Elevation, gala$Species, main = "Bivariate Relationship (Species v. Elevation)", xlab = "Elevation", ylab = "Species")
points(fake.elevation, fake.species, col = "tan4", type = 'l')


par(mfrow = c(1,2))

# plotting residuals versus fitted values to examine violation of linear assumptions
plot(predict(m3, type = 'response'), qresid(m3), col = 'tan4', main = "Residuals vs. Fitted", xlab = "Predictor", ylab = "Residuals")
abline(a = 0, b = 0, lty = 2, lwd = 2)

# Q-Q plot to assess specification of wrong error distribution
qqnorm(qresid(m3), col = 'tan4'); qqline(qresid(m3), lwd = 2, xlab = "", ylab = "")
