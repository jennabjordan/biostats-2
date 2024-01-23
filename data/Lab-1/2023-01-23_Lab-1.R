
# This Week's Assignment --------------------------------------------------

load("/Users/jennajordan/Downloads/mlb11.rdata")
head(mlb11)

#1 Construct a linear model of strikeouts (dependent/response) vs. at_bats (independent/explanatory)

m2 <- lm(strikeouts ~ at_bats, data = mlb11)
summary(m2)

capture.output(summary(m2), file = "Jordan_table1.txt")

##Plots

par(mfrow = c(2,2))
plot(mlb11$at_bats, mlb11$strikeouts, main = "Line of Best Fit", ylab = "Strikeouts", xlab = "At Bats") #a. scatterplot
abline(m2, col = "palevioletred")

plot(m2$residuals ~ m2$fitted.values, main = "Residuals vs. Fitted Values", ylab = "Residuals", xlab = "Fitted Values", col = 'palevioletred')  #b. plot of residuals vs. fitted
abline(h = 0, lty = 2)

hist(m2$residuals, main = "Histogram", xlab = "Residuals", col = 'white', border = 'palevioletred')  #c. histogram of residuals

plot(m2, which = 2, main = "Q-Q", col = 'palevioletred') #d. Q-Q Plot 

