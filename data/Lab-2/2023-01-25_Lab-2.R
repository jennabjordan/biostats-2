load("/Users/jennajordan/Downloads/evals.rdata")
head(evals)
dim(evals) #shows rows & col

eval.vars <- read.delim("/Users/jennajordan/Downloads/eval-variables.txt")
eval.vars

hist(evals$score) #evaluation score (1 = unsatisfactory, 5 = excellent)
summary(evals$score)

boxplot(score ~ gender, data = evals) 
boxplot(score ~ rank, data = evals)
boxplot(score ~ gender + rank, evals)

boxplot(score ~ gender + rank, evals,
        subset = 
          gender == c('female', 'male'),
          col = c('red', 'blue')
)

m1 <- aov(score ~ gender * rank, data = evals)
summary(m1)

model.tables(m1)


# Multiple Linear Regression ----------------------------------------------

m2 <- lm(score ~ age + rank + gender + bty_avg, data = evals)
summary(m2)

par(mfrow = c(2,2))
plot(m2, col = "royalblue4")

par(mfrow = c(1,3)) #individual distributions
hist(evals$score, ylim = c(0,90)) #ylim() creates same axis
hist(evals$age, ylim = c(0,90))
hist(evals$bty_avg, ylim = c(0,90))

#transform score using log10(5-score)
evals$score
new_score1 <- sqrt(5 - evals$score)
new_score1
hist(new_score1)

#transform bty_avg using 

evals$bty_avg
new_bty_avg <- sqrt(evals$bty_avg)
new_bty_avg
hist(new_bty_avg)

par(mfrow = c(1,3), col = 'royalblue4') #individual distributions
hist(new_score1, ylim = c(0,140),
     main = "Squareroot Transformed Score (sqrt(5 - score)",
     xlab = "New 'Score' Value",
     col = 'lightblue') 
hist(evals$age, ylim = c(0,140),
     main = "Raw Age Data",
     xlab = "Age",
     col = 'blue2')
hist(new_bty_avg, ylim = c(0,140),
     main = "Squareroot Transformed Beauty",
     xlab = "New 'Beauty' Value",
     col = 'royalblue4')

m3 <- lm(sqrt(5 - score) ~ age * rank * gender + sqrt(bty_avg), data = evals)
summary(m3)

capture.output(summary(m3), file = "Jordan_table1.txt")

par(mfrow = c(2,2))
plot(m3, col = "royalblue4")


