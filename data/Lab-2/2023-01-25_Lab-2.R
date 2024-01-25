load("/Users/jennajordan/Downloads/evals.rdata")
head(evals)
dim(evals) #shows rows & col

eval.vars <- read.delim("/Users/jennajordan/Downloads/eval-variables.txt")
eval.vars

hist(evals$score) #evaluation score (1 = unsatisfactory, 5 = excellent)
summary(evals$score)

boxplot(score ~ gender, data = evals) 
