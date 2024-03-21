embs <- read.csv("/Users/jennajordan/Documents/GitHub/biostats-2/data/Lab 6/all-species-v6-reducedForGLMMs.csv")
head(embs)
library(Matrix)
library(lme4)
library(ggplot2)
library(tidyverse)

table(embs$binomial)

aggregate(embryocount ~ binomial, embs, FUN = mean)

ggplot(
  data = embs,
  mapping = aes(x = sqrt(embryocount))
) +
  geom_histogram(binwidth = .25)

#transformations

#syntax review
m0 <- lm(
  sqrt(embryocount) ~
    (CONT.10yrmean + 
       RH.10yrmean +
       log10(MAP.10yrmean) + 
       sqrt(EVAP.10yrmean))^2 + #creating pairwise interactions between variables
    log10(headbodylength.speciesAVG),
  data = embs
)
summary(m0)

ggplot(
  data = embs, 
  mapping = aes(x = embryocount) # raw data, not square-root transformed
) + 
  geom_histogram() + 
  facet_wrap(~ binomial, scales = "free_y", nrow = 8)

ggplot(
  data = embs, 
  mapping = aes(x = CONT.10yrmean, y = sqrt(embryocount))
) + 
  geom_point() + 
  geom_smooth(method = "lm") # compute and plot relationship

ggplot(
  data = embs, 
  mapping = aes(x = CONT.10yrmean, y = sqrt(embryocount))) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~ binomial, nrow = 8, ncol = 5) # facetting by species identity

# This Week's Assignment --------------------------------------------------

#1: Contruct GLMM relating litter size to 4 climate predictors, two-way interactions and average body size

m1 <- lmer(
  sqrt(embryocount) ~
    (CONT.10yrmean +
       RH.10yrmean +
       log10(MAP.10yrmean) + 
       sqrt(EVAP.10yrmean))^2 + 
    log10(headbodylength.speciesAVG) + #creating pairwise interactions between variables
      (1 | binomial) + 
    (0 + CONT.10yrmean | binomial) +
    (0 + RH.10yrmean | binomial) +
    (0 + log10(MAP.10yrmean) | binomial) +
       (0 + sqrt(EVAP.10yrmean) | binomial),
    data = embs,
  control = lmerControl(optimizer = "bobyqa")
  )
    
summary(m1)
capture.output(summary(m1), file = "Jordan_lab6_table1.txt")

#2: Extract and plot the point estimates of random slopes & intercepts for the species plus standard errors 
ranef.m1 <- ranef(m1)

library(lattice) #to run dotplot

dotplot(ranef.m1) #intercept = species means

#3: constuct complex LMM

m2 <- lmer(
  sqrt(embryocount) ~
    (CONT.10yrmean +
       RH.10yrmean +
       log10(MAP.10yrmean) + 
       sqrt(EVAP.10yrmean))^2 + 
    log10(headbodylength.speciesAVG) + #creating pairwise interactions between variables
    (1 | binomial) + 
    (0 + CONT.10yrmean | binomial) +
    (0 + RH.10yrmean | binomial) +
    (0 + log10(MAP.10yrmean) | binomial) +
    (0 + sqrt(EVAP.10yrmean) | binomial) +
    (0 + log10(headbodylength) | binomial),
  data = embs,
  control = lmerControl(optimizer = "bobyqa")
)
summary(m2)
capture.output(summary(m2), file = "Jordan_lab6_table2.txt")

#4: 

pred <- predict(m2)
#pred

B <- model.frame(m2)[,7] #species or binomial
EC <- model.frame(m2)[,1] #sqrt(embryocount)
HBL <- model.frame(m2)[,8] #log10(headbodylength)

binomial <- aggregate(EC ~ B, FUN = min)[,1] #shows the 39 observations used
y <- aggregate(EC ~ B, FUN = min)[,2] #[,2] makes the length the same and continuous (integer portion of m2)
yend <- aggregate(EC ~ B, FUN = max)[,2]
x <- aggregate(HBL ~ B, FUN = min)[,2]
xend <- aggregate(HBL ~ B, FUN = max)[,2] 

limit <- data.frame(binomial = binomial, x = x, xend = xend, y = y, yend = yend) 



ggplot(
  data = embs) +
  ggtitle("Random Slopes and Intercepts by Species - JJ") +
  labs(x = "Individual Body Length (log transformed)", y = "Litter Size (square rooted)") +
  geom_point(aes(log10(headbodylength), sqrt(embryocount), color = binomial)) +
  geom_segment(data = limit, mapping = aes(xend = xend, yend = yend, y = y, x = x, color = binomial))


