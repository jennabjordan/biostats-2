firs <- read.csv("/Users/jennajordan/Documents/GitHub/biostats-2/data/Lab 8/firs.csv")

#PREDICTOR VARIABLE <- DBH = Diameter at breast height (tree size)
#RESPONSE VARIABLE <- TOTCONES = number of cones produced
#fecundity = measure of the capacity to reproduce offspring

firs <- firs[-which(firs$TOTCONES == 0), ]

hist(firs$TOTCONES, col = "forestgreen") #fir tree fecundity

#colored by population die-off
plot(firs$DBH, firs$TOTCONES, col = "white")
points(firs$DBH[which(firs$WAVE_NON == "w")], firs$TOTCONES[which(firs$WAVE_NON == "w")], col = "red", pch = 4)
points(firs$DBH[which(firs$WAVE_NON == "n")], firs$TOTCONES[which(firs$WAVE_NON == "n")], col = "forestgreen", pch = 21)

#does population status (recent die-off) have an effect on fecundity
#^Use GLM to answer this

h0 <- glm(TOTCONES ~ DBH, data = firs, family = Gamma(link = "log"))
summary(h0)

hA <- glm(TOTCONES ~ DBH + WAVE_NON, data = firs, family = Gamma(link = "log"))
summary(hA)
#does not show statistical difference, use logLik() to get model likelihoods from objects

h0.MLE <- logLik(h0) 
hA.MLE <- logLik(hA) #better model

h0.MLE; hA.MLE
#Better ML score = higher logLik number has better fit to the data

#Likelihood Ratio Test (LRT)
##compares fit of nested models of the same data

anova(h0, hA, test = "LRT") #prefer the simpler model (h0) since neither are significant

#Akaike's Information Criterion (AIC)
##estimator of prediction error and relative quality of stat models
##how much info is lost using a model & prefer LOWER values of AIC

h0.age <- glm(TOTCONES ~ AGE, data = firs, family = Gamma(link = "log"))
hA.age <- glm(TOTCONES ~ AGE + WAVE_NON, data = firs, family = Gamma(link = "log"))  

#set up models with interaction terms
hA.dbh.interact <- glm(TOTCONES ~ DBH * WAVE_NON, data = firs, family = Gamma(link = "log"))
hA.age.interact <- glm(TOTCONES ~ AGE * WAVE_NON, data = firs, family = Gamma(link = "log"))

#make vector of the models 
formulae <- c(
  formula(h0),
  formula(hA),
  formula(h0.age),
  formula(hA.age),
  formula(hA.dbh.interact),
  formula(hA.age.interact)
)
formulae <- as.character(formulae)
formulae

#compute AIC values
aics <- AIC(h0, hA, h0.age, hA.age, hA.dbh.interact, hA.age.interact)

mod.table <- cbind(formulae, aics)
mod.table

#find the best model
min.AIC <- min(aics$AIC) #hA has lowest AIC = 2179.674

#Delta AIC
delta.AIC <- aics$AIC - min.AIC

#AIC Weight
AIC.wt <- exp(-0.5*delta.AIC)/sum(exp(-0.5*delta.AIC))
AIC.wt <- format(AIC.wt, scientific = F) #not scientific notation

#Bind all matrix for easy viewing
mod.table <- cbind(mod.table, delta.AIC, AIC.wt)
mod.table <- mod.table[order(mod.table$AIC),] #sort by delta.AIC values (best model at top)
mod.table

#best models have delta.AIC of < 2 (top 3)


# THIS WEEK'S ASSIGNMENT --------------------------------------------------

litters <- read.csv("/Users/jennajordan/Documents/GitHub/biostats-2/data/Lab 8/litters.csv")
hist(litters$nEMBS, col = "tomato")

#LONG TERM VARIABLES:
## temp_seasonality
## precip_seasonality
## NFFD = number of frost-free days

#SHORT TERM VARIABLES:
## breeding_temp
## breeding_precip
## NFFD_anomaly = difference from long-term # of frost free days)

#1: Construct a full generalized linear model containing all long-term and short-term climate variables

?glm
p1 <- glm(nEMBS ~ temp_seasonality + precip_seasonality + NFFD + breeding_temp + breeding_precip + NFFD_anomaly, data = litters, family = poisson)

capture.output(p1, file = "Jordan_lab8_table1.txt")

#2: Construct reduced GLM containing only long-term climate variables

p2 <- glm(nEMBS ~ temp_seasonality + precip_seasonality + NFFD, data = litters, family = poisson)
capture.output(p2, file = "Jordan_lab8_table2.txt")

#3: Construct reduced GLM containing only short-term climate variables

p3 <- glm(nEMBS ~ breeding_temp + breeding_precip + NFFD_anomaly, data = litters, family = poisson)
capture.output(p3, file = "Jordan_lab8_table3.txt")
  
#4: Perform a full model comparison (three models) based on AIC and AIC-derived metrics

AICvalues <- AIC(p1, p2, p3)

formulas <- c(
  formula(p1), #maybe best model??
  formula(p2),
  formula(p3))
formulas <- as.character(formulas)
formulas

model.table <- cbind(formulas, AICvalues)
model.table

best.AIC <- min(AICvalues$AIC)
deltaAIC <- AICvalues$AIC - best.AIC
AICweight <- exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC))
AICweight <- format(AICweight, scientific = F) #not scientific notation

model.table <- cbind(model.table, deltaAIC, AICweight)
model.table <- model.table[order(model.table$AIC),] #sort by delta.AIC values (best model at top)
model.table
  
write.csv(model.table, file = "Jordan_lab8_model-table.csv") #saves full model comparison as CSV
