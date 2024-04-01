library(readxl)
normal.data <- read_excel("/Users/jennajordan/Documents/GitHub/biostats-2/normal-data.xlsx")
head(normal.data)
library(tidyverse)
library(ggplot2)
library(car)

plot(normal.data)

#Name objects
duration <- normal.data$duration
spf <- normal.data$start_peak_freq
epf <- normal.data$end_peak_freq
minf <- normal.data$min_freq
maxf <- normal.data$max_freq
bandwidth <- normal.data$bandwidth
treatment <- normal.data$Treatment
week <- normal.data$week_number


#Plot points on graph
par(mfrow = c(2,2))
plot(week ~ spf + epf + minf + maxf, pch = 10)

#Scatterplot
scatterplotMatrix( ~ duration + spf + epf + minf + maxf + bandwidth)


#Raw Data
par(mfrow = c(2, 3))
hist(duration, col = "antiquewhite4")
hist(spf, col = "lightsteelblue3")
hist(epf, col = "lavenderblush3")
hist(minf, col = "lightblue4")
hist(maxf, col = "thistle4")
hist(bandwidth, col = "snow4")

#Transformations

log.duration <- log(duration) 
inverse.spf <- 1/spf
log10.minf <- log10(minf)
log.epf <- log(epf)
log.maxf <- log(maxf - 9000)
log.bandwidth <- log(bandwidth)

#Transformed ScatterplotMatrix

scatterplotMatrix(~ log.duration + inverse.spf + log10.minf + log.epf + log.maxf + log.bandwidth)

#Transformation Histograms
par(mfrow = c(2, 3))
hist(log(duration), main = "Duration", col = "antiquewhite4", breaks = 10) #
hist((1/spf), main = "Start Peak Frequency", col = "lightsteelblue3", breaks = 10) #
hist(log10(minf), main = "Minimum Frequency", col = "lavenderblush3", breaks = 15) #
hist(log(epf), main = "End Peak Frequency", col = "lightblue4", breaks = 10) #
hist(log(maxf - 9000), main = "Frequency at Max Amplitude", col = "thistle4", breaks = 10) #
hist(log(bandwidth), main = "Bandwidth", col = "snow4", breaks = 15)

#Detection of Outliers
library(ggstatsplot)

#Duration
summary(normal.data$duration)
boxplot(normal.data$duration)$out
duration.o <- boxplot.stats(normal.data$duration)$out
ggbetweenstats(normal.data, week_number, duration, outlier.tagging = TRUE)

#out_ind <- which(duration %in% c(duration.o)) #columns with outliers
#out_ind

clean_normal.data <- normal.data[-which(normal.data$duration %in% duration.o),]
boxplot(clean_normal.data$duration)
ggbetweenstats(clean_normal.data, week_number, duration, outlier.tagging = TRUE)


#Start End Frequency
ggbetweenstats(normal.data, week_number, start_peak_freq, outlier.tagging = TRUE)
spf.o <- boxplot.stats(normal.data$start_peak_freq)$out
clean_normal.data <- normal.data[-which(normal.data$start_peak_freq %in% spf.o),]
ggbetweenstats(clean_normal.data, week_number, start_peak_freq, outlier.tagging = TRUE)

boxplot(clean_normal.data$start_peak_freq ~ clean_normal.data$Treatment)

s#End Peak Frequency
ggbetweenstats(normal.data, week_number, end_peak_freq, outlier.tagging = TRUE)
epf.o <- boxplot.stats(normal.data$end_peak_freq)$out
clean_normal.data <- normal.data[-which(normal.data$end_peak_freq %in% epf.o),]
ggbetweenstats(clean_normal.data, week_number, end_peak_freq, outlier.tagging = TRUE)

#Minimum Frequency
ggbetweenstats(normal.data, week_number, min_freq, outlier.tagging = TRUE)
minf.o <- boxplot.stats(normal.data$min_freq)$out
clean_normal.data <- normal.data[-which(normal.data$min_freq %in% minf.o),]
ggbetweenstats(clean_normal.data, week_number, min_freq, outlier.tagging = TRUE)

#Maximum Frequency
ggbetweenstats(normal.data, week_number, max_freq, outlier.tagging = TRUE)
maxf.o <- boxplot.stats(normal.data$max_freq)$out
clean_normal.data <- normal.data[-which(normal.data$max_freq %in% maxf.o),]
ggbetweenstats(clean_normal.data, week_number, max_freq, outlier.tagging = TRUE)

#Bandwidth
ggbetweenstats(normal.data, week_number, bandwidth, outlier.tagging = TRUE)
bandwidth.o <- boxplot.stats(normal.data$bandwidth)$out
clean_normal.data <- normal.data[-which(normal.data$bandwidth %in% bandwidth.o),]
ggbetweenstats(clean_normal.data, week_number, bandwidth, outlier.tagging = TRUE)

#New Visualization with Clean Data
scatterplotMatrix( ~ clean_normal.data$duration + clean_normal.data$start_peak_freq + clean_normal.data$end_peak_freq + clean_normal.data$min_freq + clean_normal.data$max_freq + clean_normal.data$bandwidth)

#Clean Data Histograms
par(mfrow = c(2, 3))
hist(clean_normal.data$duration, col = "antiquewhite4")
hist(clean_normal.data$start_peak_freq, col = "lightsteelblue3")
hist(clean_normal.data$end_peak_freq, col = "lavenderblush3")
hist(clean_normal.data$min_freq, col = "lightblue4")
hist(clean_normal.data$max_freq, col = "thistle4")
hist(clean_normal.data$bandwidth, col = "snow4")

#Clean Data Histogram Transformations

par(mfrow = c(2, 3))
hist(log(clean_normal.data$duration), main = "Duration", col = "antiquewhite4", breaks = 15) #
hist((1/clean_normal.data$start_peak_freq), main = "Start Peak Frequency", col = "lightsteelblue3", breaks = 15) #
hist((1/clean_normal.data$min_freq), main = "Minimum Frequency", col = "lavenderblush3", breaks = 10) #
hist(log10(clean_normal.data$end_peak_freq), main = "End Peak Frequency", col = "lightblue4", breaks = 10) #
hist((1/clean_normal.data$max_freq), main = "Frequency at Max Amplitude", col = "thistle4", breaks = 10) #
hist(log(clean_normal.data$bandwidth), main = "Bandwidth", col = "snow4", breaks = 10)

#Means and Ranges
mean(normal.data$duration)
mean(clean_normal.data$duration)
range(normal.data$duration)
range(clean_normal.data$duration)

#Start Peak Frequency
mean(normal.data$start_peak_freq)
mean(clean_normal.data$start_peak_freq)
range(normal.data$start_peak_freq)
range(clean_normal.data$start_peak_freq)

#End Peak Frequency
mean(normal.data$end_peak_freq)
mean(clean_normal.data$end_peak_freq)
range(normal.data$end_peak_freq)
range(clean_normal.data$end_peak_freq)

#Min Frequency
mean(normal.data$min_freq)
mean(clean_normal.data$min_freq)
range(normal.data$min_freq)
range(clean_normal.data$min_freq)

#Max Frequency
mean(normal.data$max_freq)
mean(clean_normal.data$max_freq)
range(normal.data$max_freq)
range(clean_normal.data$max_freq)

#Bandwidth
mean(normal.data$bandwidth)
mean(clean_normal.data$bandwidth)
range(normal.data$bandwidth)
range(clean_normal.data$bandwidth)

#Boxplots By Treatment (Clean Data)
par(mfrow = c(2,3))
boxplot(normal.data$duration ~ normal.data$week_number)
boxplot(normal.data$start_peak_freq ~ normal.data$week_number)
boxplot(normal.data$end_peak_freq ~ normal.data$week_number)
boxplot(normal.data$min_freq ~ normal.data$week_number)
boxplot(normal.data$max_freq ~ normal.data$week_number)
boxplot(normal.data$bandwidth ~ normal.data$week_number)

#Boxplots By Treatment (Clean Data)
par(mfrow = c(2,3))
boxplot(clean_normal.data$duration ~ clean_normal.data$week_number)
boxplot(clean_normal.data$start_peak_freq ~ clean_normal.data$week_number)
boxplot(clean_normal.data$end_peak_freq ~ clean_normal.data$week_number)
boxplot(clean_normal.data$min_freq ~ clean_normal.data$week_number)
boxplot(clean_normal.data$max_freq ~ clean_normal.data$week_number)
boxplot(clean_normal.data$bandwidth ~ clean_normal.data$week_number)

#Amplitude
boxplot(normal.data$amplitude ~ week) #45 outliers
hist(normal.data$amplitude)
lines(density(normal.data$amplitude))
qqnorm(normal.data$amplitude)
qqline(normal.data$amplitude)

hist(log10(normal.data$amplitude))
lines(density(normal.data$amplitude))

qqnorm(log10(normal.data$amplitude))
qqline(log(normal.data$amplitude))

#Check for Normality
?shapiro.test
shapiro.test(normal.data$amplitude^1/2) #p-value needs to be above 0.05


#Need to use amplitude to determine groups of call types**
#Look for groups that are similar to matinas paper and see if they are present here based on duration and amplitude!


# PCA Analysis (Principal Component Analysis ------------------------------------------------------------
##Are there groups of syllables (3+?) given for all 6 different parameters?

library(lattice)
library(vegan)

rip.normal <- subset(normal.data, select = duration:bandwidth)

pca <- prcomp(rip.normal)
screeplot(pca) #7 groups
head(pca)

#Ordination in Space
rip.normal.rda <- rda(rip.normal, scale = TRUE)
ordiplot(rip.normal.rda, choices = c(1,2), type = 'none', scaling = 2)
p <- ordiplot(rip.normal.rda, type = 'points')
identify(p, 'species')

cols = c(sample(colours(), 10))
p <- ordiplot(rip.normal.rda, type = 'points')
ordiellipse(p, groups = as.factor(normal.data$week_number), conf = 0.95, col = cols) #

p <- ordiplot(rip.normal.rda, type = 'points')
ordispider(p, groups = as.factor(normal.data$week_number), col = cols)


#transformations for regressions - for all parameters 
#K-means clustering: how many groups in this ordination
#linear model at the end: does syllable diversity differ between control and treatment

###K-Means Clustering
  #ggplot(data = normal.data, 
    #     aes(x = normal.data$`WEEK (1,2,3, etc)`, y = duration), color = normal.data$Treatment) +
    #    geom_point(alpha = 0.5)

  #cof <- data.frame(normal.data$`WEEK (1,2,3, etc)`, normal.data$duration) #get the two columns of interest
  #km <- kmeans(cof, centers = 3, nstart = 20)

#Graphing Attempt
#minfrequency <- aggregate(normal.data$min_freq ~ normal.data$week_number, FUN = min)
#maxfrequency <- aggregate(normal.data$min_freq ~ normal.data$week_number, FUN = max)

#frequencyrange <- seq.int(5000, 116200)
#totalduration <- seq.int(0.001, 0.222)

#row1 <- normal.data[1,]
#row2 <- normal.data[2,]


#ggplot(data = row1, aes(totalduration, frequencyrange)) +
  #geom_point(aes(duration, start_peak_freq), col = "green") +
  #geom_point(aes(duration, end_peak_freq), col = "red") +
  #geom_point(aes(duration, min_freq), col = "purple") +
  #geom_point(aes(duration, max_freq), col = "yellow") +
  #geom_point(aes(duration, amplitude), col = "blue") 

#min(normal.data$duration)
#max(normal.data$duration)
