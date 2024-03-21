library(readxl)
normal.data <- read_excel("/Users/jennajordan/Documents/GitHub/biostats-2/normal-data.xlsx")
head(normal.data)
library(tidyverse)
library(car)

plot(normal.data)

hist(log(normal.data$duration))

#Name objects
duration <- normal.data$duration
spf <- normal.data$start_peak_freq
epf <- normal.data$end_peak_freq
minf <- normal.data$min_freq
maxf <- normal.data$max_freq
bandwidth <- normal.data$bandwidth


#Plot points on graph
par(mfrow = c(2,2))
plot(normal.data$`WEEK (1,2,3, etc)` ~ spf + epf + minf + maxf, pch = 19)

#Scatterplot
scatterplotMatrix(~ duration + spf + epf + minf + maxf + bandwidth)

#Raw Data
par(mfrow = c(2, 3))
hist(duration, col = "antiquewhite4")
hist(spf, col = "lightsteelblue3")
hist(epf, col = "lavenderblush3")
hist(minf, col = "lightblue4")
hist(maxf, col = "thistle4")
hist(bandwidth, col = "snow4")

?hist

#Transformations
par(mfrow = c(2, 3))
hist(log(duration), main = "Duration", col = "antiquewhite4", breaks = 10) #
hist((1/spf), main = "Start Peak Frequency", col = "lightsteelblue3", breaks = 10) #
hist(log10(minf), main = "Minimum Frequency", col = "lavenderblush3", breaks = 15) #
hist(log(epf), main = "End Peak Frequency", col = "lightblue4", breaks = 10) #
hist(log(maxf - 9000), main = "Frequency at Max Amplitude", col = "thistle4", breaks = 10) #
hist(log(bandwidth), main = "Bandwidth", col = "snow4", breaks = 15)

#transformations for regressions - for all parameters 
#PCA or CVA - are there groups of syllables (3+?) given different properties
#K-means clustering: how many groups in this ordination
#linear model at the end: does syllable diversity differ between control and treatment
##

