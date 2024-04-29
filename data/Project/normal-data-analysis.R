library(readxl)
normal.data <- read_excel("/Users/jennajordan/Library/Mobile Documents/com~apple~CloudDocs/Documents/GitHub/biostats-2/data/Project/normal-data.xlsx")
head(normal.data)


# Visualize Data ------------------------------------------------------
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

#Scatterplot
scatterplotMatrix( ~ duration + spf + epf + minf + maxf + bandwidth)

#Histograms
par(mfrow = c(2, 3))
hist(duration, col = "antiquewhite4")
hist(spf, col = "lightsteelblue3")
hist(epf, col = "lavenderblush3")
hist(minf, col = "lightblue4")
hist(maxf, col = "thistle4")
hist(bandwidth, col = "snow4")

#Potential Transformations
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


# Detection of Outliers ---------------------------------------------------
library(ggstatsplot)

#Duration
boxplot(normal.data$duration)$out
ggbetweenstats(normal.data, week_number, duration, outlier.tagging = TRUE) #Raw Data
duration.o <- boxplot.stats(normal.data$duration)$out #name outliers
clean_normal.data <- normal.data[-which(normal.data$duration %in% duration.o),] #removes outliers
ggbetweenstats(clean_normal.data, week_number, duration, outlier.tagging = TRUE) #Outliers Removed

#Start End Frequency
ggbetweenstats(normal.data, week_number, start_peak_freq, outlier.tagging = TRUE)
spf.o <- boxplot.stats(normal.data$start_peak_freq)$out
clean_normal.data <- normal.data[-which(normal.data$start_peak_freq %in% spf.o),]
ggbetweenstats(clean_normal.data, week_number, start_peak_freq, outlier.tagging = TRUE) #Outliers Removed

#End Peak Frequency
ggbetweenstats(normal.data, week_number, end_peak_freq, outlier.tagging = TRUE)
epf.o <- boxplot.stats(normal.data$end_peak_freq)$out
clean_normal.data <- normal.data[-which(normal.data$end_peak_freq %in% epf.o),]
ggbetweenstats(clean_normal.data, week_number, end_peak_freq, outlier.tagging = TRUE) #Outliers Removed

#Minimum Frequency
ggbetweenstats(normal.data, week_number, min_freq, outlier.tagging = TRUE)
minf.o <- boxplot.stats(normal.data$min_freq)$out
clean_normal.data <- normal.data[-which(normal.data$min_freq %in% minf.o),]
ggbetweenstats(clean_normal.data, week_number, min_freq, outlier.tagging = TRUE) #Outliers Removed

#Maximum Frequency
ggbetweenstats(normal.data, week_number, max_freq, outlier.tagging = TRUE)
maxf.o <- boxplot.stats(normal.data$max_freq)$out 
clean_normal.data <- normal.data[-which(normal.data$max_freq %in% maxf.o),]
ggbetweenstats(clean_normal.data, week_number, max_freq, outlier.tagging = TRUE) #Outliers Removed

#Bandwidth
ggbetweenstats(normal.data, week_number, bandwidth, outlier.tagging = TRUE)
bandwidth.o <- boxplot.stats(normal.data$bandwidth)$out
clean_normal.data <- normal.data[-which(normal.data$bandwidth %in% bandwidth.o),]
ggbetweenstats(clean_normal.data, week_number, bandwidth, outlier.tagging = TRUE) #Outliers Removed

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

#Clean Data Histogram - With Transformations
par(mfrow = c(2, 3))
hist(log(clean_normal.data$duration), main = "Duration", col = "antiquewhite4", breaks = 15) #
hist((1/clean_normal.data$start_peak_freq), main = "Start Peak Frequency", col = "lightsteelblue3", breaks = 15) #
hist((1/clean_normal.data$min_freq), main = "Minimum Frequency", col = "lavenderblush3", breaks = 10) #
hist(log10(clean_normal.data$end_peak_freq), main = "End Peak Frequency", col = "lightblue4", breaks = 10) #
hist((1/clean_normal.data$max_freq), main = "Frequency at Max Amplitude", col = "thistle4", breaks = 10) #
hist(log(clean_normal.data$bandwidth), main = "Bandwidth", col = "snow4", breaks = 10)


# Mean/Range --------------------------------------------------------------

#Duration
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


# PCA Analysis (Principal Component Analysis ------------------------------------------------------------
library(lattice)
library(vegan)
library(viridisLite)
library(viridis)
library(RColorBrewer)
library(factoextra)

rip.normal <- subset(normal.data, select = duration:bandwidth)

pca <- prcomp(rip.normal, scale. = TRUE)
screeplot(pca)
biplot(pca)
pscores <- pca$x #extract PC scores

#PCA Biplot for "Clean data - Without Outliers
clean.rip.normal <- subset(clean_normal.data, select = duration:bandwidth)
pca.clean <- prcomp(clean.rip.normal, scale. = TRUE)
screeplot(pca.clean) #7 groups
head(pca.clean)
biplot(pca.clean)
clean.pscores <- pca$x

col_scale <- brewer.pal(n = 8, name = "Set3")
categorycol <- col_scale[as.numeric(unique(normal.data$week_number))]
ag <- aggregate(pscores ~ normal.data$week_number, FUN = mean)
p <- ordiplot(pca, type = 'points')
ordispider(p, groups = as.factor(normal.data$week_number), col = categorycol)
text(ag$PC1, ag$PC2, labels = ag$`normal.data$week_number`)
legend("topright", legend = ag$`normal.data$week_number`, fill = col_scale, title = "Week Number")

#Set new color pallette for Biplot
col_pal <- brewer.pal(n = 7, name = "Set3")

#Biplot for Treatment Groups
fviz_pca_biplot(pca,
                axes = c(1,2),
                geom = "point",
                col.ind = normal.data$Treatment,
                palette = c("magenta", 'blue'),
                addEllipses = TRUE,
                )

#Biplot for Weeks
fviz_pca_biplot(pca,
                axes = c(1,2),
                geom = "point",
                col.ind = as.factor(normal.data$week_number),
                palette = c("magenta", 'blue', "green", "yellow", "orange", "red", "purple", "plum"),
                addEllipses = TRUE,
)


# K-means -----------------------------------------------------------------
#K-Means Clustering
library(factoextra)
library(NbClust)
library(cluster)

kmeans_result <- kmeans(rip.normal, centers = 2)
cluster_centers <- kmeans_result$centers
print(cluster_centers)

cluster_assignments <- kmeans_result$cluster
print(cluster_assignments)

# Plots clusters
fviz_cluster(kmeans_result, data = rip.normal,
             geom = "point", 
             ellipse.type = "convex", 
             ellipse.level = 0.95,    
             main = "K-means Clustering of Data",
             palette = "terrain.colors"   
)

#Determine number of clusters
nb <- NbClust(rip.normal, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")
fviz_nbclust(nb) 

##Determine k value
silhouette_widths <- numeric()

# Loop through different values of k
for (k in 2:10) {
  kmeans_result <- kmeans(rip.normal, centers = k)
  
  # Calculate silhouette widths
  silhouette_result <- silhouette(as.integer(kmeans_result$cluster), dist(rip.normal))
  print(silhouette_result)
  
  # Extract average silhouette width
  silhouette_width <- summary(silhouette_result)$avg.width
  print(silhouette_width)
  
  # Append silhouette width to silhouette_widths vector
  silhouette_widths <- c(silhouette_widths, silhouette_width)
}

# Print silhouette_widths
print(silhouette_widths)

# Remove one element from the x-axis vector to match the length of the y-axis vector
x_values <- 2:(length(silhouette_widths) + 1)

# Plot silhouette widths
optimal_k <- which.max(silhouette_widths) + 1
plot(x_values, silhouette_widths, type = "b", pch = 19, xlab = "Number of Clusters (k)", ylab = "Average Silhouette Width")
axis(1, at = 2:length(silhouette_widths), labels = 2:length(silhouette_widths))
abline(v = optimal_k, col = "royalblue", lty = 2)



# ANOVAS ------------------------------------------------------------------
anova <- aov(pscores ~ week_number * Treatment, data = normal.data)
summary(anova)

#Define PC1: accounts for 54.5 % of the data
PC1 <- pca$x[ ,1]

#Visualize PC1
hist(PC1)
shapiro.test(PC1)

#Combine PC1 scores with data set
normal.data <- cbind(normal.data, PC1)

#PC1 Anova ~ Treatment
anova_1 <- aov(PC1 ~ Treatment, data = normal.data)
summary(anova_1)
TukeyHSD(anova_1) # p = 0.0000252 DIFFERENCE BETWEEN TREATMENTS

#PC1 Anova ~ Week
normal.data$week_number <- factor(normal.data$week_number)
anova_1w <- aov(PC1 ~ normal.data$week_number)
summary(anova_1w)
TukeyHSD(anova_1w)
#Week interactions that are significant! Week 1 with weeks 2-7

#Interactions between Treatment and Week
anova_1_int <- aov(PC1 ~ Treatment * week_number, data = normal.data)
summary(anova_1_int)
TukeyHSD(anova_1_int)

##significance:
#Control week 1 with Treatment weeks 1-3, 5-8 (week 4 = not significant)
#Treatment to treatment & control to control did NOT differ between weeks


# Linear Model ------------------------------------------------------------
library(dplyr)
library(MASS)
library(effects)

transformed_PC1 <- PC1 + 2 #makes PC score values positive
hist(transformed_PC1)

#Combine positive PC scores with data set
normal.data <- cbind(normal.data, transformed_PC1)

#Generalized Linear Model
model <- glm(transformed_PC1 ~ Treatment * week_number, data = normal.data, family = gaussian(link = "identity"))

print(model)
summary(model)
plot(model)

normal.data$week_number <- factor(normal.data$week_number)

glm_effects <- allEffects(model)
plot(glm_effects, main = "Effect Plot For Treatment", xlab = "Week") 


#slope represents strength & direction of relationship
  #steepness indicates strength of effect
    #treatment = positive: as one increases so does the other (positive effect)
    #control = negative as one decreases so does the other (negative effect)
#width of confidence intervals show uncertainty, wide = less precise predictions
