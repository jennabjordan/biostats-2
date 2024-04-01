
birdhab <- read.csv("/Users/jennajordan/Downloads/birdhab.csv", header = TRUE)
head(birdhab)
head(birdhab$SUB)

library(vegan)

rip.bird <- subset(birdhab, select = AMGO:WWPE)
head(rip.bird)

rip.hab <- subset(birdhab, select = VAREA:CONEDGE)
head(rip.hab)

rip.hab.pca <- prcomp(rip.hab, scale = TRUE) #PC1, absorbs the most amounts of variance
screeplot(rip.hab.pca) #variance of the raw habitat data explained by PC axis (10% of the variation)
head(rip.hab.pca$x) #first two axes are what are typically plotted

rip.hab.rda <- rda(rip.hab, scale = TRUE)
ordiplot(rip.hab.rda, choices = c(1,2), type = 'text', scaling = 2)

counts <- colSums(rip.hab)
ordiplot(rip.hab.rda, choices = c(1,2), type = 'none')
orditorp(rip.hab.rda, display = 'sites', col = 'maroon', pch = 19)
orditorp(rip.hab.rda, display = 'species', priority = counts, col = 'black', pch = 19)

p <- ordiplot(rip.hab.rda, choices = c(1,2), type = 'points')
identify(p, 'species')

p <- ordiplot(rip.hab.rda, choices = c(1,2), type = 'points')
identify(p, 'sites')

cols = c(sample(colours(), 10)) #10 random colors
p <- ordiplot(rip.hab.rda, choices = c(1,2), display = 'sites')
ordispider(p, groups = as.factor(birdhab$SUB), col = cols)       

p <- ordiplot(rip.hab.rda, choices = c(1,2), display = 'sites')         
ordiellipse(p, groups = as.factor(birdhab$SUB), conf = 0.9, col = cols)

###Test Group Differences (Rank-Based)

?vegdist

dissim <- vegdist(rip.hab, method = 'euclidian')
head(dissim)
y.anosim <- anosim(dissim, birdhab$SUB)
summary(y.anosim) #

###Test Group Differences (Regression Based)

y.adonis <- adonis2(rip.hab ~ SUB, data = birdhab, permutations = 1000, method = 'euclidian')
y.adonis

###NMSD

rip.bird.nmds <- metaMDS(rip.bird, distance = 'bray', k = 3, trymax = 50, autotransform = FALSE)
rip.bird.nmds
head(rip.bird.nmds$points)
head(rip.bird.nmds$species)

cols = c(sample(colours(), 10))
p <- ordiplot(rip.bird.nmds, choices = c(1,2), display = 'sites')
ordispider(p, groups = as.factor(birdhab$SUB), col = cols)

p <- ordiplot(rip.bird.nmds, choices = c(1,2), display = 'sites')
ordiellipse(p, groups = as.factor(birdhab$SUB), conf = 0.9, col = cols)

p <- ordiplot(rip.bird.nmds, choices = c(1,2), display = 'sites')
ordihull(p, groups = as.factor(birdhab$SUB), col = cols)


# This Week's Assignment --------------------------------------------------
## wood turtle habitat use
## response = turtle, and columns 4-28 are environmental predictors

turtle <- read.csv("/Users/jennajordan/Downloads/byturtle.csv", header = TRUE)
head(turtle)
hist(turtle$turtle) #presence and absence

#1: Summarize using PCA

rip.turt <- subset(turtle, select = openwater:distupland)
head(rip.turt)

#2:
rip.turt.rda <- rda(rip.turt, scale = TRUE)
ordiplot(rip.turt.rda, choices = c(1,2), type = 'text', scaling = 2)

cols = c("maroon","black")
t <- ordiplot(rip.turt.rda, choices = c(1,2), display = 'sites', main = "Environment Variation in Turtle Habitat")
ordispider(t, groups = as.factor(turtle$turtle), col = cols)


#3:
rip.turt.pca <- prcomp(rip.turt, scale = TRUE)
?screeplot
screeplot(rip.turt.pca, npcs = 5, main = "Variation Explained")
head(rip.turt.pca$x)

#4:

#ANOSIM comparison of ranks of distances among or within

dissim <- vegdist(rip.turt, method = "euclidian")
head(dissim)

#ANOSIM
t.anosim <- anosim(dissim, turtle$turtle) #environment is different where turtles are present/absent
summary(t.anosim)

#PERMANOVA - permutational multivariate analysis of variance
##variation within a group can be calculated from distance matrix
y.adonis <- adonis2(rip.turt ~ turtle, data = turtle, permutations = 1000, method = 'euclidian')
y.adonis

capture.output(summary(t.anosim), file = "Jordan_summarytable1.txt")
capture.output(summary(y.adonis), file = "Jordan_summarytable2.txt")

