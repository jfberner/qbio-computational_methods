# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Multivariate Analysis
# First version 2022-08-05
# jfb
# --------------------------------------------------#

##### Libs #####
library(palmerpenguins)
library(vegan)
library(cluster)
##### Data #####
data(dune)
data("dune.env")
table(dune.env$Management)

##### Hierarquical Clustering #####
## Not  working but from the slides
# data(penguins)
# dim(penguins)
# hclust(d = penguins[,-1])

##### Cluster analysis of the dune vegetation #####

# We calculate two dissimlairty indices between sites: Bray-Curtis distance and Chord distance

bray_distance <- vegdist(dune)
# Chord distance, which is the euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))

# Let’s use "average", who will link clusters
b_cluster <- hclust(bray_distance, method = "average")
c_cluster <- hclust(chord_distance, method = "average")


par(mfrow = c(1, 2))
plot(b_cluster)
plot(c_cluster)
par(mfrow = c(1, 1))

# Ok, that’s a little bit ugly
# Try this from Andrea:
par(mfrow = c(1, 2))
plot(b_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(c_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
par(mfrow = c(1, 1))
# I kinda like the first one better  ¯\_(ツ)_/¯






##### Ordination in R #####

is(chord_distance)
norm <- decostand(dune,'norm')
pca <- rda(norm)

summary(pca)
pca

plot(pca)
# plot(pca, choices = c(2,3)) #2nd and 3rd PCs

# playing around
pca_env <- rda(dune.env) # doesn't work, we have to work on the categorical variables OR remove them. We'll remove them:
dune.env1 <- dune.env[,c("A1","Moisture","Manure")] # create a new dune.env only containing the continuous variables
dune.env1$A1 <- as.numeric(dune.env1$A1) # change the factors to numeric
dune.env1$Moisture <- as.numeric(dune.env1$Moisture)
dune.env1$Manure <- as.numeric(dune.env1$Manure)


pca_env <- rda(dune.env1) # PC the shit out of it
pca_env # for a resume of PC contribution
# summary(pca_env) # for more info
plot(pca_env) # check it out
# thickness of soil (A1) and Moisture look very correlated but check
# cor(dune.env1) # and see its just 0.47, so in the other dimensions moisture and A1 should not look so correlated
# plot(pca_env, choices = c(2,3)) # and they point in opposite directions, as expected :)
