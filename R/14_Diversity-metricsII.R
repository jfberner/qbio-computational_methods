# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Diversity Metrics II
# First version 2022-08-03
# jfb
# --------------------------------------------------#

#### Libs ####
library(vegan)
library(ggplot2)
#### Data ####
community.A <- c(10, 6, 4, 1)
community.B <- c(17, rep(1, 7))

#### Metrics ####
diversity(community.A, "shannon") # read help, for realsies
diversity(community.B, "shannon")
diversity(community.A, "invsimpson")
diversity(community.B, "invsimpson")
# see that inv simpson gets the difference, but shannon doesnt

ren_comA <- vegan::renyi(community.A) # the 'scales' are the different values of 'a' in the Renyi formula (see help! seriously)
# when 1 it is just like shannon, when
ren_comB <- vegan::renyi(community.B)

ren_AB <- rbind(ren_comA,ren_comB)

matplot(t(ren_AB), type = 'l', axes = F, ylab = 'Rényi Diversity', xlab = 'Rényi Scales') #matplot made for matrices, t argument is transpose function, 'l' is for lines
# to make it more pretty and the axes correct:
box() ; axis(side = 2) ; axis(side = 1, labels = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "Inf"), at = 1:11) ; legend('topright', legend = c('Community A', 'Community B'), lty = c(1, 2), col = c(1, 2))

# Seeing the plot, we cannot infer which of the two communities is more diverse, because the lines intersect, so its "KINDA threshold-dependant" (it's dependant on how much we give weight 'a' for each species, regardless of abundance)


# For Fun ####
community.C <- c(19,30,51,23,0,0,1,2,41)
ren_comC <- renyi(community.C)
ren_ABC <- rbind(ren_comA,ren_comB,ren_comC)

matplot(t(ren_ABC), type = 'l', axes = F, ylab = 'Rényi Diversity', xlab = 'Rényi Scales') #matplot made for matrices, t argument is transpose function, 'l' is for lines
# to make it more pretty and the axes correct:
box() ; axis(side = 2) ; axis(side = 1, labels = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "Inf"), at = 1:11) ; legend('topright', legend = c('Community A', 'Community B','Community C'), lty = c(1, 2, 3), col = c(1, 2, 3))

 ##### Trait-based approaches #####
