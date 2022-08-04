# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# From taxonomical to functional and phylogenetic diversity in R
# First version 2022-08-03
# jfb
# --------------------------------------------------#

##### Libs #####
library(vegan)
library(cluster)
library(FD)
library(SYNCSA)
library(taxize)
library(dplyr)
library(ade4)
library(phytools)
##### Data #####
comm <- read.csv('data/raw/cestes/comm.csv')
traits <- read.csv('data/raw/cestes/traits.csv')

head(comm)[,1:6]
head(traits)[,1:6]

# You can add rownames instead of having columns:
rownames(comm)[1:6]
rownames(comm) <- paste0("Site", comm[,1]) # really nice trick

comm <- comm[,-1] # remove the sites collumn
head(comm)[,1:6]

# Transform column traits$Sp into the rownames of the dataframe:
rownames(traits) <- paste0(traits[,1])
traits <- traits[,-1]
head(traits)[,1:6]

##### Species Richness #####
richness <- specnumber(comm) # this is a vegan function
# so much easier than on script 13!

##### Taxonomic Diversity #####
shannon <- diversity(comm)
simpson <- diversity(comm, index = 'simpson')

##### Functional Diversity #####
# Taxonomic diversity indices are based on the assumption that species belong to one species or the other.

# This can be thought as a distance matrix between individuals, where individuals of the same species have a distance of zero between them, individuals of different species have a distance of 1 between them.

# When analyzing functional traits the distance between individuals is no longer determined by their belonging to a species, but to their position in the trait space.

# These traits can be continuous, but vary in different scales, or they can be categorical, and appropriate distance measures have to be used to deal with this difference. Gower distance is a common distance metric used in trait-based ecology.

gow <- daisy(traits, metric = 'gower')
gow2 <- gowdis(traits)
# these are two ways of getting the gower distance, the first is from the 'cluster' package (from baseR) and the second from the 'FD' package

# Implementations in R vary and the literature reports extensions and modifications
identical(gow, gow2) # not the same but why? # Also, didn't know this func, super useful!

class(gow) # different classes
class(gow2) # just "dist" class

plot(gow, gow2, asp = 1) # perfect overlap, same values
# In the end its the same, just the class that makes it different, and these classes are created by the function's authors

##### Rao's Quadratic Entropy Calculations #####
# Now using SYNCSA package

tax <- rao.diversity(comm)
fun <- rao.diversity(comm, traits = traits)
#plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
#abline(a = 0, b = 1, col = 'red')

##### Calculating Functional Diversity indices with package FD #####
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F) # x = matrix or dataframe of functional traits, a = matrix containing the abundances of the species in x (or 1-0)

# dbFD implements a flexible distance-based framework to compute multidimensional functional diversity (FD) indices. dbFD returns the three FD indices of Villéger et al. (2008): functional richness (FRic), functional evenness (FEve), and functional divergence (FDiv), as well functional dispersion (FDis; Laliberté and Legendre 2010), Rao's quadratic entropy (Q) (Botta-Dukát 2005), a posteriori functional group richness (FGR) (Petchey and Gaston 2006), and the community-level weighted means of trait values (CWM; e.g. Lavorel et al. 2008).
names(FuncDiv1)

# We can also do the calculation using the traits matrix directly
FuncDiv <- dbFD(x = traits, a = comm, messages = F)
names(FuncDiv) # has one more output than the last, called CWM (see above)

# View(FuncDiv)

##### Species Names and Families #####
# library in this section is taxize
spnames <- read.csv('data/raw/cestes/splist.csv')
# We don't have Families for these species. We'll get them by using the tools from 12_Biodiversity-Databases.R

classification_data <- classification(spnames$TaxonName, db = 'ncbi')

# View(classification.data)
# classification.data[[1]]

# dplyr
example <- classification_data[[1]] %>%
  filter(rank == 'family') %>%
  pull(name) # returns a vector /// select returns a dataframe
# to do this for every species though, you can use the apply family OR a for loop

# turn that into a function!
extract_family_ncbi <- function(x){
  if(!is.null(dim(x))){ # is.na does not work here, but we can use dimensions because the dimensions are NULL for an NA list in classification.data
  y <- x %>%
    filter(rank == 'family') %>%
    pull(name)
  return(y)
  }
}

extract_family_ncbi(classification_data[[4]]) # SO BEAUTIFUL OMG

# using for would look like
families <- list()


for(i in 1:length(classification_data)){
  f <- extract_family_ncbi(classification_data[[i]])
  if(length(f) > 0) families[i] <- f
}

head(families)




# now she wants to use dated trees (APG from Magallón-UNAM) to establish the relationship between these families we have here
# for that, packages ade4 and phytools are needed
# APG <- read.tree('data/ray/R20120829mod.new')
# plot(APG)# There's also ggtree() for this


# trying lapply by myself - really want to learn this family
families_apply <- lapply(classification_data, `[`, is.element(rank, species))
