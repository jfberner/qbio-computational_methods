# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Diversity Metrics I
# First version 2022-07-27
# jfb
# --------------------------------------------------#
library(dplyr)

comm <- read.csv("data/raw/cestes/comm.csv")
dim(comm)
head(comm[,1:6])
tail(head)

# Exercises
# Which are the 5 most abundant species overall in the dataset?
colSums(comm) %>% sort(decreasing = T)
# How many species are there in each plot? (Richness)
comm2 <- read.csv("data/raw/cestes/comm.csv", row.names = 1)
rich <- ifelse(comm2 > 0,1,0)
richness <- rowSums(rich)
# Which the species that is most abundant in each plot?
comm2 <- read.csv("data/raw/cestes/comm.csv", row.names = 1)
colnames(comm2)[max.col(comm2, ties.method = "first")]



# Creating Functions for Shannon Diversity Index, Simpson's Diversity Index and Inverse Simpson
shannon <- function()



simpson <- function()



inverse.simpson <- function()
