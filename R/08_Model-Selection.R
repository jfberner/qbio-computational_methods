# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Model selection
# Tutorial can be found at
# https://scientific-computing.netlify.app/08_statistical_modeling_2/08_tutorial.html
# 20 jul 2022
# jfb
# --------------------------------------------------#
#
library(bbmle)

cuckoo <- read.csv("data/raw/valletta_cuckoo.csv")


h1 <- glm(Beg ~ Mass, data = cuckoo,
          family = poisson(link = log))

h2 <- glm(Beg ~ Mass + Species, data = cuckoo,
          family = poisson(link = log))

h3 <- glm(Beg ~ Mass * Species, data = cuckoo,
          family = poisson(link = log))

h0 <- glm(Beg ~ 0, data = cuckoo,
          family = poisson(link = log))

bbmle::AICtab(h0, h1, h2, h3, base = TRUE, weights = TRUE)

# models with dAIC<2 are equally plausible
