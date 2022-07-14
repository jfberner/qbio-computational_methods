# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Script to explore R's graphic capabilities
# For more explanations follow the tutorial at: R/Graphics.rmd
# First version 2022-07-13
# --------------------------------------------------#

##### Libs #####
packages <- c("patchwork", "ggridges")
for (package in packages) {
  if (!package %in% installed.packages()) install.packages(package)
}

library(ggplot2)


##### Read Data #####
comm <- read.csv("./data/raw/cestes/comm.csv") #use ../figs if running directly from the .rmd file in ./docs/
comm_sum <- sort(colSums(comm[, -1]), decreasing = TRUE)
comm_df <- data.frame(sp = 1:length(comm_sum), n = comm_sum)

##### Creating the Graphic #####
# Defining the theme
n_breaks <- hist(comm_df$n, plot = FALSE)$breaks

p <- ggplot(comm_df) +
  geom_point(mapping = aes(x = sp, y = n), size = 3, alpha = 0.5) +
  labs(x = "Species rank", y = "Abundance") +
  scale_y_continuous(breaks = n_breaks) +
  annotate("text", x = 16, y = 126, label = "italic(Bolboschoenus~maritimus)", parse = TRUE, size = 2) +
  annotate("text", x = 16, y = 80, label = "italic(Phalaris~coerulescenss)", parse = TRUE, size = 2) +
  theme_classic()

p #view

##### Exporting #####
ggplot2::ggsave(filename = "./figs/species_abundance_ggplot.png", #use ../figs if running directly from the .rmd file in ./docs/
                dpi = 300,
                units = "in",
                width = 3.5,
                height = 3.5)
