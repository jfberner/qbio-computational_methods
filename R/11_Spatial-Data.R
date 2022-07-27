# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Spatial Data in R
# Andrea's First class :)
# First version 2022-07-26
# jfb
# --------------------------------------------------#

##### Libs & Data #####
library(sf)
library(raster)
library(tmap) #build thematic maps in R
library(dplyr)
library(rnaturalearth)
library(rnaturalearthhires)


bra <- ne_states(country = "brazil", returnclass = "sf")
data("World")

##### sf Objects #####

# package tmap has a syntax similar to ggplot. The functions start all with tm_
tm_shape(World) +
  tm_borders()

#head(World)
names(World)
class(World)
dplyr::glimpse(World)

plot(World[1])
plot(World[,14])
plot(World[1,])
plot(World["pop_est"])

# A key difference between data frames and sf objects is the presence of geometry, that looks like a column when printing the summary of any sf object

head(World[, 1:4])

# When calling World$geometry, however, we perceive it’s not a single column. Actually, geometries are a class in their own, sfc
class(World)
class(World$geometry)

# We don’t need to understand the deep structure of geometry right now, the sf package has extensive vignettes you can explore. For practical purposes, the existence of geometries and sf objects like data.frames facilitates a lot the manipulation of sf objects, including extracting and assigning geometries to existant data frames and the possibility to drop the geometries, (literally, function sf::st_drop_geometry() and use the data frame)
head(sf::st_coordinates(World))

no_geom <- sf::st_drop_geometry(World)
class(no_geom)

#bounding boxes
st_bbox(World)

##### Manipulating sf Objects #####
# For practical purposes, sf objects work like data.frames or tibbles, you can subset, filter, merge, join, and create new variables.

# For example, imagine I want to plot only countries in South America or have different colors for them

# You can filter depending on the columns, for example, all the countries in South America:
names(World)
unique(World$continent)

World %>%
  filter(continent == "South America") %>%
  tm_shape() +
  tm_borders()

# You can also create new variables and use them in your maps:
World %>%
  mutate(our_countries = if_else(iso_a3 %in% c("COL","BRA", "MEX"), "red", "grey")) %>%
  tm_shape() +
  tm_borders() +
  tm_fill(col = "our_countries") +
  tm_add_legend("fill",
                "Countries",
                col = "red")

# sp objects are going to be replaced with sf objects and packages rgdal, rgeos will be archived by the end of 2023. However, it is extremely possible that the data and tutorials you find use sp objects rather than sf and that you may have to go transform them into sf objects (function sf::st_as_sf().



##### Loading, Plotting and Saving a Raster/Shapefile from the disk #####
plot(bra)
# Shapefiles
dir.create("data/shapefiles", recursive = TRUE)

st_write(obj = bra, dsn = "data/shapefiles/bra.shp", delete_layer = TRUE)

bra2 <- read_sf("data/shapefiles/bra.shp")

class(bra)
class(bra2)

plot(bra)
plot(bra2)

# Rasters
dir.create(path = "data/raster/", recursive = TRUE)
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10, path = "data/raster/")

is(tmax_data) #the data are a raster stack, several rasters piled
dim(tmax_data)
extent(tmax_data)
res(tmax_data)


# This is really the tip of the iceberg. Choose your own adventure:

# Check the vignettes in sf
# Check the tutorials in rspatialdata
# Check the multiple CRAN TaskViews regarding spatial data
