# Add country outline code

# Set file directory
filedir <- "/home/matt/Documents/"

# Load libraries
library(dplyr)
library(sf)

# Load countries shapefile
#outlinehigh <- ggmap2::getData("countries", path=paste0(filedir, "GADM"))
#outlinehigh <- sf::st_as_sf(outlinehigh)

# Subset data by continent
#africa <- outlinehigh[outlinehigh$UNREGION2 == "Africa",]

#outlinehigh <- rgeos::gPolygonize(rgeos::gNode(as(outlinehigh, "SpatialLines")))
#outlinehigh <- rgeos::gUnaryUnion(outlinehigh)

# Combine the individual polygons to one
#outlinehigh <- sf::st_geometry(outlinehigh)
#outlinehigh <- sf::st_cast(outlinehigh, "MULTILINESTRING")
#outlinehigh <- sf::st_union(outlinehigh)

# Transform outline to wgs84
#outlinehigh <- sf::st_transform(outlinehigh, 4326)

# Check if European countries desolve correctly
#plot(outlinehigh, xlim = c(-11, 43), ylim = c(34, 68), asp = 1)

# Save outline to file
#save(outlinehigh, file="data/outlinehigh.rda", compress="xz")

# Load countries shapefile
data(countries10, package="rnaturalearthhires")

#data(countriesHigh, package="rworldxtra", envir = environment())
countries10 <- sf::st_as_sf(countries10) %>% st_wrap_dateline()
plot(st_geometry(countries10))

# Run this
outline <- sf::st_union(countries10)
outline <- st_wrap_dateline(outline)

# Check if dateline is correct
plot(st_geometry(outline))

# Check if European countries desolve correctly
plot(outline, xlim = c(-11, 43), ylim = c(34, 68), asp = 1)

# Save outline to file
save(outline, file="data/outline.rda", compress="xz")

# Load data
data(countries10, package="rnaturalearthhires")
countries10 <- sf::st_as_sf(countries10) %>% st_wrap_dateline()

continents <- countries10 %>% group_by(CONTINENT) %>% summarise()
continents <- continents %>% st_wrap_dateline()
plot(continents)

save(continents, file="data/continents.rda", compress="xz")

### GADM Islands

# The input file geodatabase
#tmp <- tempfile(fileext=".zip")
#download.file("https://www.dropbox.com/s/zkn8ebwf6wm3afj/GADM_islands_Weigelt_etal.zip?dl=0", destfile = tmp)
#dat <- unzip(tmp, junkpaths=T)
#islands <- st_read(dat[grep(dat, pattern=".dbf$")])
islands <- st_read(paste0(filedir, "GADM/GADM_islands_Weigelt_etal/GADM_islands_join_names_area_gmmc_dist.dbf"))
colnames(islands)
largeislands <- dplyr::filter(islands, Area > 5)
smallislands <- dplyr::filter(islands, Area <= 5)

save(largeislands, file="data/largeislands.rda", compress="xz")
save(smallislands, file="data/smallislands.rda", compress="xz")
