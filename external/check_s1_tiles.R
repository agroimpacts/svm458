library(sf)
library(glue)
library(ggplot2)
library(dplyr)
library(aws.s3)
library(terra)
library(stringr)

dst_path <- 'ecaas_2021/'
bkt <- 'activemapper'
ej_tiles_aws_path <- glue(dst_path, 'geoms/ecaas_tiles.geojson')

ej <- s3read_using(st_read, bucket = bkt, object = ej_tiles_aws_path)

footprints_aws_path <- glue(dst_path, 'catalogs/s1_footprints_ecaas_2021_full.geojson')
s1_footprints <- s3read_using(st_read, bucket = bkt, object = footprints_aws_path)

# combine into a list and select just a few columns you need
s1_geocat_sf <- s1_footprints %>%
  dplyr::select(id, title, startDate, completionDate, sensorMode, orbitNumber,
                orbitDirection, no_geom, geometry)

# convert to tibble
s1_cat_df <-  s1_geocat_sf %>% as_tibble()

# find unique s1 images
s1_unique_cat <- s1_cat_df[!duplicated(s1_cat_df$title), ]
#s1_unique_cat %>% View()

# convert tibble back to sf object
s1_unique_sf <- s1_unique_cat %>% st_as_sf()

s1_tiles <-list.dirs(path = '/data/sentinel1_time_series', full.names = TRUE, recursive = FALSE)


tile_size <- do.call(rbind, lapply(s1_tiles, function (tile) {
  cmd <- glue("du -sh ", tile)
  x <- system(cmd, intern = TRUE)
}))

tile_size_df <- do.call(rbind, lapply(tile_size[,1], function(each_tile) {
  size <- sub("M\t/data.*", "", each_tile)
  tile <- sub(".*sentinel1_time_series/", "", each_tile)
  data.frame(size = size, tile = tile)
}))

tiles_less_images <- tile_size_df %>% filter(as.integer(size) < 150)

tile_size_df %>% View()

tiles_of_interest <- ej %>%  filter(tile %in% tiles_less_images$tile)

ggplot() + geom_sf(data = s1_unique_sf) +
  geom_sf(data = ej) +
  geom_sf(data = tiles_of_interest, color = "red")
