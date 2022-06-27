library(sf)
library(glue)
library(ggplot2)
library(dplyr)

dst_path <- 'C:/ecaas_scripts/'

ej <- st_read(glue(dst_path, 'geoms/ecaas_tiles.geojson'))
ng <- st_read(glue(dst_path, 'geoms/ecaas_tiles_north.geojson'))

s1_footprints <- st_read(glue(dst_path, 'catalogs/s1_footprints_ecaas_2021_full.geojson'))

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
s1_unique_sf %>% View()

#nicfi tiles dissolved
ej_diss <- ej %>% st_union()
ng_diss <- ng %>% st_union()


ng_diss %>% st_bbox()

# northern ghana bounding box
lat = c(-3,0.52)
lon = c(8.9, 11.2)
poly_df = data.frame(lon, lat)
pol <- st_polygon(list(
  cbind(
    poly_df$lat[c(1,1,2,2,1)],
    poly_df$lon[c(1,2,2,1,1)]
  )
)
) %>% st_sfc(., crs=4326)


ej_diss %>% st_bbox()
# ejura tain bounding box
lat = c(-3,-0.6)
lon = c(6.5, 9.1)
poly_df = data.frame(lon, lat)
pol2 <- st_polygon(list(
  cbind(
    poly_df$lat[c(1,1,2,2,1)],
    poly_df$lon[c(1,2,2,1,1)]
  )
)
) %>% st_sfc(., crs=4326)

final_pol <- st_union(pol,pol2)

#plot of bbox, ecaas tiles for ghana
ggplot() + geom_sf(data = final_pol) +
  geom_sf(data = ej) +
  geom_sf(data = ng)

sf_use_s2(FALSE)

# calculate area of footprints intersecting with the bbox
intersect_footprints <- st_intersection(s1_unique_sf, final_pol) %>%
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(title, intersect_area)

# calculate total tile area
s1_unique_sf <- mutate(s1_unique_sf, tile_area = st_area(s1_unique_sf))

# convert into tibbles
s1_unique_sf <- s1_unique_sf %>% as_tibble()
intersect_footprints <- intersect_footprints %>%  as_tibble()

# merge tibbles
s1_unique_sf <- merge(s1_unique_sf, intersect_footprints, by = "title", all.x = TRUE)

# calculate percent of intersected area
s1_unique_sf <- s1_unique_sf %>%
  mutate(coverage = as.numeric(intersect_area/tile_area))

# filter footprints which intersect less than 50%
s1_cropping <- s1_unique_sf %>% filter(coverage < 0.5)

# convert tibble back to sf
s1_cropping_sf <- s1_cropping %>% st_as_sf()

# plot of filtered tiles intersected area
ggplot() + geom_sf(data = s1_cropping_sf$geometry.y) +
  #geom_sf(data = final_pol) +
  geom_sf(data = ej)


s1_cropping_sf$title

# Download from s3
lapply(s1_cropping_sf$title, function(s1_footprint) {
  # s1_footprint = s1_cropping_sf$title[1]
  cmd <- glue('aws s3 sync ',
              's3://activemapper/imagery/sentinel1/level2/',s1_footprint,'/ ',
             #'/data/','s1l2/',s1_footprint,'/')
              'C:/ecaas_scripts/',s1_footprint,'/')
  print(cmd)
  system(cmd)
})







