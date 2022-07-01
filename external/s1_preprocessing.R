library(sf)
library(glue)
library(ggplot2)
library(dplyr)
library(aws.s3)
library(terra)
library(stringr)

library(rgdal)

dst_path <- 'ecaas_2021/'
bkt <- 'activemapper'

# read geojson from local
#ej <- st_read(glue(dst_path, 'geoms/ecaas_tiles.geojson'))
#ng <- st_read(glue(dst_path, 'geoms/ecaas_tiles_north.geojson'))

# read geojson from aws s3 bucket
ej_tiles_aws_path <- glue(dst_path, 'geoms/ecaas_tiles.geojson')
ng_tiles_aws_path <- glue(dst_path, 'geoms/ecaas_tiles_north.geojson')

ej <- s3read_using(st_read, bucket = bkt, object = ej_tiles_aws_path)
ng <- s3read_using(st_read, bucket = bkt, object = ng_tiles_aws_path)

# read footprints from local
#s1_footprints <- st_read(glue(dst_path, 'catalogs/s1_footprints_ecaas_2021_full.geojson'))

# read footprints from aws s3 bucket
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
#s1_unique_sf %>% View()

#nicfi tiles dissolved
ej_diss <- ej %>% st_union()
ng_diss <- ng %>% st_union()


ng_diss %>% st_bbox()

# northern ghana bounding box
lat = c(-3,0.52)
lon = c(8.8, 11.2)
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
lat2 = c(-3,-0.6)
lon2 = c(6.5, 9.1)
poly_df = data.frame(lon2, lat2)
pol2 <- st_polygon(list(
  cbind(
    poly_df$lat2[c(1,1,2,2,1)],
    poly_df$lon2[c(1,2,2,1,1)]
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
ggplot() + geom_sf(data = s1_cropping_sf$geometry.x) +
  #geom_sf(data = final_pol) +
  geom_sf(data = ej)

tiles_doble <- do.call(rbind, lapply(ej$geometry, function(x) {

  lapply(s1_cropping_sf, function(y) {
    st_intersects(y,x)
  })
  list_of_intersects <- st_intersects(,x)
  if (nrow(list_of_intersects) > 100) {
    return(x)
  } else {
    return(NULL)
  }
}))

ggplot() + geom_sf(data = tiles_doble$geometry) +
  geom_sf(data = ej)

#s1_cropping_sf$title

# Download from s3
lapply(s1_cropping_sf$title[1:21], function(s1_footprint) {
  # s1_footprint = s1_cropping_sf$title[1]
  cmd <- glue('aws s3 sync ',
              's3://activemapper/imagery/sentinel1/level2/',s1_footprint,'/ ',
             '/data/','s1l2/',s1_footprint,'/')
              #'C:/ecaas_scripts/',s1_footprint,'/')
  print(cmd)
  system(cmd)
})


list_of_files <- s1_cropping_sf$title

s1_crop_list <-list.dirs(path = '/data/s1l2', full.names = TRUE, recursive = FALSE)

# s1_crop_list_df <- s1_crop_list %>% as_tibble()
# lapply(list_of_files, function(x) {
#   s1_crop_list_df %>% filter(grepl(x,s1_crop_list_df$value))
# })

vv_name <- "Sigma0_VV_db.img"
vv_hdr <- gsub("img", "hdr", vv_name)
vh_name <- "Sigma0_VH_db.img"
vh_hdr <- gsub("img", "hdr", vh_name)


sf_use_s2(FALSE)
terraOptions(mefrac = 0.9, tempdir = "/data/tmp/")

lapply(s1_crop_list, function(s1_footprint) {

  ## CROP VV raster to BBOX
  vv_rst <- terra::rast(glue("{s1_footprint}/{vv_name}"))
  #rgdal::GDALinfo(glue("{s1_footprint}/{vv_name}"))

  final_pol_newcrs <- final_pol %>% st_transform(crs = terra::crs(vv_rst))

  new_tile_bound <- terra::ext(vv_rst) %>% as.polygons(.) %>%
    st_as_sf() %>%
    st_set_crs(terra::crs(vv_rst)) %>%
    st_intersection(final_pol_newcrs)

  vv_crop_rst <- terra::crop(vv_rst, new_tile_bound)

  message("..writing cropped raster vv")
  vv_fnm <- glue("{s1_footprint}/{vv_name}")

  terra::writeRaster(vv_crop_rst, filename = vv_fnm, overwrite = TRUE)

  #rgdal::GDALinfo(glue("/s1_footprint/{vv_name}"))

  rm(vv_rst, vv_crop_rst)


  ## CROP VH raster to BBOX
  vh_rst <- terra::rast(glue("{s1_footprint}/{vh_name}"))

  new_tile_bound <- terra::ext(vh_rst) %>% as.polygons(.) %>%
    st_as_sf() %>%
    st_set_crs(terra::crs(vh_rst)) %>%
    st_intersection(final_pol_newcrs)

  vh_crop_rst <- terra::crop(vh_rst, final_pol_newcrs)

  message("..writing cropped raster vh")
  vh_fnm <- glue("{s1_footprint}/{vh_name}")

  #rgdal::GDALinfo(glue("{s1_footprint}/{vh_name}"))

  terra::writeRaster(vh_crop_rst, filename = vh_fnm, overwrite = TRUE)

  rm(vh_rst, vh_crop_rst)
  gc()

})






