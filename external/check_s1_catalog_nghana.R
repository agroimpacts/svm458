library(dplyr)
library(sf)
library(lubridate)
library(stringr)
library(aws.s3)
library(ggplot2)
library(rgdal)
library(raster)

# on local
tiles_nghana_nicfi <- read_sf(
  'C:\\Users\\svroc\\Documents\\ecaas_scripts\\geoms\\tiles_nghana_nicfi.geojson')

# read nicfi geojson
# on instance
tiles_nghana_geojson <- "ecaas_2021/geoms/tiles_nghana_nicfi.geojson"
tiles_nghana_nicfi <- s3read_using(st_read, bucket = "activemapper", object = tiles_nghana_geojson)

# read s1 catalog geojson for nghana
# on local
s1_footprints <- st_read('C:\\Users\\svroc\\Documents\\ecaas_scripts\\s1_footprints_nghana_2021.geojson')

# on instance
s1_footprints_p1 <- st_read('/home/ubuntu/projects/catalogs/s1_fooprints_nghana_2021_p1.geojson/')


#on local
s1_footprints_p1 <- st_read('C:\\Users\\svroc\\Documents\\ecaas_scripts\\catalogs\\s1_fooprints_nghana_2021_p1.geojson')

# on instance
s1_catalog_geojson <- "ecaas_2021/catalogs/s1_footprints_nghana_2021.geojson"
s1_footprints <- s3read_using(st_read, bucket = "activemapper", object = s1_catalog_geojson)

# combine into a list and select just a few columns you need
s1_geocat_sf <- s1_footprints %>%
  dplyr::select(id, title, startDate, completionDate, sensorMode, orbitNumber,
       orbitDirection, no_geom)

s1_geocat_p1 <- s1_footprints_p1 %>%
  dplyr::select(id, title, startDate, completionDate, sensorMode, orbitNumber,
                orbitDirection, no_geom)


# find unique s1 images
s1_cat_df <-  s1_geocat_sf %>% as_tibble()
s1_unique_cat <- s1_cat_df[!duplicated(s1_cat_df$title), ]
s1_unique_cat %>% View()

unique(s1_geocat_sf$title)

unique(s1_geocat_p1$title)

s1_geocat_p1 %>% group_by(title) %>% summarise()

ggplot() + geom_sf(data = tiles_nghana_nicfi) +
  geom_sf(data = s1_geocat_p1[1,]$geometry)


s1_geocat_p1 %>% filter(no_geom = 0)
s1_cat_df <-  s1_geocat_p1 %>% as_tibble()
s1_unique_cat <- s1_cat_df[!duplicated(s1_cat_df$title), ]

s1_unique_cat %>% View()

s1_unique_cat$title[[1]]

# get list of s1 items for s3 bucket
img_s1l2 <- get_bucket_df(bucket = 'activemapper',
                          prefix = 'imagery/sentinel1/level2/',
                          max = Inf)
path2 <- 'imagery/sentinel1/level2/S1A_IW_GRDH_1SDV_20220130T181932_20220130T181957_041694_04F5F3_4696'
object_exists("s3://activemapper/imagery/sentinel1/level2/S1A_IW_GRDH_1SDV_20220130T181932_20220130T181957_041694_04F5F3_4696/")


aws s3 cp
# check for any missing data
img_s1l2 %>%
  filter(grepl("img|hdr", Key)) %>%
  mutate(imgname = basename(dirname(Key))) %>%
  group_by(imgname) %>% count() %>%
  filter(n > 4)

# total count of items in s3 bucket
items %>%  View()
total_items <- count(img_s1l2)


# extract file names from the list
fnames1 <- unlist(lapply(c(2:total_items[[1]]), function(i) {
  gsub("/", "", str_extract(img_s1l2[i, ]$Key, '/S1.*_[A-Z|0-9]{4}'))
})) %>% unique()

length(fnames1)

count <- 0
lapply(s1_unique_cat$title, function(x) {
 if (x %in% fnames1) {
   print("true")
  } else {
    count <- count + 1
  }
})

# list of tiles intersecting each grid
sf_use_s2(TRUE)
target_s1_tiles <- lapply(1:nrow(tiles_nghana_nicfi), function(x) {  # x <- 5
  g <- tiles_nghana_nicfi %>% slice(x)
  s1_tiles <- s1_geocat_sf %>%
    slice(unlist(st_intersects(g, .))) %>%
    mutate(tiles_nghana_nicfi = x) %>%
    mutate(date = lubridate::as_date(substr(title, 18, 25))) %>%
    dplyr::select(tiles_nghana_nicfi, date, !!names(.))
}) %>% do.call(rbind, .)

tgrids <- unique(target_s1_tiles$no_geom)
tgrids

download_list <- lapply(tgrids, function(x) {
  x <- as_tibble(target_s1_tiles) %>% filter(no_geom == x)
  y <- unique(x$title)
})

download_list %>% View()

save(download_list, file = "C:/Users/svroc/download_list.RData")

load
################################################################################
# Section 2 - determining which tiles need to be downloaded for each target grid
# you can make a list of which tiles need to be downloaded for each target
# NICFI grid, and which tiles can be deleted and are no longer needed on disk
# when that target grid is run.
# However, it might be easiest to get all the tiles (see section 3), as this
# approach will require wrapping the file read/delete process around the
# harmonic creation process
tgrids <- unique(target_s1_tiles$no_geom)
tgrids

download_delete_list <- lapply(tgrids, function(x) {  # x <- tgrids[10]
  current <- as_tibble(target_s1_tiles) %>% filter(no_geom == x)
  future <- as_tibble(target_s1_tiles) %>% filter(no_geom > x) %>%
    dplyr::select(-no_geom) %>% distinct
  past <- as_tibble(target_s1_tiles) %>%
    filter(no_geom < x) %>%
    dplyr::select(-no_geom) %>% distinct

  todownload <- current %>% filter(!title %in% past$title)
  todelete <- past %>% filter(!title %in% c(current$title, future$title))
  tokeep <- past %>% filter(!title %in% todelete$title)

  list("todownload" = todownload, "todelete" = todelete, "tokeep" = tokeep)
})

lapply(1:length(download_delete_list), function(x) {
  unique_downloads <- download_delete_list[[x]]$todownload[!duplicated(download_delete_list[[x]]$todownload$title), ]
  nrow(unique_downloads)
})


lapply(1:length(download_delete_list), function(x) {
  unique_downloads <- download_delete_list[[x]]$todelete[!duplicated(download_delete_list[[x]]$todelete$title), ]
  nrow(unique_downloads)
})




# divide nicfi tiles into 5 parts
n_group <- 5
chunkize <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))
id_groups <- chunkize(1:nrow(tiles_nghana_nicfi), n_group)
id_groups


# find images for each part of nicfi tiles and create catalog
# on instance
dst_path <- '/home/ubuntu/projects/'

library(parallel)

sapply(1:length(id_groups), function(n){
  ply <- tiles_nghana_nicfi %>% slice(id_groups[[n]])
  catalog_new <- do.call(rbind, mclapply(1:nrow(ply), function(n){
    ply_each <- ply %>% slice(n)
    catalog_ply <- s1_geocat_sf %>%
      slice(unique(unlist(st_intersects(ply_each, .)))) %>%
      mutate(no_geom = n - 1)
  }, mc.cores = 8))
  st_write(catalog_new,
           sprintf('%s/catalogs/s1_fooprints_nghana_2021_p%d.geojson',
                   dst_path, n))
  st_write(ply, sprintf('%s/geoms/tile_nghana_p%d.geojson', dst_path, n))
  length(unique(catalog_new$title)) * 5
})



