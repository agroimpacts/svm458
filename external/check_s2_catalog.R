library(dplyr)
library(sf)
library(lubridate)
library(stringr)
library(aws.s3)
library(dplyr)
library(ggplot2)

# read nicfi tiles geojson
bkt <- 'activemapper'
tiles_nicfi_path <- 'ecaas_2021/geoms/tiles_nghana_nicfi.geojson'
s2l2_path <- 'imagery/sentinel2/level2/'
s2_footprints_path <- 'ecaas_2021/catalogs/s2_footprints_nghana_202102_202201.geojson'
cat_prefix <- "ecaas_2021/catalogs/"

# read nicfi tiles
tiles_nicfi <- s3read_using(st_read, bucket = bkt, object = tiles_nicfi_path)

# read s3 objects from bucket
items <- get_bucket(bucket = bkt,
                    prefix = s2l2_path,
                    max = Inf)

# get unique s2 file names, associated tile id and date
s2_catalog <- do.call(rbind, lapply(c(2:length(items)), function(i) {
  nm <- gsub("/", "", str_extract(items[[i]]$Key, '/SENTINEL2.*_[A-Z|0-9]{6}'))
  tile_id <- str_extract(nm, 'T[0-9]{2}[A-Z]{3}')
  str_date <- lubridate::as_date(substr(nm, 12, 19))
  data.frame(file = nm, tile = tile_id, date = str_date) %>% unique(.)
}))

# clear items object
rm(items)

# read s2 catalog
# for reading single catalog file
s2_geocats <- s3read_using(st_read, bucket = bkt, object = s2_footprints_path)

# for reading multiple catalog file
# get catalog files
cats <- get_bucket_df(
  bkt, prefix = cat_prefix, max = Inf
) %>% select(Key)

# filter the necessary catalogs and read them
cats %>%
  filter(grepl("s2_footprints_nghana", Key)) %>%
  pull(Key) %>%
  lapply(., function(x) {
    s3read_using(st_read, bucket = bkt, object = x)
  }) -> s2_geocats

# combine the catalog into single tibble and get distinct titles
s2_geocat_sf <- do.call(bind_rows, s2_geocats) %>%
  select(id, title, startDate, completionDate, sensorMode, orbitNumber,
         orbitDirection, cloudCover, geometry) %>% unique(.)

# filter images with cloudCover <= 80
# select few necessary columns and arrange by date
s2_cat_df <- s2_geocat_sf %>% mutate(tile = substr(title,39,44)) %>%
  filter(cloudCover <= 80) %>%
  mutate(date = lubridate::as_date(startDate)) %>%
  select(title, date, tile) %>% arrange(date)

# images in s2 catalog for each tile
s2_cat_df %>% group_by(tile) %>% count(.)

# distinct sentinel 2 tiles
s2_tiles_nghana <- s2_cat_df %>% distinct(tile)

# images available in aws s3 for northern ghana
# grouped by tiles
s2_catalog %>% arrange(date) %>%
  filter(date > '2021-02-01') %>%
  group_by(tile) %>% count(.) %>%
  filter (tile %in% s2_tiles_nghana$tile)

# s2 tiles for northern ghana region
s2_tiles_nghana_aws <- s2_catalog %>% distinct(tile) %>%
    filter (tile %in% s2_tiles_nghana$tile)

# list images in order of date acquired for each tile
s2_catalog %>% filter(grepl("T30PZS", file)) %>% arrange(date)

# getting one row from catalog for each tile to plot
s2_geocat_plot <- do.call(rbind, lapply(s2_tiles_nghana_aws[[1]], function(x) {
  s2_geocat_sf %>% filter(grepl(x, title)) %>% .[1,]
})
) %>% st_as_sf()

# plot s2 tiles and nicfi tiles
ggplot() +
  geom_sf(data = s2_geocat_plot$geometry) +
  geom_sf(data = tiles_nicfi$geometry)
