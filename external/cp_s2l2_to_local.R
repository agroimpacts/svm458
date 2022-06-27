# import libraries
library(aws.s3, quietly = T)
library(stringr, quietly = T)
library(dplyr, quietly = T)
library(parallel, quietly = T)
library(sf, quietly = T)
library(lubridate)
library(glue)

# aws configure!

# get Sentinel2 Level-2 imagery from S3 bucket
items <- get_bucket(bucket = 'activemapper',
                    prefix = 'imagery/sentinel2/level2/',
                    max = Inf)

# s3 path to imagery
s3_prefix_from <- 's3://activemapper/imagery/sentinel2/level2'

# ejura tain nicfi tiles geojson
tiles_nicfi <- read_sf('C:\\Users\\svroc\\Documents\\ecaas_scripts\\tiles_nicfi.geojson')

# local path to imagery
local_path_to <- '/data'

# s2 catalog geojsons for dates feb 2021 to jan 2022
s2_catalog_files = c(
  "ecaas_2021/catalogs/s2_footprints_ecaas_2021.geojson",
  "ecaas_2021/catalogs/s2_footprints_ejuratain_2021_12_2022_01.geojson")

# read catalog geojsons
catalog <-
  lapply(s2_catalog_files, function(x) {
    s3read_using(st_read, bucket = "activemapper", object = x)
  })

# combine the catalog into single tibble
s2_geocat_sf <- do.call(bind_rows, catalog) %>%
  select(id, title, startDate, completionDate, sensorMode, orbitNumber,
         orbitDirection) %>% unique(.)


# store catalog in a tibble
s2_cat_df <-  s2_geocat_sf %>% as_tibble(.)

# group by s2 tile ids
s2_cat_df2 <- s2_cat_df %>% mutate(tile = substr(title,39,44)) %>%
  mutate(date = lubridate::as_date(startDate)) %>%
  select(title, date, tile) %>% group_by(tile) %>% count(.)
s2_cat_df2

# identify distinct tiles for ejura tain region
s2_tiles_ejtain <- s2_cat_df2 %>% distinct(tile)
s2_tiles_ejtain

# create data frame using file names and tile ids from s3 bucket items
# filter rows using tile ids for ejura tain region
aws_s3_catalog <- do.call(rbind, lapply(c(2:length(items)), function(i) {
  nm <- basename(items[[i]]$Key)
  tile_id <- str_extract(nm, 'T[0-9]{2}[A-Z]{3}')
  data.frame(file = nm, tile = tile_id)
})) %>% filter(tile %in% s2_tiles_ejtain$tile)

aws_s3_catalog %>%
  group_by(tile) %>% count(.) %>% filter (tile %in% s2_tiles_ejtain$tile)
aws_s3_catalog %>% View()

aws_s3_catalog %>% filter(tile == "T30NYP")

.# download images from s3 to local for certain tiles using AWS CLI
cores <- detectCores() - 1
lapply(unique(aws_s3_catalog$tile)[9:10], function(tile_id) {
  files <-
  mclapply(files, function(nm) {
    cmd <- sprintf('aws s3 cp %s/%s %s/%s ' ,
                   s3_prefix_from, nm, local_path_to, nm)
    system(cmd)
  }, mc.cores = cores)
})

aws_s3_catalog %>%  View()
#plot
plot(s2_geocat_sf[1,]$geometry)
plot(tiles_nicfi$geometry, add = TRUE, border = "red")
ggplot(tiles_nicfi, aes(x=x, y=y, fill=NA)) +
  geom_tile(data = s2_geocat_sf[1,]$geometry)

glue("/data/",
     "Stack_S2_2021/Sentinel2__B2348_.tif")
