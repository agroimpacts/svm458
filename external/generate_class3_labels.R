library(dplyr)
library(sf)
library(glue)

dst_path <- "C:/ecaas_scripts/RF/polygons/"
polygon_files <- list.files(path = dst_path)
tiles_nicfi <- st_read("C:/ecaas_scripts/geoms/tiles_nicfi.geojson")


poly <- st_read(glue(dst_path, polygon_files[1]))
#a <- tiles_nicfi[unique(st_intersects(polys, tiles_nicfi))[[1]],]$tile
#length(a)
#a %>%  View()

tiles_nicfi <- st_transform(tiles_nicfi, st_crs(poly))
#st_crs(polys)
#st_crs(tiles_nicfi)

crop <- "Other"
all_polys <- do.call(rbind, lapply(polygon_files, function(f) {
  polys <- st_read(glue(dst_path, f))
  nicfi_tile <- tiles_nicfi[unique(st_intersects(polys, tiles_nicfi))[[1]],]$tile
  polys <- polys %>% mutate(class = crop)
  if (length(nicfi_tile) == 1) {
    polys <- polys %>% mutate(bmap_tile = nicfi_tile)
  } else {
    polys <- polys %>% mutate(bmap_tile = "NA")
  }
}))

all_polys <- all_polys %>% mutate(id = seq.int(1, nrow(all_polys)))
all_polys <- all_polys %>% mutate(area = st_area(all_polys))
all_polys %>% View()


set.seed(1)
sample_400_polys <- all_polys %>% sample_n(size = 400)

st_write(sample_400_polys, "C:/ecaas_scripts/class3_labels/other.geojson")

st_write(all_polys, "C:/ecaas_scripts/class3_labels/all_other.geojson")


maize_class3 <- st_read("C:/ecaas_scripts/class3_labels/maize.geojson")
rice_class3 <- st_read("C:/ecaas_scripts/class3_labels/all_rice.geojson")
other_class3 <- st_read("C:/ecaas_scripts/class3_labels/other.geojson")


class3_labels <- rbind(maize_class3, rice_class3, other_class3)

class3_labels <- class3_labels %>% filter(bmap_tile != "NA")


class3_labels <- class3_labels %>% mutate(id = seq.int(1, nrow(class3_labels)))
class3_labels %>%  View()

st_write(class3_labels, "C:/ecaas_scripts/class3_labels/class3_field.geojson")


