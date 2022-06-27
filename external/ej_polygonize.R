library(sf)
library(dplyr)
library(ecaascrops)
library(terra)
library(fs)
library(rmapshaper)

tiles <- st_read('C:/ecaas_scripts/geoms/tiles_nicfi.geojson')

shh <- function(x) suppressMessages(suppressWarnings(x))

out_dir <- "C:/ecaas_scripts/RF/polygons"
if(!dir.exists(out_dir)) dir.create(out_dir)


log_file <- "C:/ecaas_scripts/logs/log"
log_con <- file(log_file, open = "w+")

polygonize_labels <- function(segment_path, shrink_buffer, expand_buffer,
                              keep) {
  segs <- rast(segment_path)
  segs[segs == 0] <- NA
  segs_polys <- st_as_sf(as.polygons(segs))

  sf_use_s2(FALSE)
  polys_buf <- st_buffer(segs_polys, dist = shrink_buffer)
  polys_cast <- do.call(rbind, lapply(1:nrow(polys_buf), function(x) {
    pol <- st_cast(polys_buf[x, ], "POLYGON")
    pol
  }))

  polys_buf2 <- polys_cast %>%
    mutate(id = 1:nrow(.)) %>%
    dplyr::select(id) %>%
    st_buffer(dist = expand_buffer)

  # simplify
  polys_simpl <- rmapshaper::ms_simplify(polys_buf2, keep = keep)

  # difference
  polys_dif <- st_difference(st_make_valid(polys_simpl))
  return(polys_dif)
}


f <- glue::glue("C:/ecaas_scripts/RF/rf_probs/labeled_predictions/",
                "other_probs_labeled.tif")
r <- rast(f)

pols <- lapply(1:nrow(tiles), function(x) {

  # run this if labelled image is UTM

  tempf <- paste0(tempfile(), ".tif")
  rcrop <- crop(r, vect(st_transform(tiles[x, ], st_crs(r))), filename = tempf)

  if(!is.na(unlist(global(rcrop, fun = max, na.rm = TRUE)))) {
    f1 <- basename(tempf)
    msg <- glue::glue("Polygonizing {f}")
    cat(msg, file = log_file, sep = "\n", append = TRUE)

    label_polys <- shh(
      polygonize_labels(tempf, shrink_buffer = -0.00005,
                        expand_buffer = 0.00006, keep = 0.25)
    )
    file.remove(tempf)

    out_file <- file.path(out_dir, gsub(".tif", ".geojson", f1))
    shh(st_write(label_polys, delete_dsn = TRUE, dsn = out_file))

    msg <- glue::glue("Wrote {out_file} with {nrow(label_polys)} fields")
    cat(msg, file = log_file, sep = "\n", append = TRUE)
    cat("\n", file = log_file, append = TRUE)


  } else {
    label_polys <- "No polys"
  }

})

