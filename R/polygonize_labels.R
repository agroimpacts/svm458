#' Polygonize labelled predictions
#' @description Takes labelled predictions and converts them to polygons,
#' breaking them up and simplifying them in the process
#' @param segment_path Path to geotiff containing labels
#' @param shrink_buffer Negative buffer to reduce polygons by
#' @param expand_buffer Postive buffer to grow polygons by after shrinking
#' @param keep A proportion of vertices to keep when simplifying polygons
#' @details This function assumes a GCS
#' @export
#' @importFrom terra rast as.polygons
#' @importFrom sf st_as_sf st_cast st_buffer st_difference sf_use_s2
#' @importFrom dplyr %>%
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
