library(fasterize)
library(raster)
library(terra)
library(dplyr)
library(sf)
library(here)
library(glue)
library(data.table)
library(ggplot2)

# dst path
pth <- "C:/ecaas_scripts/RF/rf_probs/"

# read mosaic of probabilities
rf_in <- glue(pth,"mosaics/ejura_tain_rf_probs_mosaic_2021.tif")
rf_probs <- rast(rf_in)

# threshold on probs
thresh <- 0.65

# convert raster values to 0,1 based on threshold
mz_ri_oth_thresh <- (rf_probs[[1:3]] > thresh)

plot(mz_ri_oth_thresh[[1]],
     col = grey.colors(2, start=0, end=1),
     main = "maize")


# check number of pixels with value 1
crop1 <- raster::raster(mz_ri_oth_thresh[[1]])
crop2 <- raster::raster(mz_ri_oth_thresh[[2]])
crop3 <- raster::raster(mz_ri_oth_thresh[[3]])

cellStats(crop1,sum)
cellStats(crop2,sum)
cellStats(crop3,sum)


plot(crop2, col = grey.colors(2, start=0, end=1))

coords <- xyFromCell(mz_ri_oth_thresh, seq_len(ncell(mz_ri_oth_thresh)))
ndvi <- stack(as.data.frame(getValues(mz_ri_oth_thresh)))
names(ndvi) <- c('value', 'variable')

cellStats(crop2,sum)

ndvi <- cbind(coords, ndvi)
ggplot(ndvi) +
  geom_tile(aes(x, y, fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  coord_equal()

ggplot() + geom_tile(data = crop1, color = "red") +
  geom_tile(data = crop2, color = "blue") +
  geom_tile(data = crop3, color = "green")

# write rasters into tif file
crops <- c('maize', 'rice', 'other')
rs <- lapply( c(1,2,3) , function (x) {
  r <- mz_ri_oth_thresh[[2]]

  writeRaster(r, filename = glue(pth, 'croptypes/', crops[2] ,'_probs', '.tif'),
              overwrite = TRUE,
              wopt = list(gdal = c("COMPRESS=LZW"), datatype='INT1U'))
})


