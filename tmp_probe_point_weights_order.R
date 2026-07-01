source('R/sample_kmeans.R')
library(terra)
set.seed(104)
pts_df <- data.frame(
  x = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  y = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  z1 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3)),
  z2 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3))
)
pts_input <- terra::vect(pts_df, geom = c('x', 'y'), crs = 'EPSG:4326')
pts_weights <- seq_len(nrow(pts_input)) / nrow(pts_input)
probe <- pts_input
probe$weights <- pts_weights
vals <- terra::values(probe)
print(names(vals))
print(summary(vals))
df <- cbind(terra::crds(probe), terra::values(probe))
print(colnames(df))
print(summary(df[, ncol(df)]))
