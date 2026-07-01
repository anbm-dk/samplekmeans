source('R/sample_kmeans.R')
library(terra)
set.seed(104)
pts_rep_df <- data.frame(
  x = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  y = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  z1 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3)),
  z2 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3))
)
pts_rep_input <- terra::vect(pts_rep_df, geom = c('x', 'y'), crs = 'EPSG:4326')
pts_weights <- seq_len(nrow(pts_rep_input)) / nrow(pts_rep_input)
cat('before names:', paste(names(terra::values(pts_rep_input)), collapse=','), '\n')
first_before <- terra::values(pts_rep_input)[1:3, , drop = FALSE]
print(first_before)
out <- sample_kmeans(input = pts_rep_input, clusters = 3, ncells = 70, weights = pts_weights, seed = 223)
cat('after names:', paste(names(terra::values(pts_rep_input)), collapse=','), '\n')
first_after <- terra::values(pts_rep_input)[1:3, , drop = FALSE]
print(first_after)
cat('identical values:', identical(first_before, first_after), '\n')
