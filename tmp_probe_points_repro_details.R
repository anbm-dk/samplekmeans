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
out1 <- sample_kmeans(input = pts_input, clusters = 3, ncells = 70, weights = pts_weights, seed = 223)
out2 <- sample_kmeans(input = pts_input, clusters = 3, ncells = 70, weights = pts_weights, seed = 223)
cat('points identical:', identical(out1$points, out2$points), '\n')
cat('clusters identical:', identical(out1$clusters, out2$clusters), '\n')
cat('distances identical:', identical(out1$distances, out2$distances), '\n')
print(out1$points)
print(out2$points)
print(table(out1$clusters, out2$clusters))
cat('max distance delta:', max(abs(out1$distances - out2$distances)), '\n')
