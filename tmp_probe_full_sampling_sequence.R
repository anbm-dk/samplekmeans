source('R/sample_kmeans.R')
library(terra)

f <- system.file('ex/elev.tif', package = 'terra')
r <- terra::rast(f)
out_r <- sample_kmeans(input = r, clusters = 5, ncells = 1500, use_xy = TRUE, seed = 101)
myweights <- r / unlist(terra::global(r, 'max', na.rm = TRUE)); myweights <- myweights^2
r_cand <- myweights; r_cand[r_cand < 0.3] <- NA
out_r_cand <- sample_kmeans(input = r, clusters = 5, ncells = 1500, weights = myweights, use_xy = TRUE, xy_weight = c(1,2), layer_weights = 1, candidates = r_cand, seed = 102)
cand_pts <- terra::spatSample(r, size = 40, method = 'random', as.points = TRUE, na.rm = TRUE)
cand_vals <- terra::extract(r, cand_pts, ID = FALSE)
cand_xy <- terra::crds(cand_pts)
candidates_sv <- terra::vect(data.frame(x = cand_xy[,1], y = cand_xy[,2], lyr1 = cand_vals[[1]]), geom = c('x', 'y'), crs = terra::crs(r))
out_r_pts <- sample_kmeans(input = r, clusters = 4, ncells = 1200, use_xy = TRUE, candidates = candidates_sv, seed = 103)
set.seed(104)
pts_df <- data.frame(
  x = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  y = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  z1 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3)),
  z2 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3))
)
pts_input <- terra::vect(pts_df, geom = c('x', 'y'), crs = 'EPSG:4326')
out_pts <- sample_kmeans(input = pts_input, clusters = 3, use_xy = TRUE, seed = 104)
data(iris)
out_df <- sample_kmeans(input = iris[,1:4], clusters = 3, pca = TRUE, seed = 105)
df_rep <- data.frame(a = c(rnorm(60, -2, 0.2), rnorm(60, 2, 0.2)), b = c(rnorm(60, -2, 0.2), rnorm(60, 2, 0.2)))
df_rep_1 <- sample_kmeans(input = df_rep, clusters = 2, ncells = 50, seed = 222)
df_rep_2 <- sample_kmeans(input = df_rep, clusters = 2, ncells = 50, seed = 222)
set.seed(104)
pts_rep_df <- data.frame(
  x = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  y = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  z1 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3)),
  z2 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3))
)
pts_rep_input <- terra::vect(pts_rep_df, geom = c('x', 'y'), crs = 'EPSG:4326')
pts_weights <- seq_len(nrow(pts_rep_input)) / nrow(pts_rep_input)
pts_rep_1 <- sample_kmeans(input = pts_rep_input, clusters = 3, ncells = 70, weights = pts_weights, seed = 223)
pts_rep_2 <- sample_kmeans(input = pts_rep_input, clusters = 3, ncells = 70, weights = pts_weights, seed = 223)
cat('points identical:', identical(pts_rep_1$points, pts_rep_2$points), '\n')
cat('clusters identical:', identical(pts_rep_1$clusters, pts_rep_2$clusters), '\n')
cat('distances identical:', identical(pts_rep_1$distances, pts_rep_2$distances), '\n')
print(pts_rep_1$points)
print(pts_rep_2$points)
print(table(pts_rep_1$clusters, pts_rep_2$clusters))
cat('max distance delta:', max(abs(pts_rep_1$distances - pts_rep_2$distances)), '\n')
