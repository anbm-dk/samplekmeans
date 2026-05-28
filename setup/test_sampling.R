# Test functions

# library(devtools)
#
# install_github("anbm-dk/samplekmeans", force = TRUE)
#
# library(samplekmeans)

source(paste0(getwd(), "/R/sample_kmeans.R"))

# Test for raster

library(terra)

f <- system.file("ex/elev.tif", package = "terra")
r <- rast(f)

plot(r)

myweights <- r / unlist(global(r, "max", na.rm = TRUE))

myweights <- myweights^2

plot(myweights)

# 6: stop("[", f, "] ", emsg, ..., call. = FALSE)
# 5: error("app", "the number of values returned by 'fun' is not appropriate")
# 4: .local(x, ...)
# 3: terra::app(input, fun = map_clusters_fun)

# Previous error caused by non-native pipe operators has been resolved.
# Error: [app] the number of values returned by 'fun' is not appropriate

source(paste0(getwd(), "/R/sample_kmeans.R"))

myclusters_r <- sample_kmeans(
  input = r,
  clusters = 20,
  ncells = 10000,
  # weights = myweights,
  # use_xy = TRUE,
  # only_xy = TRUE,
  xy_weight = c(1, 1),
  layer_weights = 1
)

plot(myclusters_r$clusters)
points(myclusters_r$points, col = "red", pch = 20)

plot(myclusters_r$distances)
points(myclusters_r$points, col = "red", pch = 20)

plot(r)
points(myclusters_r$points, col = "red", pch = 20)


# # Test with one cluster
#
# myclusters_r <- sample_kmeans(input = r, clusters = 1, use_xy = TRUE)
#
# plot(myclusters_r$clusters)
# points(myclusters_r$points)


# Test for points

f <- system.file("ex/lux.shp", package = "terra")
f
v <- vect(f)
v

library(tidyterra)

v2 <- v |>
  centroids() |>
  select(c("AREA", "POP"))

source(paste0(getwd(), "/R/sample_kmeans.R"))

myclusters_v <- sample_kmeans(v2, use_xy = TRUE)

myclusters_v

plot(v2)
points(myclusters_v$points, col = "red", pch = 20)


# Test for data frame

library(datasets)
data(iris)

print(iris)

source(paste0(getwd(), "/R/sample_kmeans.R"))

myclusters_df <- sample_kmeans(
  iris[, 1:4],
  pca = TRUE
  # ,
  # weights = iris$Petal.Width
)

myclusters_df

iris |>
  mutate(
    cluster = myclusters_df$clusters
  )

# Test candidates for raster input (candidates raster)

r_cand <- myweights

r_cand[r_cand < 0.3] <- NA

myclusters_r_cand <- sample_kmeans(
  input = r,
  clusters = 20,
  weights = myweights,
  use_xy = TRUE,
  # only_xy = TRUE,
  xy_weight = c(1, 2),
  layer_weights = 1,
  candidates = r_cand
)

plot(r_cand)
points(myclusters_r_cand$points, col = "red", pch = 20)

plot(r_cand)
points(myclusters_r$points, col = "red", pch = 20)

# Test candidates for raster input (candidates points)

source(paste0(getwd(), "/R/sample_kmeans.R"))

myclusters_r_cand_pts <- sample_kmeans(
  input = r,
  clusters = 5,
  # weights = myweights,
  use_xy = TRUE,
  # only_xy = TRUE,
  xy_weight = c(1, 2),
  layer_weights = 1,
  candidates = v2
)

plot(r)
points(v2)
points(myclusters_r_cand_pts$points, col = "red", pch = 20)

# END
