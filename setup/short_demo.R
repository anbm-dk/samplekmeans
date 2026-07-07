# Short demo script

library(samplekmeans)
library(terra)

f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)

set.seed(123)

myclusters <- sample_kmeans(
  input = r,
  clusters = 10,
  use_xy = TRUE,
  min_cluster_size = 300,
  sp_pts = TRUE
)

table(values(myclusters$clusters))

plot(as.factor(myclusters$clusters))

plot(myclusters$distances)
plot(myclusters$points, add = TRUE, bg = "white", pch = 21)


# Test 2

mycandidates <- mask(
  r,
  myclusters$clusters,
  maskvalues = 1,
  inverse = FALSE
)

set.seed(123)

myclusters2 <- sample_kmeans(
  input = r,
  clusters = 10,
  use_xy = TRUE,
  min_cluster_size = 300,
  sp_pts = TRUE,
  candidates = mycandidates
)

plot(as.factor(myclusters$clusters))
plot(as.factor(myclusters2$clusters))

plot(myclusters2$distances)
plot(myclusters2$points, add = TRUE, bg = "white", pch = 21)

# END
