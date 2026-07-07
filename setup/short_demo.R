# Short demo script

library(samplekmeans)
library(terra)

f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)

set.seed(123)

myclusters <- sample_kmeans(
  input = r,
  clusters = 10,
  use_xy = TRUE
  ,
  min_cluster_size = 300
)

table(values(myclusters$clusters))

plot(as.factor(myclusters$clusters))

plot(myclusters$distances)

table(values(myclusters$clusters))



# END
