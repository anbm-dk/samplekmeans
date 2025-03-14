# Test functions

library(devtools)

install_github("anbm-dk/samplekmeans", force = TRUE)

library(samplekmeans)

# Test for raster

library(terra)

f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)

plot(r)



# 6: stop("[", f, "] ", emsg, ..., call. = FALSE)
# 5: error("app", "the number of values returned by 'fun' is not appropriate")
# 4: .local(x, ...)
# 3: terra::app(input, fun = map_clusters_fun)

# Error in df %<>% as.data.frame : could not find function "%<>%"
# Error: [app] the number of values returned by 'fun' is not appropriate

library(magrittr)

myclusters_r <- sample_kmeans(input = r, use_xy = TRUE)

plot(myclusters_r$clusters)
points(myclusters_r$points)

# Test with one cluster

myclusters_r <- sample_kmeans(input = r, clusters = 1, use_xy = TRUE)

plot(myclusters_r$clusters)
points(myclusters_r$points)

# Test for points

f <- system.file("ex/lux.shp", package="terra")
f
v <- vect(f)
v

library(tidyterra)

v2 <- v %>% centroids() %>% select(c("AREA",   "POP"))

getwd() %>% paste0("/R/sample_kmeans.R") %>% source()

myclusters_v <- sample_kmeans(v2, use_xy = TRUE)

myclusters_v

plot(v2)
points(myclusters_v$points, col = "red")


# END
