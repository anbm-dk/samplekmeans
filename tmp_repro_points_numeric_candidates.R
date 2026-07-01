source('R/sample_kmeans.R')
library(terra)
pts_df <- data.frame(x = c(0,0.1,5,5.1), y = c(0,0.1,5,5.1), z = c(-1,-0.9,1,0.9))
pts <- terra::vect(pts_df, geom = c('x','y'), crs = 'EPSG:4326')
res <- tryCatch(sample_kmeans(input = pts, clusters = 2, candidates = c(1,3), seed = 1), error = function(e) e)
if (inherits(res, 'error')) { cat(conditionMessage(res), '\n') } else { print(res$points) }
