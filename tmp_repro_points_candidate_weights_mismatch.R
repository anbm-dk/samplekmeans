source('R/sample_kmeans.R')
library(terra)
pts_df <- data.frame(x = c(runif(20,0,1), runif(20,3,4)), y = c(runif(20,0,1), runif(20,3,4)), z = c(rnorm(20,-1,0.1), rnorm(20,1,0.1)))
pts <- terra::vect(pts_df, geom = c('x','y'), crs = 'EPSG:4326')
weights <- rep(1, nrow(pts))
cands <- pts[c(1, 25), ]
res <- tryCatch(sample_kmeans(input = pts, clusters = 2, weights = weights, candidates = cands, seed = 1), error = function(e) e)
if (inherits(res, 'error')) { cat(conditionMessage(res), '\n') } else { print(res$points) }
