source('R/sample_kmeans.R')
library(terra)
r <- rast(nrows=10,ncols=10,xmin=0,xmax=10,ymin=0,ymax=10)
values(r) <- rep(c(rep(NA,50), rnorm(50)), 1)
input_r <- c(r, r); names(input_r) <- c('v1','v2')
pts <- vect(data.frame(x=c(1,8), y=c(1,8), cw=c(1,2)), geom=c('x','y'), crs=crs(input_r))
res <- tryCatch(sample_kmeans(input = input_r, clusters = 1, ncells = 10, candidates = pts, candidate_weight_col = 'cw', seed = 1), error = function(e) e)
if (inherits(res, 'error')) { cat(conditionMessage(res), '\n') } else { print(res$points) }
