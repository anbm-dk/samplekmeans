source('R/sample_kmeans.R')
df <- data.frame(a = c(-1,-0.9,0.9,1), b = c(-1,-0.8,0.8,1))
res <- tryCatch(sample_kmeans(input = df, clusters = 2, candidates = c(1,3), seed = 1), error = function(e) e)
if (inherits(res, 'error')) { cat(conditionMessage(res), '\n') } else { print(res$points) }
