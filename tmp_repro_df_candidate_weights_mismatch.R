source('R/sample_kmeans.R')
df <- data.frame(a = c(rnorm(20,-2,0.1), rnorm(20,2,0.1)), b = c(rnorm(20,-2,0.1), rnorm(20,2,0.1)))
weights <- rep(1, nrow(df))
cands <- df[c(1, 25), ]
res <- tryCatch(sample_kmeans(input = df, clusters = 2, weights = weights, candidates = cands, seed = 1), error = function(e) e)
if (inherits(res, 'error')) { cat(conditionMessage(res), '\n') } else { print(res$points) }
