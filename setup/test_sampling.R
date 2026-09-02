# Focused smoke tests for core sampling modes

source(file.path(getwd(), "R", "sample_kmeans.R"))

library(terra)

assert_true <- function(cond, msg) {
  if (!isTRUE(cond)) {
    stop(msg, call. = FALSE)
  }
}

assert_basic_output <- function(out, label) {
  assert_true(is.list(out), paste0(label, ": output is not a list."))
  assert_true(!is.null(out$clusters), paste0(label, ": missing clusters."))
  assert_true(!is.null(out$distances), paste0(label, ": missing distances."))
  assert_true(!is.null(out$points), paste0(label, ": missing points."))
}

assert_same_sample <- function(out1, out2, label) {
  assert_true(
    identical(out1$points, out2$points),
    paste0(label, ": selected points differ across repeated seeded runs.")
  )
  assert_true(
    identical(out1$clusters, out2$clusters),
    paste0(label, ": cluster assignments differ across repeated seeded runs.")
  )
  assert_true(
    identical(out1$distances, out2$distances),
    paste0(label, ": distances differ across repeated seeded runs.")
  )
}

assert_error <- function(expr, pattern, label) {
  err <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) e
  )
  assert_true(!is.null(err), paste0(label, ": expected an error."))
  assert_true(
    grepl(pattern, conditionMessage(err), fixed = TRUE),
    paste0(
      label,
      ": error message mismatch. Got: ",
      conditionMessage(err)
    )
  )
}

cat("Running focused sampling smoke tests...\n")

# 1) raster input smoke test
f <- system.file("ex/elev.tif", package = "terra")
r <- terra::rast(f)

out_r <- sample_kmeans(
  input = r,
  clusters = 5,
  ncells = 1500,
  use_xy = TRUE,
  seed = 101
)

assert_basic_output(out_r, "raster smoke test")
assert_true(
  inherits(
    out_r$clusters, "SpatRaster"
  ),
  "raster smoke test: clusters should be SpatRaster."
)
assert_true(
  inherits(
    out_r$distances, "SpatRaster"
  ),
  "raster smoke test: distances should be SpatRaster."
)
assert_true(
  is.data.frame(out_r$points),
  "raster smoke test: points should be data.frame."
)
assert_true(
  nrow(out_r$points) >= 1,
  "raster smoke test: expected at least one selected center."
)

cat("PASS: raster smoke test.\n")

# 2) raster input with raster weights and raster candidates
myweights <- r / unlist(terra::global(r, "max", na.rm = TRUE))
myweights <- myweights^2

r_cand <- myweights
r_cand[r_cand < 0.3] <- NA

out_r_cand <- sample_kmeans(
  input = r,
  clusters = 5,
  ncells = 1500,
  weights = myweights,
  use_xy = TRUE,
  xy_weight = c(1, 2),
  layer_weights = 1,
  candidates = r_cand,
  seed = 102
)

assert_basic_output(out_r_cand, "raster weighted+candidates smoke test")
assert_true(
  nrow(out_r_cand$points) >= 1,
  paste(
    "raster weighted+candidates smoke test:",
    "expected at least one selected center."
  )
)

cat("PASS: raster weighted + raster candidates smoke test.\n")

# 3) raster input with SpatVector point candidates
cand_pts <- terra::spatSample(
  r,
  size = 40,
  method = "random",
  as.points = TRUE,
  na.rm = TRUE
)
cand_vals <- terra::extract(r, cand_pts, ID = FALSE)
cand_xy <- terra::crds(cand_pts)

candidates_sv <- terra::vect(
  data.frame(
    x = cand_xy[, 1],
    y = cand_xy[, 2],
    lyr1 = cand_vals[[1]]
  ),
  geom = c("x", "y"),
  crs = terra::crs(r)
)

out_r_pts <- sample_kmeans(
  input = r,
  clusters = 4,
  ncells = 1200,
  use_xy = TRUE,
  candidates = candidates_sv,
  seed = 103
)

assert_basic_output(out_r_pts, "raster + point candidates smoke test")
assert_true(
  nrow(out_r_pts$points) >= 1,
  "raster + point candidates smoke test: expected at least one selected center."
)

cat("PASS: raster + point candidates smoke test.\n")

# 4) points input smoke test
set.seed(104)
pts_df <- data.frame(
  x = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  y = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  z1 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3)),
  z2 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3))
)

pts_input <- terra::vect(pts_df, geom = c("x", "y"), crs = "EPSG:4326")

out_pts <- sample_kmeans(
  input = pts_input,
  clusters = 3,
  use_xy = TRUE,
  seed = 104
)

assert_basic_output(out_pts, "points smoke test")
assert_true(
  length(out_pts$clusters) == nrow(pts_df),
  "points smoke test: cluster assignment length mismatch."
)
assert_true(
  length(out_pts$distances) == nrow(pts_df),
  "points smoke test: distance length mismatch."
)

cat("PASS: points smoke test.\n")

# 5) data.frame input smoke test
data(iris)

out_df <- sample_kmeans(
  input = iris[, 1:4],
  clusters = 3,
  pca = TRUE,
  seed = 105
)

assert_basic_output(out_df, "data.frame smoke test")
assert_true(
  length(out_df$clusters) == nrow(iris),
  "data.frame smoke test: cluster assignment length mismatch."
)
assert_true(
  length(out_df$distances) == nrow(iris),
  "data.frame smoke test: distance length mismatch."
)

cat("PASS: data.frame smoke test.\n")

# 6) data.frame sampling is reproducible when ncells triggers
# pre-kmeans sampling
df_rep <- data.frame(
  a = c(rnorm(60, -2, 0.2), rnorm(60, 2, 0.2)),
  b = c(rnorm(60, -2, 0.2), rnorm(60, 2, 0.2))
)

df_rep_1 <- sample_kmeans(
  input = df_rep,
  clusters = 2,
  ncells = 50,
  seed = 222
)

df_rep_2 <- sample_kmeans(
  input = df_rep,
  clusters = 2,
  ncells = 50,
  seed = 222
)

assert_same_sample(df_rep_1, df_rep_2, "data.frame reproducibility test")

cat("PASS: data.frame seeded sampling is reproducible.\n")

# 7) weighted points sampling smoke test
set.seed(104)
pts_rep_df <- data.frame(
  x = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  y = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  z1 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3)),
  z2 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3))
)
pts_rep_input <- terra::vect(pts_rep_df, geom = c("x", "y"), crs = "EPSG:4326")
pts_weights <- seq_len(nrow(pts_rep_input)) / nrow(pts_rep_input)

pts_weighted <- sample_kmeans(
  input = pts_rep_input,
  clusters = 3,
  ncells = 70,
  weights = pts_weights,
  seed = 223
)

assert_basic_output(pts_weighted, "points weighted smoke test")
assert_true(
  length(pts_weighted$clusters) == nrow(pts_rep_df),
  "points weighted smoke test: cluster assignment length mismatch."
)
assert_true(
  length(pts_weighted$distances) == nrow(pts_rep_df),
  "points weighted smoke test: distance length mismatch."
)

cat("PASS: points weighted sampling smoke test.\n")

# 8) weighted raster sampling is reproducible before k-means
out_r_rep_1 <- sample_kmeans(
  input = r,
  clusters = 4,
  ncells = 1000,
  weights = myweights,
  use_xy = TRUE,
  seed = 224
)

out_r_rep_2 <- sample_kmeans(
  input = r,
  clusters = 4,
  ncells = 1000,
  weights = myweights,
  use_xy = TRUE,
  seed = 224
)

assert_true(
  identical(out_r_rep_1$points, out_r_rep_2$points),
  paste(
    "raster weighted reproducibility test: selected points differ",
    "across repeated seeded runs."
  )
)

cat("PASS: raster weighted seeded sampling is reproducible.\n")

# 9) min_cluster_size prunes undersized clusters and reassigns all rows
set.seed(225)
df_small_cluster <- data.frame(
  a = c(rnorm(60, -3, 0.2), rnorm(60, 0, 0.2), rnorm(5, 4, 0.1)),
  b = c(rnorm(60, -3, 0.2), rnorm(60, 0, 0.2), rnorm(5, 4, 0.1))
)

df_min_out <- sample_kmeans(
  input = df_small_cluster,
  clusters = 3,
  min_cluster_size = 10,
  seed = 225
)

cluster_sizes <- table(df_min_out$clusters)
assert_true(
  all(cluster_sizes >= 10),
  "min_cluster_size test: found clusters smaller than requested minimum."
)
assert_true(
  nrow(df_min_out$points) == length(cluster_sizes),
  paste(
    "min_cluster_size test: number of selected centers does not match",
    "final clusters."
  )
)

cat("PASS: min_cluster_size prunes undersized clusters.\n")

# 9b) min_cluster_size with several undersized clusters of different sizes
# forces the pruning loop to run multiple iterations (smallest cluster
# removed and reassigned one at a time), exercising centroid recompute
# across iterations. Weights are included so the weighted-centroid
# recompute path also runs.
set.seed(228)
df_multi_prune <- data.frame(
  a = c(
    rnorm(60, -3, 0.2), rnorm(60, 3, 0.2),
    rnorm(3, -6, 0.1), rnorm(4, 6, 0.1), rnorm(6, 0, 0.1)
  ),
  b = c(
    rnorm(60, -3, 0.2), rnorm(60, 3, 0.2),
    rnorm(3, -6, 0.1), rnorm(4, 6, 0.1), rnorm(6, 0, 0.1)
  )
)
weights_multi_prune <- runif(nrow(df_multi_prune), min = 0.5, max = 2)

df_multi_prune_out1 <- sample_kmeans(
  input = df_multi_prune,
  clusters = 5,
  min_cluster_size = 10,
  weights = weights_multi_prune,
  seed = 228
)
df_multi_prune_out2 <- sample_kmeans(
  input = df_multi_prune,
  clusters = 5,
  min_cluster_size = 10,
  weights = weights_multi_prune,
  seed = 228
)

multi_prune_sizes <- table(df_multi_prune_out1$clusters)
assert_true(
  all(multi_prune_sizes >= 10),
  paste(
    "multi-iteration min_cluster_size test: found clusters smaller than",
    "requested minimum."
  )
)
assert_true(
  nrow(df_multi_prune_out1$points) == length(multi_prune_sizes),
  paste(
    "multi-iteration min_cluster_size test: number of selected centers",
    "does not match final clusters."
  )
)
assert_true(
  identical(df_multi_prune_out1$clusters, df_multi_prune_out2$clusters),
  paste(
    "multi-iteration min_cluster_size test: cluster assignments differ",
    "across repeated seeded runs."
  )
)
assert_true(
  identical(df_multi_prune_out1$points, df_multi_prune_out2$points),
  paste(
    "multi-iteration min_cluster_size test: selected points differ",
    "across repeated seeded runs."
  )
)

cat("PASS: min_cluster_size handles multiple pruning iterations.\n")

# 10) min_cluster_size that removes all clusters should fail clearly
assert_error(
  sample_kmeans(
    input = iris[, 1:4],
    clusters = 3,
    min_cluster_size = 500,
    seed = 226
  ),
  "All clusters were removed by candidate constraints and/or min_cluster_size.",
  "min_cluster_size all-removed error test"
)

cat("PASS: min_cluster_size all-removed case throws error.\n")

# 11) raster min_cluster_size reassignment should complete without writeValues
f_r <- system.file("ex/elev.tif", package = "terra")
r_min <- terra::rast(f_r)

r_min_out <- sample_kmeans(
  input = r_min,
  clusters = 10,
  use_xy = TRUE,
  min_cluster_size = 300,
  seed = 227
)

assert_true(
  methods::is(r_min_out$clusters, "SpatRaster"),
  "raster min_cluster_size test: clusters output is not a SpatRaster."
)

r_vals <- terra::values(r_min_out$clusters, mat = FALSE)
r_vals <- r_vals[!is.na(r_vals)]

assert_true(
  length(r_vals) > 0,
  "raster min_cluster_size test: clusters output contains no non-NA values."
)

assert_true(
  all(is.finite(r_vals) & (r_vals > 0) & (r_vals == as.integer(r_vals))),
  paste0(
    "raster min_cluster_size test: clusters contain values that are not ",
    "positive integers."
  )
)

cat("PASS: raster min_cluster_size reassignment regression test.\n")

cat("All focused sampling smoke tests passed.\n")
