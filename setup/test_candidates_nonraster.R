# Focused tests for non-raster candidate-constrained center selection

source(file.path(getwd(), "R", "sample_kmeans.R"))

library(terra)

assert_true <- function(cond, msg) {
  if (!isTRUE(cond)) {
    stop(msg, call. = FALSE)
  }
}

assert_in_candidates <- function(out_points, candidates, label) {
  assert_true(!is.null(out_points), paste0(label, ": output points is NULL."))
  assert_true("Index" %in% colnames(out_points), paste0(label, ": missing Index column."))
  assert_true(
    all(out_points$Index %in% candidates),
    paste0(label, ": found selected centers outside candidates.")
  )
}

cat("Running focused candidate tests for non-raster inputs...\n")

# 1) data.frame input: centers must come from candidate indices
set.seed(42)

df_input <- data.frame(
  v1 = c(rnorm(30, -4, 0.25), rnorm(30, 0, 0.25), rnorm(30, 4, 0.25)),
  v2 = c(rnorm(30, -4, 0.25), rnorm(30, 0, 0.25), rnorm(30, 4, 0.25))
)

df_candidates <- as.integer(c(1:15, 31:45, 61:75))

df_out <- sample_kmeans(
  input = df_input,
  clusters = 3,
  candidates = df_candidates,
  seed = 7
)

assert_true(is.list(df_out), "data.frame test: output is not a list.")
assert_in_candidates(df_out$points, df_candidates, "data.frame test")
assert_true(nrow(df_out$points) >= 1, "data.frame test: no centers returned.")

cat("PASS: data.frame candidates constrain selected centers.\n")

# 2) data.frame partial coverage: fewer centers allowed when some clusters have no candidates
# Candidates from one compact region only.
df_candidates_partial <- as.integer(1:15)

df_out_partial <- sample_kmeans(
  input = df_input,
  clusters = 3,
  candidates = df_candidates_partial,
  seed = 7
)

assert_in_candidates(df_out_partial$points, df_candidates_partial, "data.frame partial test")
assert_true(
  nrow(df_out_partial$points) < 3,
  "data.frame partial test: expected fewer than requested clusters when candidates do not cover all clusters."
)

cat("PASS: data.frame partial candidates return fewer centers.\n")

# 3) spatial points input with numeric candidate indices
set.seed(99)

pts_df <- data.frame(
  x = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  y = c(runif(40, 0, 1), runif(40, 2, 3), runif(40, 4, 5)),
  z1 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3)),
  z2 = c(rnorm(40, -3, 0.3), rnorm(40, 0, 0.3), rnorm(40, 3, 0.3))
)

pts_input <- terra::vect(pts_df, geom = c("x", "y"), crs = "EPSG:4326")
pts_candidates <- as.integer(c(1:20, 45:55, 90:100))

pts_out <- sample_kmeans(
  input = pts_input,
  clusters = 3,
  candidates = pts_candidates,
  seed = 11
)

assert_true(is.list(pts_out), "points test: output is not a list.")
assert_in_candidates(pts_out$points, pts_candidates, "points numeric candidates test")
assert_true(nrow(pts_out$points) >= 1, "points test: no centers returned.")

cat("PASS: points numeric candidates constrain selected centers.\n")

# 4) spatial points input with raster candidates mask
# Build a simple mask that only keeps the first region of points.
region1 <- pts_df[1:40, c("x", "y")]
ext <- terra::ext(
  min(region1$x) - 0.1,
  max(region1$x) + 0.1,
  min(region1$y) - 0.1,
  max(region1$y) + 0.1
)
mask_r <- terra::rast(ext, resolution = 0.05, crs = "EPSG:4326")
mask_r <- terra::init(mask_r, fun = "x")
mask_r[!is.na(mask_r)] <- 1

pts_out_mask <- sample_kmeans(
  input = pts_input,
  clusters = 3,
  candidates = mask_r,
  seed = 11
)

mask_candidate_idx <- seq_len(nrow(pts_df))[terra::extract(mask_r, pts_input, ID = FALSE, layer = 1)[, 1] == 1]
mask_candidate_idx <- as.integer(mask_candidate_idx)

assert_in_candidates(pts_out_mask$points, mask_candidate_idx, "points raster candidates test")
assert_true(
  nrow(pts_out_mask$points) < 3,
  "points raster candidates test: expected fewer than requested clusters when mask does not cover all clusters."
)

cat("PASS: points raster candidates constrain centers and allow fewer centers.\n")
cat("All focused non-raster candidate tests passed.\n")
