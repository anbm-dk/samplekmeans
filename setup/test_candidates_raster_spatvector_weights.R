# Focused tests for raster input with SpatVector candidate weights

source(file.path(getwd(), "R", "sample_kmeans.R"))

library(terra)

assert_true <- function(cond, msg) {
  if (!isTRUE(cond)) {
    stop(msg, call. = FALSE)
  }
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

assert_in_candidate_points <- function(out_points, cand_coords, label) {
  assert_true(!is.null(out_points), paste0(label, ": output points is NULL."))
  assert_true(nrow(out_points) >= 1, paste0(label, ": no points selected."))

  out_xy <- as.matrix(out_points[, c("x", "y")])
  cand_xy <- as.matrix(cand_coords)

  key <- function(m) {
    apply(round(m, 8), 1, function(r) paste(r, collapse = "|"))
  }

  out_keys <- key(out_xy)
  cand_keys <- key(cand_xy)

  assert_true(
    all(out_keys %in% cand_keys),
    paste0(label, ": selected centers outside candidate points.")
  )
}

cat("Running focused raster + SpatVector candidate-weight tests...\n")

set.seed(321)

# Build clustered raster covariates
r1 <- terra::rast(nrows = 60, ncols = 60, xmin = 0, xmax = 6, ymin = 0, ymax = 6)
r2 <- terra::rast(r1)
xy <- terra::xyFromCell(r1, seq_len(terra::ncell(r1)))

v1 <- ifelse(
  xy[, 1] < 2,
  rnorm(nrow(xy), -3, 0.2),
  ifelse(xy[, 1] < 4, rnorm(nrow(xy), 0, 0.2), rnorm(nrow(xy), 3, 0.2))
)
v2 <- ifelse(
  xy[, 2] < 2,
  rnorm(nrow(xy), -3, 0.2),
  ifelse(xy[, 2] < 4, rnorm(nrow(xy), 0, 0.2), rnorm(nrow(xy), 3, 0.2))
)

terra::values(r1) <- v1
terra::values(r2) <- v2
input_r <- c(r1, r2)
names(input_r) <- c("v1", "v2")

# Candidate points sampled from non-NA cells in input raster
cand_pts <- terra::spatSample(
  input_r,
  size = 24,
  method = "random",
  as.points = TRUE,
  na.rm = TRUE
)

cand_vals <- terra::extract(input_r, cand_pts, ID = FALSE)
cand_xy <- terra::crds(cand_pts)

candidates_sv <- terra::vect(
  data.frame(
    x = cand_xy[, 1],
    y = cand_xy[, 2],
    v1 = cand_vals[[1]],
    v2 = cand_vals[[2]],
    cw = c(rep(8, 6), rep(1, 18))
  ),
  geom = c("x", "y"),
  crs = terra::crs(input_r)
)

# 1) candidate_weight_col support: selected points should come from candidates
out_col <- sample_kmeans(
  input = input_r,
  clusters = 4,
  ncells = 1200,
  candidates = candidates_sv,
  candidate_weight_col = "cw",
  seed = 9
)

assert_true(is.list(out_col), "raster candidate_weight_col test: output is not a list.")
assert_in_candidate_points(
  out_col$points,
  terra::crds(candidates_sv),
  "raster candidate_weight_col test"
)

cat("PASS: raster candidate_weight_col support works with SpatVector candidates.\n")

# 2) precedence: both provided should warn and still run
cw_vec <- rep(2, nrow(candidates_sv))
warn_msg <- NULL

out_both <- withCallingHandlers(
  sample_kmeans(
    input = input_r,
    clusters = 4,
    ncells = 1200,
    candidates = candidates_sv,
    candidate_weights = cw_vec,
    candidate_weight_col = "cw",
    seed = 9
  ),
  warning = function(w) {
    warn_msg <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)

assert_true(is.list(out_both), "raster precedence test: output is not a list.")
assert_true(
  !is.null(warn_msg),
  "raster precedence test: expected warning when both candidate weight sources are provided."
)
assert_true(
  grepl("using candidate_weights and ignoring candidate_weight_col", warn_msg, fixed = TRUE),
  "raster precedence test: warning message mismatch."
)

cat("PASS: raster candidate weight precedence warning works.\n")

# 3) negative: invalid candidate_weight_col values (zero) should fail
candidates_zero <- candidates_sv
candidates_zero$cw <- c(0, rep(1, nrow(candidates_zero) - 1))

assert_error(
  sample_kmeans(
    input = input_r,
    clusters = 4,
    ncells = 1200,
    candidates = candidates_zero,
    candidate_weight_col = "cw",
    seed = 9
  ),
  "Candidate weights for SpatVector must contain only positive values.",
  "raster zero candidate_weight_col validation"
)

cat("PASS: raster candidate_weight_col positive-value validation throws error.\n")

# 4) negative: candidate_weights length mismatch should fail
assert_error(
  sample_kmeans(
    input = input_r,
    clusters = 4,
    ncells = 1200,
    candidates = candidates_sv,
    candidate_weights = c(1, 1, 1),
    seed = 9
  ),
  "candidate_weights length does not match the number of candidate SpatVector records.",
  "raster candidate_weights length mismatch"
)

cat("PASS: raster candidate_weights length mismatch throws error.\n")
cat("All focused raster + SpatVector candidate-weight tests passed.\n")
