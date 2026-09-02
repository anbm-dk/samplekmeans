#' k-means sampling
#'
#' @description
#' Produces representative samples by clustering input data and selecting
#' one center per cluster from observed records. The function accepts tabular
#' data (`data.frame`), rasters (`SpatRaster`), and point vectors
#' (`SpatVector` with point geometry), and can include geographic coordinates
#' as clustering variables. Optional input weights affect sampling and distance
#' ranking, while optional candidate constraints restrict which rows/points can
#' be selected as final centers. Variables can be standardized, reweighted, and
#' reduced with PCA before clustering. Outputs include cluster assignments,
#' distances to assigned centroids, and selected center locations/indices.
#'
#' @param input An input `data.frame`, or a `SpatRaster` or `SpatVector`
#' object created by package [terra]. The input should contain only numeric
#' variables for clustering.
#' @param clusters Number of clusters.
#' @param ncells Number of cells to extract, otherwise uses all cells.
#' @param use_xy Should the geographic xy coordinates be used as variables for
#' clustering?
#' @param only_xy Should the algorithm Use only xy coordinates?
#' @param weights A `SpatRaster` object or numeric vector with weights between
#' 0 and 1.
#' @param layer_weights Numeric vector with weights for each input parameter.
#' @param xy_weight Numeric vector of weights for the x and y coordinates
#' (repeated if length 1).
#' @param candidates Candidate pool for selecting cluster centers. Supports:
#'
#' - numeric indices for rows/points in `input` (data frame and point-vector
#'   inputs);
#' - a `data.frame` when `input` is a `data.frame` (same column names and
#' classes; column order can differ);
#' - a `SpatVector` with points when `input` is a `SpatVector` with points
#' (same attribute names/classes; candidate CRS is reprojected to match input if
#' needed);
#' - a `SpatRaster` mask for spatial inputs.
#'
#' When candidates map to rows/points in `input`, center selection is
#' constrained to those rows/points.
#' @param candidate_weights Optional numeric vector of positive weights for
#' candidate records. Supported when `candidates` is an independent
#' `data.frame` or `SpatVector` with points.
#' @param candidate_weight_col Optional name of a numeric, positive weight
#' column in candidate attributes. If both `candidate_weights` and
#' `candidate_weight_col` are provided, `candidate_weights` takes precedence.
#' @param min_cluster_size Minimum number of assigned rows/cells required to
#' retain a cluster. Clusters below this size are removed one at a time,
#' smallest first, and reassigned to the nearest surviving cluster, with
#' sizes refreshed after each removal.
#' @param scale Center and scale variables.
#' @param pca Use principal component analysis on variables.
#' @param tol_pca Tolerance for pca (remove PCs below threshold).
#' @param n_pcs Maximum number of principal components.
#' @param num_init See KMeans_rcpp.
#' @param max_iters See KMeans_rcpp.
#' @param initializer See KMeans_rcpp.
#' @param CENTROIDS KMeans_rcpp.
#' @param tol_kmeans See KMeans_rcpp.
#' @param tol_opt See KMeans_rcpp.
#' @param seed See KMeans_rcpp.
#' @param mini_batch Use MiniBatchKmeans (fast, less accurate).
#' @param batch_size See MiniBatchKmeans.
#' @param init_frac See MiniBatchKmeans.
#' @param early_stop See MiniBatchKmeans.
#' @param filename_cl File name for the output cluster raster.
#' @param args_cl List with arguments for writing the cluster raster.
#' @param filename_d File name for distance rasters.
#' @param args_d arguments for writing distance rasters.
#' @param sp_pts Output locations as spatial points.
#' @param filename_pts Filename for output locations.
#' @param shp Write output locations as a shapefile.
#' @param args_pts Arguments for writing output points.
#' @param cores Number of cpu cores to use.
#' @param verbose Print messages during processing.
#' @return A list with components `clusters`, `distances`, and `points`.
#' @export
#' @importFrom methods is
#' @importFrom stats  complete.cases prcomp predict sd weighted.mean
#' @importFrom rlang .data
#' @importFrom methods is
#' @importFrom tidyr drop_na
#' @importFrom terra nlyr spatSample compareGeom geomtype global mask spatSample extract crds values init subset app writeRaster rast zonal vect writeVector
#' @importFrom ClusterR KMeans_rcpp MiniBatchKmeans
#' @importFrom dplyr arrange mutate
#' @importFrom fields rdist

sample_kmeans <- function(
  input = NULL,
  clusters = 3,
  ncells = NULL,
  use_xy = FALSE, # Add xy coordinates as variables for clustering
  only_xy = FALSE, # Use only xy coordinates
  weights = NULL, # Raster layer or numeric vector with weights between 0 and
  # 1
  layer_weights = NULL, # Numeric vector with weights for each input
  # parameter.
  xy_weight = NULL, # Numeric vector of weights for the x and y coordinates
  # (repeated if length 1)
  candidates = NULL,
  candidate_weights = NULL,
  candidate_weight_col = NULL,
  min_cluster_size = 1,
  scale = TRUE, # Center and scale variables
  pca = FALSE, # Use principal component analysis on variables
  tol_pca = 0, # Tolerance for pca (remove PCs below threshold)
  n_pcs = NULL, # Maximum number of principal components
  num_init = 1, # See KMeans_rcpp
  max_iters = 10, # See KMeans_rcpp
  initializer = NULL, # See KMeans_rcpp
  CENTROIDS = NULL, # See KMeans_rcpp # nolint: object_name_linter.
  tol_kmeans = 1e-04, # See KMeans_rcpp
  tol_opt = 0.3, # See KMeans_rcpp
  seed = NULL, # See KMeans_rcpp
  mini_batch = FALSE, # Use MiniBatchKmeans (fast, less accurate)
  batch_size = 10, # See MiniBatchKmeans
  init_frac = 1, # See MiniBatchKmeans
  early_stop = 10, # See MiniBatchKmeans
  filename_cl = NULL, # File names for rasters with clusters (1) and
  # distances (2)
  args_cl = NULL, # List with arguments for writing raster
  filename_d = NULL, # Filename for output distances
  args_d = NULL, # Arguments for writing output distances
  sp_pts = FALSE, # Output locations as spatial points
  filename_pts = NULL, # Filename for output locations
  shp = FALSE, # Write output locations as a shapefile
  args_pts = NULL, # Arguments for writing output pointsx
  cores = NULL, # Number of cpu cores to use
  verbose = FALSE # Print messages during processing
) {
  if (is.null(input) && is.null(weights)) {
    stop("No input data.")
  }

  if (verbose == TRUE) {
    message("Preparing input data.")
  }

  if (is.null(input) && !is.null(weights)) {
    if (is.vector(weights)) {
      message("No input variables. Using weights as an input variable.")
      input <- data.frame(w = weights)
    } else {
      message("No input variables. Using only coordinates.")
      only_xy <- TRUE
      input <- weights
    }
  }

  # Identify input
  inputisraster <- methods::is(input, "SpatRaster")
  inputispoints <- FALSE
  inputisdf <- FALSE
  if (methods::is(input, "SpatVector")) {
    if (terra::geomtype(input) == "points") {
      inputispoints <- TRUE
    }
  }
  if (!inputisraster && !inputispoints) {
    inputisdf <- is.data.frame(input)
  }
  if ((inputisraster + inputispoints + inputisdf) != 1) {
    stop(
      "Input must be either a data frame, a SpatRaster or a ",
      "SpatVector with points."
    )
  }
  if (inputispoints) {
    input <- input[seq_len(nrow(input)), ]
  }
  if (inputisdf) {
    only_xy <- FALSE
    use_xy <- FALSE
    xy_weight <- NULL
    xy <- NULL
    sp_pts <- FALSE
  }
  if (only_xy) {
    n_vars <- 2
  } else {
    if (inputisraster) {
      n_vars <- terra::nlyr(input)
    }
    if (inputispoints) {
      n_vars <- ncol(input)
    }
    if (inputisdf) {
      n_vars <- ncol(input)
    }
    if (use_xy) {
      n_vars <- n_vars + 2
    }
  }

  # Helpers for matching candidate formats to input rows/points
  align_candidate_df <- function(candidates_df, input_df, label) {
    input_names <- colnames(input_df)
    cand_names <- colnames(candidates_df)
    if (!setequal(input_names, cand_names)) {
      stop(
        "When ", label, ", candidate columns must match input columns."
      )
    }
    candidates_df <- candidates_df[, input_names, drop = FALSE]

    class_ok <- mapply(
      function(x, y) identical(class(x), class(y)),
      input_df,
      candidates_df,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    if (!all(class_ok)) {
      stop(
        "When ", label,
        ", candidate column classes must match input column classes."
      )
    }
    candidates_df
  }

  row_keys <- function(df) {
    apply(df, 1, function(r) {
      paste(vapply(r, function(v) {
        if (is.na(v)) "__NA__" else as.character(v)
      }, character(1)), collapse = "\r")
    })
  }

  validate_numeric_weights <- function(
    x,
    label,
    require_all_positive = FALSE
  ) {
    if (!is.numeric(x)) {
      stop(label, " must be numeric.")
    }
    if (any(!is.finite(x))) {
      stop(label, " must contain only finite values.")
    }
    if (any(x < 0)) {
      stop(label, " must not contain negative values.")
    }
    if (require_all_positive) {
      if (any(x <= 0)) {
        stop(label, " must contain only positive values.")
      }
    } else {
      if (!any(x > 0)) {
        stop(label, " must contain at least one value greater than zero.")
      }
    }
    x
  }

  resolve_candidate_weights <- function(
    candidates_obj,
    weights_arg,
    weight_col,
    label
  ) {
    out <- NULL

    if (!is.null(weights_arg) && !is.null(weight_col)) {
      warning(
        "Both candidate_weights and candidate_weight_col were provided for ",
        label,
        "; using candidate_weights and ignoring candidate_weight_col."
      )
    }

    if (!is.null(weights_arg)) {
      if (length(weights_arg) != nrow(candidates_obj)) {
        stop(
          "candidate_weights length does not match the number of candidate ",
          label,
          " records."
        )
      }
      out <- weights_arg
    } else if (!is.null(weight_col)) {
      if (!(weight_col %in% colnames(candidates_obj))) {
        stop(
          "candidate_weight_col '",
          weight_col,
          "' is not present in candidate ",
          label,
          " attributes."
        )
      }
      out <- candidates_obj[[weight_col]]
    }

    if (is.null(out)) {
      return(NULL)
    }

    validate_numeric_weights(
      out,
      paste0("Candidate weights for ", label),
      require_all_positive = TRUE
    )
  }

  safe_divide_by_weights <- function(distances, w, label) {
    w <- validate_numeric_weights(w, label, require_all_positive = FALSE)
    out <- rep(NA_real_, length(distances))
    pos <- w > 0
    out[pos] <- distances[pos] / w[pos]
    out
  }

  compute_cluster_eval <- function(
    cluster_ids,
    eval_distances,
    candidate_mask,
    candidate_weights = NULL
  ) {
    ok <- !is.na(cluster_ids) & !is.na(eval_distances) & candidate_mask
    if (!any(ok)) {
      return(data.frame(clust = integer(0), mindist = numeric(0)))
    }

    d <- data.frame(
      clust = as.integer(cluster_ids[ok]),
      dist = as.numeric(eval_distances[ok])
    )
    if (!is.null(candidate_weights)) {
      d$dist <- d$dist / as.numeric(candidate_weights[ok])
    }

    stats::aggregate(
      d$dist,
      by = list(clust = d$clust),
      FUN = min
    ) |>
      (
        \(x) data.frame(clust = as.integer(x$clust), mindist = as.numeric(x$x))
      )() |>
      dplyr::arrange(.data$clust)
  }

  cluster_size_df <- function(cluster_ids) {
    cl <- as.integer(cluster_ids[!is.na(cluster_ids)])
    if (length(cl) == 0) {
      return(data.frame(clust = integer(0), size = integer(0)))
    }
    tab <- table(cl)
    data.frame(
      clust = as.integer(names(tab)),
      size = as.integer(tab)
    )
  }

  candidate_mask_from_index <- function(n, idx = NULL) {
    mask <- rep(TRUE, n)
    if (!is.null(idx)) {
      mask <- rep(FALSE, n)
      mask[idx] <- TRUE
    }
    mask
  }

  renumber_clusters <- function(cluster_ids) {
    kept <- sort(unique(as.integer(cluster_ids[!is.na(cluster_ids)])))
    map <- setNames(seq_along(kept), kept)
    out <- as.integer(cluster_ids)
    non_na <- !is.na(out)
    out[non_na] <- as.integer(map[as.character(out[non_na])])
    out
  }

  # Reassigns given rows/cells to their nearest centroid among a
  # restricted set of surviving clusters.
  reassign_to_valid_clusters <- function(
    idx_move,
    feature_values,
    valid_clusters,
    centroids_all,
    weights = NULL
  ) {
    map_valid_fun <- make_map_clusters_fun(
      centroids_all[valid_clusters, , drop = FALSE]
    )
    remap <- apply(
      feature_values[idx_move, , drop = FALSE],
      1,
      map_valid_fun
    ) |>
      t()

    new_idx <- as.integer(remap[, 1])
    new_dist <- as.numeric(remap[, 2])

    if (!is.null(weights)) {
      w_sub <- weights[idx_move]
      dist_sub <- rep(NA_real_, length(new_dist))
      pos <- w_sub > 0
      dist_sub[pos] <- new_dist[pos] / w_sub[pos]
      new_dist <- dist_sub
    }

    list(
      new_cluster = valid_clusters[new_idx],
      new_dist = new_dist
    )
  }

  # Removes the smallest surviving cluster one at a time, reassigning its
  # members and refreshing sizes, until the smallest meets the threshold.
  prune_small_clusters <- function(
    cluster_ids,
    distances,
    feature_values,
    valid_clusters,
    centroids_all,
    min_cluster_size,
    weights = NULL
  ) {
    repeat {
      if (length(valid_clusters) == 0) {
        break
      }

      sizes <- cluster_size_df(cluster_ids)
      sizes <- sizes[sizes$clust %in% valid_clusters, , drop = FALSE]
      smallest_size <- min(sizes$size)

      if (smallest_size >= min_cluster_size) {
        break
      }

      smallest_clust <- min(sizes$clust[sizes$size == smallest_size])
      valid_clusters <- setdiff(valid_clusters, smallest_clust)
      idx_move <- which(cluster_ids == smallest_clust)

      if (length(valid_clusters) == 0) {
        cluster_ids[idx_move] <- NA_integer_
        distances[idx_move] <- NA_real_
        break
      }

      remapped <- reassign_to_valid_clusters(
        idx_move = idx_move,
        feature_values = feature_values,
        valid_clusters = valid_clusters,
        centroids_all = centroids_all,
        weights = weights
      )
      cluster_ids[idx_move] <- remapped$new_cluster
      distances[idx_move] <- remapped$new_dist

      # Refresh centroids of clusters that just gained members, so any
      # further pruning in this loop reassigns against their true centers.
      centroids_all <- recompute_centroids_for_clusters(
        cluster_ids = cluster_ids,
        feature_values = feature_values,
        weights = weights,
        affected_clusters = unique(remapped$new_cluster),
        centroids_all = centroids_all
      )
    }

    list(
      clusters = cluster_ids,
      distances = distances,
      valid_clusters = valid_clusters
    )
  }

  # Puts raw feature rows into the same scaled/PCA space as the kmeans
  # centroids, mirroring the per-row transform in row_distance_to_centroids.
  transform_to_centroid_space <- function(raw_values) {
    trans_df <- as.data.frame(raw_values)

    if (scale == TRUE) {
      trans_df <- trans_df |>
        sweep(MARGIN = 2, STATS = means, check.margin = FALSE) |>
        sweep(MARGIN = 2, FUN = "/", STATS = sds, check.margin = FALSE)
    }

    if (pca == TRUE) {
      colnames(trans_df) <- pcs$rotation |> rownames()
      trans_df <- stats::predict(pcs, newdata = trans_df)
    }

    as.matrix(trans_df)
  }

  # Recomputes the centroid of each affected cluster from its current
  # members (weighted mean when weights are supplied), so distances used
  # for later reassignments reflect the true, shifted cluster centers.
  recompute_centroids_for_clusters <- function(
    cluster_ids,
    feature_values,
    weights,
    affected_clusters,
    centroids_all
  ) {
    for (clust in affected_clusters) {
      idx <- which(cluster_ids == clust)
      if (length(idx) == 0) {
        next
      }

      trans_rows <- transform_to_centroid_space(
        feature_values[idx, , drop = FALSE]
      )

      w <- NULL
      if (!is.null(weights)) {
        w <- weights[idx]
        w[!is.finite(w) | w < 0] <- 0
      }

      if (!is.null(w) && sum(w) > 0) {
        new_centroid <- vapply(
          seq_len(ncol(trans_rows)),
          function(j) stats::weighted.mean(trans_rows[, j], w = w),
          numeric(1)
        )
      } else {
        # No usable weights for this cluster: fall back to a plain mean.
        new_centroid <- colMeans(trans_rows, na.rm = TRUE)
      }

      centroids_all[clust, ] <- new_centroid
    }

    centroids_all
  }

  is_index_vector <- function(x) {
    is.atomic(x) && is.numeric(x) && is.null(dim(x))
  }

  if (is.null(seed)) {
    seed <- sample(10000, 1)
  }

  if (
    length(min_cluster_size) != 1 ||
      !is.numeric(min_cluster_size) ||
      !is.finite(min_cluster_size)
  ) {
    stop("min_cluster_size must be a single finite number.")
  }
  if (min_cluster_size != as.integer(min_cluster_size)) {
    stop("min_cluster_size must be an integer.")
  }
  min_cluster_size <- as.integer(min_cluster_size)
  if (min_cluster_size < 1) {
    stop("min_cluster_size must be at least 1.")
  }

  next_sampling_seed <- local({
    current <- seed

    function() {
      set.seed(current)
      current <<- current + 1
    }
  })

  # Identify candidates
  candidates_israster <- FALSE
  candidates_ispts <- FALSE
  candidates_isdf <- FALSE
  candidates_index <- FALSE
  candidates_use_index <- NULL
  candidate_weights_use <- NULL
  if (!is.null(candidates)) {
    if (methods::is(candidates, "SpatRaster")) {
      candidates_israster <- TRUE
      if (terra::nlyr(candidates) > 1) {
        candidates <- candidates[[1]]
      }
    }
    if (methods::is(candidates, "SpatVector")) {
      if (terra::geomtype(candidates) == "points") {
        candidates_ispts <- TRUE
      }
    }
    if (!candidates_israster && !candidates_ispts) {
      candidates_isdf <- is.data.frame(candidates)
    }
    if (!candidates_isdf) {
      if (is_index_vector(candidates)) {
        candidates_index <- TRUE
      }
    }
  }

  # Extraction for rasters
  if (inputisraster) {
    # Check weights for raster input
    if (!is.null(weights)) {
      if (!methods::is(weights, "SpatRaster")) {
        stop("When input is a SpatRaster, weights must also be a SpatRaster.")
      } else {
        if (terra::compareGeom(input, weights) == FALSE) {
          stop("Input and weights rasters do not match.")
        } else {
          w_min <- terra::global(weights, "min", na.rm = TRUE) |>
            unlist() |>
            unname() |>
            (
              \(x) x[1]
            )()
          w_max <- terra::global(weights, "max", na.rm = TRUE) |>
            unlist() |>
            unname() |>
            (
              \(x) x[1]
            )()

          if (is.na(w_max)) {
            stop("Weights raster contains only NA values.")
          }
          if (w_min < 0) {
            stop("Weights raster must not contain negative values.")
          }
          if (w_max <= 0) {
            stop(
              "Weights raster must contain at least one value greater ",
              "than zero."
            )
          }

          input <- c(input, weights)
          all_na <- !(
            input |>
              sum() |>
              terra::global("anynotNA") |>
              unlist() |>
              unname()
          )
          if (all_na) {
            stop("There os no overlap between the input and weights rasters.")
          }
        }
      }
    }

    # Check candidates for raster input
    if (!is.null(candidates)) {
      if (!candidates_israster && !candidates_ispts) {
        stop(
          "When input is a SpatRaster, candidates must be a ",
          "SpatRaster or a SpatVector with points."
        )
      }
      if (candidates_israster) {
        if (terra::compareGeom(input, candidates) == FALSE) {
          stop("Input and candidates rasters do not match.")
        }
        all_na <- !(
          c(input, candidates) |>
            sum() |>
            terra::global("anynotNA") |>
            unlist() |>
            unname()
        )
        if (all_na) {
          stop(
            "There os no overlap between the input and candidates rasters."
          )
        }
        # When candidates are a raster, they will be used as a mask when
        # selecting points.
      }
      if (candidates_ispts) {
        candidate_weights_use <- resolve_candidate_weights(
          candidates_obj = terra::values(candidates),
          weights_arg = candidate_weights,
          weight_col = candidate_weight_col,
          label = "SpatVector"
        )

        candidates_df <- candidates |>
          (\(y) {
            terra::extract(
              x = input,
              y = y,
              ID = FALSE,
              xy = TRUE
            )
          })() |>
          tidyr::drop_na()

        if (nrow(candidates_df) == 0) {
          stop(
            "There os no overlap between the input rasters and the ",
            "candidates points."
          )
        }
      } else if (
        !is.null(candidate_weights) || !is.null(candidate_weight_col)
      ) {
        stop(
          "Candidate weights are only supported when candidates are ",
          "provided as an independent data.frame or SpatVector with points."
        )
      }
    }

    # Sampling (if applicable) for raster input
    # Use all cells if ncells is NULL
    if (is.null(ncells)) {
      df <- as.data.frame(input, xy = TRUE, na.rm = TRUE)
      ncells <- nrow(df)
      # Weighted raster resampling (if relevant) when ncells is NULL
      if (!is.null(weights)) {
        prob_weights <- df[, ncol(df)]
        validate_numeric_weights(
          prob_weights,
          "Weights for raster sampling",
          require_all_positive = FALSE
        )
        sampled_unique <- 0
        seed_loop <- seed
        while (sampled_unique < (clusters + 2)) {
          next_sampling_seed()
          sampled <- sample(
            nrow(df),
            ncells,
            prob = prob_weights,
            replace = TRUE
          )
          sampled_unique <- sampled |>
            unique() |>
            length()
          seed_loop <- seed_loop + 1
        }
        df <- df[sampled, -ncol(df)]
      }
    } else {
      if (ncells < (clusters + 2)) {
        stop("ncells must at least be equal to the number of clusters + 2")
      }
      # Weighted sampling for raster input
      if (!is.null(weights)) {
        next_sampling_seed()
        sample_pts <- input[[terra::nlyr(input)]] |>
          terra::mask(mask = sum(input)) |>
          terra::spatSample(
            size = ncells,
            method = "weights",
            as.points = TRUE,
            na.rm = TRUE,
            replace = TRUE
          )

        sampled <- terra::extract(
          x = input,
          y = sample_pts,
          ID = FALSE
        ) |>
          (\(x) dplyr::bind_cols(terra::crds(sample_pts), x))() |>
          tidyr::drop_na()
        df <- sampled[, -ncol(sampled)]
        ncells <- nrow(df)
      } else {
        # Non-weighted sampling for raster input
        next_sampling_seed()
        df <- terra::spatSample(
          input,
          ncells,
          xy = TRUE,
          as.df = TRUE,
          na.rm = TRUE,
          replace = TRUE
        )
        ncells <- nrow(df)
      }
    }
  }

  # Extraction for spatial points
  if (inputispoints) {
    input_values_raw <- terra::values(input)

    if (!is.null(weights)) {
      # check weights
      if (!methods::is(weights, "SpatRaster") && !is.vector(weights)) {
        stop(
          "When the input is points, the weights must be a ",
          "numeric vector or a SpatRaster object"
        )
      }
      if (is.vector(weights)) {
        if (nrow(input) != length(weights)) {
          stop("The number of weights do not match the input points.")
        }
        weights <- validate_numeric_weights(
          weights,
          "Weights for input points",
          require_all_positive = FALSE
        )
      } else {
        weights_sample <- terra::extract(
          x = weights,
          y = input,
          ID = FALSE,
          layer = 1
        ) |>
          (\(x) x[[1]])()

        weights_sample[is.na(weights_sample)] <- 0
        weights <- validate_numeric_weights(
          weights_sample,
          "Weights extracted from raster for input points",
          require_all_positive = FALSE
        )

        weights <- weights_sample
      }
    }

    # check candidates, if input is a points data set
    # Add warning if there is no overlap
    if (!is.null(candidates)) {
      if (!candidates_israster && !candidates_index && !candidates_ispts) {
        stop(
          "When the input is points, the candidates must be a ",
          "numeric vector, SpatRaster object, or SpatVector with points"
        )
      }
      if (methods::is(candidates, "SpatRaster")) {
        if (!is.null(candidate_weights) || !is.null(candidate_weight_col)) {
          stop(
            "Candidate weights are only supported when candidates are ",
            "provided as an independent data.frame or SpatVector with points."
          )
        }

        candidates_sample <- terra::extract(
          x = candidates,
          y = input,
          ID = FALSE,
          layer = 1
        ) |>
          (\(x) x[[1]])()

        candidates <- seq_along(input)[!is.na(candidates_sample)]
        if (length(candidates) == 0) {
          stop(
            "There is no overlap between the input points and the ",
            "candidates raster."
          )
        }
        candidates_df <- input[candidates, ]
      } else if (candidates_ispts) {
        if (terra::geomtype(candidates) != "points") {
          stop(
            "When input is points, SpatVector candidates must have point ",
            "geometry."
          )
        }

        if (!identical(terra::crs(candidates), terra::crs(input))) {
          candidates <- terra::project(candidates, terra::crs(input))
        }

        cand_values <- terra::values(candidates)

        candidate_weights_raw <- resolve_candidate_weights(
          candidates_obj = cand_values,
          weights_arg = candidate_weights,
          weight_col = candidate_weight_col,
          label = "SpatVector"
        )

        if (!is.null(candidate_weight_col) &&
              (candidate_weight_col %in% colnames(cand_values))) {
          cand_values <- cand_values[
            ,
            setdiff(colnames(cand_values), candidate_weight_col),
            drop = FALSE
          ]
        }

        cand_values <- align_candidate_df(
          cand_values,
          input_values_raw,
          "input is points"
        )

        input_xy <- as.data.frame(terra::crds(input))
        cand_xy <- as.data.frame(terra::crds(candidates))
        colnames(input_xy) <- c("x", "y")
        colnames(cand_xy) <- c("x", "y")
        input_xy$x <- round(input_xy$x, 8)
        input_xy$y <- round(input_xy$y, 8)
        cand_xy$x <- round(cand_xy$x, 8)
        cand_xy$y <- round(cand_xy$y, 8)

        input_key_df <- cbind(input_xy, input_values_raw)
        cand_key_df <- cbind(cand_xy, cand_values)
        input_keys <- row_keys(input_key_df)
        cand_keys <- row_keys(cand_key_df)

        candidates_mapped <- match(cand_keys, input_keys)
        valid_map <- !is.na(candidates_mapped)

        if (!any(valid_map)) {
          stop(
            "No candidate points matched the input points with identical ",
            "coordinates and attributes."
          )
        }


        if (!is.null(weights)) {
          input$weights <- weights
        }
        if (!is.null(candidate_weights_raw)) {
          mapped_index <- candidates_mapped[valid_map]
          mapped_weights <- candidate_weights_raw[valid_map]
          keep <- !duplicated(mapped_index)
          candidates_use_index <- mapped_index[keep]
          candidate_weights_use <- mapped_weights[keep]
        } else {
          candidates_use_index <- unique(candidates_mapped[valid_map])
        }

        if (length(candidates_use_index) == 0) {
          stop(
            "No candidate points matched the input points with identical ",
            "coordinates and attributes."
          )
        }
      } else {
        if (!is.null(candidate_weights) || !is.null(candidate_weight_col)) {
          stop(
            "Candidate weights are only supported when candidates are ",
            "provided as an independent data.frame or SpatVector with points."
          )
        }

        candidates <- unique(candidates)
        candidates <- candidates[!is.na(candidates)]
        candidates <- candidates[candidates == as.integer(candidates)]
        candidates <- as.integer(candidates)
        candidates <- candidates[
          candidates >= 1 & candidates <= nrow(input)
        ]
        if (length(candidates) == 0) {
          stop("No valid candidate indices for the input points.")
        }
        candidates_df <- input[candidates, ]
      }
    }

    # Sampling (if applicable) for points dataset
    df <- cbind(terra::crds(input), terra::values(input))
    if (is.null(ncells)) { # Use all points if ncells is NULL
      ncells <- nrow(df)
    } else {
      if (ncells < (clusters + 2)) {
        stop("ncells must at least be equal to the number of clusters + 2")
      }
    }
    if (is.null(weights)) {
      if (ncells > nrow(df)) {
        message(
          "ncells is larger than the number of input points. ",
          "Using all input points instead."
        )
      } else {
        # standard sampling for points dataset
        next_sampling_seed()
        sampled <- sample(nrow(df),
          ncells,
          replace = FALSE
        )
        df <- df[sampled, ]
      }
    } else {
      # weighted sampling for points dataset
      prob_weights <- weights
      validate_numeric_weights(
        prob_weights,
        "Weights for points sampling",
        require_all_positive = FALSE
      )

      sampled_unique <- 0
      seed_loop <- seed
      while (sampled_unique < (clusters + 2)) {
        next_sampling_seed()
        sampled <- sample(
          x = nrow(df),
          ncells,
          prob = prob_weights,
          replace = TRUE
        )
        sampled_unique <- sampled |>
          unique() |>
          length()
        seed_loop <- seed_loop + 1
      }
      df <- df[sampled, -ncol(df)]
    }
  }

  # Extraction for data frame
  if (inputisdf) {
    input_df_raw <- input

    if (!is.null(weights)) {
      # check weights
      if (!is.vector(weights)) {
        stop(
          "When the input is a data frame, the weights must be a ",
          "numeric vector."
        )
      } else {
        if (nrow(input) != length(weights)) {
          stop("The number of weights do not match the input data.")
        } else {
          weights <- validate_numeric_weights(
            weights,
            "Weights for input data",
            require_all_positive = FALSE
          )
        }
      }
    }
    # check candidates for data frame input
    # Add warning if there is no overlap (if candidates are a vector)
    if (!is.null(candidates)) {
      if (!is.vector(candidates) && !candidates_isdf) {
        stop(
          "When the input is a data frame, the candidates must be ",
          "a numeric vector or a data frame."
        )
      } else if (candidates_isdf) {
        candidate_weights_raw <- resolve_candidate_weights(
          candidates_obj = candidates,
          weights_arg = candidate_weights,
          weight_col = candidate_weight_col,
          label = "data.frame"
        )

        candidates_match <- candidates
        if (!is.null(candidate_weight_col) &&
              (candidate_weight_col %in% colnames(candidates_match))) {
          candidates_match <- candidates_match[
            ,
            setdiff(colnames(candidates_match), candidate_weight_col),
            drop = FALSE
          ]
        }

        candidates_df <- align_candidate_df(
          candidates_match,
          input_df_raw,
          "input is a data frame"
        )

        input_keys <- row_keys(input_df_raw)
        cand_keys <- row_keys(candidates_df)
        candidates_mapped <- match(cand_keys, input_keys)
        valid_map <- !is.na(candidates_mapped)

        if (!any(valid_map)) {
          stop(
            "No candidate rows matched input rows after column alignment."
          )
        }

        if (!is.null(candidate_weights_raw)) {
          mapped_index <- candidates_mapped[valid_map]
          mapped_weights <- candidate_weights_raw[valid_map]
          keep <- !duplicated(mapped_index)
          candidates_use_index <- mapped_index[keep]
          candidate_weights_use <- mapped_weights[keep]
        } else {
          candidates_use_index <- unique(candidates_mapped[valid_map])
        }

        if (length(candidates_use_index) == 0) {
          stop(
            "No candidate rows matched input rows after column alignment."
          )
        }
      } else {
        if (!is.null(candidate_weights) || !is.null(candidate_weight_col)) {
          stop(
            "Candidate weights are only supported when candidates are ",
            "provided as an independent data.frame or SpatVector with points."
          )
        }

        candidates <- unique(candidates)
        candidates <- candidates[!is.na(candidates)]
        candidates <- candidates[candidates == as.integer(candidates)]
        candidates <- as.integer(candidates)
        candidates <- candidates[
          candidates >= 1 & candidates <= nrow(input)
        ]
        if (length(candidates) == 0) {
          stop("No valid candidate indices for the input data.")
        }
        candidates_df <- input_df_raw[candidates, ]
        candidates_use_index <- candidates
      }
    }

    if (!is.null(weights)) {
      input$weights <- weights
    }

    # Sampling (if applicable)
    df <- input
    if (is.null(ncells)) { # Use all points if ncells is NULL
      ncells <- nrow(df)
    } else {
      if (ncells < (clusters + 2)) {
        stop("ncells must at least be equal to the number of clusters + 2")
      }
    }
    if (is.null(weights)) {
      if (ncells > nrow(df)) {
        message(
          "ncells is larger than the number of input rows. ",
          "Using all input rows instead."
        )
      } else {
        # standard sampling
        next_sampling_seed()
        sampled <- sample(
          nrow(df),
          ncells,
          replace = FALSE
        )
        df <- df[sampled, ]
      }
    } else {
      # weighted sampling
      prob_weights <- weights
      validate_numeric_weights(
        prob_weights,
        "Weights for data frame sampling",
        require_all_positive = FALSE
      )

      sampled_unique <- 0
      seed_loop <- seed
      while (sampled_unique < (clusters + 2)) {
        next_sampling_seed()
        sampled <- sample(
          nrow(df),
          ncells,
          prob = prob_weights,
          replace = TRUE
        )
        sampled_unique <- sampled |>
          unique() |>
          length()
        seed_loop <- seed_loop + 1
      }

      df <- df[sampled, -ncol(df)]
    }
  }

  # Combine or separate coordinates and input variables
  if (!inputisdf) {
    xy <- df[, 1:2] # Extract coordinates

    if (only_xy == TRUE) {
      use_xy <- TRUE
      df <- xy
    }

    if (use_xy == FALSE) {
      df <- df[, -c(1:2)]
    }
  }
  if (!use_xy) {
    xy_weight <- NULL
  }
  if (!is.data.frame(df)) {
    df <- as.data.frame(df)
  }


  # Scaling and PCA
  # Combine feature weights for scaling
  if (!is.null(layer_weights) || !is.null(xy_weight)) {
    sds_scaler <- rep(1, ncol(df))

    if ((!is.null(xy_weight)) && (use_xy == TRUE)) {
      sds_scaler[1:2] <- xy_weight

      if ((!is.null(layer_weights)) && (only_xy == FALSE)) {
        sds_scaler[-c(1:2)] <- layer_weights
      }
    } else {
      sds_scaler <- layer_weights
    }
  }

  # Scaling
  if ((scale == TRUE) || (exists("sds_scaler"))) {
    if (verbose == TRUE) {
      message("Scaling input variables.")
    }
    if (!is.data.frame(df)) {
      df <- as.data.frame(df)
    }
    means <- apply(df, 2, mean)
    if (scale == TRUE) {
      sds <- apply(df, 2, sd)
      if (use_xy == TRUE) {
        sds[1:2] <- max(sds[1:2])
      }
      sds[sds == 0] <- 1
    } else {
      sds <- rep(1, ncol(df))
      scale <- TRUE
    }
    if (exists("sds_scaler")) {
      sds <- sds / sds_scaler
    }
    if (verbose == TRUE) {
      scaling <- rbind(means, sds)
      rownames(scaling) <- c("Mean", "SD")
      print(scaling)
    }
    df <- df |>
      sweep(MARGIN = 2, STATS = means, check.margin = FALSE) |>
      sweep(MARGIN = 2, FUN = "/", STATS = sds, check.margin = FALSE)
  }
  # Principal components analysis
  if (pca == TRUE) {
    if (verbose == TRUE) {
      message("Conducting principal components analysis.")
    }

    if (is.null(n_pcs)) {
      n_pcs <- ncol(df)
    }
    pcs <- stats::prcomp(
      df,
      scale. = FALSE,
      tol = tol_pca,
      retx = TRUE,
      rank. = n_pcs
    )
    df <- pcs$x
  }

  if (!is.data.frame(df)) {
    df <- as.data.frame(df)
  } # Make sure it's a data frame

  out <- list()

  # Run kmeans
  if (verbose == TRUE) {
    message("Running k-means.")
  }
  if (is.null(initializer)) {
    initializer <- "kmeans++"
  }

  seed_try <- seed
  diff_try <- 0
  clusters_try <- clusters
  runagain <- TRUE

  while (runagain) {
    set.seed(seed_try)

    if (mini_batch == FALSE) {
      myclusters <- ClusterR::KMeans_rcpp(
        df,
        clusters = clusters_try,
        num_init = num_init,
        max_iters = max_iters,
        initializer = initializer,
        CENTROIDS = CENTROIDS,
        tol = tol_kmeans,
        tol_optimal_init = tol_opt,
        seed = seed_try,
        verbose = verbose
      )
    } else {
      myclusters <- ClusterR::MiniBatchKmeans(
        df,
        clusters = clusters_try,
        num_init = num_init,
        max_iters = max_iters,
        initializer = initializer,
        CENTROIDS = CENTROIDS,
        tol = tol_kmeans,
        tol_optimal_init = tol_opt,
        seed = seed_try,
        batch_size = batch_size,
        init_fraction = init_frac,
        early_stop_iter = early_stop,
        verbose = verbose
      )
    }

    # Not counting empty centroids
    n_complete <- myclusters$centroids |>
      stats::complete.cases() |>
      sum()

    if (n_complete == clusters) {
      runagain <- FALSE
    } else {
      diff_try <- clusters - n_complete
      clusters_try <- max(min(clusters_try + diff_try, nrow(df) - 2), 1)
      seed_try <- seed_try + 1
    }
  }

  mycentroids <- myclusters$centroids |>
    as.data.frame() |>
    tidyr::drop_na()

  row_distance_to_centroids <- function(x, centroids_use) {
    if (x |> sum() |> is.na()) {
      return(c(NA, NA))
    }

    row_df <- matrix(x, 1) |>
      data.frame()

    if (scale == TRUE) {
      row_df <- row_df |>
        sweep(MARGIN = 2, STATS = means, check.margin = FALSE) |>
        sweep(MARGIN = 2, FUN = "/", STATS = sds, check.margin = FALSE)
    }

    if (pca == TRUE) {
      colnames(row_df) <- pcs$rotation |> rownames()
      row_df <- stats::predict(pcs, newdata = row_df)
    }

    dist <- fields::rdist(row_df, centroids_use)
    c(which.min(dist), min(dist, na.rm = TRUE))
  }

  make_map_clusters_fun <- function(centroids_use) {
    function(x) {
      row_distance_to_centroids(x, centroids_use)
    }
  }

  map_clusters_fun <- make_map_clusters_fun(mycentroids)

  # Function to find cluster centers
  findpoint <- function(x) {
    if (x |> sum() |> is.na()) {
      NA
    } else {
      zrow <- match(as.integer(x[1]), as.integer(zs[, 1]))
      if (is.na(zrow)) {
        return(NA)
      }
      ismin <- zs[zrow, 2] == x[2]
      if (!ismin) {
        NA
      } else {
        as.integer(x[1])
      }
    }
  }

  # Mapping procedure for rasters
  if (inputisraster) {
    # Create coordinate raster
    if (verbose == TRUE) {
      message("Producing coordinate raster.")
    }
    xy_r <- c(
      terra::init(input, "x"),
      terra::init(input, "y")
    ) |>
      terra::mask(sum(input))
    if (use_xy == TRUE) {
      if (only_xy == TRUE) {
        input <- xy_r
      } else {
        input <- c(xy_r, input)
      }
    }

    # Drop weights raster from the input
    if (terra::nlyr(input) > n_vars) {
      input <- terra::subset(input, c(1:n_vars))
    }

    out$points <- NA

    if (verbose == TRUE) {
      message("Mapping clusters.")
    }
    if (is.null(cores)) {
      out$clusters <- terra::app(
        input,
        fun = map_clusters_fun
      )
    } else {
      showConnections()

      cl <- parallel::makeCluster(cores)

      export_this <- c("mycentroids")

      if (exists("pcs")) {
        export_this <- c(export_this, "pcs")
      }
      if (exists("means")) {
        export_this <- c(export_this, "means", "sds")
      }

      parallel::clusterExport(
        cl,
        c(
          export_this
        ),
        envir = environment()
      )

      out$clusters <- terra::app(
        input,
        fun = map_clusters_fun,
        cores = cl
      )

      parallel::stopCluster(cl)
      foreach::registerDoSEQ()
      rm(cl)
    }

    # Calculate raster with weighted distances
    out$distances <- out$clusters[[2]]
    out$clusters <- out$clusters[[1]]
    if (!is.null(weights)) {
      if (verbose == TRUE) {
        message("Calculating weighted distances.")
      }
      s <- c(out$distances, weights)
      calc_wdist <- function(x) {
        if (x |> sum() |> is.na()) {
          NA
        } else {
          if (x[2] == 0) {
            NA
          } else {
            x[1] / x[2]
          }
        }
      }

      if (is.null(cores)) {
        wdist <- terra::app(s, fun = calc_wdist)
      } else {
        cl <- parallel::makeCluster(cores)

        wdist <- terra::app(s, fun = calc_wdist, cores = cl)

        parallel::stopCluster(cl)
        foreach::registerDoSEQ()
        rm(cl)
      }
      out$distances <- wdist
    }

    names(out$distances) <- "distance"
    names(out$clusters) <- "cluster"

    # Write clusters and distances rasters to files if requested
    if (!is.null(filename_d)) {
      if (verbose == TRUE) {
        message("Writing distance map to file.")
      }
      do.call(
        terra::writeRaster,
        args = c(
          list(
            x = out$distances,
            filename = filename_d
          ),
          args_d
        )
      )
      out$distances <- terra::rast(filename_d)
    }
    if (is.null(filename_cl)) {
      out$clusters <- out$clusters[[1]]
    } else {
      if (verbose == TRUE) {
        message("Writing cluster map to file.")
      }
      do.call(
        terra::writeRaster,
        args = c(
          list(
            x = out$clusters[[1]],
            filename = filename_cl
          ),
          args_d
        )
      )
      out$clusters <- terra::rast(filename_cl)
    }

    cluster_vals <- terra::values(out$clusters, mat = FALSE)
    dist_vals <- terra::values(out$distances, mat = FALSE)
    feature_vals <- terra::values(input, mat = TRUE)
    weight_vals <- NULL
    if (!is.null(weights)) {
      weight_vals <- terra::values(weights, mat = FALSE)
    }
    size_df <- cluster_size_df(cluster_vals)

    if (candidates_ispts) {
      cand_eval <- terra::extract(
        x = c(out$clusters, out$distances),
        y = candidates,
        ID = FALSE,
        xy = FALSE
      )

      cand_w_eval <- NULL
      if (!is.null(candidate_weights_use)) {
        cand_w_eval <- candidate_weights_use
      }
      eval_df <- compute_cluster_eval(
        cand_eval[, 1],
        cand_eval[, 2],
        rep(TRUE, nrow(cand_eval)),
        cand_w_eval
      )
    } else if (candidates_israster) {
      distmask_eval <- terra::mask(out$distances, candidates)
      clustmask_eval <- terra::mask(out$clusters, candidates)
      eval_raw <- terra::zonal(
        distmask_eval,
        clustmask_eval,
        "min",
        na.rm = TRUE
      )
      eval_df <- data.frame(
        clust = as.integer(eval_raw[, 1]),
        mindist = as.numeric(eval_raw[, 2])
      )
    } else {
      eval_raw <- terra::zonal(
        out$distances,
        out$clusters,
        "min",
        na.rm = TRUE
      )
      eval_df <- data.frame(
        clust = as.integer(eval_raw[, 1]),
        mindist = as.numeric(eval_raw[, 2])
      )
    }

    viable_clusters <- eval_df$clust
    invalid_candidates <- setdiff(size_df$clust, viable_clusters)
    valid_clusters <- sort(setdiff(size_df$clust, invalid_candidates))

    if (length(valid_clusters) == 0) {
      stop(
        "All clusters were removed by candidate constraints and/or ",
        "min_cluster_size."
      )
    }

    if (length(invalid_candidates) > 0) {
      idx_move <- which(cluster_vals %in% invalid_candidates)
      remapped <- reassign_to_valid_clusters(
        idx_move = idx_move,
        feature_values = feature_vals,
        valid_clusters = valid_clusters,
        centroids_all = mycentroids,
        weights = weight_vals
      )
      cluster_vals[idx_move] <- remapped$new_cluster
      dist_vals[idx_move] <- remapped$new_dist

      # Reflect the newly added members in their destination centroids
      # before these centroids are used for any further pruning.
      mycentroids <- recompute_centroids_for_clusters(
        cluster_ids = cluster_vals,
        feature_values = feature_vals,
        weights = weight_vals,
        affected_clusters = unique(remapped$new_cluster),
        centroids_all = mycentroids
      )
    }

    clusters_before_pruning <- valid_clusters
    pruned <- prune_small_clusters(
      cluster_ids = cluster_vals,
      distances = dist_vals,
      feature_values = feature_vals,
      valid_clusters = valid_clusters,
      centroids_all = mycentroids,
      min_cluster_size = min_cluster_size,
      weights = weight_vals
    )
    if (length(pruned$valid_clusters) == 0) {
      stop(
        "All clusters were removed by candidate constraints and/or ",
        "min_cluster_size."
      )
    }
    cluster_vals <- pruned$clusters
    dist_vals <- pruned$distances

    removed_any <- length(invalid_candidates) > 0 ||
      length(pruned$valid_clusters) < length(clusters_before_pruning)

    if (removed_any) {
      cluster_vals <- renumber_clusters(cluster_vals)
      final_k <- length(unique(cluster_vals[!is.na(cluster_vals)]))
      if (final_k < clusters) {
        warning(
          "Requested ", clusters,
          " clusters, but returned ", final_k,
          " after removing empty/undersized clusters."
        )
      }
    }

    terra::values(out$clusters) <- cluster_vals
    terra::values(out$distances) <- dist_vals

    if (!is.null(filename_d)) {
      do.call(
        terra::writeRaster,
        args = c(
          list(
            x = out$distances,
            filename = filename_d
          ),
          args_d
        )
      )
      out$distances <- terra::rast(filename_d)
    }
    if (!is.null(filename_cl)) {
      do.call(
        terra::writeRaster,
        args = c(
          list(
            x = out$clusters,
            filename = filename_cl
          ),
          args_d
        )
      )
      out$clusters <- terra::rast(filename_cl)
    }

    # Find the cluster centers for raster input
    if (verbose == TRUE) {
      message("Identifying cluster centers.")
    }

    # Centers for raster input, when candidates are a raster or NULL
    if (!candidates_israster) {
      # Without candidates raster
      zs <- terra::zonal(out$distances, out$clusters, "min", na.rm = TRUE)
      s <- c(out$clusters, out$distances)
    } else {
      # Masking With candidates raster
      distmask <- terra::mask(out$distances, candidates)
      clustmask <- terra::mask(out$clusters, candidates)

      zs <- terra::zonal(
        distmask,
        clustmask,
        "min",
        na.rm = TRUE
      )
      s <- c(
        clustmask,
        distmask
      )
    }

    # Standard mapping, when candidates are NULL or a raster
    if (!candidates_ispts) {
      if (is.null(cores)) {
        pts <- terra::app(s, fun = findpoint)
      } else {
        showConnections()

        cl <- parallel::makeCluster(cores)

        parallel::clusterExport(
          cl,
          "zs",
          envir = environment()
        )

        pts <- terra::app(s, fun = findpoint, cores = cl)

        parallel::stopCluster(cl)
        foreach::registerDoSEQ()
        rm(cl)
      }

      names(pts) <- "ID"

      out$points <- as.data.frame(pts, xy = TRUE, na.rm = TRUE) |>
        dplyr::arrange(.data$ID)

      out$points <- out$points[!duplicated(out$points$ID), ]
    } else {
      # Centers for raster input, when candidates are points
      # Extract clusters and distances for the candidate point
      s <- candidates |>
        (\(y) {
          terra::extract(
            x = s,
            y = y,
            ID = FALSE,
            xy = FALSE
          )
        })()

      names(s) <- c("clust", "dist")

      s_eval <- s
      if (!is.null(candidate_weights_use)) {
        if (length(candidate_weights_use) != nrow(s_eval)) {
          stop(
            "Mapped candidate weights do not match extracted candidate ",
            "records."
          )
        }
        s_eval$dist <- s_eval$dist / candidate_weights_use
      }

      zs <- s_eval |>
        dplyr::group_by(.data$clust) |>
        dplyr::summarise(
          mindist = min(.data$dist, na.rm = TRUE),
          .groups = "drop"
        )
      zs <- data.frame(
        clust = as.integer(zs$clust),
        mindist = as.numeric(zs$mindist)
      )
      zs <- as.matrix(zs)

      # Find points for the cluster centers
      pts <- apply(s_eval, 1, FUN = findpoint)

      out$points <- terra::crds(candidates) |>
        as.data.frame() |>
        dplyr::mutate(
          ID = pts,
          Index = seq_along(pts)
        ) |>
        tidyr::drop_na()

      out$points <- out$points[!duplicated(out$points$ID), ] |>
        dplyr::arrange(.data$ID)
    }
  }

  # Mapping procedure for spatial points
  if (inputispoints) {
    if (use_xy == TRUE) {
      if (only_xy == TRUE) {
        terra::values(input) <- terra::crds(input)
      } else {
        terra::values(input) <- cbind(terra::crds(input), terra::values(input))
      }
    }

    # Drop weights from the input points
    if (ncol(terra::values(input)) > n_vars) {
      terra::values(input) <- terra::values(input)[, c(1:n_vars)]
    }

    out$points <- NA

    if (verbose == TRUE) {
      message("Mapping clusters.")
    }

    out$clusters <- apply(
      terra::values(input),
      1,
      FUN = map_clusters_fun
    ) |> t()

    # Calculate weighted distances for the points
    out$distances <- out$clusters[, 2] |> unname()
    out$clusters <- out$clusters[, 1] |> unname()
    if (!is.null(weights)) {
      if (verbose == TRUE) {
        message("Calculating weighted distances.")
      }
      out$distances <- safe_divide_by_weights(
        out$distances,
        weights,
        "Weights for input points"
      )
    }

    search_idx <- seq_along(out$clusters)
    if (!is.null(candidates)) {
      if (!is.null(candidates_use_index)) {
        search_idx <- candidates_use_index
      } else {
        search_idx <- unique(candidates)
      }
    }
    candidate_mask <- candidate_mask_from_index(
      length(out$clusters),
      search_idx
    )
    candidate_weights_eval <- NULL
    if (!is.null(candidate_weights_use)) {
      candidate_weights_eval <- rep(NA_real_, length(out$clusters))
      candidate_weights_eval[search_idx] <- candidate_weights_use
    }

    size_df <- cluster_size_df(out$clusters)
    eval_df <- compute_cluster_eval(
      out$clusters,
      out$distances,
      candidate_mask,
      candidate_weights_eval
    )
    viable_clusters <- eval_df$clust
    invalid_candidates <- setdiff(size_df$clust, viable_clusters)
    valid_clusters <- sort(setdiff(size_df$clust, invalid_candidates))

    if (length(valid_clusters) == 0) {
      stop(
        "All clusters were removed by candidate constraints and/or ",
        "min_cluster_size."
      )
    }

    points_features <- terra::values(input)

    if (length(invalid_candidates) > 0) {
      idx_move <- which(out$clusters %in% invalid_candidates)
      remapped <- reassign_to_valid_clusters(
        idx_move = idx_move,
        feature_values = points_features,
        valid_clusters = valid_clusters,
        centroids_all = mycentroids,
        weights = weights
      )
      out$clusters[idx_move] <- remapped$new_cluster
      out$distances[idx_move] <- remapped$new_dist

      # Reflect the newly added members in their destination centroids
      # before these centroids are used for any further pruning.
      mycentroids <- recompute_centroids_for_clusters(
        cluster_ids = out$clusters,
        feature_values = points_features,
        weights = weights,
        affected_clusters = unique(remapped$new_cluster),
        centroids_all = mycentroids
      )
    }

    clusters_before_pruning <- valid_clusters
    pruned <- prune_small_clusters(
      cluster_ids = out$clusters,
      distances = out$distances,
      feature_values = points_features,
      valid_clusters = valid_clusters,
      centroids_all = mycentroids,
      min_cluster_size = min_cluster_size,
      weights = weights
    )
    if (length(pruned$valid_clusters) == 0) {
      stop(
        "All clusters were removed by candidate constraints and/or ",
        "min_cluster_size."
      )
    }
    out$clusters <- pruned$clusters
    out$distances <- pruned$distances

    removed_any <- length(invalid_candidates) > 0 ||
      length(pruned$valid_clusters) < length(clusters_before_pruning)

    if (removed_any) {
      out$clusters <- renumber_clusters(out$clusters)
      final_k <- length(unique(out$clusters[!is.na(out$clusters)]))
      if (final_k < clusters) {
        warning(
          "Requested ", clusters,
          " clusters, but returned ", final_k,
          " after removing empty/undersized clusters."
        )
      }
    }

    # Find points for the cluster centers
    if (verbose == TRUE) {
      message("Identifying cluster centers.")
    }
    s <- cbind(out$clusters, out$distances)

    s_search <- s
    if (!is.null(candidates)) {
      s_search <- s[search_idx, , drop = FALSE]
      if (!is.null(candidate_weights_use)) {
        if (length(candidate_weights_use) != nrow(s_search)) {
          stop("Mapped candidate weights do not match candidate points.")
        }
        s_search[, 2] <- s_search[, 2] / candidate_weights_use
      }
    }

    zs1 <- s_search[, 1] |>
      unique() |>
      (
        \(x) x[!is.na(x)]
      )() |>
      sort()

    zs2 <- sapply(zs1, function(x) {
      min(s_search[s_search[, 1] == x, 2], na.rm = TRUE)
    })

    zs <- cbind(zs1, zs2)

    pts <- rep(NA_integer_, nrow(s))
    pts[search_idx] <- apply(s_search, 1, FUN = findpoint)

    out$points <- terra::crds(input) |>
      as.data.frame() |>
      dplyr::mutate(
        ID = pts,
        Index = seq_along(pts)
      ) |>
      tidyr::drop_na()

    out$points <- out$points[!duplicated(out$points$ID), ] |>
      dplyr::arrange(.data$ID)
  }

  # "Mapping" procedure for data frame
  if (inputisdf) {
    # Drop weights from the input
    if (ncol(input) > n_vars) {
      input <- input[, c(1:n_vars)]
    }

    out$points <- NA

    if (verbose == TRUE) {
      message("Assigning clusters.")
    }

    out$clusters <- apply(
      input,
      1,
      FUN = map_clusters_fun
    ) |> t()

    # Calculate weighted distances for the rows
    out$distances <- out$clusters[, 2] |> unname()
    out$clusters <- out$clusters[, 1] |> unname()
    if (!is.null(weights)) {
      if (verbose == TRUE) {
        message("Calculating weighted distances.")
      }
      out$distances <- safe_divide_by_weights(
        out$distances,
        weights,
        "Weights for input data"
      )
    }

    search_idx <- seq_along(out$clusters)
    if (!is.null(candidates)) {
      if (!is.null(candidates_use_index)) {
        search_idx <- candidates_use_index
      } else {
        search_idx <- unique(candidates)
      }
    }
    candidate_mask <- candidate_mask_from_index(
      length(out$clusters),
      search_idx
    )
    candidate_weights_eval <- NULL
    if (!is.null(candidate_weights_use)) {
      candidate_weights_eval <- rep(NA_real_, length(out$clusters))
      candidate_weights_eval[search_idx] <- candidate_weights_use
    }

    size_df <- cluster_size_df(out$clusters)
    eval_df <- compute_cluster_eval(
      out$clusters,
      out$distances,
      candidate_mask,
      candidate_weights_eval
    )
    viable_clusters <- eval_df$clust
    invalid_candidates <- setdiff(size_df$clust, viable_clusters)
    valid_clusters <- sort(setdiff(size_df$clust, invalid_candidates))

    if (length(valid_clusters) == 0) {
      stop(
        "All clusters were removed by candidate constraints and/or ",
        "min_cluster_size."
      )
    }

    if (length(invalid_candidates) > 0) {
      idx_move <- which(out$clusters %in% invalid_candidates)
      remapped <- reassign_to_valid_clusters(
        idx_move = idx_move,
        feature_values = input,
        valid_clusters = valid_clusters,
        centroids_all = mycentroids,
        weights = weights
      )
      out$clusters[idx_move] <- remapped$new_cluster
      out$distances[idx_move] <- remapped$new_dist

      # Reflect the newly added members in their destination centroids
      # before these centroids are used for any further pruning.
      mycentroids <- recompute_centroids_for_clusters(
        cluster_ids = out$clusters,
        feature_values = input,
        weights = weights,
        affected_clusters = unique(remapped$new_cluster),
        centroids_all = mycentroids
      )
    }

    clusters_before_pruning <- valid_clusters
    pruned <- prune_small_clusters(
      cluster_ids = out$clusters,
      distances = out$distances,
      feature_values = input,
      valid_clusters = valid_clusters,
      centroids_all = mycentroids,
      min_cluster_size = min_cluster_size,
      weights = weights
    )
    if (length(pruned$valid_clusters) == 0) {
      stop(
        "All clusters were removed by candidate constraints and/or ",
        "min_cluster_size."
      )
    }
    out$clusters <- pruned$clusters
    out$distances <- pruned$distances

    removed_any <- length(invalid_candidates) > 0 ||
      length(pruned$valid_clusters) < length(clusters_before_pruning)

    if (removed_any) {
      out$clusters <- renumber_clusters(out$clusters)
      final_k <- length(unique(out$clusters[!is.na(out$clusters)]))
      if (final_k < clusters) {
        warning(
          "Requested ", clusters,
          " clusters, but returned ", final_k,
          " after removing empty/undersized clusters."
        )
      }
    }

    # Find the cluster centers for the dataframe
    if (verbose == TRUE) {
      message("Identifying cluster centers.")
    }
    s <- cbind(out$clusters, out$distances)

    s_search <- s
    if (!is.null(candidates)) {
      s_search <- s[search_idx, , drop = FALSE]
      if (!is.null(candidate_weights_use)) {
        if (length(candidate_weights_use) != nrow(s_search)) {
          stop("Mapped candidate weights do not match candidate rows.")
        }
        s_search[, 2] <- s_search[, 2] / candidate_weights_use
      }
    }

    zs1 <- s_search[, 1] |>
      unique() |>
      (
        \(x) x[!is.na(x)]
      )() |>
      sort()

    zs2 <- sapply(zs1, function(x) {
      min(s_search[s_search[, 1] == x, 2], na.rm = TRUE)
    })

    zs <- cbind(zs1, zs2)

    pts <- rep(NA_integer_, nrow(s))
    pts[search_idx] <- apply(s_search, 1, FUN = findpoint)

    out$points <- data.frame(
      ID = pts,
      Index = seq_along(pts)
    ) |>
      tidyr::drop_na()

    out$points <- out$points[!duplicated(out$points$ID), ] |>
      dplyr::arrange(.data$ID)
  }


  # Write points to file if requested
  if (!inputisdf) {
    if (!is.null(filename_pts)) {
      if (tools::file_ext(filename_pts) == "shp") {
        shp <- TRUE
      }
    }

    if (shp == TRUE || sp_pts == TRUE) {
      points_sp <- terra::vect(
        out$points,
        geom = c("x", "y"),
        terra::crs(input)
      )
    }

    if (!is.null(filename_pts)) {
      if (verbose == TRUE) {
        message("Writing points to file.")
      }

      if (shp == FALSE) {
        do.call(
          utils::write.table,
          c(
            list(
              x = out$points,
              file = filename_pts
            ),
            args_pts
          )
        )
      } else {
        do.call(
          terra::writeVector,
          c(
            list(
              x = points_sp,
              filename = filename_pts
            ),
            args_pts
          )
        )
      }
    }
    if (sp_pts == TRUE) {
      out$points <- points_sp
    }
  } else {
    # write selected rows to file if requested?
  }
  out
}

# END
