#' k-means sampling
#'
#' Produces a set of points sampled from a raster or point dataset.
#'
#' @param input An input `data.frame`, or a `SpatRaster` or `SpatVector` object created by package [terra]. The input should contain only numeric variables for clustering.
#' @param clusters Number of clusters.
#' @param ncells Number of cells to extract, otherwise uses all cells.
#' @param use_xy Should the geographic xy coordinates be used as variables for clustering?
#' @param only_xy Should the algorithm Use only xy coordinates?
#' @param weights A `SpatRaster` object or numeric vector with weights between 0 and 1.
#' @param layer_weights Numeric vector with weights for each input parameter.
#' @param xy_weight Numeric vector of weights for the x and y coordinates (repeated if length 1).
#' @param candidates Numeric vector with indices for the points or rows that can be selected as centers for the clusters, or a `SpatRaster` object with a mask for the areas that can be selected.
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
#' @param MiniBatch Use MiniBatchKmeans (fast, less accurate).
#' @param batch_size See MiniBatchKmeans.
#' @param init_frac See MiniBatchKmeans.
#' @param early_stop See MiniBatchKmeans.
#' @param filename_cl File names for rasters with clusters (1) and distances (2).
#' @param args_cl List with arguments for writing raster.
#' @param filename_d File name for distance rasters.
#' @param args_d arguments for writing distance rasters.
#' @param sp_pts Output locations as spatial points.
#' @param filename_pts Filename for output locations.
#' @param shp Write output locations as a shapefile.
#' @param args_pts Arguments for writing output pointsx.
#' @param cores Number of cpu cores to use.
#' @param verbose Print messages during processing.
#' @export
#' @importFrom methods is
#' @importFrom stats  complete.cases prcomp predict sd
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#' @importFrom methods is
#' @importFrom tidyr drop_na
#' @importFrom terra nlyr spatSample compareGeom geomtype
#' @importFrom ClusterR KMeans_rcpp MiniBatchKmeans
#' @importFrom dplyr arrange mutate
#' @importFrom fields rdist

sample_kmeans <- function(
    input = NULL,
    clusters = 3,
    ncells = NULL,
    use_xy = FALSE, # Add xy coordinates as variables for clustering
    only_xy = FALSE, # Use only xy coordinates
    weights = NULL, # Raster layer or numeric vector with weights between 0 and 1
    layer_weights = NULL,  # Numeric vector with weights for each input parameter.
    xy_weight = NULL,  # Numeric vector of weights for the x and y coordinates (repeated if length 1)
    candidates = NULL,
    scale = TRUE, # Center and scale variables
    pca = FALSE, # Use principal component analysis on variables
    tol_pca = 0, # Tolerance for pca (remove PCs below threshold)
    n_pcs = NULL, # Maximum number of principal components
    num_init = 1, # See KMeans_rcpp
    max_iters = 10, # See KMeans_rcpp
    initializer = NULL, # See KMeans_rcpp
    CENTROIDS = NULL, # See KMeans_rcpp
    tol_kmeans = 1e-04, # See KMeans_rcpp
    tol_opt = 0.3, # See KMeans_rcpp
    seed = NULL, # See KMeans_rcpp
    force_seed = FALSE,
    MiniBatch = FALSE, # Use MiniBatchKmeans (fast, less accurate)
    batch_size = 10, # See MiniBatchKmeans
    init_frac = 1, # See MiniBatchKmeans
    early_stop = 10, # See MiniBatchKmeans
    filename_cl = NULL, # File names for rasters with clusters (1) and distances (2)
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
  . <- NULL # To avoid warnings in the package check.

  backup_options <- options()
  options(error = traceback) # Did this make it work? Find out how to reset options

  if (is.null(input) & is.null(weights)) {
    stop("No input data.")
  }

  if (verbose == TRUE) {
    message("Preparing input data.")
  }

  if (is.null(input) & !is.null(weights)) {
    if (is.vector(weights)) {
      message("No input variables. Using weights as an input variable.")
      input <- data.frame(w = weights)
    } else {
      message("No input variables. Using only coordinates.")
      only_xy <- TRUE
      input <- weights
    }
  }

  # Extraction for rasters
  inputisraster <- methods::is(input, "SpatRaster")

  inputispoints <- FALSE
  if (methods::is(input, "SpatVector")) {
    if (terra::geomtype(input) == "points") {
      inputispoints <- TRUE
    }
  }

  inputisdf <- FALSE
  if (!inputisraster & !inputispoints) {
    inputisdf <- is.data.frame(input)
  }

  if ((inputisraster + inputispoints + inputisdf) != 1) {
    stop("Input must be either a data frame, a SpatRaster or a SpatVector with points.")
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

  # Extraction for rasters
  if (inputisraster) {
    if (!is.null(weights)) {
      if (!methods::is(weights, "SpatRaster")) {
        stop("When input is a SpatRaster, weights must also be a SpatRaster.")
      } else {
        if (terra::compareGeom(input, weights) == FALSE) {
          stop("Input and weights rasters do not match.")
        } else {
          input <- c(input, weights)
        }
      }
    }

    # Check candidates
    if (!is.null(candidates)) {
      stop <- FALSE
      if (!methods::is(candidates, "SpatRaster") & !methods::is(candidates, "SpatVector")) {
        stop_now <- TRUE
      }
      if (methods::is(candidates, "SpatVector")) {
        if (terra::geomtype(input) != "points") {
          stop_now <- TRUE
        }
      }
      if (stop_now) {
        stop("When input is a SpatRaster, candidates must be a SpatRaster or a SpatVector with points.")
      }
    }

    # Sampling (if applicable)
    if (is.null(ncells)) {  # Use all cells if ncells is NULL
      df <- as.data.frame(input, xy = TRUE, na.rm = TRUE)
      ncells <- nrow(df)
    } else {
      if (ncells < (clusters + 2)) {
        stop("ncells must at least be equal to the number of clusters + 2")
      }
      df <- terra::spatSample(input, ncells, xy = TRUE, as.df = TRUE)
      ncells <- nrow(df)
    }
    if (!is.null(weights)) # Weighted sampling
    {
      sampled_unique <- 0
      seed_loop <- seed
      while (sampled_unique < (clusters + 2)) {
        sampled <- sample(
          nrow(df),
          ncells,
          prob = df[, ncol(df)],
          replace = TRUE
        )

        sampled_unique <- sampled %>% unique() %>% length()
        seed_loop %<>% magrittr::add(1)
      }
      w <- df[sampled, ncol(df)]
      df <- df[sampled, -ncol(df)]
    }

    # Extract candidates (if applicable)
    if (!is.null(candidates)) {
      if (methods::is(candidates, "SpatRaster")) {
        if (terra::compareGeom(input, candidates) == FALSE) {
          stop("Input and candidates rasters do not match.")
        } else {
          candidates_sample <- df %>%
            dplyr::select(x, y) %>%
            terra::vect(geom = c("x", "y"), terra::crs(input)) %>%
            terra::extract(
              x = candidates,
              y = .,
              ID = FALSE,
              layer = 1
            ) %>%
            magrittr::extract2(1)

          candidates <- c(1:nrow(df))[!is.na(candidates_sample)]

          candidates_df <- df[, candidates]
        }
      }
      if (methods::is(candidates, "SpatVector")) {
        candidates_df <- candidates %>%
          terra::extract(
            x = input,
            y = .,
            ID = FALSE,
            xy = TRUE,
            layer = 1
          ) %>%
          tidyr::drop_na()
        }
      }
    }

  # Extraction for spatial points
  if (inputispoints) {
    if (!is.null(weights)) {
      # check weights
      if (!methods::is(weights, "SpatRaster") & !is.vector(weights)) {
        stop("When the input is points, the weights must be a numeric vector or SpatRaster object")
      }
      if (is.vector(weights)) {
        if (length(input) != length(weights)) {
          stop("The number of weights do not match the input points.")
        }
      } else {
        input$weights <- weights
      }
      if (methods::is(weights, "SpatRaster")) {
        weights_sample <- terra::extract(
          x = weights,
          y = input,
          ID = FALSE,
          layer = 1
        ) %>%
          magrittr::extract2(1)
        weights <- weights_sample
        input$weights <- weights
      }
    }

    # check candidates, if input is a points dataset
    if (!is.null(candidates)) {
      if (!methods::is(candidates, "SpatRaster") & !is.vector(candidates)) {
        stop("When the input is points, the candidates must be a numeric vector or SpatRaster object")
      }
      if (methods::is(candidates, "SpatRaster")) {
        candidates_sample <- terra::extract(
          x = candidates,
          y = input,
          ID = FALSE,
          layer = 1
        ) %>%
          magrittr::extract2(1)

        candidates <- c(1:length(input))[!is.na(candidates_sample)]
        candidates_df <- input[candidates, ]
      } else {
        candidates <- unique(candidates)
        candidates <- candidates[candidates <= nrow(input)]
        candidates_df <- input[candidates, ]
      }
    }

    # Sampling (if applicable)
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
        message("ncells is larger than the number of input points. Using all input points instead.")
      } else {
        # standard sampling
        sampled <- sample(nrow(df),
          ncells,
          replace = FALSE
        )
        df <- df[sampled, ]
      }
    } else {
      # weighted sampling
      sampled_unique <- 0
      seed_loop <- seed
      while (sampled_unique < (clusters + 2)) {
        sampled <- sample(
          nrow(df),
          ncells,
          prob = df[, ncol(df)],
          replace = TRUE
        )
        sampled_unique <- sampled %>% unique() %>% length()
        seed_loop %<>% magrittr::add(1)
      }
      w <- df[sampled, ncol(df)]
      df <- df[sampled, -ncol(df)]
    }
  }

  # Extraction for data frame
  if (inputisdf) {
    if (!is.null(weights)) {
      # check weights
      if (!is.vector(weights)) {
        stop("When the input is a data frame, the weights must be a numeric vector.")
      } else {
        if (nrow(input) != length(weights)) {
          stop("The number of weights do not match the input data.")
        } else {
          input$weights <- weights
        }
      }
    }
    # check candidates
    if (!is.null(candidates)) {
      if (!is.vector(candidates)) {
        stop("When the input is a data frame, the candidates must be a numeric vector.")
      } else {
        candidates <- unique(candidates)
        candidates <- candidates[candidates <= nrow(input)]
        candidates_df <- input[candidates, ]
      }
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
        message("ncells is larger than the number of input rows. Using all input rows instead.")
      } else {
        # standard sampling
        sampled <- sample(nrow(df),
                          ncells,
                          replace = FALSE
        )
        df <- df[sampled, ]
      }
    } else {
      # weighted sampling
      sampled_unique <- 0
      seed_loop <- seed
      while (sampled_unique < (clusters + 2)) {
        sampled <- sample(
          nrow(df),
          ncells,
          prob = df[, ncol(df)],
          replace = TRUE
        )
        sampled_unique <- sampled %>% unique() %>% length()
        seed_loop %<>% magrittr::add(1)
      }

      w <- df[sampled, ncol(df)]
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

  # Scaling and PCA
  # Combine feature weights for scaling
  if (!is.null(layer_weights) | !is.null(xy_weight)) {
    sds_scaler <- df %>% ncol() %>% rep(1, .)

    if ( (!is.null(xy_weight) ) & (use_xy == TRUE) ) {
      sds_scaler[1:2] <- xy_weight

      if ( (!is.null(layer_weights)) & (only_xy == FALSE)) {
        sds_scaler[-c(1:2)] <- layer_weights
      }
    } else {
      sds_scaler <- layer_weights
    }
  }

  # Scaling
  if ( (scale == TRUE) | (exists("sds_scaler")) )
    {
      if (verbose == TRUE) {
        message("Scaling input variables.")
      }
      if (!is.data.frame(df)) {
        df %<>% as.data.frame
      }
      means <- apply(df, 2, mean)
      if (scale == TRUE) {
        sds <- apply(df, 2, sd)
        if (use_xy == TRUE) {
          sds[1:2] <- max(sds[1:2])
        }
        sds[sds == 0] <- 1
      } else {
        sds <- df %>% ncol() %>% rep(1, .)
        scale <- TRUE
      }
      if (exists("sds_scaler")) {
        sds %<>% magrittr::divide_by(sds_scaler)
      }
      if (verbose == TRUE) {
        scaling <- rbind(means, sds)
        rownames(scaling) <- c("Mean", "SD")
        print(scaling)
      }
      df %<>%
        sweep(MARGIN = 2, STATS = means, check.margin = FALSE) %>%
        sweep(MARGIN = 2, FUN = "/", STATS = sds, check.margin = FALSE)
  }
  # Principal components analysis
  if (pca == TRUE)
    {
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
    df %<>% as.data.frame
  } # Make sure it's a data frame

  out <- list()

  # Run kmeans
  if (verbose == TRUE) {
    message("Running k-means.")
  }

  if (is.null(seed)) {
    seed <- sample(10000, 1)
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

    if (MiniBatch == FALSE) {
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
    n_complete <- myclusters$centroids %>%
      stats::complete.cases() %>%
      sum()

    if (n_complete == clusters) {
      runagain <- FALSE
    } else {
      diff_try <- clusters - n_complete
      clusters_try %<>%
        magrittr::add(diff_try) %>%
        min(., nrow(df) - 2) %>%
        max(., 1)
      seed_try %<>% magrittr::add(1)
    }
  }

  mycentroids <- myclusters$centroids %>%
    as.data.frame() %>%
    tidyr::drop_na()

  # Functions to map clusters
  if (pca == FALSE & scale == FALSE) {
    map_clusters_fun <- function(x) {
      if (x %>% sum() %>% is.na()) {
        out <- c(NA, NA)
      } else {
        dist <- x %>%
          matrix(1) %>%
          data.frame() %>%
          fields::rdist(., mycentroids)
        out <- c(which.min(dist), min(dist, na.rm = TRUE))
      }
      return(out)
    }
  }
  if (pca == FALSE & scale == TRUE) {
    map_clusters_fun <- function(x) {
      if (x %>% sum() %>% is.na()) {
        out <- c(NA, NA)
      } else {
        dist <- x %>%
          "-"(means) %>%
          "/"(sds) %>%
          matrix(1) %>%
          data.frame() %>%
          fields::rdist(., mycentroids)
        out <- c(which.min(dist), min(dist, na.rm = TRUE))
      }
      return(out)
    }
  }
  if (pca == TRUE & scale == FALSE) {
    map_clusters_fun <- function(x) {
      if (x %>% sum() %>% is.na()) {
        out <- c(NA, NA)
      } else {
        x %<>%
          matrix(1) %>%
          data.frame()
        colnames(x) <- pcs$rotation %>% rownames()
        dist <- x %>%
          stats::predict(pcs, .) %>%
          fields::rdist(., mycentroids)
        out <- c(which.min(dist), min(dist, na.rm = TRUE))
      }
      return(out)
    }
  }
  if (pca == TRUE & scale == TRUE) {
    map_clusters_fun <- function(x) {
      if (x %>% sum() %>% is.na()) {
        out <- c(NA, NA)
      } else {
        x %<>%
          "-"(means) %>%
          "/"(sds) %>%
          matrix(1) %>%
          data.frame()
        colnames(x) <- pcs$rotation %>% rownames()
        dist <- x %>%
          predict(pcs, .) %>%
          fields::rdist(., mycentroids)
        out <- c(which.min(dist), min(dist, na.rm = TRUE))
      }
      return(out)
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
    ) %>%
      terra::mask(., sum(input))
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

    # Calculate weighted distances
    out$distances <- out$clusters[[2]]
    out$clusters <- out$clusters[[1]]
    if (!is.null(weights)) {
      if (verbose == TRUE) {
        message("Calculating weighted distances.")
      }
      s <- c(out$distances, weights)
      calc_wdist <- function(x) {
        if (x %>% sum() %>% is.na()) {
          out <- NA
        } else {
          if (x[2] == 0) {
            out <- NA
          } else {
            out <- x[1] / x[2]
          }
        }
        return(out)
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

    # Write clusters and distances to files if requested
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

    # Find the cluster centers
    if (verbose == TRUE) {
      message("Identifying cluster centers.")
    }
    zs <- terra::zonal(out$distances, out$clusters, "min", na.rm = TRUE)
    s <- c(out$clusters, out$distances)

    findpoint <- function(x) {
      if (x %>% sum() %>% is.na()) {
        out <- NA
      } else {
        ismin <- zs[as.integer(x[1]), 2] == x[2]
        if (!ismin) {
          out <- NA
        } else {
          out <- as.integer(x[1])
        }
      }
      return(out)
    }

    if (is.null(cores)) {
      pts <- terra::app(s, fun = findpoint)
    } else {
      showConnections()

      cl <- parallel::makeCluster(cores)

      parallel::clusterExport(cl,
        "zs",
        envir = environment()
      )

      pts <- terra::app(s, fun = findpoint, cores = cl)

      parallel::stopCluster(cl)
      foreach::registerDoSEQ()
      rm(cl)
    }

    names(pts) <- "ID"

    out$points <- as.data.frame(pts, xy = TRUE, na.rm = TRUE) %>%
      dplyr::arrange(.data$ID)

    out$points <- out$points[!duplicated(out$points$ID), ]
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

    # Drop weights from the input
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
    ) %>% t()

    # Calculate weighted distances
    out$distances <- out$clusters[, 2] %>% unname()
    out$clusters <- out$clusters[, 1] %>% unname()
    if (!is.null(weights)) {
      if (verbose == TRUE) {
        message("Calculating weighted distances.")
      }
      out$distances <- out$distances / weights
    }

    # Find the cluster centers
    if (verbose == TRUE) {
      message("Identifying cluster centers.")
    }
    zs1 <- out$clusters %>%
      unique() %>%
      sort()

    zs2 <- sapply(zs1, function(x) {
      cl_min <- min(out$distances[out$clusters == x], na.rm = TRUE)
      return(cl_min)
    })

    zs <- cbind(zs1, zs2)
    s <- cbind(out$clusters, out$distances)

    findpoint <- function(x) {
      if (x %>% sum() %>% is.na()) {
        out <- NA
      } else {
        ismin <- zs[as.integer(x[1]), 2] == x[2]
        if (!ismin) {
          out <- NA
        } else {
          out <- as.integer(x[1])
        }
      }
      return(out)
    }

    pts <- apply(s, 1, FUN = findpoint)

    out$points <- terra::crds(input) %>%
      as.data.frame() %>%
      dplyr::mutate(
        ID = pts,
        Index = c(1:length(pts))
      ) %>%
      tidyr::drop_na()

    out$points <- out$points[!duplicated(out$points$ID), ] %>%
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
    ) %>% t()

    # Calculate weighted distances
    out$distances <- out$clusters[, 2] %>% unname()
    out$clusters <- out$clusters[, 1] %>% unname()
    if (!is.null(weights)) {
      if (verbose == TRUE) {
        message("Calculating weighted distances.")
      }
      out$distances <- out$distances / weights
    }

    # Find the cluster centers
    if (verbose == TRUE) {
      message("Identifying cluster centers.")
    }
    zs1 <- out$clusters %>%
      unique() %>%
      sort()

    zs2 <- sapply(zs1, function(x) {
      cl_min <- min(out$distances[out$clusters == x], na.rm = TRUE)
      return(cl_min)
    })

    zs <- cbind(zs1, zs2)
    s <- cbind(out$clusters, out$distances)

    findpoint <- function(x) {
      if (x %>% sum() %>% is.na()) {
        out <- NA
      } else {
        ismin <- zs[as.integer(x[1]), 2] == x[2]
        if (!ismin) {
          out <- NA
        } else {
          out <- as.integer(x[1])
        }
      }
      return(out)
    }

    pts <- apply(s, 1, FUN = findpoint)

    out$points <- data.frame(
      ID = pts,
      Index = c(1:length(pts))
    ) %>%
      tidyr::drop_na()

    out$points <- out$points[!duplicated(out$points$ID), ] %>%
      dplyr::arrange(.data$ID)
  }


  # Write points to file if requested
  if (!inputisdf) {
    if (!is.null(filename_pts)) {
      if (tools::file_ext(filename_pts) == "shp") {
        shp <- TRUE
      }
    }

    if (shp == TRUE | sp_pts == TRUE) {
      points_sp <- terra::vect(out$points, geom = c("x", "y"), terra::crs(input))
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



  options(backup_options)

  return(out)
}

# END
