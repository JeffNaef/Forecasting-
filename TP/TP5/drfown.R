require(drf)            

drfown <-               function(X, Y,
                                 num.trees = 500,
                                 splitting.rule = "FourierMMD",
                                 num.features = 10,
                                 bandwidth = NULL,
                                 response.scaling = TRUE,
                                 node.scaling = FALSE,
                                 sample.weights = NULL,
                                 sample.fraction = 0.5,
                                 mtry = min(ceiling(sqrt(ncol(X)) + 20), ncol(X)),
                                 min.node.size = 15,
                                 honesty = TRUE,
                                 honesty.fraction = 0.5,
                                 honesty.prune.leaves = TRUE,
                                 alpha = 0.05,
                                 imbalance.penalty = 0,
                                 compute.oob.predictions = TRUE,
                                 num.threads = NULL,
                                 seed = stats::runif(1, 0, .Machine$integer.max),
                                 compute.variable.importance = FALSE) {
  
  # initial checks for X and Y
  if (is.data.frame(X)) {
    
    if (is.null(names(X))) {
      stop("the regressor should be named if provided under data.frame format.")
    }
    
    if (any(apply(X, 2, class) %in% c("factor", "character"))) {
      any.factor.or.character <- TRUE
      X.mat <- as.matrix(fastDummies::dummy_cols(X, remove_selected_columns = TRUE))
    } else {
      any.factor.or.character <- FALSE
      X.mat <- as.matrix(X)
    }
    
    mat.col.names.df <- names(X)
    mat.col.names <- colnames(X.mat)
  } else {
    X.mat <- X
    mat.col.names <- NULL
    mat.col.names.df <- NULL
    any.factor.or.character <- FALSE
  }
  
  if (is.data.frame(Y)) {
    
    if (any(apply(Y, 2, class) %in% c("factor", "character"))) {
      stop("Y should only contain numeric variables.")
    }
    Y <- as.matrix(Y)
  }
  
  if (is.vector(Y)) {
    Y <- matrix(Y,ncol=1)
  }
  
  
  #validate_X(X.mat)
  
  if (inherits(X, "Matrix") && !(inherits(X, "dgCMatrix"))) {
    stop("Currently only sparse data of class 'dgCMatrix' is supported.")
  }
  
  drf:::validate_sample_weights(sample.weights, X.mat)
  #Y <- validate_observations(Y, X)
  
  # set legacy GRF parameters
  clusters <- vector(mode = "numeric", length = 0)
  samples.per.cluster <- 0
  equalize.cluster.weights <- FALSE
  ci.group.size <- 1
  
  num.threads <- drf:::validate_num_threads(num.threads)
  
  all.tunable.params <- c("sample.fraction", "mtry", "min.node.size", "honesty.fraction",
                          "honesty.prune.leaves", "alpha", "imbalance.penalty")
  
  # should we scale or not the data
  if (response.scaling) {
    Y.transformed <- scale(Y)
  } else {
    Y.transformed <- Y
  }
  
  data <- drf:::create_data_matrices(X.mat, outcome = Y.transformed, sample.weights = sample.weights)
  
  # bandwidth using median heuristic by default
  if (is.null(bandwidth)) {
    bandwidth <- sqrt(drf:::medianHeuristic(Y.transformed))
  }
  
  
  args <- list(num.trees = num.trees,
               clusters = clusters,
               samples.per.cluster = samples.per.cluster,
               sample.fraction = sample.fraction,
               mtry = mtry,
               min.node.size = min.node.size,
               honesty = honesty,
               honesty.fraction = honesty.fraction,
               honesty.prune.leaves = honesty.prune.leaves,
               alpha = alpha,
               imbalance.penalty = imbalance.penalty,
               ci.group.size = ci.group.size,
               compute.oob.predictions = compute.oob.predictions,
               num.threads = num.threads,
               seed = seed,
               num_features = num.features,
               bandwidth = bandwidth,
               node_scaling = ifelse(node.scaling, 1, 0))
  
  if (splitting.rule == "CART") {
    ##forest <- do.call(gini_train, c(data, args))
    forest <- drf:::do.call.rcpp(drf:::gini_train, c(data, args))
    ##forest <- do.call(gini_train, c(data, args))
  } else if (splitting.rule == "FourierMMD") {
    forest <- drf:::do.call.rcpp(drf:::fourier_train, c(data, args))
  } else {
    stop("splitting rule not available.")
  }
  
  class(forest) <- c("drf")
  forest[["ci.group.size"]] <- ci.group.size
  forest[["X.orig"]] <- X.mat
  forest[["is.df.X"]] <- is.data.frame(X)
  forest[["Y.orig"]] <- Y
  forest[["sample.weights"]] <- sample.weights
  forest[["clusters"]] <- clusters
  forest[["equalize.cluster.weights"]] <- equalize.cluster.weights
  forest[["tunable.params"]] <- args[all.tunable.params]
  forest[["mat.col.names"]] <- mat.col.names
  forest[["mat.col.names.df"]] <- mat.col.names.df
  forest[["any.factor.or.character"]] <- any.factor.or.character
  
  if (compute.variable.importance) {
    forest[['variable.importance']] <- variableImportance(forest, h = bandwidth)
  }
  
  forest
}







