#' Model features
#' 
#' This function fits a sequential conditional logit model
#' that predicts chord voicings from chord features.
#' 
#' @param x
#' A corpus's feature analysis as created by 
#' \code{\link{get_corpus_features}}.
#' 
#' @param features
#' (Character vector)
#' Features to model, corresponding to columns of \code{x}.
#' By default this is extracted from the metadata of \code{x},
#' and corresponds to all computed features.
#' 
#' @param formula
#' (Formula)
#' The formula used to predict chord voicings.
#' The default is created by \code{\link{linear_formula}},
#' and corresponds to a simple linear combination of the features
#' specified in the \code{features} argument.
#' 
#' @param keep_model
#' (Logical scalar)
#' Determines whether the returned object should include the fitted model
#' from \code{\link[mclogit]{mclogit}},
#' which can be rather large.
#' 
#' @param eval_model
#' (Logical scalar)
#' Whether to compute and return model evaluation metrics.
#' 
#' @param perm_int
#' (Logical scalar)
#' Whether to compute and return permutation-based feature importance metrics
#' (also known as model reliance metrics).
#' 
#' @param verbose
#' (Logical scalar)
#' Whether to show progress messages during the computation.
#' 
#' @return 
#' An object of class \code{voicer_mod} with the following slots:
#' 
#' - \code{features} - 
#' A character vector listing the features used to fit the model.
#' 
#' - \code{formula} - 
#' A character vector corresponding to the formula used to fit the model.
#' 
#' - \code{weights} - 
#' A \code{\link[tibble]{tibble}} describing the fitted weights.
#' This tibble has five columns: 
#' \code{feature} (the name of the feature, or more generally the effect),
#' \code{estimate} (the estimate of the corresponding regression weight),
#' \code{std_err} (the standard error of the regression weight),
#' \code{z} (the z-statistic associated with the regression weight),
#' and \code{p} (the p-value associated with the regression weight).
#' 
#' - \code{mod} - 
#' A fitted model as created by \code{\link[mclogit]{mclogit}}.
#' 
#' - \code{eval} - 
#' Evaluation metrics for the fitted model.
#' This is a list with two components.
#' \code{summary} is a \code{\link[tibble]{tibble}} listing six model statistics:
#' \code{probability}, the mean probability assigned to the observed voicing;
#' \code{accuracy}, the mean accuracy when predicting the correct voicing;
#' \code{info_content}, the mean log probability of the observed voicing (base 2);
#' \code{num_options}, the mean number of candidate voicings for each chord;
#' \code{abs_rank}, the mean absolute rank of the chosen voicing
#' within the list of candidates as ranked by assigned probability;
#' \code{pct_rank}, the mean percentile rank of the chosen voicing
#' within the list of candidates as ranked by assigned probability.
#' \code{perm_int} is a \code{\link[tibble]{tibble}} listing permutation importances
#' (also known as model reliances) by feature,
#' with the permutation importance metrics mapping to 
#' the columns of \code{summary}.
#' 
#' @md
#' @export
model_features <- function(x,
                           features = attr(x, "features"),
                           formula = linear_formula(x, features),
                           keep_model = TRUE,
                           eval_model = TRUE,
                           perm_int = TRUE,
                           verbose = TRUE) {
  if (!is.data.frame(x))
    stop("'x' must be a data frame as created by 'get-corpus-features'")
  if (perm_int && !eval_model) {
    warning("cannot compute permutation importance without ",
            "evaluating the model; setting eval_model to TRUE.")
    eval_model <- TRUE
  }
  checkmate::qassert(keep_model, "B1")
  checkmate::qassert(eval_model, "B1")
  checkmate::qassert(perm_int, "B1")
  if (verbose) message("Excluding the first chord of each sequence...")
  x <- dplyr::filter(x, .data$pos > 1)
  if (verbose) message("Fitting model...")
  mod <- fit_model(formula, x, verbose)
  eval <- if (eval_model) eval_mod(mod, x)
  if (verbose) message("Getting permutation-based feature importances...")
  perm_int <- if (perm_int) get_perm_int(x, eval, mod, features, formula, verbose)
  weights <- get_weights(mod)
  if (!keep_model) mod <- NULL
  res <- list(
    features = features,
    formula = as.character(formula),
    weights = weights,
    mod = mod,
    eval = eval,
    perm_int = perm_int
  )
  class(res) <- c("voicer_model", class(res))
  res
}

#' Is it a voicer model object?
#' 
#' Checks whether an object is of class \code{voicer_model},
#' as produced by \code{\link{model_features}}.
#' 
#' @param x
#' Object to check.
#' 
#' @return 
#' Logical scalar.
#' 
#' @export
is_voicer_model <- function(x) {
  is(x, "voicer_model")
}

#' Is it a voicer weights object?
#' 
#' Checks whether an object is of class \code{voicer_weights},
#' as found in the \code{weights} slot of a \code{voicer_model}
#' object produced by \code{\link{model_features}}.
#' 
#' @param x
#' Object to check.
#' 
#' @return 
#' Logical scalar.
#' 
#' @export
is_voicer_weights <- function(x) {
  is(x, "voicer_weights")
}

fit_model <- function(formula, data, verbose) {
  mclogit::mclogit(formula, data = data, na.action = "na.exclude",
                   control = mclogit::mclogit.control(trace = verbose))
}

#' Linear formula
#' 
#' Defines a formula for the \code{formula} argument of 
#' \code{\link{model_features}} corresponding
#' to a simple linear combination of features.
#' 
#' @param x 
#' (Data frame)
#' Dataset with respect to which features are defined.
#' 
#' @param features
#' Character vector of features,
#' with each feature corresponding to a unique column 
#' of \code{x}.
#' 
#' @return 
#' A formula object to be passed to \code{\link{model_features}}.
#' 
#' @export
linear_formula <- function(x, features) {
  if (!checkmate::qtest(features, "S"))
    stop("'features' must be a character vector")
  if (!all(features %in% names(x)))
    stop("not all features found in the columns of 'x'")
  glue::glue("cbind(chosen, id) ~ {paste(features, collapse = '+')}") %>% 
    as.formula
}

get_weights <- function(x) {
  if (!is(x, "mclogit"))
    stop("'x' must be an object of class 'mclogit'")
  df <- summary(x)$coefficients
  features <- rownames(df)
  df <- df %>% 
    tibble::as_tibble() %>% 
    tibble::add_column(feature = features, .before = 1) %>% 
    dplyr::rename(
      estimate = "Estimate",
      std_err = "Std. Error",
      z = "z value",
      p = "Pr(>|z|)"
    ) 
  class(df) <- c("voicer_weights", class(df))
  df
}
