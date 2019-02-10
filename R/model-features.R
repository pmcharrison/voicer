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
  x <- dplyr::filter(x, pos > 1)
  if (verbose) message("Fitting model...")
  mod <- fit_model(formula, x, verbose)
  eval <- if (eval_model) eval_mod(mod, x)
  if (verbose) message("Getting permutation-based feature importances...")
  perm_int <- if (perm_int) get_perm_int(x, eval, mod, features, formula, verbose)
  weights <- get_weights(mod)
  if (!keep_model) mod <- NULL
  res <- list(
    features = features,
    formula = formula,
    weights = weights,
    mod = mod,
    eval = eval,
    perm_int = perm_int
  )
  class(res) <- c("voicer_model", class(res))
  res
}

#' @export
is_voicer_model <- function(x) {
  is(x, "voicer_model")
}

#' @export
is_voicer_weights <- function(x) {
  is(x, "voicer_weights")
}

fit_model <- function(formula, data, verbose) {
  mclogit::mclogit(formula, data = data, na.action = "na.exclude",
                   control = mclogit::mclogit.control(trace = verbose))
}

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
