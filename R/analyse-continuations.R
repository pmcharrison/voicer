get_features_for_continuations <- function(funs,
                                           context,
                                           continuations,
                                           force_vectorise = TRUE) {
  purrr::map(funs,
             get_feature_for_continuation,
             context,
             continuations,
             force_vectorise) %>%
    tibble::as_tibble()
}

get_feature_for_continuation <- function(fun, context, continuations, force_vectorise) {
  if (seqopt::is_context_sensitive(fun))
    gffc_context_sensitive(fun, context, continuations, force_vectorise) else
      gffc_context_insensitive(fun, continuations)
}

gffc_context_insensitive <- function(fun, continuations) {
  purrr::map_dbl(continuations, ~ as.numeric(fun(.)))
}

gffc_context_sensitive <- function(fun, context, continuations, force_vectorise) {
  if (is.null(context))
    gffc_cs_no_context(continuations) else
      gffc_cs_with_context(fun, context, continuations, force_vectorise)
}

gffc_cs_no_context <- function(continuations) {
  rep(as.numeric(NA), times = length(continuations))
}

gffc_cs_with_context <- function(fun, context, continuations, force_vectorise) {
  if (seqopt::is_vectorised(fun))
    gffc_cs_wc_vectorised(fun, context, continuations) else
      gffc_cs_wc_unvectorised(fun, context, continuations, force_vectorise)
}

gffc_cs_wc_unvectorised <- function(fun, context, continuations, force_vectorise) {
  if (force_vectorise) {
    stop("if force_vectorise is TRUE, all context-sensitive functions ",
         "must be vectorised")
  } else {
    purrr::map_dbl(continuations, ~ as.numeric(fun(context, .)))
  }
}

gffc_cs_wc_vectorised <- function(fun, context, continuations) {
  if (seqopt::is_symmetric(fun)) {
    fun(continuations, context)
  } else if (seqopt::has_reverse(fun)) {
    fun(continuations, context, reverse = TRUE)
  } else stop("cannot use a vectorised cost function that is neither symmetric ",
              "nor has a reverse option")
}
