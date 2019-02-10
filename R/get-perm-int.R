get_perm_int <- function(dat, mod_eval, mod, features, formula, verbose) {
  plyr::llply(features, feature_perm_int, dat, mod_eval, mod, formula,
              .progress = if (verbose) "text" else "none") %>% 
    dplyr::bind_rows()
}

# Permute the feature, keep the model the same,
# and evaluate the model.
feature_perm_int <- function(feature, dat, mod_eval, mod, formula) {
  dat[[feature]] <- sample(dat[[feature]], size = nrow(dat), replace = FALSE)
  eval_mod(mod, dat) %>% 
    {.$summary} %>% 
    {mod_eval$summary - .} %>% 
    tibble::add_column(feature = feature, .before = 1) %>% 
    dplyr::select(- c(num_options, num_missing)) %>% 
    tibble::as_tibble()
}
