eval_mod <- function(mod, dat) {
  pred <- predict(mod, type = "response", na.action = "na.exclude")
  eval_pred(dat, pred)
}

eval_pred <- function(dat, pred) {
  dat %>% 
    dplyr::select(id, seq, pos, chosen) %>% 
    dplyr::mutate(chosen = as.logical(chosen),
                  pred = pred) %>% 
    split_by_chord() %>% 
    purrr::map(eval_chord_pred) %>% 
    dplyr::bind_rows() %>% 
    summarise_chord_preds()
}

split_by_chord <- function(x) {
  split(x, x$id) %>% magrittr::set_names(NULL)
}

eval_chord_pred <- function(x) {
  id <- unique(x$id)
  stopifnot(length(id) == 1L)
  res <- tibble::tibble(
    id = id,
    probability = x %>% dplyr::filter(chosen) %>% dplyr::pull(pred),
    info_content = - log2(probability),
    num_options = nrow(x),
    abs_rank = rank(- x$pred)[x$chosen],
    pct_rank = (abs_rank - 0.5) / num_options
  )
  stopifnot(nrow(res) == 1L)
  res
}

summarise_chord_preds <- function(x) {
  num_missing <- sum(is.na(x$probability))
  list(
    by_chord = x,
    summary = x %>% 
      dplyr::select(- id) %>% 
      dplyr::summarise_all(mean, na.rm = TRUE) %>% 
      tibble::add_column(num_missing = num_missing)
  )
}
