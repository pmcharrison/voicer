eval_mod <- function(mod, dat) {
  # Note: predict.mclogit ignores na.action;
  # we must have already removed NA rows.
  pred <- predict(mod, newdata = dat, type = "response")
  if (length(pred) != nrow(dat)) stop("data contained unexpected missing values")
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
    accuracy = as.numeric(which.max(x$pred) == which(x$chosen)),
    info_content = - log2(probability),
    num_options = nrow(x),
    abs_rank = rank(- x$pred)[x$chosen],
    pct_rank = (abs_rank - 0.5) / num_options
  )
  stopifnot(nrow(res) == 1L)
  res
}

summarise_chord_preds <- function(x) {
  list(
    by_chord = x,
    summary = x %>% 
      dplyr::select(- id) %>% 
      dplyr::summarise_all(mean, na.rm = TRUE)
  )
}
