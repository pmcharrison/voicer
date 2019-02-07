get_corpus_features <- function(x,
                                cost_funs = "default",
                                revoice_from = "pc_chord",
                                min_octave = -2,
                                max_octave = 1,
                                dbl_change = TRUE,
                                dbl_min = 3,
                                dbl_max = 4) {
  args <- as.list(environment())
  names(args)[1] <- ".x"
  do.call(purrr::map, append(args, values = list(.f = get_seq_features), after = 1)) %>%
    purrr::map2(seq_along(x), ~ tibble::add_column(.data = .x, seq = .y, .before = 1)) %>%
    do.call(rbind, .) %>%
    tibble::add_column(id = NA, .before = 1) %>%
    dplyr::mutate(id = paste(seq, pos, sep = "-"),
                  id = factor(id, levels = unique(id), ordered = TRUE),
                  id = as.integer(id))
}

get_seq_features <- function(x, cost_funs, revoice_from, ...) {
  if (identical(cost_funs, "default")) cost_funs <- voice_cost_funs()
  revoicings <- purrr::map(x, all_revoicings, revoice_from = revoice_from, ...)
  plyr::llply(seq_along(x), function(i) {
    get_features_for_continuations(
      funs = cost_funs,
      context = if (i == 1) NULL else x[[i - 1]],
      continuations = revoicings[[i]]
    ) %>%
      tibble::add_column(
        chosen = purrr::map_lgl(revoicings[[i]],
                                ~ identical(as.numeric(.), as.numeric(x[[i]]))),
        .before = 1) %>%
      tibble::add_column(pos = i, .before = 1)
  }, .progress = "text") %>%
    do.call(rbind, .)
}

all_revoicings <- function(chord, revoice_from, ...) {
  checkmate::qassert(chord, "N")
  checkmate::qassert(revoice_from, "S1")
  if (revoice_from == "pc_set") {
    voicer::all_voicings_pc_set(hrep::pc_set(chord), ...)
  } else if (revoice_from == "pc_chord") {
    voicer::all_voicings_pc_chord(hrep::pc_chord(chord), ...)
  } else stop("unrecognised value of 'revoice_from'")
}
