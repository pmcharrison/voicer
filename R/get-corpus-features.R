#' @export
get_corpus_features <- function(x,
                                revoice_from,
                                min_octave,
                                max_octave,
                                dbl_change,
                                dbl_min,
                                dbl_max,
                                cost_funs = "default") {
  purrr::map2(x, seq_along(x), function(seq, i) {
    "Analysing sequence {i}/{length(x)}..." %>% glue::glue() %>% message()
    get_seq_features(seq,
                     cost_funs = cost_funs,
                     revoice_from = revoice_from,
                     min_octave = min_octave,
                     max_octave = max_octave,
                     dbl_change = dbl_change,
                     dbl_min = dbl_min,
                     dbl_max = dbl_max)
  }) %>%
    add_seq_id(x) %>%
    do.call(rbind, .) %>%
    add_id()
}

add_seq_id <- function(z, original) {
  purrr::map2(z,
              seq_along(original),
              ~ tibble::add_column(.x,
                                   seq = .y,
                                   .before = 1))
}

add_id <- function(z) {
  z %>%
    tibble::add_column(id = NA, .before = 1) %>%
    dplyr::mutate(id = paste(seq, pos, sep = "-"),
                  id = factor(id, levels = unique(id), ordered = TRUE),
                  id = as.integer(id))
}

get_seq_features <- function(x, cost_funs, revoice_from, ...) {
  if (identical(cost_funs, "default")) cost_funs <- voice_cost_funs()
  message("Enumerating all possible voicings...")
  revoicings <- purrr::map(x, all_revoicings, revoice_from = revoice_from, ...)
  message("Iterating over sequence to compute features...")
  plyr::llply(seq_along(x), function(i) {
    get_features_for_continuations(
      funs = cost_funs,
      context = if (i == 1) NULL else x[[i - 1]],
      continuations = revoicings[[i]]
    ) %>%
      tibble::add_column(midi = purrr::map_chr(revoicings[[i]],
                                               paste, collapse = " "),
                         .before = 1) %>%
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
