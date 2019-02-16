#' @export
get_corpus_features <- function(x,
                                revoice_from,
                                min_octave,
                                max_octave,
                                dbl_change,
                                dbl_min = NA,
                                dbl_max = NA,
                                features = voice_features(),
                                verbose = TRUE) {
  checkmate::qassert(verbose, "B1")
  stopifnot(!is.null(names(features)),
            !anyDuplicated(names(features)))
  check_revoice_options(x, 
                        verbose,
                        min_octave,
                        max_octave,
                        dbl_change,
                        dbl_min,
                        dbl_max)
  purrr::map2(x, seq_along(x), function(seq, i) {
    if (verbose) "Analysing sequence {i}/{length(x)}..." %>% glue::glue() %>% message()
    get_seq_features(seq,
                     features = features,
                     revoice_from = revoice_from,
                     min_octave = min_octave,
                     max_octave = max_octave,
                     dbl_change = dbl_change,
                     dbl_min = dbl_min,
                     dbl_max = dbl_max,
                     verbose = verbose)
  }) %>%
    add_seq_id(x) %>%
    do.call(rbind, .) %>%
    add_id() %>%
    add_feature_names(features) %>%
    add_class_corpus_features()
}

check_revoice_options <- function(x, 
                                  verbose,
                                  min_octave,
                                  max_octave,
                                  dbl_change,
                                  dbl_min,
                                  dbl_max) {
  if (verbose) message("Checking revoicing options...")
  min_pitch <- min(purrr::map_dbl(x, function(seq) min(purrr::map_dbl(seq, min))))
  max_pitch <- max(purrr::map_dbl(x, function(seq) max(purrr::map_dbl(seq, max))))
  if (min_pitch < 60 + min_octave * 12)
    stop("corpus has minimum pitch of ", min_pitch, 
         " which cannot be reproduced with min_octave = ", min_octave)
  if (max_pitch >= 72 + max_octave * 12)
    stop("corpus has maximum pitch of ", max_pitch, 
         " which cannot be reproduced with max_octave = ", max_octave)
  
  if (dbl_change) {
    if (is.na(dbl_min) || is.na(dbl_max))
      stop("if dbl_change = TRUE, then dbl_min and dbl_max cannot be NA")
    checkmate::qassert(dbl_min, "X1")
    checkmate::qassert(dbl_max, "X1")
    min_size <- min(purrr::map_int(x, function(seq) min(purrr::map_int(seq, length))))
    max_size <- max(purrr::map_int(x, function(seq) max(purrr::map_int(seq, length))))
    if (min_size < dbl_min)
      stop("corpus has minimum chord size of ", min_size, 
           " which cannot be reproduced with dbl_min = ", dbl_min)
    if (max_size > dbl_max)
      stop("corpus has maximum chord size of ", max_size, 
           " which cannot be reproduced with dbl_max = ", dbl_max)
  }
  invisible(TRUE)
}

#' @export
is_corpus_features <- function(x) {
  is(x, "corpus_features")
}

add_class_corpus_features <- function(z) {
  class(z) <- c("corpus_features", class(z))
  z
}

add_feature_names <- function(z, features) {
  attr(z, "features") <- names(features)
  z
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

get_seq_features <- function(x, features, revoice_from, verbose, ...) {
  if (verbose) message("Enumerating all possible voicings...")
  revoicings <- purrr::map(x, all_revoicings, revoice_from = revoice_from, ...)
  if (verbose) message("Iterating over sequence to compute features...")
  plyr::llply(seq_along(x), function(i) {
    get_features_for_continuations(
      funs = features,
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
  }, .progress = if (verbose) "text" else "none") %>%
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
