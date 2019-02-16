#' Get corpus features
#' 
#' Analyzes a musical corpus to return a \code{\link[tibble]{tibble}}
#' describing feature values for all possible voicings
#' of a series of chord sequences.
#' 
#' @param x 
#' The corpus to analyze, expressed as a list of chord sequences,
#' where each chord sequence is expressed as a list of chords,
#' and each chord is expressed as a numeric vector of
#' non-duplicated MIDI note numbers in ascending order.
#' The corpus may also be expressed as a \code{\link[hrep]{corpus}}
#' object from the \code{hrep} package,
#' and chords may be expressed as \code{\link[hrep]{pi_chord}}
#' objects from the \code{hrep} package.
#' 
#' @param revoice_from
#' (Character scalar)
#' Determines the representation to which each chord is reduced 
#' before generating candidate revoicings.
#' Currently supported values are \code{"pc_chord"} and \code{"pc_set"},
#' corresponding to \code{\link[hrep]{pc_chord}}
#' and \code{\link[hrep]{pc_set}} from the \code{hrep} package respectively.
#' In the former case, bass pitch classes are preserved,
#' but in the latter they are not.
#' 
#' @param min_octave
#' (Numeric scalar)
#' The minimum octave from which the candidate voicings should be drawn,
#' expressed relative to middle C.
#' For example, \code{min_octave = -1} means that all voicings
#' will be drawn from C3 (MIDI note number 48) or above
#' (see also \code{\link{voice_opt}}).
#' An error will be thrown if the corpus contains chords that
#' span lower than permitted by this argument.
#' 
#' @param max_octave
#' (Numeric scalar)
#' The maximum octave from which the candidate voicings should be drawn,
#' expressed relative to middle C.
#' For example, \code{min_octave = 0} means that all voicings
#' will be drawn from the octave beginning on middle C or lower,
#' corresponding to pitch heights strictly less than C5
#' (MIDI note number 72)
#' (see also \code{\link{voice_opt}}).
#' An error will be thrown if the corpus contains chords that
#' span higher than permitted by this argument.
#' 
#' @param min_notes
#' (Integer scalar)
#' The minimum number of unique notes permitted in each candidate voicing.
#' An error will be thrown if the corpus contains chords with fewer 
#' notes than this number.
#' 
#' @param max_notes
#' (Integer scalar)
#' The maximum number of unique notes permitted in each candidate voicing.
#' An error will be thrown if the corpus contains chords with more 
#' notes than this number.
#' 
#' @param features
#' A list of features to compute for each chord voicing
#' (see \code{\link{voice_opt}}).
#' 
#' @param verbose
#' (Logical scalar)
#' Whether or not to print progress messages.
#'
#' @return
#' A \code{\link[tibble]{tibble}} where each row describes
#' a candidate voicing for a particular chord in the corpus,
#' along with its computed features, with the following columns:
#' - \code{id} - An integer ID variable that uniquely indexes each chord position
#' in the corpus, beginning at 1 and counting upwards.
#' - \code{seq} - An integer ID variable that uniquely indexes each sequence
#' in the corpus, beginning at 1 and counting upwards.
#' - \code{pos} - An integer ID variable that uniquely indexes each chord position
#' in each sequence, restarting at 1 for the beginning of each sequence,
#' and counting upwards.
#' - \code{chosen} - A logical variable indicating whether this particular 
#' voicing was the one observed in the original corpus.
#' Each level of \code{id} should have \code{chosen = TRUE} exactly once.
#' - \code{midi} - A character variable describing the pitch content
#' of the voicing, with each pitch represented as a MIDI note number,
#' and with these MIDI note numbers pasted together and separated 
#' by spaces.
#' - Subsequent columns provide features values as computed by 
#' the functions provided in \code{features}.
#'  
#' @md
#' @export
get_corpus_features <- function(x,
                                revoice_from,
                                min_octave,
                                max_octave,
                                min_notes,
                                max_notes,
                                features = voice_features(),
                                verbose = TRUE) {
  checkmate::qassert(verbose, "B1")
  checkmate::qassert(revoice_from, "S1")
  stopifnot(revoice_from %in% c("pc_set", "pc_chord"),
            !is.null(names(features)),
            !anyDuplicated(names(features)))
  check_revoice_options(x, 
                        verbose,
                        min_octave,
                        max_octave,
                        min_notes,
                        max_notes)
  purrr::map2(x, seq_along(x), function(seq, i) {
    if (verbose) "Analysing sequence {i}/{length(x)}..." %>% glue::glue() %>% message()
    get_seq_features(seq,
                     features = features,
                     revoice_from = revoice_from,
                     min_octave = min_octave,
                     max_octave = max_octave,
                     min_notes = min_notes,
                     max_notes = max_notes,
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
                                  min_notes,
                                  max_notes) {
  if (verbose) message("Checking revoicing options...")
  checkmate::qassert(min_octave, "N1")
  checkmate::qassert(max_octave, "N1")
  min_pitch <- min(purrr::map_dbl(x, function(seq) min(purrr::map_dbl(seq, min))))
  max_pitch <- max(purrr::map_dbl(x, function(seq) max(purrr::map_dbl(seq, max))))
  if (min_pitch < 60 + min_octave * 12)
    stop("corpus has minimum pitch of ", min_pitch, 
         " which cannot be reproduced with min_octave = ", min_octave)
  if (max_pitch >= 72 + max_octave * 12)
    stop("corpus has maximum pitch of ", max_pitch, 
         " which cannot be reproduced with max_octave = ", max_octave)
  
  checkmate::qassert(min_notes, "X1")
  checkmate::qassert(max_notes, "X1")
  min_size <- min(purrr::map_int(x, function(seq) min(purrr::map_int(seq, length))))
  max_size <- max(purrr::map_int(x, function(seq) max(purrr::map_int(seq, length))))
  
  if (min_size < min_notes)
    stop("corpus has minimum chord size of ", min_size, 
         " which cannot be reproduced with min_notes = ", min_notes)
  if (max_size > max_notes)
    stop("corpus has maximum chord size of ", max_size, 
         " which cannot be reproduced with max_notes = ", max_notes)
  
  invisible(TRUE)
}

#' Is it a corpus features object?
#' 
#' Checks whether an object is of class \code{corpus_features},
#' as produced by \code{\link{get_corpus_features}}.
#' 
#' @param x
#' Object to check.
#' 
#' @return 
#' Logical scalar.
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
    dplyr::mutate(id = paste(.data$seq, .data$pos, sep = "-"),
                  id = factor(.data$id, levels = unique(.data$id), ordered = TRUE),
                  id = as.integer(.data$id))
}

get_seq_features <- function(x, features, revoice_from, verbose, ...) {
  if (verbose) message("Enumerating all possible voicings...")
  revoicings <- purrr::map(x, all_revoicings, revoice_from = revoice_from, 
                           dbl_change = TRUE, ...)
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
