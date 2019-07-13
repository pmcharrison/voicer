#' @export
voice.vec_pc_set <- function(x, 
                             opt = voice_opt(),
                             fix_melody = rep(NA_integer_, times = length(x)), 
                             fix_content = lapply(x, function(...) integer()),
                             fix_chords = vector("list", length(x))) {
  if (any(purrr::map_lgl(x, function(z) length(z) == 0L)))
    stop("empty pitch-class sets not permitted")
  if (opt$verbose) message("Enumerating all possible chord voicings...")
  y <- purrr::pmap(
    list(x = x,
         fix_melody = fix_melody,
         fix_content = fix_content,
         fix_chord = fix_chords),
    all_voicings_pc_set,
    min_octave = opt$min_octave, 
    max_octave = opt$max_octave,
    dbl_change = opt$dbl_change, 
    min_notes = opt$min_notes, 
    max_notes = opt$max_notes
  )
  if (any(purrr::map_lgl(y, function(z) length(z) == 0L)))
    stop("no legal revoicings found")
  seqopt::seq_opt(y,
                  cost_funs = opt$features,
                  weights = opt$weights,
                  minimise = FALSE,
                  exp_cost = opt$exp_cost,
                  norm_cost = opt$norm_cost,
                  log_cost = opt$log_cost,
                  verbose = opt$verbose) %>%
    hrep::vec(type = "pi_chord")
}

#' All voicings (pitch-class set)
#'
#' Lists all the possible voicings for an object of class
#' \code{\link[hrep]{pc_set}}.
#' @param x Pitch-class set to voice.
#' @param min_octave See \code{\link{voice_opt}}.
#' @param max_octave See \code{\link{voice_opt}}.
#' @param dbl_change See \code{\link{voice_opt}}.
#' @param min_notes See \code{\link{voice_opt}}.
#' @param max_notes See \code{\link{voice_opt}}.
#' 
#' @param fix_melody
#' (Numeric scalar)
#' Determines the MIDI pitch for the melody (i.e. the top note of the voicing).
#' If NA, no constraint is applied.
#' 
#' @param fix_content
#' (Numeric vector)
#' Specifies a set of MIDI pitches that must be contained in the voicing.
#' 
#' @param fix_chord
#' (NULL or numeric vector)
#' If not NULL, the function returns just one voicing with the MIDI pitches
#' specified in this vector.
#' 
#' @return A list of possible voicings.
#' @export
all_voicings_pc_set <- function(x,
                                min_octave, max_octave,
                                dbl_change, min_notes, max_notes,
                                fix_melody = NA_integer_, 
                                fix_content = integer(),
                                fix_chord = NULL) {
  if (!is.null(fix_chord)) return(check_fix_chord(fix_chord))

  x <- as.numeric(x)
  checkmate::qassert(x, "N[0,12)")
  if (length(x) == 0L) stop("empty pitch-class sets not permitted")
  stopifnot(!anyDuplicated(x))
  if (dbl_change && max_notes < length(x))
    stop("cannot voice this pitch-class set with ",
         max_notes, " note(s) without omitting pitch classes")
  sizes <- if (dbl_change)
    seq(from = pmax(length(x),
                    min_notes),
        to = max_notes) else
          length(x)
  res <- purrr::map(sizes, function(size) {
    n_extra <- size - length(x)
    if (n_extra == 0L) all_voicings_pc_multiset(x, min_octave, max_octave) else {
      extra <- gtools::combinations(n = length(x),
                                    r = n_extra,
                                    v = x,
                                    repeats.allowed = TRUE)
      purrr::map(seq_len(nrow(extra)), function(i) {
        all_voicings_pc_multiset(c(x, extra[i, ]), min_octave, max_octave)
      }) %>% unlist(recursive = FALSE)
    }
  }) %>% unlist(recursive = FALSE)
  
  fix(res, fix_melody, fix_content)
}
all_voicings_pc_set <- memoise::memoise(all_voicings_pc_set)

check_fix_chord <- function(fix_chord) {
  if (!is.numeric(fix_chord)) stop("fix_chord must be NULL or numeric")
  if (anyDuplicated(fix_chord)) stop("fix_chord may not contain duplicates")
  if (anyNA(fix_chord)) stop("fix_chord may not contain NA values")
  list(hrep::pi_chord(fix_chord))
}

fix <- function(x, fix_melody, fix_content) {
  if (!is.na(fix_melody)) x <- purrr::keep(x, function(z) z[length(z)] == fix_melody)
  if (length(fix_content) > 0) x <- purrr::keep(x, function(z) all(fix_content %in% z))
  x
}

#' All voicings (pitch-class multiset)
#'
#' Lists all the possible voicings for a pitch-class multiset.
#' @param x Pitch-class multiset to voice, expressed as a numeric vector
#' with potentially repeated elements.
#' @param min_octave See \code{\link{voice_opt}}.
#' @param max_octave See \code{\link{voice_opt}}.
#' 
#' @param fix_melody
#' (Numeric scalar)
#' Determines the MIDI pitch for the melody (i.e. the top note of the voicing).
#' If NA, no constraint is applied.
#' 
#' @param fix_content
#' (Numeric vector)
#' Specifies a set of MIDI pitches that must be contained in the voicing.
#' 
#' @param fix_chord
#' (NULL or numeric vector)
#' If not NULL, the function returns just one voicing with the MIDI pitches
#' specified in this vector.
#' 
#' @return A list of possible voicings.
#' 
#' @export
all_voicings_pc_multiset <- function(x, 
                                     min_octave, 
                                     max_octave,
                                     fix_melody = NA_integer_, 
                                     fix_content = integer(),
                                     fix_chord = NULL) {
  if (!is.null(fix_chord)) return(check_fix_chord(fix_chord))
  x <- as.numeric(x)
  checkmate::qassert(x, "N[0,12)")
  if (min_octave > max_octave) stop("<min_octave> cannot be greater than <max_octave>")
  octaves <- seq(from = min_octave, to = max_octave)
  gtools::permutations(n = length(octaves),
                       r = length(x),
                       v = 60L + 12L * octaves,
                       repeats.allowed = TRUE) %>%
    (function(y) purrr::map(seq_len(nrow(y)), function(i) sort(y[i, ] + x))) %>%
    purrr::keep(function(z) !anyDuplicated(z)) %>%
    unique %>%
    purrr::map(hrep::pi_chord) %>% 
    fix(fix_melody, fix_content)
}
all_voicings_pc_multiset <- memoise::memoise(all_voicings_pc_multiset)
