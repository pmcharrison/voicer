#' @export
voice.vec_pc_set <- function(x, opt = voice_opt()) {
  if (any(purrr::map_lgl(x, function(z) length(z) == 0L)))
    stop("empty pitch-class sets not permitted")
  if (opt$verbose) message("Enumerating all possible chord voicings...")
  y <- purrr::map(x, function(pc_set) all_voicings_pc_set(
    pc_set,
    opt$min_octave, opt$max_octave,
    opt$dbl_change, opt$dbl_min, opt$dbl_max
  ))
  if (any(purrr::map_lgl(y, function(z) length(z) == 1L)))
    stop("no legal revoicings found")
  seqopt::seq_opt(y,
                  cost_funs = opt$cost_funs,
                  weights = opt$weights,
                  minimise = FALSE,
                  exponentiate = TRUE,
                  verbose = opt$verbose) %>%
    hrep::vec(type = "pi_chord")
}

#' All voicings (pitch-class set)
#'
#' Lists all the possible voicings for an object of class
#' \code{\link[hrep]{pc_set}}.
#' @param x Pitch-class set to voice.
#' @return A list of possible voicings.
#' @export
all_voicings_pc_set <- function(x,
                                min_octave, max_octave,
                                dbl_change, dbl_min, dbl_max) {
  x <- as.numeric(x)
  checkmate::qassert(x, "N[0,12)")
  if (length(x) == 0L) stop("empty pitch-class sets not permitted")
  stopifnot(!anyDuplicated(x))
  if (dbl_change && dbl_max < length(x))
    stop("cannot voice this pitch-class set with ",
         dbl_min, " notes without omitting pitch classes")
  sizes <- if (dbl_change)
    seq(from = pmax(length(x),
                    dbl_min),
        to = dbl_max) else
          length(x)
  purrr::map(sizes, function(size) {
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
}
all_voicings_pc_set <- memoise::memoise(all_voicings_pc_set)

#' All voicings (pitch-class multiset)
#'
#' Lists all the possible voicings for a pitch-class multiset.
#' @param x Pitch-class multiset to voice, expressed as a numeric vector
#' with potentially repeated elements.
#' @return A list of possible voicings.
#' @export
all_voicings_pc_multiset <- function(x, min_octave, max_octave) {
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
    purrr::map(hrep::pi_chord)
}
all_voicings_pc_multiset <- memoise::memoise(all_voicings_pc_multiset)
