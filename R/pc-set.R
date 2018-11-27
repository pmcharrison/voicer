#' @export
voice.vec_pc_set <- function(x, opt = voice_opt()) {
  if (any(purrr::map_lgl(x, function(z) length(z) == 0L)))
    stop("empty pitch-class sets not permitted")
  y <- purrr::map(x, function(pc_set) all_voicings_pc_set(pc_set, opt))
  if (any(purrr::map_lgl(y, function(z) length(z) == 1L)))
    stop("no legal revoicings found")
  seqopt::seq_opt(y, cost_funs = opt$cost_funs, progress = opt$progress) %>%
    hrep::vec(type = "pi_chord")
}

all_voicings_pc_set <- function(x, opt) {
  checkmate::qassert(x, "N[0,12)")
  if (length(x) == 0L) stop("empty pitch-class sets not permitted")
  stopifnot(!anyDuplicated(x))
  if (opt$dbl_min < length(x))
    stop("cannot voice this pitch-class set with ",
         opt$dbl_min, "notes without omitting pitch classes")
  sizes <- if (opt$dbl_change)
    seq(from = opt$dbl_min, to = opt$dbl_max) else
      length(x)
  purrr::map(sizes, function(size) {
    n_extra <- size - length(x)
    if (n_extra == 0L) all_voicings_pc_multiset(x, opt) else {
      extra <- gtools::combinations(n = length(x),
                                    r = n_extra,
                                    v = x,
                                    repeats.allowed = TRUE)
      purrr::map(seq_len(nrow(extra)), function(i) {
        all_voicings_pc_multiset(c(x, extra[i, ]), opt)
      }) %>% unlist(recursive = FALSE)
    }
  }) %>% unlist(recursive = FALSE)
}

all_voicings_pc_multiset <- function(x, opt) {
  checkmate::qassert(x, "N[0,12)")
  octaves <- seq(from = opt$min_octave, to = opt$max_octave)
  gtools::permutations(n = length(octaves),
                       r = length(x),
                       v = 60L + 12L * octaves,
                       repeats.allowed = TRUE) %>%
    (function(y) purrr::map(seq_len(nrow(y)), function(i) sort(y[i, ] + x))) %>%
    purrr::keep(function(z) !anyDuplicated(z)) %>%
    unique %>%
    purrr::map(hrep::pi_chord)
}
