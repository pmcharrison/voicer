#' @export
voice <- function(x, opt = voice_opt()) {
  UseMethod("voice")
}

#' @export
voice.vec <- function(x, opt = voice_opt()) {
  if (length(x) == 0L) return(x)
  type <- hrep::type(x)
  checkmate::qassert(type, "S1")
  f <- paste0("voice.vec_", type)
  do.call(f, args = list(x = x, opt = opt))
}

#' @export
voice.coded_vec <- function(x, opt = voice_opt()) {
  voice(hrep::decode(x), opt = opt)
}
