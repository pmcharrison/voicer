#' @export
voice <- function(x, opt = voice_opt()) {
  UseMethod("voice")
}

#' @export
voice.vec <- function(x, opt = voice_opt()) {
  if (hutil::is.empty(x)) return(x)
  type <- hutil::type(x)
  checkmate::qassert(type, "S1")
  f <- paste0("voice.vec_", type)
  do.call(f, args = list(x = x, opt = opt))
}
