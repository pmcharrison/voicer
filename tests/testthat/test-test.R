context("test-voice")

test_that("voice.candidates", {
  x <- voiceR:::candidates(
    list(
      list(c(60, 64, 67), c(64, 67, 72), c(67, 72, 76)),
      list(c(62, 67, 71), c(67, 71, 74), c(71, 74, 79)),
      list(c(60, 64, 69), c(64, 69, 72), c(69, 72, 76))
    )
  )
  voiceR:::voice(x)
})
