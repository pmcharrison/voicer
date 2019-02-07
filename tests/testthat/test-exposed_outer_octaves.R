context("test-exposed_outer_octaves")

r.exposed_outer_octaves <- (function(x, y) {
  y_bass <- min(y)
  y_treb <- max(y)
  y_int <- y_treb - y_bass
  y_outer_octave <- (y_int > 0) && (y_int %% 12 == 0)
  res <- if (y_outer_octave) {
    x_bass <- min(x)
    x_treb <- max(x)
    treb_motion <- y_treb - x_treb
    bass_motion <- y_bass - x_bass
    similar_motion <- (treb_motion * bass_motion) > 0
    similar_motion &&
      abs(treb_motion) > 2L &&
      abs(bass_motion) > 2L
  } else {
    FALSE
  }
})

f <- function(x, y) exposed_outer_octaves(list(x), y)

test_that("examples", {
  # No change
  f(c(0, 4, 7), c(0, 4, 7)) %>% expect_false()

  # Ascending
  f(c(0, 4, 7), c(1, 4, 13)) %>% expect_false()
  f(c(0, 4, 7), c(3, 4, 15)) %>% expect_true()

  # Descending
  f(c(60, 64, 67), c(59, 64, 71)) %>% expect_false()
  f(c(60, 64, 67), c(58, 64, 70)) %>% expect_false()
  f(c(60, 64, 74), c(57, 63, 69)) %>% expect_true()

  # Contrary
  f(c(60, 64), c(55, 67)) %>% expect_false()

  # One note
  f(c(60), c(55, 67)) %>% expect_false()
  f(c(60), c(67, 67 + 12)) %>% expect_true()
})

test_that("vectorisation", {
  s1 <- purrr::map(1:300, ~ sort(sample(50:70, 3)))
  s2 <- c(60, 63, 72)
  expect_equal(
    exposed_outer_octaves(s1, s2),
    purrr::map_lgl(s1, r.exposed_outer_octaves, s2)
  )
})
