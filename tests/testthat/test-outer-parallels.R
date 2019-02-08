context("test-outer-parallels")

r.outer_parallels <- (function(x, y) {
  if (length(x) < 2 || length(y) < 2) return(FALSE)
  x_bass <- min(x)
  x_treble <- max(x)
  x_int <- x_treble - x_bass
  if ((x_int %% 12) %in% c(0, 7)) {
    y_bass <- min(y)
    y_treble <- max(y)
    y_int <- y_treble - y_bass
    if (y_int == x_int && x_bass != y_bass) TRUE else FALSE
  } else FALSE
}) %>% seqopt::cost_fun(context_sensitive = TRUE)

f <- function(x, y) outer_parallels(list(x), y)

test_that("examples", {
  expect_true(
    f(c(0, 4, 7), c(1, 5, 8))
  )
  expect_true(
    f(c(0, 4, 7), c(61, 65, 68))
  )
  expect_false(
    f(c(0, 4, 7), c(0, 4, 7))
  )
  expect_true(
    f(c(0, 4, 7), c(60, 64, 67))
  )
})

test_that("vectorisation", {
  s1 <- purrr::map(1:300, ~ sort(sample(50:70, size = 3)))
  s2 <- c(60, 63, 67)
  expect_equal(
    outer_parallels(s1, s2),
    purrr::map_lgl(s1, r.outer_parallels, s2)
  )
})
