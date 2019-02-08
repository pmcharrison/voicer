context("test-any-parallels")

library(magrittr)

r.any_parallels <- (function(x, y) {
  if (length(x) < 2 || length(y) < 2) return(FALSE)
  min_vl <- minVL::min_vl(x, y, elt_type = "pitch")
  n <- length(min_vl$start)
  pairs <- gtools::combinations(n, 2)
  for (i in seq_len(nrow(pairs))) {
    v1 <- pairs[i, 1]
    v2 <- pairs[i, 2]
    moved <- (min_vl$end[v1] - min_vl$start[v1]) != 0
    if (moved) { # if the lower note has moved
      fifth_or_octave <- ((min_vl$start[v2] - min_vl$start[v1]) %% 12) %in% c(0, 7)
      if (fifth_or_octave) { # if the two notes originally formed a (compound) fifth or octave
        parallel <- # if the two notes moved by the same interval
          (min_vl$end[v1] - min_vl$start[v1]) ==
          (min_vl$end[v2] - min_vl$start[v2])
        if (parallel) {
          return(TRUE)
        }
      }
    }
  }
  FALSE
})

test_that("examples", {
  f <- function(x, y) any_parallels(list(x), y)[[1]]

  # Parallel fifths
  f(c(0, 4, 7), c(1, 5, 8)) %>% expect_equal(TRUE)
  f(c(0, 4, 7), c(0, 4, 7)) %>% expect_equal(FALSE)
  f(c(0, 7, 12), c(2, 9, 12)) %>% expect_equal(TRUE)
  f(c(0, 4, 7, 12), c(2, 4, 9, 12)) %>% expect_equal(TRUE)
  f(c(-1, 0, 4, 7, 12), c(-1, 2, 4, 9, 12)) %>% expect_equal(TRUE)

  # Parallel compound fifths
  f(c(0, 4, 7 + 12), c(1, 5, 8 + 12)) %>% expect_equal(TRUE)
  f(c(0, 4, 7 + 12), c(0, 4, 7 + 12)) %>% expect_equal(FALSE)
  f(c(0, 7 + 12, 12 + 12), c(2, 9 + 12, 12 + 12)) %>% expect_equal(TRUE)
  f(c(0, 4, 7 + 12, 12 + 12), c(2, 4, 9 + 12, 12 + 12)) %>% expect_equal(TRUE)
  f(c(-1, 0, 4, 7, 12), c(-1, 2, 4, 9, 12)) %>% expect_equal(TRUE)

  # Parallel (compound) octaves
  f(c(0, 4, 7, 12), c(1, 4, 7, 13)) %>% expect_equal(TRUE)
  f(c(0, 4, 7, 12), c(1, 4, 7, 13) + 12) %>% expect_equal(TRUE)
  f(c(0, 4, 7, 12), c(0, 4, 7, 12)) %>% expect_equal(FALSE)
  f(c(0, 12, 15), c(3, 15, 18)) %>% expect_equal(TRUE)

  # Vectorisation
  s1 <- purrr::map(1:100, ~ sort(sample(50:70, size = 5)))
  s2 <- c(60, 64, 67)
  expect_equal(
    any_parallels(s1, s2),
    purrr::map_lgl(s1, r.any_parallels, s2)
  )
})
