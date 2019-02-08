context("test-melody-dist")

r.melody_dist <- (function(x, y) {
  abs(max(y) - max(x))
})

f <- function(x, y) melody_dist(list(x), y)

test_that("misc", {
  f(c(60, 64), c(50, 65)) %>% expect_equal(1)
  f(c(60, 64), c(50, 66)) %>% expect_equal(2)
})

test_that('vectorise', {
  s1 <- purrr::map(1:300, ~ sort(sample(50:70, 3)))
  s2 <- c(60, 63, 72)
  expect_equal(
    melody_dist(s1, s2),
    purrr::map_dbl(s1, r.melody_dist, s2)
  )
})
