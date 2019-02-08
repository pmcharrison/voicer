context("test-change_num_notes")

r.change_num_notes <- (function(x, y) {
  abs(length(x) - length(y))
})

f <- function(x, y) change_num_notes(list(x), y)

test_that("examples", {
  f(c(0, 4, 7), c(0, 3, 7)) %>% expect_equal(0)
  f(c(0, 4), c(0, 3, 7)) %>% expect_equal(1)
  f(c(0), c(0, 3, 7)) %>% expect_equal(2)
  f(c(0, 4, 7), c(0, 3)) %>% expect_equal(1)
  f(c(0, 4, 7), c(0)) %>% expect_equal(2)
  f(c(0, 4, 7), numeric()) %>% expect_equal(3)
})

test_that("vectorisation", {
  s1 <- purrr::map(1:100, ~ sort(sample(50:70, size = sample(5, 1))))
  s2 <- c(60, 64, 67)
  expect_equal(
    change_num_notes(s1, s2),
    purrr::map_int(s1, r.change_num_notes, s2)
  )
})
