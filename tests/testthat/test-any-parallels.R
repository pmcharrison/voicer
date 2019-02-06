context("test-any-parallels")

library(magrittr)

test_that("examples", {
  # Parallel fifths
  any_parallels(c(0, 4, 7), c(1, 5, 8)) %>% expect_equal(TRUE)
  any_parallels(c(0, 4, 7), c(0, 4, 7)) %>% expect_equal(FALSE)
  any_parallels(c(0, 7, 12), c(2, 9, 12)) %>% expect_equal(TRUE)
  any_parallels(c(0, 4, 7, 12), c(2, 4, 9, 12)) %>% expect_equal(TRUE)
  any_parallels(c(-1, 0, 4, 7, 12), c(-1, 2, 4, 9, 12)) %>% expect_equal(TRUE)

  # Parallel compound fifths
  any_parallels(c(0, 4, 7 + 12), c(1, 5, 8 + 12)) %>% expect_equal(TRUE)
  any_parallels(c(0, 4, 7 + 12), c(0, 4, 7 + 12)) %>% expect_equal(FALSE)
  any_parallels(c(0, 7 + 12, 12 + 12), c(2, 9 + 12, 12 + 12)) %>% expect_equal(TRUE)
  any_parallels(c(0, 4, 7 + 12, 12 + 12), c(2, 4, 9 + 12, 12 + 12)) %>% expect_equal(TRUE)
  any_parallels(c(-1, 0, 4, 7, 12), c(-1, 2, 4, 9, 12)) %>% expect_equal(TRUE)

  # Parallel (compound) octaves
  any_parallels(c(0, 4, 7, 12), c(1, 4, 7, 13)) %>% expect_equal(TRUE)
  any_parallels(c(0, 4, 7, 12), c(1, 4, 7, 13) + 12) %>% expect_equal(TRUE)
  any_parallels(c(0, 4, 7, 12), c(0, 4, 7, 12)) %>% expect_equal(FALSE)
  any_parallels(c(0, 12, 15), c(3, 15, 18)) %>% expect_equal(TRUE)
})
