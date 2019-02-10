context("test-regression-test")

library(hrep)

test_that("examples", {
  expect_equal(
    list(
      c(55L, 62L, 71L),
      c(52L, 55L, 60L, 71L),
      c(54L, 62L, 69L),
      c(55L, 62L, 71L),
      c(50L, 57L, 66L, 71L),
      c(52L, 67L, 71L),
      c(48L, 52L, 67L, 71L),
      c(45L, 52L, 67L, 72L),
      c(43L, 50L, 71L),
      c(50L, 54L, 69L)),
    hcorp::classical_1[[1]][1:10] %>%
      voice(opt = cheung_2019_opt(verbose = FALSE)) %>%
      as.list() %>%
      lapply(as.integer)
  )
})
