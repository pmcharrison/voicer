context("test-parallels")

test_that("examples", {
  expect_true(
    parallels(c(0, 4, 7), c(1, 5, 8))
  )
  expect_true(
    parallels(c(0, 4, 7), c(61, 65, 68))
  )
  expect_false(
    parallels(c(0, 4, 7), c(0, 4, 7))
  )
  expect_true(
    parallels(c(0, 4, 7), c(60, 64, 67))
  )
})
