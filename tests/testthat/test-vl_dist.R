context("test-vl_dist")

test_that("examples", {
  list(c(0, 4, 7), c(0, 3, 7)) %>%
    vl_dist(c(0, 4, 7)) %>%
    expect_equal(c(0, 1))

  list(c(0, 1, 2), c(3, 4, 5)) %>%
    vl_dist(c(3, 4, 5)) %>%
    expect_equal(c(9, 0))

  list(c(60, 61, 62), c(63, 64, 65)) %>%
    vl_dist(c(63, 64, 65)) %>%
    expect_equal(c(9, 0))
})
