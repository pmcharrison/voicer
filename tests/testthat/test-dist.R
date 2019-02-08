context("test-dist")

test_that("misc", {
  x <- dist_above_top(65)
  x(c(50, 66)) %>% expect_equal(1)
  x(c(50, 60)) %>% expect_equal(0)

  x <- dist_below_bottom(43)
  x(c(50, 66)) %>% expect_equal(0)
  x(c(40, 60)) %>% expect_equal(3)

  x <- dist_from_middle(40)
  x(c(39, 41)) %>% expect_equal(0)
  x(c(38, 41)) %>% expect_equal(0.5)
})
