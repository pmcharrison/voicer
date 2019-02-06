context("test-exposed_outer_octaves")

test_that("examples", {
  # No change
  exposed_outer_octaves(c(0, 4, 7), c(0, 4, 7)) %>% expect_false()

  # Ascending
  exposed_outer_octaves(c(0, 4, 7), c(1, 4, 13)) %>% expect_false()
  exposed_outer_octaves(c(0, 4, 7), c(3, 4, 15)) %>% expect_true()

  # Descending
  exposed_outer_octaves(c(60, 64, 67), c(59, 64, 71)) %>% expect_false()
  exposed_outer_octaves(c(60, 64, 67), c(58, 64, 70)) %>% expect_false()
  exposed_outer_octaves(c(60, 64, 74), c(57, 63, 69)) %>% expect_true()

  # Contrary
  exposed_outer_octaves(c(60, 64), c(55, 67)) %>% expect_false()

  # One note
  exposed_outer_octaves(c(60), c(55, 67)) %>% expect_false()
  exposed_outer_octaves(c(60), c(67, 67 + 12)) %>% expect_true()
})
