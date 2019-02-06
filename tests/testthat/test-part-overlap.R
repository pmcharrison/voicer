context("test-part-overlap")

test_that("examples", {
  # No overlap
  part_overlap(c(0, 4, 7), c(0, 4, 7)) %>% expect_false()
  part_overlap(c(0, 4, 7), c(1, 5, 8)) %>% expect_false()

  # Cross above
  part_overlap(c(0, 4, 7), c(5, 6, 7)) %>% expect_true()
  part_overlap(c(0, 4, 10), c(5, 6, 9)) %>% expect_true()

  # Cross below
  part_overlap(c(60, 64, 67), c(48, 59, 67))
  part_overlap(c(60, 64, 67), c(60, 62, 63))
})
