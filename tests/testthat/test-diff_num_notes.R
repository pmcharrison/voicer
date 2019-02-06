context("test-diff_num_notes")

test_that("examples", {
  diff_num_notes(c(0, 4, 7), c(0, 3, 7)) %>% expect_equal(0)
  diff_num_notes(c(0, 4), c(0, 3, 7)) %>% expect_equal(1)
  diff_num_notes(c(0), c(0, 3, 7)) %>% expect_equal(2)
  diff_num_notes(c(0, 4, 7), c(0, 3)) %>% expect_equal(1)
  diff_num_notes(c(0, 4, 7), c(0)) %>% expect_equal(2)
  diff_num_notes(c(0, 4, 7), c()) %>% expect_equal(3)
})
