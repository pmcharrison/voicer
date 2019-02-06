context("test-diff_num_notes")

test_that("examples", {
  f <- diff_num_notes(3)
  c(0, 4, 7) %>% f %>% expect_equal(0)
  c(0, 4, 7, 10) %>% f %>% expect_equal(1)
  c(0) %>% f %>% expect_equal(2)

  f <- diff_num_notes(2)
  c(0, 4, 7) %>% f %>% expect_equal(1)
  c(0, 4, 7, 10) %>% f %>% expect_equal(2)
  c(0, 4) %>% f %>% expect_equal(0)
  c(0) %>% f %>% expect_equal(1)
})
