context("test-part-overlap")

r.part_overlap <- (function(x, y) {
  if (length(x) == 0 || length(y) == 0) return(FALSE)
  vl <- minVL::min_vl(x, y, elt_type = "pitch")
  n <- length(vl$start)
  cross_above <- any(vl$end[- n] > vl$start[- 1L])
  cross_below <- any(vl$end[- 1L] < vl$start[- n])
  cross_above || cross_below
}) %>% seqopt::cost_fun(context_sensitive = TRUE)

test_that("examples", {
  f <- function(x, y) part_overlap(list(x), y)[[1]]

  # No overlap
  f(c(0, 4, 7), c(0, 4, 7)) %>% expect_false()
  f(c(0, 4, 7), c(1, 5, 8)) %>% expect_false()

  # Cross above
  f(c(0, 4, 7), c(5, 6, 7)) %>% expect_true()
  f(c(0, 4, 10), c(5, 6, 9)) %>% expect_true()

  # Cross below
  f(c(60, 64, 67), c(48, 59, 67))
  f(c(60, 64, 67), c(60, 62, 63))

  # Vectorisation
  s1 <- purrr::map(1:100, ~ sort(sample(50:70, size = 5)))
  s2 <- c(55, 64, 70)

  z <- which(part_overlap(s1, s2) != purrr::map_lgl(s1, r.part_overlap, s2))

  f(s1[[4]], s2)
  r.part_overlap(s1[[4]], s2)
  s1[[4]]
  s2

  expect_equal(
    part_overlap(s1, s2),
    purrr::map_lgl(s1, r.part_overlap, s2)
  )
})

