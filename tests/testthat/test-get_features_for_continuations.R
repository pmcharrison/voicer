context("test-get_features_for_continuations")

test_that("examples", {
  continuations <- list(c(60, 64, 67), c(59, 62, 67), c(60, 65, 69),
                        c(59, 63, 64, 66))

  res <- get_features_for_continuations(
    voice_features(),
    context = c(60, 64, 67),
    continuations = continuations)

  expect_equal(res$any_parallels, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(res$change_num_notes, c(0, 0, 0, 1))
  expect_equal(res$diff_num_notes, c(1, 1, 1, 0))
  expect_equal(res$dist_above_top, c(0, 0, 0, 0))
  expect_equal(res$dist_below_bottom, c(0, 0, 0, 0))
  expect_equal(res$dist_from_middle, purrr::map_dbl(continuations, dist_from_middle(60)))
  expect_equal(res$exposed_outer_octaves,
               purrr::map_lgl(continuations,
                              ~ exposed_outer_octaves(list(c(60, 64, 67)), .)))
})
