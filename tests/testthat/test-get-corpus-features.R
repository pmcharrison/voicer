context("test-get-corpus-features")

test_that("corpus features regression test", {
  corpus <- list(
    hcorp::bach_chorales_1[[1]][1:5],
    hcorp::bach_chorales_1[[2]][1:5]
  )
  new <- get_corpus_features(corpus,
                             revoice_from = "pc_set",
                             min_octave = -2,
                             max_octave = 1,
                             dbl_change = TRUE,
                             dbl_min = 3,
                             dbl_max = 4)
  old <- readRDS(system.file("regression-tests/get-corpus-features.rds", package = "voicer"))

  new_2 <- new %>%
    dplyr::select(seq, pos, chosen,
                  hutch_78, vl_dist, melody_dist, outer_parallels) %>%
    as.data.frame
  old_2 <- old %>%
    dplyr::select(- c(id, mean_pitch, min_pitch, max_pitch)) %>%
    dplyr::rename(outer_parallels = parallels) %>%
    as.data.frame
  expect_equal(new_2, old_2)
})
