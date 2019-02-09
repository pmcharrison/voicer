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
                             dbl_max = 4,
                             features = voice_cost_funs()[c(
                               "hutch_78", "vl_dist", "melody_dist", "outer_parallels"
                             )],
                             verbose = interactive())
  old <- readRDS(system.file("regression-tests/get-corpus-features.rds", 
                             package = "voicer"))

  expect_equal(attr(new, "features"), c("hutch_78", "vl_dist",
                                        "melody_dist", "outer_parallels"))
  expect_true(is_corpus_features(new))

  new_2 <- new %>%
    dplyr::select(seq, pos, chosen,
                  hutch_78, vl_dist, melody_dist, outer_parallels) %>%
    as.data.frame
  old_2 <- old %>%
    dplyr::select(- c(id, mean_pitch, min_pitch, max_pitch)) %>%
    dplyr::rename(outer_parallels = parallels) %>%
    as.data.frame
  expect_equal(new_2, old_2, check.attributes = FALSE)
  
  set.seed(1)
  mod <- model_features(new,
                        features = c("hutch_78", "vl_dist"),
                        verbose = FALSE)
  
  expect_true(is_voicer_model(mod))
  expect_true(is_voicer_weights(mod$weights))
  expect_is(mod$weights, "data.frame")
  expect_equal(mod$weights$feature, c("hutch_78", "vl_dist"))
  expect_is(mod$mod, "mclogit")
  expect_is(mod$eval, "list")
  expect_is(mod$eval$by_chord, "data.frame")
  expect_equal(nrow(mod$eval$by_chord), 10)
  expect_is(mod$eval$summary, "data.frame")
  expect_is(mod$perm_int, "data.frame")
  expect_equal(mod$perm_int$feature, c("hutch_78", "vl_dist"))
})
