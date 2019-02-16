context("test-model")

# Checks the output of the predict() method.
check_pred <- function(model, df) {
  weights <- model$weights
  features <- model$weights$feature
  mod <- model$mod
  
  # Check the linear predictor
  df <- dplyr::mutate(
    df,
    linear_pred = as.numeric(as.matrix(df[, features]) %*%
                               as.matrix(weights[, "estimate"])))
  expect_equal(df$linear_pred, 
               mod$linear.predictors[seq_along(df$linear_pred)])
  
  # Check predicted probabilities
  df$exp_linear_pred <- exp(df$linear_pred)
  partition <- df %>% 
    dplyr::group_by(id) %>% 
    dplyr::summarise(partition = sum(exp_linear_pred)) %>% 
    dplyr::ungroup()
  df <- dplyr::left_join(df, partition, by = "id")
  df$p <- df$exp_linear_pred / df$partition
  expect_equal(df$p, predict(mod, newdata = df, type = "response"))
  
  TRUE
}

test_that("testing features and modelling", {
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
                             features = voice_features()[c(
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
  expect_equal(mod$features, c("hutch_78", "vl_dist"))
  expect_is(mod$formula, "character")
  expect_is(mod$weights, "data.frame")
  expect_equal(mod$weights$feature, c("hutch_78", "vl_dist"))
  expect_is(mod$mod, "mclogit")
  expect_is(mod$eval, "list")
  expect_is(mod$eval$by_chord, "data.frame")
  expect_equal(nrow(mod$eval$by_chord), 8) # excludes first chord of each sequence
  expect_is(mod$eval$summary, "data.frame")
  expect_is(mod$perm_int, "data.frame")
  expect_equal(mod$perm_int$feature, c("hutch_78", "vl_dist"))
  check_pred(mod, na.omit(new))
  
  expect_equal(format_weights(mod),
               c(hutch_78 = 8.98687309122839,
                 vl_dist = -0.436249659994516))
  expect_error(format_weights("letters"),
               "don't know how to deal with weights of class character")
})

test_that("check_revoice_options", {
  corpus <- list(list(c(48, 60, 64), c(50, 60, 63), c(50)))
  expect_error(
    get_corpus_features(corpus,
                        revoice_from = "pc_set",
                        min_octave = 0,
                        max_octave = 1,
                        dbl_change = FALSE,
                        verbose = interactive()),
    "corpus has minimum pitch of 48 which cannot be reproduced with min_octave = 0")
  expect_error(
    get_corpus_features(corpus,
                        revoice_from = "pc_set",
                        min_octave = -1,
                        max_octave = 1,
                        dbl_change = TRUE,
                        dbl_min = 2,
                        dbl_max = 3,
                        verbose = interactive()),
    "corpus has minimum chord size of 1 which cannot be reproduced with dbl_min = 2")
  expect_error(
    get_corpus_features(corpus,
                        revoice_from = "pc_set",
                        min_octave = -1,
                        max_octave = 1,
                        dbl_change = TRUE,
                        dbl_min = 1,
                        dbl_max = 2,
                        verbose = interactive()),
    "corpus has maximum chord size of 3 which cannot be reproduced with dbl_max = 2")
  corpus <- list(list(c(48, 60, 64), c(50, 60, 63), c(73)))
  expect_error(
    get_corpus_features(corpus,
                        revoice_from = "pc_set",
                        min_octave = -1,
                        max_octave = 0,
                        dbl_change = FALSE,
                        verbose = interactive()),
    "corpus has maximum pitch of 73 which cannot be reproduced with max_octave = 0")
  # This should work without an error
  get_corpus_features(corpus,
                      revoice_from = "pc_set",
                      min_octave = -1,
                      max_octave = 1,
                      dbl_change = FALSE,
                      verbose = interactive())
})
