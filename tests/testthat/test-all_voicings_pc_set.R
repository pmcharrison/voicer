context("test-all_voicings_pc_set")

test_that("examples", {
  expect_error(all_voicings_pc_set(c(0, 4, 7), voice_opt(min_octave = 2, max_octave = 1)))
  expect_equal(all_voicings_pc_set(c(0, 4, 7), voice_opt(min_octave = 0, max_octave = 0)),
               list(c(60, 64, 67)) %>% purrr::map(hutil::as.pi_chord))
  expect_equal(all_voicings_pc_set(c(0, 4, 7), voice_opt(min_octave = -1, max_octave = -1)),
               list(c(48, 52, 55)) %>% purrr::map(hutil::as.pi_chord))
  expect_equal(all_voicings_pc_set(c(0, 4, 7), voice_opt(min_octave = -1, max_octave = 0)),
               list(
                 c(48, 52, 55),
                 c(48, 52, 67),
                 c(48, 55, 64),
                 c(48, 64, 67),
                 c(52, 55, 60),
                 c(52, 60, 67),
                 c(55, 60, 64),
                 c(60, 64, 67)
               ) %>% purrr::map(hutil::as.pi_chord))
})

test_that("changing doubles", {
  expect_equal(all_voicings_pc_set(
    c(0, 1),
    voice_opt(dbl_change = TRUE,
              min_octave = -1, max_octave = 0,
              dbl_min = 2, dbl_max = 4)),
    list(
      c(48, 49),
      c(48, 61),
      c(49, 60),
      c(60, 61),
      c(48, 49, 60),
      c(48, 60, 61),
      c(48, 49, 61),
      c(49, 60, 61),
      c(48, 49, 60, 61)
    ) %>% purrr::map(hutil::as.pi_chord))
})