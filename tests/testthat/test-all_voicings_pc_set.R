context("test-all_voicings_pc_set")

library(hrep)

test_that("examples", {
  expect_error(all_voicings_pc_set(c(0, 4, 7), min_octave = 2, max_octave = 1, dbl_change = FALSE),
               "<min_octave> cannot be greater than <max_octave>")
  expect_equal(all_voicings_pc_set(c(0, 4, 7), min_octave = 0, max_octave = 0, dbl_change = FALSE),
               list(c(60, 64, 67)) %>% purrr::map(pi_chord))
  expect_equal(all_voicings_pc_set(c(0, 4, 7), min_octave = -1, max_octave = -1, dbl_change = FALSE),
               list(c(48, 52, 55)) %>% purrr::map(pi_chord))
  expect_equal(all_voicings_pc_set(c(0, 4, 7), min_octave = -1, max_octave = 0, dbl_change = FALSE),
               list(
                 c(48, 52, 55),
                 c(48, 52, 67),
                 c(48, 55, 64),
                 c(48, 64, 67),
                 c(52, 55, 60),
                 c(52, 60, 67),
                 c(55, 60, 64),
                 c(60, 64, 67)
               ) %>% purrr::map(pi_chord))
})

test_that("changing doubles", {
  expect_equal(all_voicings_pc_set(
    c(0, 1),
    dbl_change = TRUE,
    min_octave = -1, max_octave = 0,
    dbl_min = 2, dbl_max = 4),
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
    ) %>% purrr::map(pi_chord))
})
