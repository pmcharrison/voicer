context("test-all_voicings_pi_chord")

library(hrep)

test_that("empty input", {
  expect_error(all_voicings_pi_chord(numeric()),
               "empty chords not permitted")
})

test_that("simple example", {
  expect_equal(all_voicings_pi_chord(c(60, 64, 67),
                                     min_octave = -1, max_octave = 0,
                                     dbl_change = FALSE),
               list(c(48, 52, 55),
                    c(48, 52, 67),
                    c(48, 55, 64),
                    c(48, 64, 67),
                    c(60, 64, 67)) %>% purrr::map(pi_chord))
})

test_that("duplicated pitch class", {
  expect_equal(all_voicings_pi_chord(c(60, 72),
                                     min_octave = -1, max_octave = 0,
                                     dbl_change = FALSE),
               list(c(48, 60)) %>% purrr::map(pi_chord))
  expect_equal(all_voicings_pi_chord(c(60, 72),
                                     min_octave = -1, max_octave = 1,
                                     dbl_change = FALSE),
               list(c(48, 60),
                    c(48, 72),
                    c(60, 72)) %>% purrr::map(pi_chord))
  expect_equal(all_voicings_pi_chord(c(60, 64, 67, 72),
                                     min_octave = -1, max_octave = 0,
                                     dbl_change = FALSE),
               list(c(48, 52, 55, 60),
                    c(48, 52, 60, 67),
                    c(48, 55, 60, 64),
                    c(48, 60, 64, 67)) %>% purrr::map(pi_chord))
})


test_that("changing doubles", {
  expect_equal(
    all_voicings_pi_chord(
      c(60, 61),
      dbl_change = TRUE,
      min_octave = -1, max_octave = 0,
      min_notes = 2, max_notes = 4),
    list(
      c(48, 49),
      c(48, 61),
      c(60, 61),
      c(48, 49, 60),
      c(48, 60, 61),
      c(48, 49, 61),
      c(48, 49, 60, 61)
    ) %>% purrr::map(pi_chord))
})
