context("test-all_voicings_pc_chord")

library(hrep)

test_that("examples", {
  expect_equal(
    all_voicings_pc_chord(pc_chord(c(0, 4, 7)),
                          voice_opt(min_octave = -1, max_octave = 0)),
    list(c(48, 52, 55),
         c(48, 52, 67),
         c(48, 55, 64),
         c(48, 64, 67),
         c(60, 64, 67)) %>% purrr::map(pi_chord)
  )

  expect_equal(
    all_voicings_pc_chord(pc_chord(c(0, 7)),
                          voice_opt(min_octave = -1, max_octave = 0)),
    list(c(48, 55),
         c(48, 67),
         c(60, 67)) %>% purrr::map(pi_chord)
  )
})
