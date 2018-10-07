context("test-all_pc_set_voicings")

test_that("examples", {
  expect_error(all_pc_set_voicings(c(0, 4, 7), min_octave = 2, max_octave = 1))
  expect_equal(all_pc_set_voicings(c(0, 4, 7), min_octave = 0, max_octave = 0),
               list(c(60, 64, 67)))
  expect_equal(all_pc_set_voicings(c(0, 4, 7), min_octave = -1, max_octave = -1),
               list(c(48, 52, 55)))
  expect_equal(all_pc_set_voicings(c(0, 4, 7), min_octave = -1, max_octave = 0),
               list(
                 c(48, 52, 55),
                 c(48, 52, 67),
                 c(48, 55, 64),
                 c(48, 64, 67),
                 c(52, 55, 60),
                 c(52, 60, 67),
                 c(55, 60, 64),
                 c(60, 64, 67)
  ))
})
