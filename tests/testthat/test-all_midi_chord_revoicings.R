context("test-all_midi_chord_revoicings")

test_that("empty input", {
  expect_equal(all_midi_chord_revoicings(numeric(), -1, 0),
               list())
})

test_that("simple example", {
  expect_equal(all_midi_chord_revoicings(c(60, 64, 67), -1, 0),
               list(c(48, 52, 55),
                    c(48, 52, 67),
                    c(48, 55, 64),
                    c(48, 64, 67),
                    c(60, 64, 67)))
})

test_that("duplicated pitch class", {
  expect_equal(all_midi_chord_revoicings(c(60, 72), -1, 0),
               list(c(48, 60)))
  expect_equal(all_midi_chord_revoicings(c(60, 72), -1, 1),
               list(c(48, 60),
                    c(48, 72),
                    c(60, 72)))
  expect_equal(all_midi_chord_revoicings(c(60, 64, 67, 72), -1, 0),
               list(c(48, 52, 55, 60),
                    c(48, 52, 60, 67),
                    c(48, 55, 60, 64),
                    c(48, 60, 64, 67)))
})
