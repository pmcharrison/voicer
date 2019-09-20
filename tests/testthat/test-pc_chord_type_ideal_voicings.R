test_that("examples", {
  pc_chord_type <- hrep::pc_chord_type(c(0, 1, 6, 9))
  i <- as.integer(hrep::encode(pc_chord_type))
  voicing <- pc_chord_type_ideal_voicings[[i]]
  
  expect_equal(pc_chord_type, hrep::pc_chord_type(voicing))
  expect_equal(pc_chord_type_ideal_voicings[[i]],
               hrep::pi_chord(c(48, 54, 61, 66, 69)))
  
  j <- as.integer(hrep::encode(hrep::pc_chord_type(0)))
  expect_equal(pc_chord_type_ideal_voicings[[j]],
               hrep::pi_chord(c(48, 60)))
})
