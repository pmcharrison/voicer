test_that("examples", {
  i <- 100
  pc_chord_type <- hrep::decode(hrep::coded_vec(i, "pc_chord_type"))[[1]]
  voicing <- pc_chord_type_ideal_voicings[[i]]
  
  expect_equal(pc_chord_type, hrep::pc_chord_type(voicing))
  expect_equal(pc_chord_type_ideal_voicings[[100]],
               hrep::pi_chord(c(48, 54, 61, 66, 69)))
  
  expect_equal(pc_chord_type_ideal_voicings[[1]],
               hrep::pi_chord(c(48, 60)))
})
