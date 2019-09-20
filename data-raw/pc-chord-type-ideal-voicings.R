# To speed this up, someone needs to rewrite the
# expand_harmonics function in the hrep package in C++.

voice_pc_chord_type <- function(pc_chord_type) {
  x <- hrep::vec(list(hrep::pc_chord(pc_chord_type)),
                 "pc_chord")
  size <- length(pc_chord_type)
  opt <- voicer::voice_opt(min_notes = 1L,
                           max_notes = pmax(5, size),
                           min_octave = -1, 
                           max_octave = 0,
                           features = voicer::voice_features(ideal_num_notes = 5L),
                           verbose = FALSE)
  voicer::voice(x, opt)[[1]]
}

generate_ideal_voicings <- function() {
  message("Getting ideal pc_chord_type voicings...")
  
  pc_chord_types <- hrep::list_chords("pc_chord_type")
  pc_chord_type_ideal_voicings <- plyr::llply(pc_chord_types,
                                              voice_pc_chord_type,
                                              .progress = "time")
  usethis::use_data(pc_chord_type_ideal_voicings, overwrite = TRUE)
}

generate_ideal_voicings()
