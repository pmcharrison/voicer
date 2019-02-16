# context("test-voice_weights")
# 
# test_that("misc", {
#   w0 <- voice_weights()
#   is.numeric(w0) %>% expect_true()
#   length(w0) %>% expect_equal(length(voice_features()))
# 
#   w1 <- voice_weights(melody_dist = 2)
#   w1["melody_dist"] %>% as.numeric %>% expect_equal(2)
# 
#   expect_error(
#     voice_weights(blah = 3),
#     "unrecognised cost function(s): blah",
#     fixed = TRUE
#   )
# })
