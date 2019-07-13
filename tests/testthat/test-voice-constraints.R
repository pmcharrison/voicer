library(hrep)
library(magrittr)

test_that("constraints", {
  chords <- list(pc_set(c(0, 4, 7)),
                 pc_set(c(2, 7, 11)),
                 pc_set(c(0, 4, 7))) %>% 
    vec("pc_set")
  
  for (format in c("pc_set", "pc_chord", "pi_chord")) {
    input <- hrep::represent(chords, format)
    
    x <- input %>% 
      voice(fix_melody = c(72, 74, 76))
    
    expect_equal(x[[1]][4], 72)
    expect_equal(x[[2]][4], 74)
    expect_equal(x[[3]][4], 76)
    
    y <- input %>% 
      voice(fix_melody = c(72, NA, 67),
            opt = voice_opt(min_notes = 4, max_notes = 4))
    
    expect_equal(y[[1]][4], 72)
    expect_equal(y[[3]][4], 67)
    
    z <- input %>% 
      voice(fix_content = list(c(76, 79),
                               c(74, 79),
                               c(76, 79)))
    expect_equal(z[[1]][3:4], c(76, 79))
    expect_equal(z[[2]][3:4], c(74, 79))
    expect_equal(z[[3]][3:4], c(76, 79))
    
    z <- input %>% 
      voice(fix_content = list(c(76, 79),
                               numeric(),
                               c(67, 79)))
    
    expect_true(all(c(76, 79) %in% z[[1]]))
    expect_true(all(c(67, 79) %in% z[[3]]))
    
    z <- input %>% 
      voice(fix_chords = list(NULL,
                              c(55, 59, 62),
                              NULL))
    expect_equal(as.numeric(z[[2]]), c(55, 59, 62))
  }
})
