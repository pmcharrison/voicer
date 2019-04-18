
# voicer

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/pmcharrison/voicer.svg?branch=master)](https://travis-ci.org/pmcharrison/voicer)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/pmcharrison/voicer?branch=master&svg=true)](https://ci.appveyor.com/project/pmcharrison/voicer)
[![Coverage
status](https://coveralls.io/repos/github/pmcharrison/voicer/badge.svg)](https://coveralls.io/r/pmcharrison/voicer?branch=master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2613565.svg)](https://doi.org/10.5281/zenodo.2613565)

Chord voicing is the art of assigning pitch heights to pitch classes.
This package, `voicer`, optimises the voicings of chord sequences
according to an objective cost function. The Viterbi algorithm is used
to guarantee solution optimality.

## Paper

Harrison, P. M. C., & Pearce, M. T. (2019). A computational model for
the analysis and generation of chord voicings. *PsyArXiv*.
[doi:10.31234/osf.io/wrgj7](https://doi.org/10.31234/osf.io/wrgj7)

## Installation

To install from GitHub:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("pmcharrison/voicer")
```

## Example usage

`voice` is the main function to use. We pass `voice` vectors of chords
as created by the `hrep` package. Important chord representations from
the `hrep` package are:

  - `pc_set()` - create a pitch-class set (unordered pitch classes from
    0 to 11)
  - `pc_chord()` - create a pitch-class chord (a pitch-class set where
    the bass note is identified)
  - `pi_chord()` - create a pitch chord (pitches as MIDI note numbers)

<!-- end list -->

``` r
library(hrep)
library(magrittr)
library(voicer)

pc_set(c(0, 4, 7))
#> Pitch-class set: 0 4 7
pc_chord(c(4, 0, 7))
#> Pitch-class chord: [4] 0 7
pi_chord(c(60, 64, 67))
#> Pitch chord: 60 64 67
```

Using the function `vec` from the `hrep` package, we combine lists of
chords into typed vectors.

``` r
x <- list(pc_set(c(0, 4, 7)), 
          pc_set(c(0, 5, 9)),
          pc_set(c(2, 7, 11)), 
          pc_set(c(0, 4, 7))) %>% vec("pc_set")
x
#> Vector of type 'pc_set', length = 4
```

We can then pass the resulting object to `voice`.

``` r
y <- voice(x)
y 
#> Vector of type 'pi_chord', length = 4
as.list(y)
#> [[1]]
#> Pitch chord: 48 52 67 72
#> 
#> [[2]]
#> Pitch chord: 48 53 69 72
#> 
#> [[3]]
#> Pitch chord: 50 55 67 71
#> 
#> [[4]]
#> Pitch chord: 52 55 67 72
```

We can do the same for vectors of type `pc_chord`.

``` r
list(pc_chord(c(0, 4, 7)), 
     pc_chord(c(9, 0, 5)),
     pc_chord(c(7, 2, 11)), 
     pc_chord(c(0, 4, 7))) %>% 
  vec("pc_chord") %>% 
  voice() %>% 
  as.list()
#> [[1]]
#> Pitch chord: 60 64 67 72
#> 
#> [[2]]
#> Pitch chord: 57 60 65 72
#> 
#> [[3]]
#> Pitch chord: 55 62 67 71
#> 
#> [[4]]
#> Pitch chord: 60 64 67 72
```

See `?voice` for more details.
