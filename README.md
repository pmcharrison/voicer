
# voicer

Chord sequences can be voiced in different ways, some better than
others. This package, `voicer`, tries to find optimal voicings for chord
sequences.

Status: **pre-alpha**

## Example usage

`voice` is the main function to use. We pass `voice` vectors of chords
as created by the `hutils` package. Important chord representations from
the `hutils` package are:

  - `pc_set()` - create a pitch-class set (unordered pitch classes from
    0 to 11)
  - `pc_chord()` - create a pitch-class chord (a pitch-class set where
    the bass note is identified)
  - `pi_chord()` - create a pitch chord (pitches as MIDI note numbers)

<!-- end list -->

``` r
library(hutil)
library(magrittr)
library(voicer)

pc_set(c(0, 4, 7))
#> Pitch-class set: 0 4 7
pc_chord(4, c(0, 4, 7))
#> Pitch-class chord: [4] 0 7
pi_chord(c(60, 64, 67))
#> Pitch chord: 60 64 67
```

Using the function `vec` from the `hutils` package, we combine lists of
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
#> Pitch chord: 43 64 72
#> 
#> [[2]]
#> Pitch chord: 45 65 72
#> 
#> [[3]]
#> Pitch chord: 43 62 71
#> 
#> [[4]]
#> Pitch chord: 43 64 72
```

We can do the same for vectors of type `pc_chord` and `pi_chord`.

``` r
list(pc_chord(0, c(4, 7)), 
     pc_chord(9, c(0, 5)),
     pc_chord(7, c(2, 11)), 
     pc_chord(0, c(4, 7))) %>% 
  vec("pc_chord") %>% 
  voice() %>% 
  as.list()
#> [[1]]
#> Pitch chord: 48 55 76
#> 
#> [[2]]
#> Pitch chord: 45 60 77
#> 
#> [[3]]
#> Pitch chord: 43 59 74
#> 
#> [[4]]
#> Pitch chord: 48 55 76
```

By default, revoicing chords of type `pi_chord` preserves the
pitch-class of the bass note. To allow different inversions, you can
first convert your `pi_chord` objects to `pc_set` objects.

``` r
list(pi_chord(c(60, 64, 67)), 
     pi_chord(c(60, 65, 69)),
     pi_chord(c(62, 67, 71)), 
     pi_chord(c(60, 64, 67))) %>% 
  vec("pi_chord") %>% 
  voice() %>% 
  as.list()
#> [[1]]
#> Pitch chord: 48 55 76
#> 
#> [[2]]
#> Pitch chord: 48 57 77
#> 
#> [[3]]
#> Pitch chord: 50 59 79
#> 
#> [[4]]
#> Pitch chord: 48 52 79
```

By default, revoicing chords of type `pi_chord` also preserves the
number of occurrences of each pitch class in the chord. This, and other
behaviours, can be changed using the `opt` argument to `voice()`.
