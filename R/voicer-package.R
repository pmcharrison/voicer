#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

#' Pitch-class chord type ideal voicings
#' 
#' This object provides ideal voicings for the alphabet of pitch-class chord types
#' as defined in the hrep package: \code{\link[hrep]{pc_chord_type}}.
#' Specifically, the object takes the form of a list,
#' where the ith element corresponds to an object of class \code{\link[hrep]{pi_chord}}
#' created by finding an ideal voicing for the ith element 
#' of the \code{\link[hrep]{pc_chord_type}} alphabet.
#' 
#' For details of how this voicing is computed, see the source code 
#' in \code{data-raw/pc-chord-type-ideal-voicings.R}. 
#' The general principle is that dissonance should be minimized while
#' keeping within certain constraints of tessitura
#' and preferring voicings containing a certain number of notes.
#' 
#' @name pc_chord_type_ideal_voicings
#' @docType data
#' @keywords data
NULL
