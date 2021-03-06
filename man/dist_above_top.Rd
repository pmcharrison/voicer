% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cf-dist.R
\name{dist_above_top}
\alias{dist_above_top}
\title{Distance above top}
\usage{
dist_above_top(top)
}
\arguments{
\item{top}{(Numeric scalar)
The pitch height to which the chord is compared,
specified as a MIDI note number where 60 corresponds to middle C.}
}
\description{
Defines and returns a \code{voicer} feature, suitable for passing
to the \code{features} argument of \code{\link{voice_opt}},
that computes the extent to which a chord spans 
above a predefined pitch height specified in the 
\code{top} argument, in semitones.
If the chord does not span above \code{top},
then the function returns \code{0}.
The resulting function takes one argument, \code{x},
corresponding to a chord represented as a numeric vector
of MIDI note numbers, without duplicates,
sorted in ascending order.
}
\note{
The resulting function is not vectorized.
}
