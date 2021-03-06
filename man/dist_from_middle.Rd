% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cf-dist.R
\name{dist_from_middle}
\alias{dist_from_middle}
\title{Distance from middle}
\usage{
dist_from_middle(middle)
}
\arguments{
\item{middle}{(Numeric scalar)
The pitch height to which the chord is compared,
specified as a MIDI note number where 60 corresponds to middle C.}
}
\description{
Defines and returns a \code{voicer} feature, suitable for passing
to the \code{features} argument of \code{\link{voice_opt}},
that compares the chord's mean pitch to a reference pitch
specified in the \code{middle} argument.
The resulting function takes one argument, \code{x},
corresponding to a chord represented as a numeric vector
of MIDI note numbers, without duplicates,
sorted in ascending order,
and returns an unsigned distance in semitones.
}
\note{
The resulting function is not vectorized.
}
