% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cf-dist.R
\name{melody_dist}
\alias{melody_dist}
\title{Melody distance}
\usage{
melody_dist(contexts, continuation)
}
\arguments{
\item{contexts}{A list of chords corresponding to the first chord
in the transition. 
Each chord should be represented as a numeric vector
of MIDI note numbers, without duplicates,
sorted in ascending order.
The function is vectorized over this argument.}

\item{continuation}{The second chord in the transition,
represented as a numeric vector
of MIDI note numbers, without duplicates,
sorted in ascending order.}
}
\value{
An integer vector, vectorized over \code{contexts},
providing the unsigned distance traversed 
by the highest voice in the transition
between the ith element of \code{contexts}
and \code{continuation}.
}
\description{
Computes the distance traversed by the top voice
in a chord transition.
}
\note{
This is a \code{voicer} feature suitable for passing
to the \code{features} argument of \code{\link{voice_opt}}.
}
