% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Spectrum.R
\name{Spectrum}
\alias{Spectrum}
\alias{class:Spectrum}
\alias{Spectrum-class}
\alias{[[<-,Spectrum-method}
\alias{[<-,Spectrum-method}
\alias{$<-,Spectrum-method}
\alias{Spectrum_elements}
\title{Spectrum class}
\arguments{
\item{...}{Mandatory spectrum elements or list thereof. See
\code{Spectrum_elements} for their names and classes.}
}
\description{
The \code{Spectrum} class is a simple list of elements. There are two
classes of elements. Mandatory elements such as the MS level of
the spectrum (\code{msLevel}), its retention time (\code{rtime}), ... that
must be of a pre-defined class (see the \code{Spectrum_elements} vector
for their names and classes). Optional elements with arbitrary
names and classes can also be added.

If, as some point, some elements were to become mandatory, their
names and classes would simply need to be added to the
\code{Spectrum_elements} vector.

Object are created with the \code{Spetrum(...)}
constructor. \code{Spectrum()} generates a instance with empty valid
values.
}
\examples{
## An empty spectrum
sp <- Spectrum()
sp

## Mandatory elements
Spectrum_elements

## Updating mandatory elements
sp$msLevel
class(sp$msLevel)
sp$msLevel <- 2L ## works
sp$msLevel
try(sp$msLevel <- "not an integer")
sp$msLevel

pa <- PeakAnnotation(peaks = list(c(345.3, 345.32, 345.35, 345.39, 345.4),
                                  657.01,
                                  1231.182),
                     annotations = list("a5", "z3", "y7"))
sp$peakAnnotation
sp$peakAnnotation <- pa
sp$peakAnnotation

## Adding arbitrary elements
sp$foo <- "bar"
sp

## All current elements (mandatory and optional) and their
## respective classes
sapply(sp, class)

## Setting intensity and m/z values. These must be set together,
## as these two elements must be of the same length.
mz <- as.numeric(sample(1000, 10))
i <- as.numeric(sample(1e3:1e5, 10))
sp[c("intensity", "mz")] <- list(i, mz)
plot(sp$mz, sp$intensity, type = "h")
}
