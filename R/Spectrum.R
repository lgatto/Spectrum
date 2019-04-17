##' @title Spectrum class
##'
##' @name Spectrum
##'
##' @aliases class:Spectrum Spectrum Spectrum-class [[<-,Spectrum-method [<-,Spectrum-method $<-,Spectrum-method Spectrum_elements
##'
##' @description
##'
##' The `Spectrum` class is a simple list of elements. There are two
##' classes of elements. Mandatory elements such as the MS level of
##' the spectrum (`msLevel`), its retention time (`rtime`), ... that
##' must be of a pre-defined class (see the `Spectrum_elements` vector
##' for their names and classes). Optional elements with arbitrary
##' names and classes can also be added.
##'
##' If, as some point, some elements were to become mandatory, their
##' names and classes would simply need to be added to the
##' `Spectrum_elements` vector. 
##'
##' Object are created with the `Spetrum(...)`
##' constructor. `Spectrum()` generates a instance with empty valid
##' values.
##'
##' @param ... Mandatory spectrum elements or list thereof. See
##'     `Spectrum_elements` for their names and classes.
##'
##' @md
##' 
##' @exportClass Spectrum
##' 
##' @examples
##' ## An empty spectrum
##' sp <- Spectrum()
##' sp
##'
##' ## Mandatory elements
##' Spectrum_elements
##'
##' ## Updating mandatory elements
##' sp$msLevel
##' class(sp$msLevel)
##' sp$msLevel <- 2L ## works
##' sp$msLevel
##' try(sp$msLevel <- "not an integer")
##' sp$msLevel
##'
##' pa <- PeakAnnotation(peaks = list(c(345.3, 345.32, 345.35, 345.39, 345.4),
##'                                   657.01,
##'                                   1231.182),
##'                      annotations = list("a5", "z3", "y7"))
##' sp$peakAnnotation
##' sp$peakAnnotation <- pa
##' sp$peakAnnotation
##'
##' ## Adding arbitrary elements
##' sp$foo <- "bar"
##' sp
##'
##' ## All current elements (mandatory and optional) and their
##' ## respective classes
##' sapply(sp, class)
##'
##' ## Setting intensity and m/z values. These must be set together,
##' ## as these two elements must be of the same length.
##' mz <- as.numeric(sample(1000, 10))
##' i <- as.numeric(sample(1e3:1e5, 10))
##' sp[c("intensity", "mz")] <- list(i, mz)
##' plot(sp$mz, sp$intensity, type = "h")
NULL

.Spectrum <- setClass("Spectrum",
                      contains = "SimpleList")


##' @export Spectrum
Spectrum <- function(...) {
    if (missing(...)) args <- .Spectrum_prototype()
    else args <- list(...)
    if (length(args) == 1L && extends(class(args[[1L]]), "list"))
        args <- args[[1L]]
    new2("Spectrum",
         listData = args,
         check = TRUE)
}


.Spectrum_prototype <- function() {
    list(msLevel = NA_integer_,
         peaksCount = NA_integer_,
         rtime = NA_real_,
         acquisitionNum = NA_integer_,
         scanIndex  =  NA_integer_,
         tic  =  NA_real_,
         mz  =  NA_real_,
         intensity  =  NA_real_,
         fromFile  =  NA_integer_,
         centroided  =  NA,
         smoothed  =  NA,
         polarity = NA_integer_,
         precScanNum = NA_integer_,
         precursorMz = NA_real_,
         precursorIntensity  =  NA_real_,
         precursorCharge  =  NA_integer_,
         collisionEnergy  =  NA_real_,
         peakAnnotation = PeakAnnotation())
}


##' @export Spectrum_elements
Spectrum_elements <- c(msLevel = "integer",
                       peaksCount = "integer",
                       rtime = "numeric",
                       acquisitionNum = "integer",
                       scanIndex = "integer",
                       tic = "numeric",
                       mz = "numeric",
                       intensity = "numeric",
                       fromFile = "integer",
                       centroided = "logical",
                       smoothed = "logical",
                       polarity = "integer",
                       precScanNum = "integer",
                       precursorMz = "numeric",
                       precursorIntensity = "numeric",
                       precursorCharge = "integer",
                       collisionEnergy = "numeric",
                       peakAnnotation = "PeakAnnotation")

.valid_Spectrum <- function(object) {
    if (!all(names(Spectrum_elements) %in% names(object)))
        return("Mandatory elements are missing")
    slot_classes <- sapply(object@listData, class)
    if (!all(slot_classes[names(Spectrum_elements)] == Spectrum_elements))
        return("Mandatory element of wrong class")
    if (!identical(length(object$mz), length(object$intensity)))
        return("Intensities and m/z must be of same lengths.")
    ## TODO: check cardinality of mandatory elements. All but
    ## PeaksAnnotation must be 1.
    NULL
}

setValidity("Spectrum",
            function(object) {
                msg <- .valid_Spectrum(object)
                if (is.null(msg)) TRUE
                else msg
            })

setReplaceMethod("[[", "Spectrum",
    function(x, i, j, ..., value)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid replacement")
        xl <- x@listData
        Spectrum(setListElement(xl, i, value))        
    }
)

setReplaceMethod("$", "Spectrum",
                 function(x, name, value) {
                     xl <- x@listData
                     xl[[name]] <- value
                     Spectrum(xl)
                 })



##' @importFrom methods callNextMethod
setReplaceMethod("[", "Spectrum",
    function(x, i, j, ..., value)
        Spectrum(callNextMethod(x@listData, i, value = value)))
