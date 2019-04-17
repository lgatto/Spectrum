##' @title Peak annotations
##'
##' @name PeakAnnotation
##'
##' @aliases class:PeakAnnotation class:PeakAnnotations PeakAnnotation-class PeakAnnotations-class PeakAnnotation PeakAnnotation-classs
##'
##' @description
##'
##' This class defines how peaks in and MS spectrum are annotated.
##'
##' @param peaks A `list` of peak m/z values to be annotated.
##' @param annotations A `list` of peak annotations.
##' @param  ... Objects of class `PeaksAnnotation`.
##'
##' @md
##' @exportClass PeakAnnotation PeakAnnotations
##'
##' @examples
##' pa1 <- PeakAnnotation(peaks = list(123.3,
##'                                    c(234.1, 234.12, 234.15)),
##'                       annotations = list("b2", "y3"))
##' pa1
##'
##' pa2 <- PeakAnnotation(peaks = list(c(345.3, 345.32, 345.35, 345.39, 345.4),
##'                                    657.01,
##'                                    1231.182),
##'                       annotations = list("a5",
##'                                          "z3",
##'                                          "y7"))
##' pa2
##'
##' pas <- PeakAnnotations(pa1, pa2)
##' pas
##'
##' dfr <- DataFrame(x = 1:2,
##'                  y = LETTERS[1:2],
##'                  annot = pas)
##' dfr
NULL

.PeakAnnotation <-
    setClass("PeakAnnotation",
         slots = c(peaks = "list",
                   annotations = "list"))

.valid_peak_annotation <- function(object) {
    np <- length(object@peaks)
    na <- length(object@annotations)
    if (np != na)
        return("Number of peaks and annotations don't match")
    NULL
}

##' @importFrom methods .valueClassTest is new validObject
setValidity("PeakAnnotation",
            function(object) {
                msg <- .valid_peak_annotation(object)
                if (is.null(msg)) TRUE
                else msg
            })

##' @export PeakAnnotation
PeakAnnotation <- function(peaks, annotations)
    .PeakAnnotation(peaks = peaks, annotations = annotations)

##' @exportMethod show
setMethod("show", "PeakAnnotation",
          function(object) {
              n <- length(object@peaks)
              cat(n, "annotated peaks\n")
          })

##' @import S4Vectors
.PeakAnnotations <-
    setClass("PeakAnnotations",
             contains = "SimpleList")

##' @export PeakAnnotations
PeakAnnotations <- function(...) {
    args <- list(...)
    if (length(args) == 1L && extends(class(args[[1L]]), "list"))
        args <- args[[1L]]
    x <- new2("PeakAnnotations",
              listData = args,
              elementType = "PeakAnnotation",
              check = FALSE)
    if (validObject(x)) x
}


##' @exportMethod showAsCell
setMethod("showAsCell", "PeakAnnotations",
          function(object)
              sapply(object,
                     function(.pa) paste(.pa@annotations, collapse = ", ")))
