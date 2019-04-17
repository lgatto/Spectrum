.Spectrum <- setClass("Spectrum",
                      contains = "SimpleList")

Spectrum <- function(...) {
    args <- list(...)
    if (length(args) == 1L && extends(class(args[[1L]]), "list"))
        args <- args[[1L]]
    x <- new2("Spectrum",
              listData = args,
              check = FALSE)
    if (validObject(x)) x
}


.Spectrum_prototype <- function() {
    List(
        msLevel = NA_integer_,
        peaksCount = NA_integer_,
        rt = NA_real_,
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


.Spectrum_elements <- c(msLevel = "NA_integer_",
            peaksCount = "NA_integer_",
            rt = "NA_real_",
            acquisitionNum = "NA_integer_",
            scanIndex = "NA_integer_",
            tic = "NA_real_",
            mz = "NA_real_",
            intensity = "NA_real_",
            fromFile = "NA_integer_",
            centroided = "NA",
            smoothed = "NA",
            polarity = "NA_integer_",
            precScanNum = "NA_integer_",
            precursorMz = "NA_real_",
            precursorIntensity = "NA_real_",
            precursorCharge = "NA_integer_",
            collisionEnergy = "NA_real_",
            peakAnnotation = "PeakAnnotation")

.valid_Spectrum <- function(object) {



}