library(rcdk)
library(plyr)

#' Annotate peak subformulas
#' 
#' Annotates possible subformulas within a ppm window for a given
#' parent formula.
#'
#' @param sp A `Spectrum` object.
#' @param cutoffRelative Relative intensity cutoff below which to ignore peaks
#' @param cutoffAbsolute Absolute intensity cutoff below which to ignore peaks
#' @param verbose if `TRUE`, add additional information: `dppm`,`dbe` for each
#'  predicted formula
#' @param ion The ion type (in the RMassBank list, e.g. `"pH"` = `"[M+H]+"`)
#' @param formula The parent formula
#' @param ppmLimit The ppm window for generating candidate formuals
#'
#' @return A `PeakAnnotation` with fields `formula`, `mzCalculated` and optional
#'  `dppm`, `dbe` for all successfully annotated peaks.
#' @export
#'
#' @examples
annotateSubformula <- function(sp,
                               cutoffRelative = 0.05,
                               cutoffAbsolute = 0,
                               verbose = FALSE,
                               ion=sp$ion,
                               formula=sp$formula,
                               ppmLimit = 5)
{
  spectrum <- do.call(data.frame, sp[c("mz", "intensity")])
  spectrum$peakIndex <- seq_len(nrow(spectrum))
  # spectrum <- data.frame(sp[c("peakIndex", "mz", "intensity")])
  
  # add filtering columns by the cutoff criteria
  spectrum$low  <- (spectrum$intensity < cutoffAbsolute) |
    (spectrum$intensity < cutoffRelative * max(spectrum$intensity, na.rm = TRUE))
  spectrum <- spectrum[!spectrum$low,,drop=FALSE]

  adduct <- getAdductProperties(mode=ion)
  parentFormula <- add.formula(formula, adduct$addition)
  parentDbe <- dbe(parentFormula)
  # check whether the formula is valid, i.e. has no negative or zero element numbers.
  if(!is.valid.formula(parentFormula))
    stop("Invalid formula")
  limits <- to.limits.rcdk(parentFormula)
  
  annotations <- alply(spectrum, 1, function(row) {
    # Circumvent bug in rcdk (I haven't checked if it is still around): 
    # correct the mass for the charge first, then calculate uncharged formulae
    # finally back-correct calculated masses for the charge
    mzCorr <- row[["mz"]] + adduct$charge * .emass
    peakformula <- tryCatch(
      suppressWarnings(generate.formula(mzCorr, 
                                        ppm(mzCorr, ppmLimit, p=TRUE),
                                        limits, charge=0)),
      error = function(e) list())
    if(length(peakformula) == 0)
      return(peakformula)
    lapply(peakformula, function(f) {
      row[["formula"]] = f@string
      row[["mzCalculated"]] = f@mass - adduct$charge * .emass
      return(row)
    }
    )
  })
  # Remove unmatched peaks, chain to single list
  annotations[unlist(lapply(annotations, length))==0] <- NULL
  annotations <- do.call(c, annotations)
  # dataframize
  annotations <- do.call(rbind.data.frame, annotations)
  # add extra information for filtering
  if(verbose) {
    annotations$dppm <- (annotations$mz / annotations$mzCalculated - 1) * 1e6
    annotations$dbe <- dbe(annotations$formula)
  }
  peaks <- as.list(annotations$peakIndex)
  # Zero out fields that don't belong in the PeakAnnotation
  annotations$mz <- NULL
  annotations$low <- NULL
  annotations$intensity <- NULL
  annotations$peakIndex <- NULL
  anno <- alply(annotations, 1, function(row) as.list(row))
  return(PeakAnnotation(peaks = peaks, annotations =  anno))
}