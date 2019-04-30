
library(magrittr)
library(plyr)

#' Parse CFM-ID output to PeakAnnotation
#'
#' @param cfmOut Output from `cfm-annotate`
#' @param sp `Spectrum` for which the annotation was run
#'
#' @return A `PeakAnnotations` with fields `smiles`, `mz` for each annotated peak.
#' @export
#'
#' @examples
parseCfmId <- function(cfmOut, sp) {
  mzOrder <- order(sp$mz)
  eps <- 0.002
  cfmSplit <- split(cfmOut, cumsum(grepl("^$", cfmOut)))
  cfmSpecs <- cfmSplit[[1]][-1] %>% split(., cumsum(grepl("energy[0-9]", .)))
  
  #lineRegex <- "^([0-9\\.]+) [0-9\\.]+(.*)\\(|$"#[\\($]"#[$|\ ([0-9\\ +])\\(.*"
  lineRegex <- "^([0-9\\.]+) [0-9\\.]+([^(]*)"
  cfmSp <- cfmSpecs[[1]][-1]
  peaksMatched <- regexec(lineRegex, cfmSp, perl=TRUE)
  peaksData <- regmatches(cfmSp, peaksMatched)
  # make sure that the peaks in the spectrum match the peaks in the output
  mzCfm <- as.numeric(laply(peaksData, `[[`, 2))
  if(any(abs(sp$mz[mzOrder] - mzCfm) > eps))
    stop("CFM-ID output doesn't match the spectrum data")
  annotations <- laply(peaksData, `[[`, 3)
  # retrieve fragments, and map CFM peaks to annotated peak in original spectrum
  annotatedPeaks <- which(laply(annotations, nchar) > 0)
  fragmentIndex <- annotations[annotatedPeaks] %>% strsplit(" ") %>% llply(`[`, -1) %>% llply(as.integer)
  annotatedPeakIndex <- mzOrder[annotatedPeaks]
  names(fragmentIndex) <- annotatedPeakIndex
  fragmentTable <- cfmSplit[[2]][-c(1,2)] %>% 
    read.table(text=., header = FALSE, col.names = c("fragmentIndex", "mz", "smiles"),
               comment.char = "", stringsAsFactors = FALSE)
  # build PeakAnnotation object
  annotations <- ldply(fragmentIndex, function(i) fragmentTable[match(i, fragmentTable$fragmentIndex),,drop=FALSE])
  pa <- PeakAnnotation(
    as.list(as.integer(annotations$.id)),
    alply(annotations, 1, function(row) as.list(row[-c(1,2)]))
  )
  pa
}


