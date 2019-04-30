


options("path.tools.cfm.bin" = "C:/Software/cfm-id/cfm-annotate.exe")
options("path.tools.cfm.param_output" = "C:/Software/cfm-id/se/param_output0.log")
options("path.tools.cfm.param_config" = "C:/Software/cfm-id/se/param_config.txt")

#' Annotate substructures with CFM-ID
#'
#' @param sp `Spectrum` to annotate
#' @param ppmTolerance tolerance for fragment matching, ppm
#' @param absTolerance tolerance for fragment matching, absolute
#' @param mockOutput For demo purposes, the output can be provided as a text file; provide a file name here.
#'
#' @return
#' @export
#'
#' @examples
annotateCfmId <- function(sp, ppmTolerance = 5, absTolerance = 0.001, 
                          smiles=sp$smiles,
                          mockOutput=NULL)
{
  # Write spectrum to file
  specOut <- tempfile()
  specDf <- do.call(data.frame, sp[c("mz", "intensity")])

  bufferCon <- textConnection("buffer", open = "w", local = TRUE)
  # For demonstration, we use the single-CE model and write the same spectrum 3x.
  writeLines("energy0", con = bufferCon)
  write.table(specDf, file = bufferCon, row.names = FALSE, sep=" ", col.names = FALSE)
  writeLines("energy1", con = bufferCon)
  write.table(specDf, file = bufferCon, row.names = FALSE, sep=" ", col.names = FALSE)
  writeLines("energy2", con = bufferCon)
  write.table(specDf, file = bufferCon, row.names = FALSE, sep=" ", col.names = FALSE)
  close(bufferCon)
  writeLines(buffer, specOut)

  # Run cfm-annotate (or retrieve output from file)
  if(is.null(mockOutput)) {
    cfmIdOut <- system(paste(
      getOption("path.tools.cfm.bin"),
      sp$smiles,
      specOut,
      "1",
      ppmTolerance,
      absTolerance,
      getOption("path.tools.cfm.param_output"),
      getOption("path.tools.cfm.param_config")
    ), intern = TRUE)
    #writeLines(cfmIdOut, con = "cfm-out.txt")
  }
  else {
    cfmIdOut <- readLines(mockOutput)
  }
  # cfm-annotate.exe CNC[C@H](O)C1=CC=C(O)C(O)=C1 spec.txt 1 10 10 param_output.log param_config.txt > out.txt
  return(parseCfmId(cfmIdOut, sp))
}


