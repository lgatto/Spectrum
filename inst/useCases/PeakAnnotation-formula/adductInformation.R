#' Data frame with adduct information
#' 
#' Subset of the one from the treutler branch of RMassBank, to be improved?
#'
#' @return `data.frame` with `mode`, `addition`, `charge`, `adductString`
#' @export
#'
#' @examples
.getAdductInformation <- function() {
  adductDf <- as.data.frame(rbind(
    
    ## positive: M+X
    c(mode = "pH",       addition = "H",         charge = 1, adductString = "[M+H]+"),
    c(mode = "pLi",      addition = "Li",        charge = 1, adductString = "[M+Li]+"),
    c(mode = "pNa",      addition = "Na",        charge = 1, adductString = "[M+Na]+"),
    c(mode = "pNa_mO3S_mH",      addition = "Na1O-3S-1H-1",        charge = 1, adductString = "[M-O3S-H+Na]+"),
    c(mode = "pK",       addition = "K",         charge = 1, adductString = "[M+K]+"),
    c(mode = "pM",       addition = "",          charge = 1, adductString = "[M]+"),
    ## negative: M-X
    c(mode = "mH",      addition = "H-1",    charge = -1, adductString = "[M-H]-"),
    c(mode = "mCl",     addition = "Cl-1",   charge = -1, adductString = "[M+Cl]-"),
    c(mode = "mFA",     addition = "C1O2H",  charge = -1, adductString = "[M+HCOOH-H]-"),
    c(mode = "mH_pTFA", addition = "C2F3O2", charge = -1, adductString = "[M+CF3CO2H-H]-"),
    c(mode = "mM",      addition = "",       charge = -1, adductString = "[M]-"),
    ## ???
    c(mode = "",        addition = "",       charge = 0,  adductString = "[M]")
  ), stringsAsFactors = F)
  adductDf$charge <- as.integer(adductDf$charge)
  
  if(any(any(duplicated(adductDf$mode)), any(duplicated(adductDf$adductString)))) stop("Invalid adduct table")
  
  return(adductDf)
}



#' Retrieve adduct information for computations
#' 
#' @param mode 
#'
#' @return `list` of `mode`, `addition` (elemental change to the formula),
#'   `charge` (charge state), and `adductString` (chemical description of the adduct type)
#' 
#' @export
#'
#' @examples
getAdductProperties <- function(mode){
  adductDf <- .getAdductInformation()
  if(!(mode %in% adductDf$mode))
    stop("mode = \"", mode, "\" not defined")
  mzopt <- as.list(adductDf[adductDf$mode==mode,,drop=FALSE])
  return(mzopt)
}
