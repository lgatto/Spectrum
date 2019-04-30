library(Spectrum)

# RMassBank subformula annotation
source(system.file("useCases/PeakAnnotation-formula/annotateSubformula.R", package = "Spectrum"))
source(system.file("useCases/PeakAnnotation-formula/formulaCalculator.R", package = "Spectrum"))
source(system.file("useCases/PeakAnnotation-formula/adductInformation.R", package = "Spectrum"))
# CFM-ID fragment SMILES annotations
source(system.file("useCases/PeakAnnotation-formula/annotateCfmId.R", package = "Spectrum"))
source(system.file("useCases/PeakAnnotation-formula/parseCfmId.R", package = "Spectrum"))

# A sample spectrum: Cefalexin ESI-QFT, CE 45
# A cropped version of https://massbank.eu/MassBank/RecordDisplay.jsp?id=EQ301403&dsn=Eawag
sp <- Spectrum()
sp$msLevel <- 2L
sp[c("mz", "intensity")] <- list(
  mz = c(68.0495, 72.9981, 74.0059, 79.0542, 91.0542,  106.0651, 112.0216, 
         # some junk peaks follow to test unmatched peaks
         11.1111, 11.1112, 11.1113, 11.1114,
        114.0008, 118.0413, 118.0651, 119.0492, 146.0601,  150.0373, 158.0271, 
        162.0372, 164.0529, 174.055, 192.0478),
  intensity = c(12767165.7,  1183432.5, 1519136.8, 3139475.7, 4575524.8, 47016675.3,
              2046562.7, 
              # some junk peaks follow to test unmatched peaks
              9999.9999, 9999.9999, 9999.9999, 9999.9999,
              8937737.9, 2455015.6, 11956711.5, 2017874.7, 4024150.7,
              1428832.7,  43328690.1, 1273242.2, 2963361.8, 12342430.4, 1649427.7)
)
# Some metadata necessary for annotation 
sp$name <- "Cefalexin"
sp$formula <- "C16H17N3O4S"
sp$smiles <- "CC1=C(N2C(C(C2=O)NC(=O)C(C3=CC=CC=C3)N)SC1)C(=O)O"
# the unlucky, hopefully at some point substituted
# designation for "[M+H]+" in RMassBank
sp$ion <- "pH"

# Annotate by subformula 
pa1 <- annotateSubformula(sp)
# Note: the metadata can also be passed directly, if one prefers not to pollute the `Spectrum`.
# pa1 <- annotateSubformula(sp, formula = "C16H17N3O4S", ion = "pH")
sp$peakAnnotation <- pa1

# Annotate using CFM-ID fragment prediction
cfmMockOutput <- system.file("useCases/PeakAnnotation-formula/cfm-annotate-cefalexin.txt", package = "Spectrum")
pa2 <- annotateCfmId(sp, mockOutput = cfmMockOutput)



