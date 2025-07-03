library(here)
library(testthat)
source(here( "src","preprocessing.R" ) )

 psuedo <- read.table(here("data", "psuedo.tsv"))
 cleanPseudo <- read.table(here("data", "cleanPsuedo.tsv"))
 
 cat("psuedo: nrow " , nrow(psuedo) , "ncol", ncol(psuedo)) 
 cat("\ncleanPsuedo: nrow ", nrow(cleanPseudo), "ncol", ncol(cleanPseudo))
 View(cleanPseudo)