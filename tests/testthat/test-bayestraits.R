library(here)
library(readr)
library(testthat)
library(ape)
library(phytools)

#source(here( "src","bayestraitsRun.R" ))

#' problem with multiple genome assemblies still... issue with tree?
#' Must test tree for duplicate scientific names?
#' 
  
test_that("species is identical across all data frames i will be working with", {

  gene_sp <- read_tsv(here("data", "cleanPsuedo.tsv"))
  #' Format: DATA FRAME
  #'        Species: ... 
  #' GENEID: PRESENCE...
  #'  ...      ...  
  
  sci_full_fa <- read_tsv(here("data", "allNames"), show_col_types = FALSE)
  #' Format: CHARACTER VECTOR
  #'  Sci  Full  Fa  || 
  #'  ...  ...  ... 
  
  diet_sp <- read_tsv(here("data", "dietTraits"), show_col_types = FALSE)
  #' Format:  # DATA FRAME
  #' Species:     Diet: 
  #'   ...         ...
  #'   
  
  tree <- read.table(here("data", "cleanTree")) 
  
  trSpecies <- tree$tip.label 
  
  sp <- sci_full_fa$scientific # allNames --> scientific
  
  psuedoNames <-gene_sp[1,] # psuedo  --> scientific
  psuedoNames=as.character(psuedoNames)
  psuedoNames <- psuedoNames[-1]
  
  dSpecies <-diet_sp$ScientificNameFull #diet --> scientific name
  
  browser()
  
  trSp_ <- which(duplicated(trSpecies))
  sp_ <- which(duplicated(sp))
  dsp_ <- which(duplicated(dSpecies))
  
  sp_psuedo<- identical(sp, psuedoNames)
  diet_psuedo<- identical(psuedoNames, dSpecies)
  expect_equal(sp_psuedo, TRUE)
  expect_equal(diet_psuedo, TRUE)
})