

library(here)
 
#' ISSUES TO ADDRESS:
#' 1.) psuedo.tsv contains irrelevant species
#' 2.) Dont have correct tree file
#' 3.)

main <- function() {
  read.table(here("data", "cleanPsuedo.tsv"))
  #' Format: DATA FRAME
    #'        Species: ... 
    #' GENEID: PRESENCE...
    #'  ...      ...  
    
  read.table(here("data", "UpdatedListOfSpecies"))
  #' Format: CHARACTER VECTOR
  #'  Species
  #'  ...
  
  read.table(here("data", "dietTraits.txt"))
  #' Format:  # DATA FRAME
    #' Species:     Diet: 
    #'   ...         ...
  read.table(here("data", "cleanTree.tree")) #to create
  
  
  
  }