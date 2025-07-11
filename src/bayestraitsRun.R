

library(here)
library(readr)

#' ISSUES TO ADDRESS:
#' 1.) need to merge diet to binary values
#' 2.) AM NOT IGNORING DUPE PATTERNS!!!

main <- function() {
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
  
  
  # Processing Species Data
  sp <- sci_full_fa$full
  numSp <- length(sp) # need for  ??
  
  #' verify assumptions here
  #' check to see if i can align species against diet_species and cleanPseudo
  #' how can i check and see if two entire cols are the same?
  
  #' Processing gene data
  genePattern <- unique(gene_sp[-1, 2:length(gene_sp)])
  geneID <- gene_sp[, 1]
  n1_PerGeneID <- sum(genePattern == "1") #do i need this?

  cat("Num Patterns: ", length(genePattern))

  browser()
  
  # Creating bayestraits file:
  for( i in genePattern){
    input <- data.frame(
      species = sp,
      trait = diet_sp[,2],
      presence = genePattern[i]
    )
    write.table(input, here("data", "inputBayes"))
    }
  
  system(paste("BayesTraitsV3.exe data/inputBayes.txt data/cleanTree.tree < data/bt_indep"))
  
  #trim output
}

main()