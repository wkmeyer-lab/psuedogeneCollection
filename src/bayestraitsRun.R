

library(here)
 
#' ISSUES TO ADDRESS:
#' 1.) need to merge diet to binary values
#' 2.) AM NOT IGNORING DUPE PATTERNS!!!

main <- function() {
  
  independent <- read.table(here("data", "bt_indep")) #need to make
  dependent <- read.table(here("data", "bt_dep")) #need to make
  
  gene_sp <- read.table(here("data", "cleanPsuedo.tsv"))
  #' Format: DATA FRAME
    #'        Species: ... 
    #' GENEID: PRESENCE...
    #'  ...      ...  
    
  sci_full_fa <- read.table(here("data", "allNames"))
  #' Format: CHARACTER VECTOR
  #'  Sci  Full  Fa  || 
  #'  ...  ...  ... 
  
  diet_sp <- read.table(here("data", "dietTraits.txt"))
  #' Format:  # DATA FRAME
    #' Species:     Diet: 
    #'   ...         ...
    #'   
  
  tree <- read.table(here("data", "cleanTree.tree")) 
  
  
  # Processing Species Data
  sp <- sci_full_fa$full
  numSp <- length(sp)
  
  #' Processing gene data
  genePattern <- gene_sp[-1, 2:length(gene_sp)]
  geneID <- gene_sp[, 1]
  pattern_ID <- data.frame(id = geneID, pattern = genePattern)
  
  n1_PerGeneID <- sum(pattern_ID$pattern == "1")
  uniquePatterns <- length(unique(pattern_ID$id))

  # Creating bayestraits file:
  for( i in pattern_ID$id){
    input <- data.frame(
      species = sp,
      trait = diet_trait[,2],
      presence = pattern_ID$pattern[i]
    )
    # write as input for bayestraits
  }
  
  system("path/to/bayestraits input tree")
  
  #trim output
  }