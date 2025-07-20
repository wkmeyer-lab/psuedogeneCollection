

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
  

  
  #' verify assumptions here
  #' check to see if i can align species against diet_species and cleanPseudo
  #' how can i check and see if two entire cols are the same?
  
  #' Processing gene data
  genePat <- gene_sp[-1, 2:length(gene_sp)]
  uniqueGenePat <- unique(genePat)
  geneID <- gene_sp[1 , ]
  n1_PerGeneID <- sum(uniqueGenePat == "1") #do i need this?

  cat("Num Patterns: ", length(uniqueGenePat))

  browser()
  
  #' Aligning uniqueGenePat species against diet_sp and sci_full_fa
  uniquePatternIndicies<- which(genePat %in% uniqueGenePat)
  diet_sp_aligned = diet_sp[uniquePatternIndicies,]
  sci_full_fa_aligned = sci_full_fa[uniquePatternIndicies, ]
  gene_sp

  # Processing Species Data
  sp <- sci_full_fa_aligned$full
  numSp <- length(sp) # need for  ??
    
  
  
  browser()
  
  # Creating bayestraits file:
  index = 0
  for( i in uniqueGenePat){
    index = index + 1
    input <- data.frame(
      species = sp,
      trait = diet_sp_aligned[,2],
      presence = uniqueGenePat[index]
      
    )
    write.table(input, here("data", "inputBayes.txt"))
    output <- system(paste("BayesTraitsV3.exe data/inputBayes.txt data/cleanTree.tree < data/bt_indep"))
    }
  
 
  
  #trim output
}

main()