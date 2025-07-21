

library(here)
library(readr)

#' ISSUES TO ADDRESS:
#' 1.) need to merge diet to binary values


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
 tree <- read.nexus(here("data", "cleanTree")) 
  # tree <- read.tree(here("data", "cleanTree")) 
  
  #' Need to ensure species are sorted the SAME across gene_sp, sci_full_fa, diet_sp
  nogeneID_sp <- gene_sp[,-1] #to remove "species" col name, will mess up alignment by 1 otherwise
  gene_sp_aligned <- nogeneID_sp[, order(colnames(nogeneID_sp))] # full, does not contain geneID
  s_f_fa_aligned <- sci_full_fa[order(sci_full_fa$full), ] # full 
  diet_sp_aligned <- diet_sp[order(diet_sp$species), ] # scientific
  sp <- s_f_fa_aligned$full
  

  #' Processing gene data
  uniqueGenePat <- unique(gene_sp_aligned) 
  geneID <- gene_sp[,1] 
  n1_PerGeneID <- sum(uniqueGenePat == "1") #do i need this?
  
  cat("Num Patterns: ", length(uniqueGenePat))
  
  # Creating bayestraits file:
  for( i in 1:nrow(uniqueGenePat)){
    current_pattern <- as.character(uniqueGenePat[i, ]) # need to convert from data frame to char vector
    
    # Create data frame with proper alignment
    input <- data.frame(
      species = sp,                          
      trait = diet_sp_aligned[,2],          
      presence = current_pattern, 
      stringsAsFactors = FALSE
    )
    write.table(input, here("data", "inputBayes.txt"))
    browser()
    output <- system(paste("BayesTraitsV3.exe data/cleanTree data/inputBayes.txt"))
    
    
    # output <- system(paste("BayesTraitsV3.exe data/cleanTree.tree data/inputBayes.txt < data/bt_indep"))
    }
  #BayesTraitsV3.exe Primates.trees Primates.txt
 
  
  #trim output
}

main()