

library(here)
library(readr)

#' ISSUES TO ADDRESS:
#' 1.) need to merge diet to binary values


#' Method to convert categorical diets to bayestraits-readable multistate values
convert_diet_multistate <- function(diet_vector) {
  diet_vector[diet_vector == "Vertivore"] <- "1"
  diet_vector[diet_vector == "Herbivore"] <- "2"
  diet_vector[diet_vector == "Omnivore"] <- "3"
  diet_vector[diet_vector == "Insectivore"] <- "4"
  return(diet_vector)
}

convert_diet_discrete <- function(diet_vector) {
  diet_vector[diet_vector == "Vertivore"] <- "0"
  diet_vector[diet_vector == "Herbivore"] <- "0"
  diet_vector[diet_vector == "Omnivore"] <- "1"
  diet_vector[diet_vector == "Insectivore"] <- "1"
  return(diet_vector)
}

runBayes <- function(uniqueGenePat, sp, diet_sp_aligned){
  
  for( i in 1:nrow(uniqueGenePat)){
    current_pattern <- as.character(uniqueGenePat[i, ]) # need to convert from data frame to char vector
    current_pattern[current_pattern == "UN"] <- "-" # mislabled unknown values, bayestraits need unknown values to be hyphens
    #diet <- convert_diet_multistate(diet_sp_aligned$diet)
    diet <- convert_diet_discrete(diet_sp_aligned$diet)
    
    # Create data frame with proper alignment
    input <- data.frame(
      species = sp,                          
      trait = diet,          
      presence = current_pattern, 
      stringsAsFactors = FALSE
    )
    write.table(input, here("data", "inputBayes.txt"), 
                row.names = FALSE, 
                col.names = FALSE,   
                sep = "\t",          
                quote = FALSE)       
    # according to gpt bayestraits cannot process row or col names, cannot verify this in documentation but existing table i wrote did not work so im grasping for straws atp
    browser()
    output <- system(paste("BayesTraitsV3.exe data/cleanTree.nex data/inputBayes.txt" < "data/bt_ind_run"))
    
    temp = data.frame( out= output)
  }
}

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
  
  #tree <- read.tree(here("data", "cleanTree")) 
  tree <- readNexus(file = here("data", "cleanTree.nex"))
  
  
  #' Need to ensure species are sorted the SAME across gene_sp, sci_full_fa, diet_sp
  nogeneID_sp <- gene_sp[,-1] #to remove "species" col name, will mess up alignment by 1 otherwise
  gene_sp_aligned <- nogeneID_sp[, order(colnames(nogeneID_sp))] # full, does not contain geneID
  s_f_fa_aligned <- sci_full_fa[order(sci_full_fa$full), ] # full 
  diet_sp_aligned <- diet_sp[order(diet_sp$species), ] # scientific
  sp <- s_f_fa_aligned$fa
  

  #' Processing gene data
  uniqueGenePat <- unique(gene_sp_aligned) 
  geneID <- gene_sp[,1] 
  n1_PerGeneID <- sum(uniqueGenePat == "1") #do i need this?
  
  cat("Num Patterns: ", length(uniqueGenePat))

  output <- runBayes(uniqueGenePat, sp, diet_sp_aligned)
  
  #trim output
}

main()