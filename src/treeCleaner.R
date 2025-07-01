
# Make file of all species i excluded using unique

library(ape)
library(here)
library(phytools)

#' Takes a phylogenetic tree and a list of species as input and return a phylogenetic tree with only species present in both 

main <- function(){
  species <- read.table(here("data", "speciesName_FaName")) #df
  tree <- read.tree(here("data","AllSpeciesMasterTree.tre")) 
  #' obj composed of:
    #' edge: matrix = connections between species
    #' tip.label: chr vector = species
    #' edge.length = int vector = length of connection
    #' Nnode: Int = internal node count 
  
  trSpecies <- tree$tip.label
  rmSp <- setdiff(trSpecies, species[[1]])
  prunedTree <- drop.tip(tree, rmSp)
 
# # Make file of all both files did not have in common
  # write.table(rmSp, here("data, nonUnionSpecies_Tree-speciesName_FaName"))
   cat("numSpecies: ", length(species[[1]]), "numSpecies in tree: ", length(trSpecies), " numSpecies not in both files: ", length(rmSp))
# 
#   write.tree(prunedTree, here("data, cleanTree"))
}


main()


# allDuplicates<- cleanLiSpecies[duplicated(cleanLiSpecies) | duplicated(cleanLiSpecies, fromLast=TRUE)] # grab "duplicate species to inspect later
# allDupeIndex <- which(cleanLiSpecies %in% allDuplicates)
# dupeDF <- data.frame(allDuplicates, allDupeIndex) # to write
#cleanLiSpecies <- unique(cleanLiSpecies) # remove "duplicate" species
