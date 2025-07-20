#' Goal: Generate tree containing:
  #'  15 Isectivores
  #'  10 herbivores
  #'  5 carnivores
#' 1.) Prune tree to include the provided range of species @note DONE
#'    a.) Will be done randomly to 30 species or not?  @note DONE
#' 2.) Color code according to diet type  
  #'  a.) Determine diet type for each species @note DONE
#' 3.) Dotted lines to denote where gene loss is
#' 4.) Change tree tip names to scientific names to enhance readability


library(ape)
library(phytools)
library(ggplot2)
library(ggtree)

library(readr)
library(here)

sci_full_fa_diet <- read_tsv(here("data", "allNames"), show_col_types = FALSE)
#' Format: CHARACTER VECTOR
#'  Sci  Full  Fa  || 
#'  ...  ...  ... 

tree <- read.tree(here("data", "cleanTree"))

main <- function(tree, sci_full_fa_diet){
  faDiet<- dietSpeciesMatch(tree, sci_full_fa_diet)
  prunedtree <- treePrune(tree, faDiet)
  prunedfaDiet <- faDiet[faDiet$fa %in% prunedtree$tip.label,]
  prunedtree <- styleTree(prunedtree, prunedfaDiet)
  print(prunedtree)

}

main(tree, sci_full_fa_diet)

styleTree <- function(tr, faDi){
  # Create trait data frame
  trait_data <- data.frame(
    tip_name = tr$tip.label,  
    trait = faDi$diet[match(tr$tip.label, faDi$fa)], #subsetting just in case tree tips and fa arent alinged... i did sort earlier but idk if i make a new tip.label vector if sort will persist.
    stringsAsFactors = FALSE
  )
  
  # Plot with trait-based coloring
  colorfulTreeYippie<- ggtree(tr, aes(linetype = trait)) %<+% trait_data +
    geom_tiplab(aes(color = trait)) +
    scale_color_manual(values = c("Herbivore" = "green", 
                                  "Vertivore" = "red", 
                                  "Omnivore" = "orange",
                                  "Insectivore" = "purple"))+
    scale_linetype_manual(values = c("Herbivore" = "solid", 
                                   "Insectivore" = "dotted", 
                                   "Omnivore" = "solid",
                                   "Vertivore" = "solid"))
  return(colorfulTreeYippie)
}

treePrune <- function(tr, fa_di) {
  trSp<-sample(fa_di$fa, 30)
  nonUnion <- fa_di[!(fa_di$fa %in% trSp), 1]
  drop.tip(tr, nonUnion)
  return(tr)  
}

dietSpeciesMatch <- function(tree, sci_full_fa_diet){
  treeSp <- sort(tree$tip.label)
  fa <- sort(sci_full_fa_diet$fa)
  s_f_fa_di <- sci_full_fa_diet[match(fa, sci_full_fa_diet$fa), ]
  return(data.frame(fa = s_f_fa_di$fa, diet = s_f_fa_di$diet))
}



testBackup <- function() {
  sci_full_fa_diet <- read_tsv(here("data", "allNames"), show_col_types = FALSE)
  tree <- read.tree(here("data", "AllSpeciesMasterTree.tre"))
  
  treeSP = tree$tip.label
  unionSp<- treeSP[treeSP %in% sci_full_fa_diet$fa]
  nonunionSp<- treeSP[!(treeSP %in% sci_full_fa_diet$fa)]
  sci_full_fa_diet<-sci_full_fa_diet[sci_full_fa_diet$fa %in% treeSP,]
  
  #To deduce there are do differences
  print(setdiff(sci_full_fa_diet$fa, unionSp))
  
  #Checking for duplicates
  print(sci_full_fa_diet[duplicated(sci_full_fa_diet$fa) | duplicated(sci_full_fa_diet$fa, fromLast = TRUE), ])
  
  browser()
  visual <- data.frame( treeSp = unionSp, faSP = sci_full_fa_diet$fa)
}
 
# testBackup()
