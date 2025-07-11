
 #' Creates trait file to be used as input for bayestraits
 #' Creates allName file for testing and documentation
 #' Creates non-union species between all names and tree file for documentation
 #' Creates pruned tree containing only species present in dataset from existing tree

 
library(ape)
library(here)
library(phytools)
library(readr)

#' @note
  #' Issue: fa_fullS_scientific contains NA diet names
  #' test for: cleanSpecies
            #' 
main <- function(){
  # Creating trait file, preparing to process tree
  dirtyPsuedo <- read.table(here("data", "psuedo.tsv"))
  dirtySpDi <- read.csv(here("data", "mergedData.csv")) #contains species diet info
  allSp <- read.table(here("data", "ListOfSpecies"))[,1] # 1 species/row --> n elements in list
  faNames_diet <-cleanFa(dirtySpDi$manualAnnotations_FaName, dirtySpDi$ZoonomiaTip)
  fa_full_scientific_union <- cleanSpecies(faNames_diet, allSp, dirtySpDi[,length(dirtySpDi)]) 
  Spec_Diet <- data.frame(species = fa_full_scientific_union$scientific, diet = fa_full_scientific_union$diet)
  
  as <- data.frame(i = allSp)
  s <- data.frame( i = faNames_diet)
  
  tree <- read.tree(here("data","AllSpeciesMasterTree.tre"))
  #' obj composed of:
    #' edge: matrix = connections between species
    #' tip.label: chr vector = species
    #' edge.length = int vector = length of connection
    #' Nnode: Int = internal node count 
  
  prunedTree <- prune_tree_to_union(tree, fa_full_scientific_union$fa)
  fa_full_scientific_union <- rmDupe(fa_full_scientific_union, prunedTree$tip.lable)
  cleanPsuedo <- cleanPsuedoData(dirtyPsuedo, fa_full_scientific_union$full) 
  
  
  browser()
  #for use in Bayestraits script
  write_tsv(cleanPsuedo, here("data", "cleanPsuedo.tsv")) 
  
  #for use in Bayestraits script
  save_tsv(fa_full_scientific_union, "allNames") 

  #for use in Bayestraits script 
  save_tsv(Spec_Diet, "dietTraits") # WORKS

  
  #for use in Bayestraits script
  write.tree(prunedTree, here("data", "cleanTree")) 
  
}

#' @method to automatically write tsv, saves time...
save_tsv <- function(obj, filename) {
  write.table(obj, here("data", filename), sep = "\t", row.names = FALSE, quote = FALSE)
}

#'@method Prune a tree by removing species not in union
#' @param tree The phylogenetic tree object
#' @param union_df Dataframe with a $fa column (species to keep)
#' @return List with pruned tree, non-matching species, and indices
prune_tree_to_union <- function(tree, fa) {
  trSpecies <- tree$tip.label
  indiciesNOTSame <- which(!(trSpecies %in% fa))
  notInCommon <- fa[indiciesNOTSame]
  prunedTree <- drop.tip(tree, notInCommon)
  
  #for documentation and testing  
  save_tsv(notInCommon, "nonUnionSpecies_Tree__allNames" )
  return(prunedTree)
}

#' @method removes duplicates from psu. 
#' @param psu dataframe containg GENE PRESENCE per species for GENE ID
#' @param fullSp character vector containing all species to keep in psu
 cleanPsuedoData<- function(psu, fullSp){
   indices <- which(psu[1,] %in% fullSp)
   cleanPSU <-psu[, c(1, indices)]
   
   # full --> scientific name
   sciNames<- truncateSp(cleanPSU[ 1,])
   cleanPSU[1,] <- sciNames
   
   return(  cleanPSU )
   
 }
 #' Finds union between fa name and zoonomia fa names
 #' @param man character vector: manual annotation names
 #' @param zoo character vector: Zoonomia names
 #' @return character vector: subset of man also present in zoo
 cleanFa <- function(man, zoo){
   zoo[is.na(zoo)] <- "" 
   return(man[(man %in% zoo)])
 }
 
#' @method to exclude low quality gene assemblies that have identical scientific names. Include only those found in tree
#' @param scientific_full_fa data frame to obtain duplicates from
#' @param trSpecies character vector to determine what duplicates exist
#' @return data frame containing no duplicates

rmDupe <- function(scientific_full_fa, trSpecies){
  sciNames <- scientific_full_fa$scientific
  allDuplicates<- scientific_full_fa[duplicated(sciNames) | duplicated(sciNames, fromLast=TRUE), ] # grab "duplicate species to inspect later
  badSpecies <- allDuplicates[!(allDuplicates$fa %in% trSpecies), ]
  clean_Sci_Full_Fa<- scientific_full_fa[!(scientific_full_fa$fa %in% badSpecies$fa), ]
  return (clean_Sci_Full_Fa)
}

#' @method to take convert naming convention to tree naming convention
#' @param sp list containing names to be converted
#' @return data frame containing correctly formatted names --> col 1 = full name, col 2 = tree name
nameToTreeName <- function(sp){
  sp <- as.character(sp)
  dirtySpecies <- strsplit(sp, "__")
  dirtySpecies <- sapply(dirtySpecies,tail,1)
  faName<- paste("vs",dirtySpecies, sep = "_")
  return(faName)
}


#' @method to take vector and dataframe, and grab each row where elements in vector match the nth row, first col of dataframe
#' @param clean_sp contains species list to grab
#' @param dirty_sp_di contains excess species, need to clean 
#' @return dataframe that contains only species and whatever I'm looking for from clean_sp
 mapSpeciesToSomething <- function ( clean_sp, diet){
  noDietFound <- clean_sp[!(clean_sp %in% diet)]
  
   
  matchingIndices <- match(clean_sp, diet)  
  clean_sp_di <- diet[matchingIndices,]
  
  spDi_spNoDi = data.frame(species = clean_sp, noDiet = noDietFound)
  return(spDi_spNoDi)
 }

#'@method to find the same species between two data frames, then return those species DIETS, FULL NAME, SCIENTIFIC NAME, TREE NAME
 #'@param species all species obtained from merged data --> fa names
 #' @param allSp all species obtained from ListofSpecies
 #'@return list containing all species that are the same in both species within listOfSpecies and mergedData, KEEPS duplicates
 cleanSpecies <- function(species, allSp, diet){
  
  trunAllSp <- nameToTreeName(allSp) #taking fa names
  
  sp = c()
  indexes = c()
  fullsp = c()
  lenList = 0
  
  # Diet contains 'NA' values. This is a placeholder value in R and will throw errors when such values are compared against anything
  
   for(i in 1:length(trunAllSp)){
     for( n in 1:length(species)){
       if(species[n] == trunAllSp[i] && !(is.na(diet[n]))){
          lenList = lenList + 1
          sp[lenList] = trunAllSp[i]
          fullsp[lenList] = allSp[i]
          indexes[lenList] = n
         
       }
     }
   }
  
  sciName <- truncateSp(fullsp)
  union = data.frame(fa = sp, scientific = sciName, full = fullsp, diet = diet[indexes])
  return(union) 
 }
 
#' @Method to keep only scientific name, remove '_' and replace w space
#' @param sp is species name obtained from listofspecies
 truncateSp <- function(sp){
  #truncate by looking for first instance of '__'
   truncated <- sub("__.*$", "", sp)  #. including, * until $ end of line
   smallSp <- gsub("_", " ", truncated) #see also grep
   return (smallSp)
 }
 
 main()
 
 
 
 