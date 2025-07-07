
 #' Creates trait file to be used as input for bayestraits
 #' Creates allName file for testing and documentation
 #' Creates non-union species between all names and tree file for documentation
 #' Creates pruned tree containing only species present in dataset from existing tree

 
library(ape)
library(here)
library(phytools)
library(readr)

#' @note 
  #' 1:) need to clean psuedoGene data for bayestraitsRun
  #' 2:) need to exclude duplicates from allSpecies
        #' Identify duplicates by matching nonUnion with scientific_full_fa
        #' Delete all duplicates duplicates   
  #' 3:) ensure that files I am writing into do not contain duplicates 
main <- function(){
  # Creating trait file, preparing to process tree
  
  dirtyPsuedo <- read.table(here("data", "psuedo.tsv"))
  dirtySpDi <- read.csv(here("data", "mergedData.csv")) #contains species diet info
  allSp <- read.table(here("data", "ListOfSpecies"))[,1] # 1 species/row --> n elements in list
  
  species <- dirtySpDi[,3] # obtains scientific name
  cleanLiSpecies <- cleanSpecies(species, allSp) # remove unnecessary species, keep dupe
  fullSpNames <- allSp[cleanLiSpecies$index]
  faNames <- nameToTreeName(fullSpNames) 
  scientific_full_fa <- data.frame(scientific = cleanLiSpecies$sp, full = fullSpNames, fa = faNames)
  

# need tree to determine what duplicate species gene assemblies need to be removed  
  
  tree <- read.tree(here("data","AllSpeciesMasterTree.tre"))
  #' obj composed of:
    #' edge: matrix = connections between species
    #' tip.label: chr vector = species
    #' edge.length = int vector = length of connection
    #' Nnode: Int = internal node count 
  
  trSpecies <- tree$tip.label
  indiciesNOTSame <- which(!(trSpecies %in% scientific_full_fa$fa))
  notInCommon <- scientific_full_fa[indiciesNOTSame, ]
  prunedTree <- drop.tip(tree, notInCommon$fa)
  
 # browser()
  scientific_full_fa <- rmDupe(scientific_full_fa, trSpecies)
  
  dirty_Spec_diet <- dirtySpDi[, c(3, length(dirtySpDi))] #grab scientific name, diet
  Spec_Diet <- mapSpeciesToSomething(scientific_full_fa$scientific, dirty_Spec_diet)
  
  cleanPsuedo <- cleanPsuedoData(dirtyPsuedo, scientific_full_fa$full) 
  
  #for use in Bayestraits script
  write_tsv(cleanPsuedo, here("data", "cleanPsuedo.tsv")) 
  
  #for use in Bayestraits script
  save_tsv(scientific_full_fa, "allNames") 

  #for use in Bayestraits script 
  save_tsv(Spec_Diet, "dietTraits") # WORKS

  #for documentation and testing  
  save_tsv(notInCommon, "nonUnionSpecies_Tree__allNames" ) #IDK
  
  #for use in Bayestraits script
  write.tree(prunedTree, here("data", "cleanTree")) #IDK
  
}

#' @method to automatically write tsv, saves time...
save_tsv <- function(obj, filename) {
  write.table(obj, here("data", filename), sep = "\t", row.names = FALSE, quote = FALSE)
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
 mapSpeciesToSomething <- function ( clean_sp, dirty_sp_di){
  matchingIndices <- match(clean_sp, dirty_sp_di[,1])  
  clean_sp_di <- dirty_sp_di[matchingIndices,]
  return(clean_sp_di)
 }

#'@method to find the same string between two lists
 #'@param species all species obtained from merged data
 #' @param allSp all species obtained from ListofSpecies
 #'@return list containing all species that are the same in both species within listOfSpecies and mergedData, KEEPS duplicates
 cleanSpecies <- function(species, allSp){
  allSp <- truncateSp(allSp)
  sp = c()
  indexes = c()
  lenList = 0
  
   for(i in 1:length(allSp)){
     for( n in 1:length(species)){
       if(species[n] == allSp[i]){
         lenList = lenList + 1
         sp[lenList] = allSp[i]
         indexes[lenList] = i
       }
     }
   }

  sameSpecies = data.frame(sp = sp, index = indexes)
   return(sameSpecies) 
   
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
 
 
 
 