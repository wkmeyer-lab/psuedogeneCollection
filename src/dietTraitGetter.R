
 #' Creates trait file to be used as input for bayestraits
 #' Creates FaName file to be used for tree pruning
 
#' Output format: col 1: Species, col 2: Phenotype
#' #############################################
#' Is it OK to have NA?
#' Issue: I truncated names! 

library(here)

main <- function(){

  dirtySpDi <- read.csv(here("data", "mergedData.csv"))
 
  species <- dirtySpDi[,3] # obtains scientific name
  cleanLiSpecies <- cleanSpecies(species) # remove unnecessary species
  fa_species <- sub(".*__", "vs", cleanLiSpecies) # regex = pain 
  dirty_Spec_diet <- dirtySpDi[, c(3, length(dirtySpDi))] #grab scientific name, diet
  Spec_Diet <- mapSpeciesToSomething(cleanLiSpecies, dirty_Spec_diet)

  

  #for use in treeCleaner: Pruning tree + use to keep track of what species from tree wasn't on this
  save_tsv(fa_species, "fa_species")

  #for use in treeCleaner: use w/ fa_species to convert fa_species back to normal naming convenction
  save_tsv(cleanLiSpecies, "UpdatedListOfSpecies")
  
  #for use in Bayestraits script
  save_tsv(Spec_Diet, "dietTraits")
  

}

#' @method to automatically write tsv, saves time...
save_tsv <- function(obj, filename) {
  write.table(obj, here("data", filename), sep = "\t", row.names = FALSE, quote = FALSE)
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
 #'@return list containing all species that are the same in both species within listOfSpecies and mergedData, removed duplicates
 cleanSpecies <- function(species){
  allSpDirty <- read.table(here("data", "ListOfSpecies"))[,1] # 1 species/row --> n elements in list
  
  # cat("with repeats: ", length(allSpClean), length(species))
  # cat( "\n\n without repeats: ", length(unique(allSpClean))," ", length(unique(species)))
  
  sameSpecies = c()
   for(i in 1:length(allSpClean)){
     for( n in 1:length(species)){
       if(species[n] == allSpClean[i]){
         sameSpecies = c(sameSpecies, allSpClean[i])
       }
     }
   }
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
 
 
 
 