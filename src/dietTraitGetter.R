#' Creates trait file to be used as input for bayestraits
#' Output format: col 1: Species, col 2: Phenotype 
#'
#'
#'@issue: species names need to be the same for comparison
#'        not the same at the moment
#'

library(here)

main <- function(){

  dirtyData <- read.csv(here("data", "mergedData.csv"))
  
  species <- dirtyData[,3] # obtains scientific name
  cleanLiSpecies <- cleanSpecies(species)
  Spec_Diet <- mapSpeciesToDiet()
  
  write.table(Spec_Diet, here("data, dietTraits"), sep = "\t", row.names = FALSE, quote = FALSE)
  return( list(species, diet, Spec_Diet, cleanLiSpecies) )
}
 

#need diet data for all the species in cleanLiSpecies
#' figure out indexes for each occurance of elements in cleanLiSpecies
#' Store in list
#' Grab diet data at those elements 
#' --> use MATCH()



#'@method to find the same string between two lists

 #'@param species all species obtained from merged data
 #'@return list containing all species that are the same in both species within listOfSpecies and mergedData
 cleanSpecies <- function(species){
  allSpDirty <- read.table(here("data", "ListOfSpecies"))[,1] # 1 species/row --> n elements in list
  allSpClean <- truncateSp(allSpDirty)
  
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
   sameSpecies <- unique(sameSpecies)
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
 
 temp <- main()
 
 
 