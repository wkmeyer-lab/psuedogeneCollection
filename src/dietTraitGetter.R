#' Creates trait file to be used as input for bayestraits
#' Output format: col 1: Species, col 2: Phenotype 
#'
#'
#'@issue: species names need to be the same for comparison
#'        not the same at the moment
#' /


getwd()
main <- function(){
  dirtyData <- read.csv("../data/mergedData")
  
  species <- dirtyData[,3] # obtains scientific name
  cleanLiSpecies <- cleanSpecies(species)
  
  diet <- dirtyData[,length(dirtyData)] #obtains diet type
  
  Spec_Diet <- data.frame( species = species, diet = diet)
  
  write.table(Spec_Diet, "../output/dietTraits", sep = "\t", row.names = FALSE, quote = FALSE)
  return( list(species, diet, Spec_Diet) )
}


 temp <- main()

 #' Method to include species only present in "species" within listOfSpecies
 #'@param species all species obtained from merged data
 cleanSpecies <- function(species){
  allSpecies <- as.list(read.table("../data/listOfSpecies")) # 1 species/row --> n elements in list
   
   for(i in 1:length(allSpecies)){
     for( n in 1:length(species)){
       if(species[n] == 1:length(allSpecies[i]) ){
         allSpecies =  allSpecies[-i]
       }
     }
   }
   
   return(allSpecies)
   
 }
 
 testlistOfSpeciesIsClean <- function(){
   
 }
 
 #' Test to obtain species and compare against listOfSpecies file to ensure all species are present
 testComparingSpeciesFiles <- function(testVars){
    sMerged <- testVars[[1]]
    sLossSum <- readLines("../output/ListOfSpecies")
    
    if(length(sMerged) < length(sLossSum)){
      print(" More Loss Sum Species than from MergedData.csv")
      cat( "losssum species count: ", length(sLossSum), " || mergedData species count: ", length(sMerged))
    }
    
 }
 
 #' Each value was compared visually to ensure I grabbed the intended cols
 #' @param  testVars contains test variables
 testVisual <- function(testVars) {
   View(testVars[[1]])
   View(testVars[[2]])
   View(testVars[[3]])
 }
 
 #testVisual(temp)
 #testComparingSpeciesFiles(temp)
 
 