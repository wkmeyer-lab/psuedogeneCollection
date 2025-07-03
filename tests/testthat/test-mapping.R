
#run test on console
library(here)
library(testthat)
source(here( "src","preprocessing.R" ))

#' @method to take vector and dataframe, and grab each row where elements in vector match the nth row, first col of dataframe
#' @param clean_sp contains species list to grab
#' @param dirty_sp_di contains excess species, need to clean 
#' @return dataframe that contains only species and diets from clean_sp
 
test_that( " mapSpeciesToDiet maps Species from clean_sp to diet of same Species within dirty_sp_di" , {
   clean_sp <- c("SpeciesB", "SpeciesC", "SpeciesA")
   dirty_sp_di <- data.frame(
     species = c("SpeciesA", "SpeciesB", "SpeciesC", "SpeciesD"),
     diet = c("Herbivore", "Carnivore", "Omnivore", "Detritivore")
   )
   
   expectedOutput <- data.frame(
     species = c("SpeciesB", "SpeciesC", "SpeciesA"),
     diet = c("Carnivore", "Omnivore", "Herbivore")
   )
   
   #stupid row names are fucking things up --> so i will remove names... dont need or care for naming of dataframe... 
   
   actualOutput <- mapSpeciesToSomething(clean_sp, dirty_sp_di) 
   rownames(actualOutput) <- NULL
   rownames(expectedOutput) <- NULL
   
      expect_equal( actualOutput, expectedOutput)
   })

