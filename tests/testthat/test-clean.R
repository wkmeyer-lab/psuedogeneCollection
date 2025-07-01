#run test on console
library(here)
library(testthat)
source(here( "src","dietTraitGetter.R" ) )

#' @Method to keep only scientific name, remove '_' and replace w space
#' @param sp is species name obtained from listofspecies
#' @return truncated character vector
test_that("truncateSp removes __ and replaces _", {
  expect_equal(truncateSp(c("Chrysochloris_asiatica__Cape_golden_mole__chrAsi1", "Felis_catus__cat_imbored", "powfu justdropped anotherAlb_um :D")), c("Chrysochloris asiatica", "Felis catus", "powfu justdropped anotherAlb um :D"))
  expect_equal(truncateSp("Felis_catus__cat_imbored"), "Felis catus")
})


#'@method find the same string between two lists
#'@param species all species obtained from mergedData
#'@return list containing all species that are the same in both species within listOfSpecies and mergedData
 test_that("cleanSpecies return ONLY common strings between two lists", {
   sample_species <- read.table(here("data", "ListOfSpecies"), stringsAsFactors = FALSE)
   sam_sp <- truncateSp(c( sample_species[2:4, 1], "1+1=3s"))
   correctOutput <- c("Chrysochloris asiatica", "Dugong dugon", "Echinops telfairi" )
   

   print(correctOutput) 
   expect_equal(cleanSpecies(sam_sp), correctOutput)
 })
 
 
 
 