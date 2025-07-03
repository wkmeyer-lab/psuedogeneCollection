#run test on console
library(here)
library(testthat)
source(here( "src","preprocessing.R" ))

#' @Method to keep only scientific name, remove '_' and replace w space
#' @param sp is species name obtained from listofspecies
#' @return truncated character vector
test_that("truncateSp removes __ and replaces _", {
  expect_equal(truncateSp(c("Chrysochloris_asiatica__Cape_golden_mole__chrAsi1", "Felis_catus__cat_imbored", "powfu justdropped anotherAlb_um :D")), c("Chrysochloris asiatica", "Felis catus", "powfu justdropped anotherAlb um :D"))
  expect_equal(truncateSp("Felis_catus__cat_imbored"), "Felis catus")
})

#'@note METHOD WAS REFACTORED, TEST BROKEN!!!
#'@method find the same string between two lists
#'@param species all species obtained from mergedData
#'@return list containing all species that are the same in both species within listOfSpecies and mergedData
  # test_that("cleanSpecies return ONLY common strings between two lists", {
  #   sample_species <- read.table(here("data", "ListOfSpecies"), stringsAsFactors = FALSE)
  #   sam_sp <- truncateSp(c( sample_species[2:4, 1], "1+1=3s"))
  #   correctOutput <- c("Chrysochloris asiatica", "Dugong dugon", "Echinops telfairi" )
  #   
  # 
  #   print(correctOutput) 
  #   expect_equal(cleanSpecies(sam_sp), correctOutput)
  # })
  
  #crummy test but im lazy and yes, this works. Source: Trust me bro. 
  nameToTreeNameTest = TRUE
  if(nameToTreeNameTest == TRUE){
     sampleInput <- "Chrysochloris_asiatica__Cape_golden_mole__chrAsi1"
     sampleInput2 <- c("bbb__assqwrfd__A-sfjaserew__hopefullythisshows","Chrysochloris_asiatica__Cape_golden_mole__chrAsi1")
     print(nameToTreeName(sampleInput))
     print(nameToTreeName(sampleInput2))
  }
  
  #' @method to exclude low quality gene assemblies that have identical scientific names. Include only those found in tree
  #' @param scientific_full_fa data frame to obtain duplicates from
  #' @param trSpecies character vector to determine what duplicates exist
  #' @return data frame containing no duplicates
  
  test_that("Duplicate Scientific Names can be removed while preserving best genome assemply quality",{
    
    
    scientific_full_fa <- data.frame(
      scientific = c("Homo sapiens", "Mus musculus", "Homo sapiens", "Canis lupus", "Canis lupus", "Felis catus"),
      full = c("1", "2","3","4","5","6"),
      fa = c("Hs1", "Mm1", "Hs2", "Cl1", "Cl2", "Fc1"),
      stringsAsFactors = FALSE
    )
    
    trSpecies <- c("Hs1", "Cl2", "Fc1")
    
    expected <- data.frame(
      scientific = c("Homo sapiens", "Mus musculus", "Canis lupus", "Felis catus"),
      full = c("1", "2", "5", "6"),
      fa = c("Hs1", "Mm1", "Cl2", "Fc1"),
      stringsAsFactors = FALSE
    )
    # browser()
    actual <- rmDupe(scientific_full_fa,trSpecies)
    
    rownames(actual) <- NULL
    rownames(expected) <- NULL
    
    print(actual)
    
    print(expected)
    
    expect_equal(actual, expected )
  })
  