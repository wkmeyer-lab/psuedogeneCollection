#run test on console

library(testthat)

setwd("../../")
source("src/dietTraitGetter.R")

#cat(getwd(), "\n ", list.files("src"))
temp <- main()

species <- temp[[1]]
diet <- temp[[2]]
Spec_Diet <- temp[[3]]
cleanLiSpecies <- temp[[4]]





#' Each value was compared visually to ensure I grabbed the intended cols
#' @param  testVars contains test variables
testVisual <- function(species, diet, Spec_diet) {
  View(species)
  View(diet)
  View(Spec_diet)
}