
knitr::purl("geneCollector.Rmd") #obtains code chunks of rmd
source("geneCollector.R")

library(stringr)
library(readr)
library(dplyr)

names(var1) <- c("species_gene", "genes")

sg <- var1$species_gene
genes <- var1$gene

#Method to print the beginning 6 genes. Will visually inspect the first 6 genes of the first loss_sum_data to see if results are identical. If so, then pass test. Otherwise, fail. 
#Test passed
testGenes <- function(v){
  print(head(v))
}
testGenes(genes) 


##Debugging
# for (i in 1:3){
#   cat("Contents: ")
#   species_gene[[i]][,1]
#   cat("Type: ")
#   typeof(species_gene[[i]][,1])
# }

# ###Testing: 
# cat("\n\n\n Testing species_gene dictionary: \n") # --> Test passed
# for (key in names(species_gene)) {
#   value <- species_gene [[key]]
#   cat( "\n\n\n\n\ ","Key:", key, " Value: ", " \n\n\n\n")
#   print(value[1:10,]) 
#   }
#   
# cat("\n\n\n Testing for genes: \n") # --> Test passed 
# print(genes[1:10])
# head(species_gene[[1]]) #looking at first few rows of the df of key 1
