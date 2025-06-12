
knitr::purl("geneCollector.Rmd") #obtains code chunks of rmd
source("geneCollector.R")

library(stringr)
library(readr)
library(dplyr)

setwd("~/meyerLab/psuedogeneCollector/src")
names(var1) <- c("species_gene", "genes")

sg <- var1$species_gene
genes <- var1$gene

#Method to print the beginning 6 genes. Will visually inspect the first 6 genes of the first loss_sum_data to see if results are identical. If so, then pass test. Otherwise, fail. 
#Test passed
testGenes <- function(v){
  print(head(v))
}
#testGenes(genes) 

generalTesting<- function(sg, genes){
  ##Debugging

    cat("df_sg: ")
    df_sg <- data.frame(gene = sg[[1]][[1]][1:5]) 
    print((df_sg))
    cat("nrow: ", nrow(df_sg), "ncol: ", ncol(df_sg))
    
    cat("\n\n df_genes: ")
    df_genes <- data.frame(allGenes = genes[1:5])
    print(df_genes)
}
generalTesting(sg, genes)

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