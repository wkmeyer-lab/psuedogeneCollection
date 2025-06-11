


library(stringr)
library(readr)
library(dplyr)

generalTests <- function(){
  geneCollector()
  ##Debugging
  for (i in 1:3){
    cat("Contents: ")
    species_gene[[i]][,1]
    cat("Type: ")
    typeof(species_gene[[i]][,1])
  }
  
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
}

#geneCollector accesses and cleans loss_summ_data for each species to be examined. Then, it  
geneCollector <- function(){
  # Access losssum.tsv
  setwd("~/meyerLab/psuedogeneCollector/data")
  files <- list.files(
    path =".", 
    recursive=TRUE, 
    pattern = "loss_summ_data",
    full.names=TRUE)
  
  species_gene <- grabData(files)
  genes <- updateUniqueGenes(files) #should pass species_gene as argument and iterate over every value
  
  return(list(species_gene = species_gene, genes = genes))
}

#' Obtains and cleans loss_summ_data
#' @param files --> Vector containing absolute paths to every instance of loss_summ_data
#' @return A list formatted as a dictionary --> key = character (species name), value = df (clean loss_summ_data)
#' @author Tyler gruver
grabData <- function(files){
  species_gene <- list() #key == species name, value == .tsv file containing losssum data
   counter <- 0
   
  for(i in 1:length(files)){
    temp <- str_locate_all(files[i], "/")[[1]] #temp returns a list of matrixes. [[1]] grabs the matrix  at position 1... temp will only ever be a list of length 1 
    
    counter <- counter + 1
    if(counter > 5) break

    
    #Setting key-value pairs
    name <- substring(files[i], temp[3, 1] +1 , temp[4, 1] -1)
    key <- paste("Key: ", name , sep = " ")
    value <- cleanData(files[i]) #returns clean .tsv
    species_gene[[key]] <- value

    cat("\n\n\n\n ", counter, " iterations /n/n/n/n/n")

  }  
  
  
  return(species_gene)
}

#' cleans loss_summ_data
#' @param f --> Vector containing absolute paths to a single instance of loss_summ_data
#' @return A df --> Clean loss_summ_data
#' @author Tyler gruver
cleanData <- function(f){
  df <- read_tsv(f, show_col_types = FALSE)
  clean_df <- df[df$PROJECTION == "GENE",]
  return(clean_df[, -1])
}

#' Creates a list of unique genes seen under every instance of loss_summ_data
#' @param files --> Vector containing absolute paths to a single instance of loss_summ_data 
#' @return A list containing every unique gene under every instance of loss_summ_data
#' @author Tyler gruver    
updateUniqueGenes <- function(files){
  genes <- character(length=0)
  
     counter = 0
asd
  
  for(f in files) {
    counter <- counter + 1
    if(counter > 5) break
    
    df <- read_tsv(f, show_col_types = FALSE)
    genes <- union(genes, df[,1])  # performs set union operation on genes character vector AND df[,1] column subset.
  }
  return(genes)
# }