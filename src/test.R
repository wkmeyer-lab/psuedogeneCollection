


setwd("~/meyerLab/psuedogeneCollector/data")
getwd()
list.files()

generalTesting<- function(){
   df <- read_tsv("psuedo.tsv", show_col_types = FALSE)
   View(df)
}
generalTesting()

