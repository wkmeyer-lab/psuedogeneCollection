library(here)



save_tsv <- function(obj, filename) {
  write.table(obj, here("data", filename), sep = "\t", row.names = FALSE, quote = FALSE)
}


a <-read_tsv(here("data", "allNames"))


b<- a[which(names_diet$scientific %in% sample(names_diet$scientific, 40)),]


save_tsv(b, "to michael")