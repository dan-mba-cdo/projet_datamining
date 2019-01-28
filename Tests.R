# Tests

table_articles_column_structure <- data.frame("CODEARTICLE"="I", "CODEUNIVERS"="D", "CODEFAMILLE"="D", "CODESOUSFAMILLE"="D")

column <- "CODESOUSFAMILLE"
index_colonne_a_trouver <- which(column == colnames(table_articles_column_structure))
variable_type <- table_articles_column_structure[1, index_colonne_a_trouver]
if (variable_type == "I") {
  text_a_retourner <- "Identifiant"
}
if (variable_type == "C") {
  text_a_retourner <- "Continue"
}
if (variable_type == "D") {
  text_a_retourner <- "Discrète"
}
if (variable_type == "DATE") {
  text_a_retourner <- "Date"
}

column_structure <- column_name
  
valeurs_uniques <- length(unique(articles[[column]]))



freq_table <- freq(magasins[["LIBELLEREGIONCOMMERCIALE"]])[,1:2]
freq_table <- cbind(row.names(freq_table),freq_table)
names(freq_table)[1]<-"Modalités"
names(freq_table)[2]<-"Effectifs"
names(freq_table)[3]<-"Pourcentage"

df_test <- as.data.frame(freq_table)

ma_table_output <- data.frame("Modalités"="Magasins", "Effectifs"=1, "Pourcentage"=1.1)

ma_table_output <- rbind(ma_table_output, freq_table)

freq_table <- freq(magasins[["LIBELLEREGIONCOMMERCIALE"]])[,1:2]
names(freq_table)[1]<-"Effectifs"
names(freq_table)[2]<-"Pourcentage"

freq_table_df <- as.data.frame(freq_table)

new_row <- data.frame("Effectifs"=freq_table[1], "Pourcentage"=freq_table[2])

freq_table %>% 
  mutate(Modalités=cell_spec(Modalités, bold = TRUE)) %>% 
  kable(format="markdown")

