# Seleccionem les variables a fer servir (eliminem la resta de columnes):
# Veient que en les variables: fnlwgt, capital-loss, capital-gain
# hi ha molts valors a 0 i NULLs, hem decidit
# que no treballariem amb elles, per tant eliminem les columnes corresponents
# de la matriu:

clean_dataset <- function(dataset){
  
  # Eliminem les columnes que representen les variables que no necessitem
  dataset$fnlwgt <- NULL
  dataset$capital.loss <- NULL
  dataset$capital.gain <- NULL
  
  # Eliminem files amb valors NULLS a alguna variable:
  dataset <- dataset[!grepl("NULL", dataset$age),]
  dataset <- dataset[!grepl("NULL", dataset$workclass),]
  dataset <- dataset[!grepl("NULL", dataset$education),]
  dataset <- dataset[!grepl("NULL", dataset$education.num),]
  dataset <- dataset[!grepl("NULL", dataset$marital.status),]
  dataset <- dataset[!grepl("NULL", dataset$occupation),]
  dataset <- dataset[!grepl("NULL", dataset$relationship),]
  dataset <- dataset[!grepl("NULL", dataset$race),]
  dataset <- dataset[!grepl("NULL", dataset$sex),]
  dataset <- dataset[!grepl("NULL", dataset$hours.per.week),]
  dataset <- dataset[!grepl("NULL", dataset$native.country),]
  
}