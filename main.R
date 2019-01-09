#########################################
# authors        = 'Martí Ferret'       #
#                = 'Raúl García'        #
# date           = '2019-01-11          #
#########################################


#########################################################################
# 1. Pre-process the data and choose the variables you are going to use
#########################################################################

# Llegim les dades:
DataAdult <- read.csv("adult_data.csv")

# Seleccionem les variables a fer servir (eliminem la resta de columnes):
# Veient que en les variables: fnlwgt, capital-loss, capital-gain
# hi ha molts valors a 0 i NULLs, hem decidit
# que no treballariem amb elles, per tant eliminem les columnes corresponents
# de la matriu:
DataAdult$fnlwgt <- NULL
DataAdult$capital.loss <- NULL
DataAdult$capital.gain <- NULL


# Eliminem files amb valors NULLS a alguna variable:
DataAdult <- DataAdult[!grepl("NULL", DataAdult$age),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$workclass),]
#DataAdult <- DataAdult[!grepl("NULL", DataAdult$fnlwgt),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$education),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$education.num),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$marital.status),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$occupation),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$relationship),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$race),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$sex),]
#DataAdult <- DataAdult[!grepl("NULL", DataAdult$capital.gain),]
#DataAdult <- DataAdult[!grepl("NULL", DataAdult$capital.loss),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$hours.per.week),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$native.country),]

# Apliquem One-Hot Encoding:
dmy <- dummyVars(" ~ .", data = DataAdult)
DataTraining <- data.frame(predict(dmy, newdata = DataAdult))

#########################################################################
# 2. Perform a basic statistical description
#########################################################################

summary(DataAdult)
# Falta comentar

#########################################################################
# 3. Choose the resampling method to fit, select and test your models
#########################################################################

# En el nostre cas les dades de training i de test ja anaven donades.

#########################################################################
# 4. Visualize the data; if need be, cluster the data
#########################################################################


#########################################################################
# 5. Perform a full modelling process, using linear/quadratic techniques
#########################################################################


#########################################################################
# 6. Perform a full modelling process, using non-linear techniques
#########################################################################