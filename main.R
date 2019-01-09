#########################################
# authors        = 'Martí Ferret'       #
#                = 'Raúl García'        #
# date           = '2019-01-11          #
#########################################


#########################################################################
# 1. Pre-process the data and choose the variables you are going to use
#########################################################################
library(caret)
library(class)
library(tibble)

# Llegim les dades:
DataAdult <- read.csv("adult_data.csv", sep=";")
DataAdult2 <- read.csv("adult_test.csv", sep=";")

# Seleccionem les variables a fer servir (eliminem la resta de columnes):
# Veient que en les variables: fnlwgt, capital-loss, capital-gain
# hi ha molts valors a 0 i NULLs, hem decidit
# que no treballariem amb elles, per tant eliminem les columnes corresponents
# de la matriu:

DataAdult$fnlwgt <- NULL
DataAdult$capital.loss <- NULL
DataAdult$capital.gain <- NULL

DataAdult2$fnlwgt <- NULL
DataAdult2$capital.loss <- NULL
DataAdult2$capital.gain <- NULL


# Eliminem files amb valors NULLS a alguna variable:
DataAdult <- DataAdult[!grepl("NULL", DataAdult$age),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$workclass),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$education),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$education.num),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$marital.status),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$occupation),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$relationship),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$race),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$sex),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$hours.per.week),]
DataAdult <- DataAdult[!grepl("NULL", DataAdult$native.country),]

DataAdult2 <- DataAdult2[!grepl("NULL", DataAdult2$age),]
DataAdult2 <- DataAdult2[!grepl("NULL", DataAdult2$workclass),]
DataAdult2 <- DataAdult2[!grepl("NULL", DataAdult2$education),]
DataAdult2 <- DataAdult2[!grepl("NULL", DataAdult2$education.num),]
DataAdult2 <- DataAdult2[!grepl("NULL", DataAdult2$marital.status),]
DataAdult2 <- DataAdult2[!grepl("NULL", DataAdult2$occupation),]
DataAdult2 <- DataAdult2[!grepl("NULL", DataAdult2$relationship),]
DataAdult2 <- DataAdult2[!grepl("NULL", DataAdult2$race),]
DataAdult2 <- DataAdult2[!grepl("NULL", DataAdult2$sex),]
DataAdult2 <- DataAdult2[!grepl("NULL", DataAdult2$hours.per.week),]
DataAdult2 <- DataAdult2[!grepl("NULL", DataAdult2$native.country),]



# Apliquem One-Hot Encoding:
dmy <- dummyVars(" ~ .", data = DataAdult)
DataTraining <- data.frame(predict(dmy, newdata = DataAdult))

DataTraining$workclass..NULL <- NULL
DataTraining$occupation..NULL <- NULL
DataTraining$native.country..NULL <- NULL

dmy2 <- dummyVars(" ~ .", data = DataAdult2)
DataTest <- data.frame(predict(dmy2, newdata = DataAdult2))

DataTest$workclass..NULL <- NULL
DataTest$occupation..NULL <- NULL
DataTest$native.country..NULL <- NULL


native.country..Holand.Netherlands <- rep(0,15060)
DataTest <- add_column(DataTest, native.country..Holand.Netherlands, .before = "native.country..Honduras") # no hi ha valors


# Separem els parametres de la columna a predir
len <- length(DataTraining)

DataTraining.input <- DataTraining[,1:len-1]
DataTraining.class <- DataTraining[,len]

DataTest.input <- DataTest[,1:len-1]
DataTest.class <- DataTest[,len]

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


## setup a kNN model with 3 neighbours
## Notice there is no "learning" ... the data is the model (just test!)

myknn <- knn (DataTraining.input, DataTest.input, DataTraining.class, k = 2, prob=TRUE) 

####################################
save(myknn, file = "myknn.mod")

load ("myknn.mod")
####################################

(tab <- table(myknn, DataTest.class))
(1 - sum(tab[row(tab)==col(tab)])/sum(tab))*100




#########################################################################
# 6. Perform a full modelling process, using non-linear techniques
#########################################################################