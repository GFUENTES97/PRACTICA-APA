#########################################
# authors        = 'Martí Ferret'       #
#                = 'Raúl García'        #
# date           = '2019-01-12          #
#########################################


#########################################################################
# 1. Pre-process the data and choose the variables you are going to use
#########################################################################

# Llegim les dades:
Data <- read.csv("adult_data.csv")

# Seleccionem les variables a fer servir (eliminem la resta de columnes):

# Eliminem files amb valors NULLS a alguna variable:
Data <- Data[!grepl("NULL", Data$age),]
Data <- Data[!grepl("NULL", Data$workclass),]
Data <- Data[!grepl("NULL", Data$fnlwgt),]
Data <- Data[!grepl("NULL", Data$education),]
Data <- Data[!grepl("NULL", Data$education.num),]
Data <- Data[!grepl("NULL", Data$marital.status),]
Data <- Data[!grepl("NULL", Data$occupation),]
Data <- Data[!grepl("NULL", Data$relationship),]
Data <- Data[!grepl("NULL", Data$race),]
Data <- Data[!grepl("NULL", Data$sex),]
Data <- Data[!grepl("NULL", Data$capital.gain),]
Data <- Data[!grepl("NULL", Data$capital.loss),]
Data <- Data[!grepl("NULL", Data$hours.per.week),]
Data <- Data[!grepl("NULL", Data$native.country),]

#########################################################################
# 2. Perform a basic statistical description
#########################################################################

#########################################################################
# 3. Choose the resampling method to fit, select and test your models
#########################################################################

#########################################################################
# 4. Visualize the data; if need be, cluster the data
#########################################################################

#########################################################################
# 5. Perform a full modelling process, using linear/quadratic techniques
#########################################################################

#########################################################################
# 6. Perform a full modelling process, using non-linear techniques
#########################################################################