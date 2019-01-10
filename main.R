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
library(plotly)
library(reshape2)
library(ggplot2)


# Llegim les dades:
DataTraining.Categoric <- read.csv("adult_data.csv", sep=";")
DataTest.Categoric <- read.csv("adult_test.csv", sep=";")

# Seleccionem les variables a fer servir (eliminem la resta de columnes):
# Veient que en les variables: fnlwgt, capital-loss, capital-gain
# hi ha molts valors a 0 i NULLs, hem decidit
# que no treballariem amb elles, per tant eliminem les columnes corresponents
# de la matriu:

DataTraining.Categoric$fnlwgt <- NULL
DataTraining.Categoric$capital.loss <- NULL
DataTraining.Categoric$capital.gain <- NULL

DataTest.Categoric$fnlwgt <- NULL
DataTest.Categoric$capital.loss <- NULL
DataTest.Categoric$capital.gain <- NULL


# Eliminem files amb valors NULLS a alguna variable:
DataTraining.Categoric <- DataTraining.Categoric[!grepl("NULL", DataTraining.Categoric$age),]
DataTraining.Categoric <- DataTraining.Categoric[!grepl("NULL", DataTraining.Categoric$workclass),]
DataTraining.Categoric <- DataTraining.Categoric[!grepl("NULL", DataTraining.Categoric$education),]
DataTraining.Categoric <- DataTraining.Categoric[!grepl("NULL", DataTraining.Categoric$education.num),]
DataTraining.Categoric <- DataTraining.Categoric[!grepl("NULL", DataTraining.Categoric$marital.status),]
DataTraining.Categoric <- DataTraining.Categoric[!grepl("NULL", DataTraining.Categoric$occupation),]
DataTraining.Categoric <- DataTraining.Categoric[!grepl("NULL", DataTraining.Categoric$relationship),]
DataTraining.Categoric <- DataTraining.Categoric[!grepl("NULL", DataTraining.Categoric$race),]
DataTraining.Categoric <- DataTraining.Categoric[!grepl("NULL", DataTraining.Categoric$sex),]
DataTraining.Categoric <- DataTraining.Categoric[!grepl("NULL", DataTraining.Categoric$hours.per.week),]
DataTraining.Categoric <- DataTraining.Categoric[!grepl("NULL", DataTraining.Categoric$native.country),]

DataTest.Categoric <- DataTest.Categoric[!grepl("NULL", DataTest.Categoric$age),]
DataTest.Categoric <- DataTest.Categoric[!grepl("NULL", DataTest.Categoric$workclass),]
DataTest.Categoric <- DataTest.Categoric[!grepl("NULL", DataTest.Categoric$education),]
DataTest.Categoric <- DataTest.Categoric[!grepl("NULL", DataTest.Categoric$education.num),]
DataTest.Categoric <- DataTest.Categoric[!grepl("NULL", DataTest.Categoric$marital.status),]
DataTest.Categoric <- DataTest.Categoric[!grepl("NULL", DataTest.Categoric$occupation),]
DataTest.Categoric <- DataTest.Categoric[!grepl("NULL", DataTest.Categoric$relationship),]
DataTest.Categoric <- DataTest.Categoric[!grepl("NULL", DataTest.Categoric$race),]
DataTest.Categoric <- DataTest.Categoric[!grepl("NULL", DataTest.Categoric$sex),]
DataTest.Categoric <- DataTest.Categoric[!grepl("NULL", DataTest.Categoric$hours.per.week),]
DataTest.Categoric <- DataTest.Categoric[!grepl("NULL", DataTest.Categoric$native.country),]

TotalData <- rbind(DataTraining.Categoric, DataTest.Categoric)

# Apliquem One-Hot Encoding:
dmy <- dummyVars(" ~ .", data = DataTraining.Categoric)
DataTraining <- data.frame(predict(dmy, newdata = DataTraining.Categoric))

DataTraining$workclass..NULL <- NULL
DataTraining$occupation..NULL <- NULL
DataTraining$native.country..NULL <- NULL

dmy2 <- dummyVars(" ~ .", data = DataTest.Categoric)
DataTest <- data.frame(predict(dmy2, newdata = DataTest.Categoric))

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

summary(TotalData)

# VARIABLE: AGE
ages <- as.data.frame(table(TotalData$age))
plot_ly(y=ages$Freq, x=ages$Var1 , type="scatter", mode="markers+lines")

#comparing AGE-INCOME:
temp <- as.data.frame(cbind(TotalData$age, TotalData$income)); names(temp) <- c("AGE", "INCOME")
ages <- sort(unique(TotalData$age))
# percentage of <=50k for each age
lower <- list()
higher <- list()
for(a in ages){
  tempValue <- as.numeric((table(temp[temp$AGE==a,][2])/nrow(temp[temp$AGE==a,])*100)[1])
  lower <- c(lower, as.integer(tempValue))
  higher <- c(higher, as.integer(100-tempValue))
}

ageIncome <- as.matrix(rbind(unlist(lower), unlist(higher)))
colnames(ageIncome) <- ages
rownames(ageIncome) <- c("<=50k", ">50k")
barplot(ageIncome, col=c(1,2), border="white", font.axis=2, beside=T, legend=rownames(ageIncome), xlab="age", ylab="percentage", font.lab=2)


## HARÉ LO MISMO PARA EL RESTO DE VARIABLES


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

### 1. The kNN classifier ###
###############################################
neighbours <- seq(11,40, 2)
errors <- matrix (nrow=length(neighbours), ncol=2)
colnames(errors) <- c("k","LOOCV error")

for (i in c(1:length(neighbours)))
{
  myknn.cv <- knn.cv (DataTraining.input, DataTraining.class, k = neighbours[i]) 

  errors[i, "k"] <- neighbours[i]
  
  tab <- table(myknn.cv, DataTraining.class)
  errors[i, "LOOCV error"] <- (1 - sum(tab[row(tab)==col(tab)])/sum(tab))*100
}

errors


### 2. The Naïve Bayes classifier ###
###############################################
library (e1071)

ModelNaiveBayes <- naiveBayes(as.factor(income) ~ ., data = DataTraining.Categoric)

# compute the test (prediction) error
pred <- predict(ModelNaiveBayes, DataTest.Categoric)

# form and display confusion matrix & overall error
(tab <- table(Pred=pred, True=DataTest.class))
(1 - sum(tab[row(tab)==col(tab)])/sum(tab))*100

### 3. Logistic Regression ###
###############################################

ModelGlm <- glm (income ~ ., data = DataTraining.Categoric)
ModelGlm.AIC <- step (ModelGlm)

gl1t <- predict(ModelGlm.AIC, newdata=DataTest.Categoric,type="response")
gl1predt <- NULL
P <- 0.5
gl1predt[gl1t<P] <- 0
gl1predt[gl1t>=P] <- 1

(M1.TEtable <- table(Truth=DataTest.class,Pred=gl1predt))

(100*(1-sum(diag(M1.TEtable))/nrow(DataTest.Categoric)))


#########################################################################
# 6. Perform a full modelling process, using non-linear techniques
#########################################################################

### 1. MLP ###
###############################################

### 2. Suport Vector Machines ###
###############################################
ModelSVM <- svm (DataTraining.input,as.factor(DataTraining.class),epsilon=0.01, kernel = "radial")

# compute the test (prediction) error
pred <- predict(ModelSVM, DataTest.input)


# form and display confusion matrix & overall error
(tab <- table(Pred=pred, True=DataTest.class))
(1 - sum(tab[row(tab)==col(tab)])/sum(tab))*100

#########################################################################