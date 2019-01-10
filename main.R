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
library (e1071)
library(randomForest)

source("auxiliar_functions.R")  # a file with auxiliary functions, to keep code cleaner
LABELS <- c("<=50k", ">50k")    # a constant 

# Llegim les dades:
DataTraining.Categoric <- read.csv("adult_data.csv", sep=";")
DataTest.Categoric <- read.csv("adult_test.csv", sep=";")

# Fem una neteja del dataset
DataTraining.Categoric <- clean_dataset(DataTraining.Categoric)
DataTest.Categoric <- clean_dataset(DataTest.Categoric)
TotalData <- rbind(DataTraining.Categoric, DataTest.Categoric)

# Apliquem One-Hot Encoding:
DataTraining <- one_hot_encoding(DataTraining.Categoric)
DataTest <- one_hot_encoding(DataTest.Categoric)

native.country..Holand.Netherlands <- rep(0,15060)
DataTest <- add_column(DataTest, native.country..Holand.Netherlands, .before = "native.country..Honduras") # no hi ha valors


# Separem els parametres de la columna a predir
len <- length(DataTraining)

DataTraining.input <- DataTraining[,1:len-1]
DataTraining.class <- DataTraining[,len]

DataTest.input <- DataTest[,1:len-1]
DataTest.class <- DataTest[,len]

####################################################################################################
# 2. Perform a basic statistical description 
# 4. Visualize the data; if need be, cluster the data
####################################################################################################

# Small summary of the dataset
summary(TotalData)

# VARIABLE: AGE
ages <- as.data.frame(table(TotalData$age))
plot_ly(y=ages$Freq, x=ages$Var1 , type="scatter", mode="markers+lines")

# comparing AGE - INCOME:
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
rownames(ageIncome) <- LABELS

# plot: Percentage of individuals earning >50k(red) / <=50k(black) depending on their age
barplot(ageIncome, col=c(1,2), border="white", font.axis=2, beside=T, legend=LABELS, xlab="age", ylab="percentage", font.lab=2, args.legend = list(x = "topright", bty = "n", inset=c(.40, 0)))


## VARIABLE EDUCATION.NUM:
education.levels <- as.data.frame(table(TotalData$education.num))
plot_ly(y=education.levels$Freq, x=education.levels$Var1 , type="scatter", mode="markers+lines")

# comparing Education.level - INCOME
temp <- as.data.frame(cbind(TotalData$education.num, TotalData$income)); names(temp) <- c("EDUCATION", "INCOME")
education.levels <- sort(unique(TotalData$education.num))

# percentage of <=50k for each EDUCATION LEVEL
lower <- list()
higher <- list()
for(a in education.levels){
  tempValue <- as.numeric((table(temp[temp$EDUCATION==a,][2])/nrow(temp[temp$EDUCATION==a,])*100)[1])
  lower <- c(lower, as.integer(tempValue))
  higher <- c(higher, as.integer(100-tempValue))
}

eduIncome <- as.matrix(rbind(unlist(lower), unlist(higher)))
colnames(eduIncome) <- education.levels
rownames(eduIncome) <- LABELS
# plot: Percentage of individuals earning >50k(red) / <=50k(black) depending on their education level
barplot(eduIncome,col=c(1,2),border="white", font.axis=2, beside=T, legend=LABELS, xlab="education level", ylab="percentage", font.lab=2, args.legend = list(x = "topright", bty = "n", inset=c(.15, 0)))


## VARIABLE HOURS.PER.WEEK:
hours <- as.data.frame(table(TotalData$hours.per.week))
plot_ly(y=hours$Freq, x=hours$Var1 , type="scatter", mode="markers+lines")

# comparing hours.per.week - INCOME
temp <- as.data.frame(cbind(TotalData$hours.per.week, TotalData$income)); names(temp) <- c("WEEK.HOURS", "INCOME")
hours <- sort(unique(TotalData$hours.per.week))

# percentage of <=50k for each Week-hours worked
lower <- list()
higher <- list()
for(a in hours){
  tempValue <- as.numeric((table(temp[temp$WEEK.HOURS==a,][2])/nrow(temp[temp$WEEK.HOURS==a,])*100)[1])
  lower <- c(lower, as.integer(tempValue))
  higher <- c(higher, as.integer(100-tempValue))
}

hoursIncome <- as.matrix(rbind(unlist(lower), unlist(higher)))
colnames(hoursIncome) <- hours
rownames(hoursIncome) <- LABELS
barplot(hoursIncome,col=c(1,2),border="white", font.axis=2, beside=T, legend=LABELS, xlab="Hours per week", ylab="percentage", font.lab=2, args.legend = list(x = "topright", bty = "n", inset=c(.35, -.10)))


#########################################################################
# 3. Choose the resampling method to fit, select and test your models
#########################################################################

# En el nostre cas el dataset venia dividit en Train (66%) / Test (33%)

#########################################################################
# 5. Perform a full modelling process, using linear/quadratic techniques
#########################################################################

### 1. The kNN classifier ###
###############################################


# Optimitzem el millor valor de k
# (triga molt, el millor és 10, explicat a la Documentació)
###
neighbours <- seq(1,10, 1)
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
###

# Veiem que a partir de K = 10 l'error ja no disminueix notablement, per
# tant considerem k = 10 com la millor opció.

ModelKnn.10 <- knn(DataTraining.input, DataTest.input, DataTraining.class, k = 10)

(tab <- table(ModelKnn.10, DataTest.class))
(1 - sum(tab[row(tab)==col(tab)])/sum(tab))*100

### 2. The Naïve Bayes classifier ###
###############################################
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


### 1. Suport Vector Machines ###
###############################################

# Anem a optimitzar gamma:

vec.gamma <- c(1, 5, 10, 20, 50, 100, 150, 200, 250)
errors <- matrix (nrow=length(vec.gamma), ncol=2)
colnames(errors) <- c("gamma","error")

for (i in c(1:length(vec.gamma)))
{
  mysvm <- svm (DataTraining.input,as.factor(DataTraining.class),epsilon=0.01, gamma = vec.gamma[i], kernel = "radial")
  
  errors[i, "gamma"] <- vec.gamma[i]
  pred.rf <- predict (mysvm, DataTest.input)
  
  tab <-  table(Truth=DataTest.class, Pred=pred.rf)
  errors[i, "error"] <- (1 - sum(tab[row(tab)==col(tab)])/sum(tab))*100
  print(vec.gamma[i])
}

errors


ModelSVM <- svm (DataTraining.input,as.factor(DataTraining.class),epsilon=0.01, kernel = "radial")

# compute the test (prediction) error
pred <- predict(ModelSVM, DataTest.input)


# form and display confusion matrix & overall error
(tab <- table(Pred=pred, True=DataTest.class))
(1 - sum(tab[row(tab)==col(tab)])/sum(tab))*100

### 2. Random Forest ###
###############################################

# Anem a optimitzar el nombre d'arbres (ntree)

vec.ntree <- c(1, 5, 10, 20, 50, 100, 250, 500, 1000, 2000)
errors <- matrix (nrow=length(vec.ntree), ncol=2)
colnames(errors) <- c("ntree","error")

for (i in c(1:length(vec.ntree)))
{
  myrf <- randomForest (x = DataTraining.input, y=as.factor(DataTraining.class), ntree=vec.ntree[i], proximity=FALSE)
  
  errors[i, "ntree"] <- vec.ntree[i]
  pred.rf <- predict (myrf, DataTest.input)
  
  tab <-  table(Truth=DataTest.class, Pred=pred.rf)
  errors[i, "error"] <- (1 - sum(tab[row(tab)==col(tab)])/sum(tab))*100
  print(vec.ntree[i])
}

errors

# El millor es 20 (veure documentació), a partir d'aqui no millorem error


ModelRandomForest <- randomForest(x = DataTraining.input, y=as.factor(DataTraining.class), ntree = 20, proximity=FALSE)

pred.rf <- predict (ModelRandomForest, DataTest.input)

(ct <- table(Truth=DataTest.class, Pred=pred.rf))

# real test error is 
round(100*(1-sum(diag(ct))/sum(ct)),2)

#########################################################################