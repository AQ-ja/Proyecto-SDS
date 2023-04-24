
library(dplyr)
library(tidyr)
library(ROSE)
library(randomForest)
library(caret)
library(e1071)
library(corrplot)

#db<-read.csv('datasetSucio.csv')

#saveRDS(db, "db.rds")

db<-readRDS('db.rds')

#Convertimos la variable a predecir en factor
db$Label<-as.factor(db$Label)
summary(db)


ataques <- subset(db,db$Label == 0)
ataquesMalignos <- subset(db,db$Label == 1)
db<- full_join(ataques,ataquesMalignos[1:(nrow(ataques)),] )
