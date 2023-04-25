#Librerias a utilizar
library(e1071)
library(caret)
# Cargar datos
db<-readRDS('dbLimpios.rds')
#preprocesamiento
head(db,5)
db$Label<-as.numeric(db$Label)
cor(db$L4_SRC_PORT,db$Label)

df <- db[,-c(1,3,41,40)]

data<-db[,c("DST_TO_SRC_SECOND_BYTES","SHORTEST_FLOW_PKT","MAX_TTL","PROTOCOL","L7_PROTO","FLOW_DURATION_MILLISECONDS","DURATION_IN","DURATION_OUT","MIN_TTL","Label")]

data$Label<-as.factor(data$Label)
str(data)

#Shuffle data
data<-data[sample(1:nrow(data)), ]
data<-data[1:100000,]
table(data$Label)

# Separación de datos (55% entrenamiento, 15% validación y 30% pruebas)
set.seed(123) # Para reproducibilidad
trainIndex <- createDataPartition(data$Label, p = 0.55, list = FALSE)
train <- data[trainIndex,]
temp <- data[-trainIndex,]
validationIndex <- createDataPartition(temp$Label, p = 0.15, list = FALSE)
validation <- temp[validationIndex,]
test <- temp[-validationIndex,]

# Evaluación cruzada con K-10 folds para K = 10
set.seed(123) # Para reproducibilidad
folds <- createFolds(train$Label, k = 10)
summary(folds)

# SVM
set.seed(123) # Para reproducibilidad
acc_svm <- c()
precision_svm <- c()
recall_svm <- c()
f1_svm <- c()

#Evaluación cruzada con K-10 folds para K = 10
for (i in 1:10) {
  train_cv <- train[-folds[[i]], ]
  test_cv <- train[folds[[i]], ]
  modelosvm <- svm(Label~., data = train_cv)
  predi<-predict(modelosvm, test_cv)
  cm_svm <- confusionMatrix(as.factor(predi), as.factor(test_cv$Label))
  acc_svm <- c(acc_svm, cm_svm$overall['Accuracy'])
  precision_svm <- c(precision_svm, cm_svm$byClass['Precision'])
  recall_svm <- c(recall_svm, cm_svm$byClass['Recall'])
  f1_svm <- c(f1_svm, cm_svm$byClass['F1'])
}

# Métricas de evaluación para SVM
acc_svm_mean <- mean
acc_svm_mean
precision_svm_mean <- mean(precision_svm)
precision_svm_mean
recall_svm_mean <- mean(recall_svm)
recall_svm_mean
f1_svm_mean <- mean(f1_svm)
f1_svm_mean

# Naive Bayes
set.seed(123) # Para reproducibilidad
acc_nb <- c()
precision_nb <- c()
recall_nb <- c()
f1_nb <- c()

for (i in 1:10) {
  train_cv <- train[-folds[[i]], ]
  test_cv <- train[folds[[i]], ]
  modelo_nb <- naiveBayes(Label~., data=train_cv)
  pred_nb <- predict(modelo_nb, newdata = test_cv)
  cm_nb <- confusionMatrix(as.factor(pred_nb), as.factor(test_cv$Label))
  acc_nb <- c(acc_nb, cm_nb$overall['Accuracy'])
  precision_nb <- c(precision_nb, cm_nb$byClass['Precision'])
  recall_nb <- c(recall_nb, cm_nb$byClass['Recall'])
  f1_nb <- c(f1_nb, cm_nb$byClass['F1'])
}

# Métricas de evaluación para Naive Bayes
acc_nb_mean <- mean(acc_nb)
acc_nb_mean
precision_nb_mean <- mean(precision_nb)
precision_nb_mean
recall_nb_mean <- mean(na.omit(recall_nb))
recall_nb_mean
f1_nb_mean <- mean(na.omit(f1_nb))
f1_nb_mean
