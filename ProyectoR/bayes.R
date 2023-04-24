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
data<-data[1:1000000,]
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
modelosvm <- svm(Label~., data = train)
summary(modelosvm)

# Métricas de evaluación para SVM
predi<-predict(modelosvm, test)
cm_svm <- confusionMatrix(as.factor(predi), as.factor(test$Label))
cm_svm
acc_svm <- cm_svm$overall['Accuracy']
acc_svm
precision_svm <- cm_svm$byClass['Precision']
precision_svm
recall_svm <- cm_svm$byClass['Recall']
recall_svm
f1_svm <- cm_svm$byClass['F1']
f1_svm

# Naive Bayes
modelo_nb <- naiveBayes(Label~., data=train)

# Métricas de evaluación para Naive Bayes
pred_nb <- predict(modelo_nb, newdata = validation)
cm_nb <- confusionMatrix(as.factor(pred_nb), as.factor(validation$Label))
cm_nb
acc_nb <- cm_nb$overall['Accuracy']
acc_nb
precision_nb <- cm_nb$byClass['Precision']
precision_nb
recall_nb <- cm_nb$byClass['Recall']
recall_nb
f1_nb <- cm_nb$byClass['F1']
f1_nb

# #SVM
# #Indicamos el porcentaje de entranamiento
# porcentaje<-0.7
# 
# #Datos de entrenamiento y prueba
# corte <- sample(nrow(data),nrow(data)*porcentaje)
# train<-data[corte,]
# test<-data[-corte,]
# 
# modelosvm<-svm(Label~., data = train)
# summary(modelosvm)
# 
# predi<-predict(modelosvm,test)
# confusionMatrix(as.factor(predi),as.factor(test$Label))
# 
# 
# 
# #Entrenar el modelo
# modelo<-naiveBayes(Label~., data=train)
# #Realizamos la prediccion
# predBayes<-predict(modelo, newdata =test)
# cm<-caret::confusionMatrix(as.factor(predBayes),as.factor(test$Label))
# cm
