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

# Separación de los datos
set.seed(123)
train_index <- sample(seq_len(nrow(data)), size = floor(0.55 * nrow(data)), replace = FALSE)
train_data <- data[train_index,]
temp_data <- data[-train_index,]
validation_index <- sample(seq_len(nrow(temp_data)), size = floor(0.15 * nrow(data)), replace = FALSE)
validation_data <- temp_data[validation_index,]
test_data <- temp_data[-validation_index,]

# Modelos Naive Bayes y SVM
library(e1071)
library(caret)
set.seed(123)
model_nb <- naiveBayes(Label ~ ., data = train_data)
#model_svm <- svm(Label ~ ., data = train_data)

# Predicción en datos de validación
prediction_nb_val <- predict(model_nb, newdata = validation_data[,-10])
#prediction_svm_val <- predict(model_svm, newdata = validation_data[,-10])

# Métricas de evaluación para datos de validación
metrics_nb_val <- confusionMatrix(prediction_nb_val, validation_data$Label)
#metrics_svm_val <- confusionMatrix(prediction_svm_val, validation_data$Label)

# Predicción en datos de prueba
prediction_nb_test <- predict(model_nb, newdata = test_data[,-10])
#prediction_svm_test <- predict(model_svm, newdata = test_data[,-10])

# Métricas de evaluación para datos de prueba
metrics_nb_test <- confusionMatrix(prediction_nb_test, test_data$Label)
#metrics_svm_test <- confusionMatrix(prediction_svm_test, test_data$Label)

# Evaluación cruzada con K-10 folds
set.seed(123)
nb_kfold <- train(Label ~ ., data = data, method = "nb", trControl = trainControl(method = "cv", number = 10))
#svm_kfold <- train(Label ~ ., data = data, method = "svmRadial", trControl = trainControl(method = "cv", number = 10))

# Métricas de evaluación para K-10 folds
metrics_nb_kfold <- nb_kfold$results[5]
#metrics_svm_kfold <- svm_kfold$results[5]