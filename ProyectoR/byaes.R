#Librerias a utilizar
library(e1071)
library(caret)
# Cargar datos
db<-readRDS('dbLimpios.rds')
#preprocesamiento
head(db,5)

barplot(prop.table(table(db$Label)),col=c("orange","blue"),
        legend.text=c("Ataque","Ataques malignos"))

db$Label<-as.numeric(db$Label)
cor(db$L4_SRC_PORT,db$Label)

df <- db[,-c(1,3,41,40)]


for (i in colnames(df)){
  print(i)
  print(cor(df[[i]],db$Label))
}

data<-db[,c("DST_TO_SRC_SECOND_BYTES","SHORTEST_FLOW_PKT","MAX_TTL","PROTOCOL","L7_PROTO","FLOW_DURATION_MILLISECONDS","DURATION_IN","DURATION_OUT","MIN_TTL","Label")]

data$Label<-as.factor(data$Label)
str(data)


#Shuffle data
data<-data[sample(1:nrow(data)), ]
#SVM
#Indicamos el porcentaje de entranamiento
porcentaje<-0.7

#Datos de entrenamiento y prueba
corte <- sample(nrow(data),nrow(data)*porcentaje)
train<-data[corte,]
test<-data[-corte,]

modelosvm<-svm(Label~., data = train)
summary(modelosvm)

predi<-predict(modelosvm,test)
confusionMatrix(data$Label,predi)



#Entrenar el modelo
modelo<-naiveBayes(Label~., data=train)
#Realizamos la prediccion
predBayes<-predict(modelo, newdata =test)
cm<-caret::confusionMatrix(as.factor(predBayes),as.factor(test$Label))
cm