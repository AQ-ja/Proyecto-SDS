library(dplyr)
library(tidyr)
library(ROSE)
library(randomForest)
library(caret)
library(e1071)
library(corrplot)

db<-read.csv('datasetSucio.csv')

#saveRDS(db, "db.rds")

#db<-readRDS('db.rds')

nrow(db)
ncol(db)
names(db)
summary(db)

db$Label<-as.factor(db$Label)
summary(db$Label)

barplot(prop.table(table(db$Label)),col=c("orange","blue"),
        legend.text=c("Ataque","Ataques malignos"))

ataques <- db[db$Label == 0, ]

ataquesMalignos <- db[db$Label ==1,]

nrow(ataquesMalignos[1:(nrow(ataques)),])

db<- full_join(ataques,ataquesMalignos[1:(nrow(ataques)),] )

ncol(db)
nrow(db)

barplot(prop.table(table(db$Label)),col=c("orange","blue"),
        legend.text=c("Ataque","Ataques malignos"))

View(head(db))

#Removemos la ultima columna la cual significa de donde salio el dato, lo cual no nos proporciona mayor informacion
df2 <- db[,!names(db) %in% c("Dataset")]

#Datos numericos 
dfNumericos<- db[,c('L4_SRC_PORT','L4_DST_PORT','PROTOCOL')]
dfFactor<- db[,c('Label')]
summary(df2)

pairs(dfNumericos)
corPlot(data, cex = 1.2, main = "Matriz de correlación")


# Graficas de correlacion


corrplot(cor(db[,c('IPV4_SRC_ADDR','L4_SRC_PORT','IPV4_DST_ADDR','L4_DST_PORT','PROTOCOL')]),        # Matriz de correlación
         method = "shade", # Método para el gráfico de correlación
         type = "full",    # Estilo del gráfico (también "upper" y "lower")
         diag = TRUE,      # Si TRUE (por defecto), añade la diagonal
         tl.col = "black", # Color de las etiquetas
         bg = "white",     # Color de fondo
         title = "",       # Título
         col = NULL)       # Paleta de colores


pairs(db,                     # Data frame de variables
      labels = colnames(data),  # Nombres de las variables
      pch = 21,                 # Símbolo pch
      bg = rainbow(3)[grupos],  # Color de fondo del símbolo (pch 21 a 25)
      col = rainbow(3)[grupos], # Color de borde del símbolo
      main = "Iris",            # Título del gráfico
      row1attop = TRUE,         # Si FALSE, cambia la dirección de la diagonal
      gap = 1,                  # Distancia entre subplots
      cex.labels = NULL,        # Tamaño del texto de la diagonal
      font.labels = 1)          # Estilo de fuente del texto de la diagonal
