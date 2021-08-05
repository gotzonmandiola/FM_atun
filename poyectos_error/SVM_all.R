#1.leer los datos
library(tidyverse)
library(caret)
setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
datos <- read_delim(file = "wide_data1000.csv", delim = ";")
datos <- datos %>% select(-c(ref, set))

#1.SVM model, Support Vector Machine===============

#1.1. particiones de datos para crear modelo y hacer predicciones====================

set.seed(123)
train <- createDataPartition(y = datos$sp, p = 0.8, list = FALSE, times = 1)
datos_train <- datos[train, ]
datos_test  <- datos[-train, ]

#1.2. creamos modelo con datos de entrenamiento================

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#para hacer CV necesitamos particiones y repeticiones, como es CV, no necesitamos particion de datos para crear modelo

particiones = 5
repeticiones = 5

#hiperparametros
hiperparametros <- expand.grid(sigma = 0.01,
                               C = 4)

#asignamos semillas para cada particion y repeticion con bucle, si no seria imposible afinar.

seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)#vector para que me guarde los outputs del loop
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1081, nrow(hiperparametros))#para que seed sea una fila del df:datos_train
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1081, 1)

#Entrenamiento
control_train = trainControl(
  method = "repeatedcv", 
  number = particiones, 
  repeats = repeticiones,
  seeds = seeds,
  classProbs = TRUE, #SVM no calcula las probabilidades si no le decimos
  verbose = TRUE
)

#Ajuste de modelo
svm_model = train(sp ~ ., 
                  data = datos_train,
                  method = "svmRadial",
                  tuneGrid = hiperparametros,
                  trControl = control_train,
                  returnSample = TRUE,
)
svm_model
svm_model$resample #para viisualizar resultados de todas las particiones y repeticiones

#visualizacion de parametros optimos para afinar el modelo (si uso mas de un parametro)
ggplot(svm_model, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo en función de parametros") +
  theme_bw()

#Predicción (una vez ajustados los parámetros, ver como funciona nuestro modelo con datos de test)

predicciones_prob <- predict(svm_model, newdata = datos_test,
                             type = "prob")
predicciones_prob %>% head()

#confusionmatrix

confusionMatrix(data = predicciones_prob, reference = datos_test$sp,
                positive = "si")

