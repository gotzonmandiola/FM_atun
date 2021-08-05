#1.leer los datos
library(tidyverse)
library(caret)
setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
datos <- read_delim(file = "wide_data1000.csv", delim = ";")
datos <- datos %>% select(-c(ref, set))

#1.GBM model, Gradient Boosting Machine ===============


#1.2. creamos modelo con datos de entrenamiento================

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#para hacer CV necesitamos particiones y repeticiones, como es CV, no necesitamos particion de datos para crear modelo

particiones = 5
repeticiones = 5

#hiperparametros
hiperparametros = expand.grid(interaction.depth = 2,
                              n.trees = 50,
                              shrinkage = 0.1,
                              n.minobsinnode = 10)

#asignamos semillas para cada particion y repeticion con bucle, si no seria imposible afinar.

seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1) 
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1081, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1081, 1)

#Entrenamiento
control_train = trainControl(
  method = "repeatedcv", 
  number = particiones, 
  repeats = repeticiones,
  seeds = seeds,
  classProbs = TRUE, 
  verbose = TRUE
)

#Ajuste de modelo
gbm_model = train(sp ~ ., 
                  data = datos_train,
                  method = "gbm",
                  tuneGrid = hiperparametros,
                  trControl = control_train,
)
gbm_model
gbm_model$resample #para viisualizar resultados de todas las particiones y repeticiones

#rf
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
svm_model$resample 

t.test(x = gbm_model$resample, y = svm_model$resample, paired = TRUE, alternative = "two.sided")
