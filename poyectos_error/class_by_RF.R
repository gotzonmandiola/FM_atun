#1.leer los datos
library(caret)
library(tidyverse)
setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
datos <- read_delim(file = "wide_data1000.csv", delim = ";")
glimpse(datos)
str(datos)
datos <- datos %>% select(-c(ref, set))

# 1.HIPERPAR�METROS, N�MERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICI�N ----
particiones  <- 5
repeticiones <- 2
hiperparametros <- expand.grid(mtry = c(3, 4, 5, 6),
                               min.node.size = 1,
                               splitrule = "gini")

# 2. DEFINICI�N DEL ENTRENAMIENTO ----

control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones,
                              returnResamp = "final", verboseIter = F,
                              allowParallel = TRUE)

# 3. AJUSTE DEL MODELO ----

set.seed(123) 
modelo_rf <- train(sp ~ ., data = datos,
                   method = "ranger",
                   tuneGrid = hiperparametros, #aqui se pueden poner los hiperparametros
                   metric = "Accuracy",
                   trControl = control_train,
                   # N�mero de �rboles ajustados
                   num.trees = 500)
modelo_rf
modelo_rf$finalModel
confusionMatrix(modelo_rf)
#grafica que visualiza la evoluci�n del modelo seg�n ajuste de hierparametro
ggplot(model, highlight = TRUE) +
  labs(title = "Evoluci�n del accuracy del modelo en funci�n de C") +
  theme_bw()


#modelo1 -----

set.seed(123)
model_1 <- train(
  sp ~ .,
  tuneLength = 5,
  data = datos, 
  method = "ranger",
  trControl = trainControl(
    method = "repeatedcv", 
    number = particiones,
    repeats = repeticiones,
    verboseIter = F
  )
)
model_1


ggplot(model_1, highlight = TRUE) +
  labs(title = "Evoluci�n del accuracy del modelo en funci�n de 'mtry'") +
  theme_bw()

#modelo2 -> no renta alargar los paraetros aleatorios -----

set.seed(123)
model_2 <- train(
  sp ~ .,
  tuneLength = 10,
  data = datos, 
  method = "ranger",
  trControl = trainControl(
    method = "repeatedcv", 
    number = particiones,
    repeats = repeticiones,
    verboseIter = F
  )
)
model_2


ggplot(model_2, highlight = TRUE) +
  labs(title = "Evoluci�n del accuracy del modelo en funci�n de 'mtry'") +
  theme_bw()

#modelo3 = ajustando hiperparametros -----

set.seed(123)
#hiperparametros, particiones y repeticiones
particiones  <- 5
repeticiones <- 2
hiperparametros <- expand.grid(mtry = c(6, 7, 8, 9),
                               min.node.size = 1,
                               splitrule = "gini")

control_train = trainControl(
  method = "repeatedcv", 
  number = particiones,
  repeats = repeticiones,
  verboseIter = F
)

model_3 <- train(
  sp ~ .,
  tuneGrid = hiperparametros,
  data = datos, 
  method = "ranger",
  trControl = control_train
)
model_3


ggplot(model_3, highlight = TRUE) +
  labs(title = "Evoluci�n del accuracy del modelo en funci�n de 'mtry'") +
  theme_bw()


