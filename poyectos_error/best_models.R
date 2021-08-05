#modelos creados despues de afinar modelos con CV, usando todos los datos.

#1.leer los datos
library(tidyverse)
library(caret)
setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
datos <- read_delim(file = "wide_data1000.csv", delim = ";")
datos <- datos %>% select(-c(ref, set))


#modelo GBM (Gradient Boosting Machine) ----
particiones = 5
repeticiones = 5

hiperparametros_gbm_4 = expand.grid(interaction.depth = c(2),
                                    n.trees = c(70),
                                    shrinkage = 0.1,
                                    n.minobsinnode = 10)

set.seed(123)
gbm_model_4 = train(sp ~ .,
                    data = datos,
                    method = "gbm",
                    tuneGrid = hiperparametros_gbm_4,
                    trControl = trainControl(
                      method = "repeatedcv", 
                      number = particiones, 
                      repeats = repeticiones,
                      verbose = TRUE
                    )
)

gbm_model_4

#modelo RF (Random Forest)----
set.seed(123)
particiones  <- 5
repeticiones <- 5
hiperparametros <- expand.grid(mtry = c(9),
                               min.node.size = 1,
                               splitrule = "gini")

control_train = trainControl(
  method = "repeatedcv", 
  number = particiones,
  repeats = repeticiones,
  verboseIter = T
)

model_3 <- train(
  sp ~ .,
  tuneGrid = hiperparametros,
  data = datos, 
  method = "ranger",
  trControl = control_train
)
model_3

#model SVM (Support Vector Machine)----
set.seed(123)
particiones = 5
repeticiones = 5
hiperparametros <- expand.grid(sigma = c(0.05),
                               C = c(4))

svm_model = train(sp ~ ., 
                  data = datos,
                  method = "svmRadial",
                  tuneGrid = hiperparametros,
                  trControl = trainControl(
                    method = "repeatedcv", 
                    number = particiones, 
                    repeats = repeticiones,
                    classProbs = TRUE,
                    verbose = TRUE
                  )
)
svm_model

predicciones_raw <- predict(svm_model, newdata = datos,
                            type = "prob")
predicciones_raw
#comparación de modelos
modelos <- list(gbm = gbm_model_4, rf = model_3, svm = svm_model)

resultados_resamples <- resamples(modelos) #con la función resamples extraemos, de uno o varios modelos creados con train(), 
                                           #las métricas obtenidas para cada repetición del proceso de validación, es decir,
                                           #los resultados accuracy de cada repeticion (Rep) y particion (fold) en la CV. 
                                           #otra manera es poner returnResamp = "final" en el control de entrenamiento del modelo. (no lo he probado)
resultados_resamples$values %>% head(10)

#ordenar los resultados en un cuadro por modelo, metrica a tener en cuenta y valor
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()

#visualizar el promedio de todos los resultados de cada modelo.
metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))

metricas_resamples %>% 
  split(f = metricas_resamples$metrica) %>%
  split()

t.test_1 = t.test(x = accuracy_gbm$gbm.valor, y = accuracy_rf$rf.valor)
t.test_2 = t.test(x = accuracy_gbm$gbm.valor, y = accuracy_svm$svm.valor)
t.test_3 = t.test(x = accuracy_svm$svm.valor, y = accuracy_rf$rf.valor)