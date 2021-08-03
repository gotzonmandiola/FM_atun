#leer los datos------
library(tidyverse)
library(caret)
#setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
datos <- read_delim(file = "datos/wide_data1000.csv", delim = ";")
datos.lr <- read_delim(file = "datos/wide_data1000.csv", delim = ";")
datos <- datos %>% select(-c(ref, set))

#1.GBM model, Gradient Boosting Machine ===============

#1.2. creamos modelo con datos de entrenamiento================

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN

particiones = 5
repeticiones = 5


#asignamos semillas para cada particion y repeticion con bucle, si no seria imposible afinar.

set.seed(123)

#hiperparametros
hiperparametros_gbm = expand.grid(interaction.depth = 2,
                                  n.trees = 50,
                                  shrinkage = 0.1,
                                  n.minobsinnode = 10)

#Entrenamiento
control_train = trainControl(
  method = "repeatedcv", 
  number = particiones, 
  repeats = repeticiones,
  classProbs = TRUE, 
  verbose = TRUE
)

#Ajuste de modelo
gbm_model = train(sp ~ ., 
                  data = datos,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train,
)
gbm_model
gbm_model$resample #para viisualizar resultados de todas las particiones y repeticiones

#2.RF model, Random Forest ===============

#2.2. creamos modelo con datos de entrenamiento================

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN

#hiperparametros
set.seed(123)
hiperparametros_rf <- expand.grid(mtry = 7,
                               min.node.size = 1,
                               splitrule = "gini")

#Entrenamiento
control_train = trainControl(
  method = "repeatedcv", 
  number = particiones, 
  repeats = repeticiones,
  classProbs = TRUE, 
  verbose = TRUE
)

#Ajuste de modelo
rf_model = train(sp ~ ., 
                 data = datos,
                 method = "ranger",
                 tuneGrid = hiperparametros_rf,
                 trControl = control_train,
)
rf_model
rf_model$resample #para visualizar resultados de todas las particiones y repeticiones

#3.SVM model, Support Vector Machine===============

#3.2. creamos modelo con datos de entrenamiento================

#hiperparametros
set.seed(123)
hiperparametros_svm <- expand.grid(sigma = 0.01,
                               C = 4)

#Entrenamiento
control_train = trainControl(
  method = "repeatedcv", 
  number = particiones, 
  repeats = repeticiones,
  classProbs = TRUE, #SVM no calcula las probabilidades si no le decimos
  verbose = TRUE
)

#Ajuste de modelo
svm_model = train(sp ~ ., 
                  data = datos,
                  method = "svmRadial",
                  tuneGrid = hiperparametros_svm,
                  trControl = control_train,
                  returnSample = TRUE,
)
svm_model
svm_model$resample #para viisualizar resultados de todas las particiones y repeticiones

# Check significative differences between hauls ----------------------------
tallas <- read_delim(file = "datos/datos_bio_tidy.csv", delim = ",")
glimpse(tallas)

tallas.l <- pivot_longer(data = tallas, cols = skj:Others, names_to = "sp", values_to = "tallas")

# tarea: ver si hay diferencies significativas entre pescas

lance12 <- tallas.l%>% filter(Lance == 12) %>% select(tallas) %>%  filter(is.na(tallas) == F) %>% pull
lance23 <- tallas.l%>% filter(Lance == 23) %>% select(tallas) %>%  filter(is.na(tallas) == F) %>% pull

t.test(lance12, lance23)

#Resultados----
#crear data.frame con los resultados de los modelos
modelos <- list(gbm = gbm_model, rf = rf_model, svm = svm_model)
resultados_resamples <- resamples(modelos)
resultados_resamples$values %>% head(10)
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()
write.table(x = metricas_resamples, file = "models_results.csv", sep = ";", col.names = T, row.names = F)
read.delim(file = "resultados/models_results.csv", header = T, sep = ";")

#separar accuracy por modelos
resultados_modelos = metricas_resamples %>% 
  split(f = metricas_resamples$metrica)
accuracy_allmodels = resultados_modelos$Accuracy #para tener un df y poder hacer la grafica
kappa_allmodels = resultados_modelos$Kappa
ac = accuracy_allmodels%>% #para separar los resultados por modelos y poder hacer el test estadistico
  split(f = accuracy_allmodels$modelo)

kp = kappa_allmodels%>% #para separar los resultados por modelos y poder hacer el test estadistico
  split(f = kappa_allmodels$modelo)

#visualizar resultados de accuracy
accuracy_allmodels %>%
  ggplot(aes(x = Resample, y = valor))+
  geom_point()+
  facet_wrap(~ accuracy_allmodels$modelo)

#accuracy y kappa por modelo------------
accuracy_gbm_10 = data.frame(ac["gbm"])
accuracy_rf_10 = data.frame(ac["rf"])
accuracy_svm_10 = data.frame(ac["svm"])

kappa_gbm_10 = data.frame(kp["gbm"])
kappa_rf_10 = data.frame(kp["rf"])
kappa_svm_10 = data.frame(kp["svm"])

#test-estadistico para comparar modelos ------
t.test_1 = t.test(x = accuracy_gbm_10$gbm.valor, y = accuracy_rf_10$rf.valor)
t.test_2 = t.test(x = accuracy_gbm_10$gbm.valor, y = accuracy_svm_10$svm.valor)
t.test_3 = t.test(x = accuracy_svm_10$svm.valor, y = accuracy_rf_10$rf.valor)



