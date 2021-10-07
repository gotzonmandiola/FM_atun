#leer los datos------
library(tidyverse)
library(caret)
#setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
datos <- read.table(file = "datos/ts_fm_intersected_wide.csv", header = T, sep = ",", dec = ".")
datos %>% purrr::map_dbl(.f = ~ sum(is.na))
datos.sinna <- datos %>% select(-X98500)
names(datos)
datos <- datos.sinna %>% 
  filter(sp %in% c("skj", "yft")) %>%
  select(-c(ref, set, time, z.038, z.070, z.120, z.200))

any(is.na(datos)) #hay valores ausentes, no funcionan los modelos.
sum(is.na(datos)) #hay 406 resultados con Na

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
gbm_model_all = train(sp ~ ., 
                  data = datos,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train
)
gbm_model_all
gbm_model_all$resample #para viisualizar resultados de todas las particiones y repeticiones

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
rf_model_all = train(sp ~ ., 
                 data = datos,
                 method = "ranger",
                 tuneGrid = hiperparametros_rf,
                 trControl = control_train,
)
rf_model_all
rf_model_all$resample #para visualizar resultados de todas las particiones y repeticiones

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
svm_model_all = train(sp ~ ., 
                  data = datos,
                  method = "svmRadial",
                  tuneGrid = hiperparametros_svm,
                  trControl = control_train,
                  returnSample = TRUE,
)
svm_model_all
svm_model_all$resample #para viisualizar resultados de todas las particiones y repeticiones


#Resultados----
#crear data.frame con los resultados de los modelos
modelos_all <- list(gbm_all = gbm_model_all, rf_all = rf_model_all, svm_all = svm_model_all)
resultados_resamples_all <- resamples(modelos_all)
resultados_resamples_all$values %>% head(10)
metricas_resamples_all <- resultados_resamples_all$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples_all %>% head()
#separar accuracy y kappa por modelos
resultados_modelos_all = metricas_resamples_all %>% 
  split(f = metricas_resamples_all$metrica)
accuracy_allmodels_all = resultados_modelos_all$Accuracy #para tener un df y poder hacer la grafica
ac_all = accuracy_allmodels_all%>% #para separar los resultados por modelos y poder hacer el test estadistico
  split(f = accuracy_allmodels_all$modelo)
kappa_allmodels_all = resultados_modelos_all$Kappa #para tener un df y poder hacer la grafica
kp_all = kappa_allmodels_all%>% #para separar los resultados por modelos y poder hacer el test estadistico
  split(f = kappa_allmodels_all$modelo)

#visualizar resultados de accuracy
accuracy_allmodels_all %>%
  ggplot(aes(x = Resample, y = valor))+
  geom_point()+
  facet_wrap(~ accuracy_allmodels$modelo)

#accuracy por modelo------------
accuracy_gbm_all = data.frame(ac_all["gbm_all"])
accuracy_rf_all = data.frame(ac_all["rf_all"])
accuracy_svm_all = data.frame(ac_all["svm_all"])
kappa_gbm_all = data.frame(kp_all["gbm_all"])
kappa_rf_all = data.frame(kp_all["rf_all"])
kappa_svm_all = data.frame(kp_all["svm_all"])
#test-estadistico para comparar modelos ------
t.test_1 = t.test(x = accuracy_gbm_all$gbm.valor, y = accuracy_rf_all$rf.valor)
t.test_2 = t.test(x = accuracy_gbm_all$gbm.valor, y = accuracy_svm_all$svm.valor)
t.test_3 = t.test(x = accuracy_svm_all$svm.valor, y = accuracy_rf_all$rf.valor)

accuracy_with.na = data.frame(median(accuracy_gbm_all$gbm.valor), 
                             median(accuracy_svm_all$svm.valor), 
                             median(accuracy_rf_all$rf.valor))

