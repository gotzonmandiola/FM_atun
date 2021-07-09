#leer los datos------
library(tidyverse)
library(caret)
library(recipes)
library(mice)
library(VIM)
#setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
datos <- read.table(file = "datos/ts_fm_intersected_wide.csv", header = T, sep = ",", dec = ".")
datos <- datos %>% 
  filter(sp %in% c("skj", "yft")) %>%
  select(-c(ref, set, time, z.038, z.070, z.120, z.200))
#try data imputation as preprocessing 
datis = mice(datos, m =1, maxit = 50, method = "pmm", seed = 500) #generate new data
datis$imp$X98500 #to check all 5 sets of imputed values
tail(datis$imp$X98500)
completeData <- complete(datis) #add the generated new data back to original data
#generate missing data


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
                  data = completeData,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train
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
                 data = completeData,
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
                  data = completeData,
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
#separar accuracy por modelos
resultados_modelos = metricas_resamples %>% 
  split(f = metricas_resamples$metrica)
accuracy_allmodels = resultados_modelos$Accuracy #para tener un df y poder hacer la grafica
ac = accuracy_allmodels%>% #para separar los resultados por modelos y poder hacer el test estadistico
  split(f = accuracy_allmodels$modelo)

#visualizar resultados de accuracy
accuracy_allmodels %>%
  ggplot(aes(x = Resample, y = valor))+
  geom_point()+
  facet_wrap(~ accuracy_allmodels$modelo)

#accuracy por modelo------------
accuracy_gbm_imp = data.frame(ac["gbm"])
accuracy_rf_imp = data.frame(ac["rf"])
accuracy_svm_imp = data.frame(ac["svm"])

#test-estadistico para comparar modelos ------
t.test_1 = t.test(x = accuracy_gbm$gbm.valor, y = accuracy_rf$rf.valor)
t.test_2 = t.test(x = accuracy_gbm$gbm.valor, y = accuracy_svm$svm.valor)
t.test_3 = t.test(x = accuracy_svm$svm.valor, y = accuracy_rf$rf.valor)


fix.na.accuracy = data.frame(median(accuracy_gbm$gbm.valor), 
           median(accuracy_svm$svm.valor), 
           median(accuracy_rf$rf.valor))

comp = accuracy_with.na %>%
  rbind(fix.na.accuracy)

rownames(comp) = list("with.na", "fix.na")
colnames(comp) = c("gbm", "svm", "rf")
comp
