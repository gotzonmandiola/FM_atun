library(tidyverse)
library(caret)
#setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
datos <- read.table(file = "datos/ts_fm_intersected_wide.csv", header = T, sep = ",", dec = ".")
datos %>% purrr::map_dbl(.f = ~ sum(is.na))
datos.sinna <- datos %>% select(-X98500)
names(datos)
datos <- datos.sinna %>% 
  filter(sp %in% c("skj", "yft")) %>%
  select(-c(ref, time, z.038, z.070, z.120, z.200))
#separar datos por lance ------
#y así poder entrenar y validar sin mezclar lances

grupos = split(datos, f = datos$set)
a = data.frame(grupos[["12"]]) %>% select(-set)
b = data.frame(grupos[["23"]]) %>% select(-set)
c = data.frame(grupos[["46"]]) %>% select(-set)
d = data.frame(grupos[["48"]]) %>% select(-set)
e = data.frame(grupos[["49"]]) %>% select(-set)
f = data.frame(grupos[["52"]]) %>% select(-set)
g = data.frame(grupos[["54"]]) %>% select(-set)

#grupos para entrenar y testear ------

train1 = rbind(c, d, e, a)
test1 = rbind(f, g, b)
train2 = rbind(d, e, f, b)
test2 = rbind(c, g, a)  
train3 = rbind(e, f, g, a)  
test3 = rbind(c, d, b)  
train4 = rbind(c, e, g, b)  
test4 = rbind(d, f, a)  
train5 = rbind(c, e, f, a)  
test5 = rbind(d, g, b)  
train6 = rbind(d, f, g, b)  
test6 = rbind(c, e, a)  
train7 = rbind(d, e, g, a)  
test7 = rbind(c, f, b)  
train8 = rbind(c, f, g, b)
test8 = rbind(d, e, a)
train9 = rbind(c, d, f, a)
test9 = rbind(e, g, b)
train10 = rbind(c, d, g, b)
test10 = rbind(e, f, a)

#hacer lista con datos train y datos test, y luego crear modelos con bucle

train_data = list(train1, train2, train3, train4, train5, train6, train7, train8, train9, train10)
test_data = list(test1, test2, test3, test4, test5, test6, test7, test8, test9, test10)

#hiperparametros -----
hiperparametros_svm <- expand.grid(sigma = 0.01,
                                   C = 4)
hiperparametros_gbm = expand.grid(interaction.depth = 2,
                                  n.trees = 50,
                                  shrinkage = 0.1,
                                  n.minobsinnode = 10)
hiperparametros_rf <- expand.grid(mtry = 7,
                                  min.node.size = 1,
                                  splitrule = "gini")

#modo de entrenamiento de datos
control_train = trainControl(
  method = "none",
  classProbs = TRUE, #SVM no calcula las probabilidades si no le decimos
  verbose = TRUE
)

#modelo para train1, hacer uno a uno o loop para todos los datos
#modelos Support Vector Machine ------
#1-----
svm_model = train(sp ~ .,
                  data = train1,
                  method = "svmRadial",
                  tuneGrid = hiperparametros_svm,
                  trControl = control_train,
                  returnSample = TRUE)
pred1 = predict(svm_model, newdata = test1)
pred.metric1 = postResample(test1$sp, pred1)
#2-----
svm_model = train(sp ~ .,
                  data = train2,
                  method = "svmRadial",
                  tuneGrid = hiperparametros_svm,
                  trControl = control_train,
                  returnSample = TRUE)
pred2 = predict(svm_model, newdata = test2)
pred.metric2 = postResample(test2$sp, pred2)
#3-----
svm_model = train(sp ~ .,
                  data = train3,
                  method = "svmRadial",
                  tuneGrid = hiperparametros_svm,
                  trControl = control_train,
                  returnSample = TRUE)
pred3 = predict(svm_model, newdata = test3)
pred.metric3 = postResample(test3$sp, pred3)
#4-----
svm_model = train(sp ~ .,
                  data = train4,
                  method = "svmRadial",
                  tuneGrid = hiperparametros_svm,
                  trControl = control_train,
                  returnSample = TRUE)
pred4 = predict(svm_model, newdata = test4)
pred.metric4 = postResample(test4$sp, pred4)
#5-----
svm_model = train(sp ~ .,
                  data = train5,
                  method = "svmRadial",
                  tuneGrid = hiperparametros_svm,
                  trControl = control_train,
                  returnSample = TRUE)
pred5 = predict(svm_model, newdata = test5)
pred.metric5 = postResample(test5$sp, pred5)
#6-----
svm_model = train(sp ~ .,
                  data = train6,
                  method = "svmRadial",
                  tuneGrid = hiperparametros_svm,
                  trControl = control_train,
                  returnSample = TRUE)
pred6 = predict(svm_model, newdata = test6)
pred.metric6 = postResample(test6$sp, pred6)
#7-----
svm_model = train(sp ~ .,
                  data = train7,
                  method = "svmRadial",
                  tuneGrid = hiperparametros_svm,
                  trControl = control_train,
                  returnSample = TRUE)
pred7 = predict(svm_model, newdata = test7)
pred.metric7 = postResample(test7$sp, pred7)
#8-----
svm_model = train(sp ~ .,
                  data = train8,
                  method = "svmRadial",
                  tuneGrid = hiperparametros_svm,
                  trControl = control_train,
                  returnSample = TRUE)
pred8 = predict(svm_model, newdata = test8)
pred.metric8 = postResample(test8$sp, pred8)
#9-----
svm_model = train(sp ~ .,
                  data = train9,
                  method = "svmRadial",
                  tuneGrid = hiperparametros_svm,
                  trControl = control_train,
                  returnSample = TRUE)
pred9 = predict(svm_model, newdata = test9)
pred.metric9 = postResample(test9$sp, pred9)
#10-----
svm_model = train(sp ~ .,
                  data = train10,
                  method = "svmRadial",
                  tuneGrid = hiperparametros_svm,
                  trControl = control_train,
                  returnSample = TRUE)
pred10 = predict(svm_model, newdata = test10)
pred.metric10 = postResample(test10$sp, pred10)
#results_svm-----
results_svm_all_sep = rbind(pred.metric1, pred.metric2, pred.metric3, pred.metric4, pred.metric5, pred.metric6, pred.metric7,
                    pred.metric8, pred.metric9, pred.metric10)


#modelos Gradient Boosting Machine ------
#1-----
gbm_model = train(sp ~ .,
                  data = train1,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train)
pred1_2 = predict(gbm_model, newdata = test1)
pred.metric1_2 = postResample(test1$sp, pred1_2)
#2-----
gbm_model = train(sp ~ .,
                  data = train2,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train)
pred2_2 = predict(gbm_model, newdata = test2)
pred.metric2_2 = postResample(test2$sp, pred2_2)
#3-----
gbm_model = train(sp ~ .,
                  data = train3,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train)
pred3_2 = predict(gbm_model, newdata = test3)
pred.metric3_2 = postResample(test3$sp, pred3_2)
#4-----
gbm_model = train(sp ~ .,
                  data = train4,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train)
pred4_2 = predict(gbm_model, newdata = test4)
pred.metric4_2 = postResample(test4$sp, pred4_2)
#5-----
gbm_model = train(sp ~ .,
                  data = train5,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train)
pred5_2 = predict(gbm_model, newdata = test5)
pred.metric5_2 = postResample(test5$sp, pred5_2)
#6-----
gbm_model = train(sp ~ .,
                  data = train6,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train)
pred6_2 = predict(gbm_model, newdata = test6)
pred.metric6_2 = postResample(test6$sp, pred6_2)
#7-----
gbm_model = train(sp ~ .,
                  data = train7,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train)
pred7_2 = predict(gbm_model, newdata = test7)
pred.metric7_2 = postResample(test7$sp, pred7_2)
#8-----
gbm_model = train(sp ~ .,
                  data = train8,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train)
pred8_2 = predict(gbm_model, newdata = test8)
pred.metric8_2 = postResample(test8$sp, pred8_2)
#9-----
gbm_model = train(sp ~ .,
                  data = train9,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train)
pred9_2 = predict(gbm_model, newdata = test9)
pred.metric9_2 = postResample(test9$sp, pred9_2)
#10-----
gbm_model = train(sp ~ .,
                  data = train10,
                  method = "gbm",
                  tuneGrid = hiperparametros_gbm,
                  trControl = control_train)
pred10_2 = predict(gbm_model, newdata = test10)
pred.metric10_2 = postResample(test10$sp, pred10_2)
#results_gbm-----
results_gbm_all_sep = rbind(pred.metric1_2, pred.metric2_2, pred.metric3_2, pred.metric4_2, pred.metric5_2, 
                    pred.metric6_2, pred.metric7_2, pred.metric8_2, pred.metric9_2, pred.metric10_2)

#modelos Random Forest-------
#1-----
rf_model = train(sp ~ .,
                 data = train1,
                 method = "ranger",
                 tuneGrid = hiperparametros_rf,
                 trControl = control_train)
pred1_3 = predict(rf_model, newdata = test1)
pred.metric1_3 = postResample(test1$sp, pred1_3)
#2-----
rf_model = train(sp ~ .,
                 data = train2,
                 method = "ranger",
                 tuneGrid = hiperparametros_rf,
                 trControl = control_train)
pred2_3 = predict(rf_model, newdata = test2)
pred.metric2_3 = postResample(test2$sp, pred2_3)
#3-----
rf_model = train(sp ~ .,
                 data = train3,
                 method = "ranger",
                 tuneGrid = hiperparametros_rf,
                 trControl = control_train)
pred3_3 = predict(rf_model, newdata = test3)
pred.metric3_3 = postResample(test3$sp, pred3_3)
#4-----
rf_model = train(sp ~ .,
                 data = train4,
                 method = "ranger",
                 tuneGrid = hiperparametros_rf,
                 trControl = control_train)
pred4_3 = predict(rf_model, newdata = test4)
pred.metric4_3 = postResample(test4$sp, pred4_3)
#5-----
rf_model = train(sp ~ .,
                 data = train5,
                 method = "ranger",
                 tuneGrid = hiperparametros_rf,
                 trControl = control_train)
pred5_3 = predict(rf_model, newdata = test5)
pred.metric5_3 = postResample(test5$sp, pred5_3)
#6-----
rf_model = train(sp ~ .,
                 data = train6,
                 method = "ranger",
                 tuneGrid = hiperparametros_rf,
                 trControl = control_train)
pred6_3 = predict(rf_model, newdata = test6)
pred.metric6_3 = postResample(test6$sp, pred6_3)
#7-----
rf_model = train(sp ~ .,
                 data = train7,
                 method = "ranger",
                 tuneGrid = hiperparametros_rf,
                 trControl = control_train)
pred7_3 = predict(rf_model, newdata = test7)
pred.metric7_3 = postResample(test7$sp, pred7_3)
#8-----
rf_model = train(sp ~ .,
                 data = train8,
                 method = "ranger",
                 tuneGrid = hiperparametros_rf,
                 trControl = control_train)
pred8_3 = predict(rf_model, newdata = test8)
pred.metric8_3 = postResample(test8$sp, pred8_3)
#9-----
rf_model = train(sp ~ .,
                 data = train9,
                 method = "ranger",
                 tuneGrid = hiperparametros_rf,
                 trControl = control_train)
pred9_3 = predict(rf_model, newdata = test9)
pred.metric9_3 = postResample(test9$sp, pred9_3)
#10-----
rf_model = train(sp ~ .,
                 data = train10,
                 method = "ranger",
                 tuneGrid = hiperparametros_rf,
                 trControl = control_train)
pred10_3 = predict(rf_model, newdata = test10)
pred.metric10_3 = postResample(test10$sp, pred10_3)
#results_rf-----
results_rf_all_sep = rbind(pred.metric1_3, pred.metric2_3, pred.metric3_3, pred.metric4_3, pred.metric5_3, 
                   pred.metric6_3, pred.metric7_3, pred.metric8_3, pred.metric9_3, pred.metric10_3)
#all_results-----
rbind(results_svm_all_sep, results_gbm_all_sep, results_rf_all_sep)

