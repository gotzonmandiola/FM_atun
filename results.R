library(tidyverse)
library(caret)
#**********************************************
#primero vamos a ver los resultados de los modelos creados a principio
#Valores de todas las repeticiones y particiones del modelo creado con cv y reduced data----
#plotear accuracy y kappa del primer modelo (CV y reduced)
g <- metricas_resamples %>%
  ggplot(aes(x = modelo, y = valor, fill = metrica)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="Precisión y Kappa",
       x="Método",
       y="Valor") +
  scale_fill_manual(labels=c("Precisión", "Kappa"),values=c("gray46", "ghostwhite"))
g
#un test estadistico de los modelos
t.test_1 = t.test(x = accuracy_gbm_10$gbm.valor, y = accuracy_rf_10$rf.valor)
t.test_2 = t.test(x = accuracy_gbm_10$gbm.valor, y = accuracy_svm_10$svm.valor)
t.test_3 = t.test(x = accuracy_svm_10$svm.valor, y = accuracy_rf_10$rf.valor)

#ACCURACY----
#**********************************************
#crearemos un dataframe con todos los resultados----
#1. datos reducidos y lances separados
acc_svm_10_sep = cbind(rep("svm", each = 10), as.numeric(results_svm10_sep[, 1]), rep("reduced", each = 10), rep("separated", each = 10))
acc_gbm_10_sep = cbind(rep("gbm", each = 10), results_gbm_10_sep[, 1], rep("reduced", each = 10), rep("separated", each = 10))
acc_rf_10_sep = cbind(rep("rf", each = 10), results_rf10_sep[, 1], rep("reduced", each = 10), rep("separated", each = 10))
#2. datos completos y lances separados
acc_svm_all_sep = cbind(rep("svm", each = 10), results_svm_all_sep[, 1], rep("complete", each = 10), rep("separated", each = 10))
acc_gbm_all_sep = cbind(rep("gbm", each = 10), results_gbm_all_sep[, 1], rep("complete", each = 10), rep("separated", each = 10))
acc_rf_all_sep = cbind(rep("rf", each = 10), results_rf_all_sep[, 1], rep("complete", each = 10), rep("separated", each = 10))
#3. datos reducidos y lances unidos
acc_svm_10_all = cbind(rep("svm", each = 25), accuracy_svm_10[, 4], rep("reduced", each = 25), rep("united", each = 25))
acc_gbm_10_all = cbind(rep("gbm", each = 25), accuracy_gbm_10[, 4], rep("reduced", each = 25), rep("united", each = 25))
acc_rf_10_all = cbind(rep("rf", each = 25), accuracy_rf_10[, 4], rep("reduced", each = 25), rep("united", each = 25))
#4. datos completos (eliminando NA) y lances unidos
acc_svm_all = cbind(rep("svm", each = 25), accuracy_svm_all[, 4], rep("complete", each = 25), rep("united_na_rm", each = 25))
acc_gbm_all = cbind(rep("gbm", each = 25), accuracy_gbm_all[, 4], rep("complete", each = 25), rep("united_na_rm", each = 25))
acc_rf_all = cbind(rep("rf", each = 25), accuracy_rf_all[, 4], rep("complete", each = 25), rep("united_na_rm", each = 25))
#5. datos completos (importando NA) y lances unidos
acc_svm_all_imp = cbind(rep("svm", each = 25), accuracy_svm_imp[, 4], rep("complete", each = 25), rep("united_na_imp", each = 25))
acc_gbm_all_imp = cbind(rep("gbm", each = 25), accuracy_gbm_imp[, 4], rep("complete", each = 25), rep("united_na_imp", each = 25))
acc_rf_all_imp = cbind(rep("rf", each = 25), accuracy_rf_imp[, 4], rep("complete", each = 25), rep("united_na_imp", each = 25))

#dataframe con todos los resultados----
#("modelo", "accuracy", "tipo de dato", "gestion de lances")
#dataframe de lances separados por modelo y tipo de dato usado----
separados = data.frame(cbind(acc_svm_10_sep, acc_gbm_10_sep, acc_rf_10_sep, acc_svm_all_sep, acc_gbm_all_sep, acc_rf_all_sep))
colnames(separados) = c("svm_reduced", "gbm_reduced", "rf_reduced", "svm_alldata", "gbm_alldata", "rf_alldata")

#data frame de lances unidos por modelo y tipo de dato----
unidos = data.frame(cbind(acc_svm_10_all, acc_gbm_10_all, acc_rf_10_all, acc_svm_all, acc_gbm_all, acc_rf_all, acc_svm_all_imp, acc_gbm_all_imp, acc_rf_all_imp))
colnames(unidos) = c("svm_reduced", "gbm_reduced", "rf_reduced", "svm_nadeleted", "gbm_nadeleted", "rf_nadeleted", 
                     "svm_naimputed", "gbm_naimputed", "rf_naimputed")  
#resultados formato tidy----
resultados_tidy = data.frame(rbind(acc_svm_10_sep, acc_gbm_10_sep, acc_rf_10_sep, acc_svm_all_sep, acc_gbm_all_sep, acc_rf_all_sep,
                        acc_svm_10_all, acc_gbm_10_all, acc_rf_10_all, acc_svm_all, acc_gbm_all, acc_rf_all, acc_svm_all_imp,
                        acc_gbm_all_imp, acc_rf_all_imp)) 
colnames(resultados_tidy) = c("model", "accuracy", "data", "set")
#graficos de resultados----
#datos reducidos, accuracy de modelos comparando lances unidos y separados
#******************************
g1 <- resultados_tidy %>%
  filter(data == "reduced") %>%
  ggplot(aes(x = model, y = as.numeric(accuracy), fill = set)) +
  geom_boxplot(alpha = 0.7) +
  ylim(0.9, 0.98) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="Datos reducidos",
       x="Método",
       y="Precisión",
       fill = "Lances") +
  scale_fill_manual(labels=c("Separados", "Unidos"), values=c("gray46", "ghostwhite"))
g1 

#datos completos, accuracy de modelos comparando lances unidos (eliminando NA y importando NA) y lances separados.
g2 <- resultados_tidy %>%
  filter(data == "complete", set == c("separated", "united_na_rm")) %>%
  ggplot(aes(x = model, y = as.numeric(accuracy), fill = set)) +
  geom_boxplot(alpha = 0.7) +
  ylim(0.9, 0.98) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="Datos completos",
       x="Método",
       y="Precisión",
       fill = "Lances")
g2 + 
  scale_fill_manual(labels=c("separados", "unidos"), values = c("gray46", "ghostwhite"))

#************************************
#datos unidos (eliminando NA), accuracy de modelos comparando datos reducidos y completos
g3 <- resultados_tidy %>%
  filter(set == c("united_na_rm", "united")) %>%
  ggplot(aes(x = model, y = as.numeric(accuracy), fill = data)) +
  geom_boxplot(alpha = 0.7) +
  ylim(0.9, 0.98) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="Lances unidos",
       x="Método",
       y="Precisión",
       fill = "datos") +
  scale_fill_manual(values=c("gray46", "ghostwhite"), labels = c("Completos", "Reducidos"))
g3 

#datos unidos (importando NA), accuracy de modelos comparando datos reducidos y completos
g4 <- resultados_tidy %>%
  filter(set == c("united_na_imp", "united")) %>%
  ggplot(aes(x = model, y = as.numeric(accuracy), fill = data)) +
  geom_boxplot(alpha = 0.7) +
  ylim(0.9, 0.98) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="United sets, importing NA values
       NA values",
       x="Method",
       y="Accuracy")
g4 

#datos separados, accuracy de modelos comparando datos reducidos y completos
g5 <- resultados_tidy %>%
  filter(set == c("separated")) %>%
  ggplot(aes(x = model, y = as.numeric(accuracy), fill = data)) +
  geom_boxplot(alpha = 0.7) +
  ylim(0.9, 0.98) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="Separated data, comparing reduced and completed data",
       x="Method",
       y="Accuracy")
g5 
#**********************************************
#datos unidos, completos, comparar valores ausentes (eliminados e imputados)

g6 <- resultados_tidy %>%
  filter(set == c("united_na_imp", "united_na_rm")) %>%
  ggplot(aes(x = model, y = as.numeric(accuracy), fill = set)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="Lances unidos y datos completos",
       x="Método",
       y="Precisión",
       fill = "Solucion aplicada") +
  scale_fill_manual(labels = c("Eliminación de valores NA", "Importación de valores NA"), values=c("gray46", "ghostwhite"))
  
g6 












  

#**********************************************
#KAPPA----
#**********************************************
#crearemos un dataframe con todos los resultados----
#1. datos reducidos y lances separados
kp_svm_10_sep = cbind(rep("svm", each = 10), results_svm10_sep[, 2], rep("reduced", each = 10), rep("separated", each = 10))
kp_gbm_10_sep = cbind(rep("gbm", each = 10), results_gbm_10_sep[, 2], rep("reduced", each = 10), rep("separated", each = 10))
kp_rf_10_sep = cbind(rep("rf", each = 10), results_rf10_sep[, 2], rep("reduced", each = 10), rep("separated", each = 10))
#2. datos completos y lances separados
kp_svm_all_sep = cbind(rep("svm", each = 10), results_svm_all_sep[, 2], rep("complete", each = 10), rep("separated", each = 10))
kp_gbm_all_sep = cbind(rep("gbm", each = 10), results_gbm_all_sep[, 2], rep("complete", each = 10), rep("separated", each = 10))
kp_rf_all_sep = cbind(rep("rf", each = 10), results_rf_all_sep[, 2], rep("complete", each = 10), rep("separated", each = 10))
#3. datos reducidos y lances unidos
kp_svm_10_all = cbind(rep("svm", each = 25), kappa_svm_10[, 4], rep("reduced", each = 25), rep("united", each = 25))
kp_gbm_10_all = cbind(rep("gbm", each = 25), kappa_gbm_10[, 4], rep("reduced", each = 25), rep("united", each = 25))
kp_rf_10_all = cbind(rep("rf", each = 25), kappa_rf_10[, 4], rep("reduced", each = 25), rep("united", each = 25))
#4. datos completos (eliminando NA) y lances unidos
kp_svm_all = cbind(rep("svm", each = 25), kappa_svm_all[, 4], rep("complete", each = 25), rep("united_na_rm", each = 25))
kp_gbm_all = cbind(rep("gbm", each = 25), kappa_gbm_all[, 4], rep("complete", each = 25), rep("united_na_rm", each = 25))
kp_rf_all = cbind(rep("rf", each = 25), kappa_rf_all[, 4], rep("complete", each = 25), rep("united_na_rm", each = 25))
#5. datos completos (importando NA) y lances unidos
kp_svm_all_imp = cbind(rep("svm", each = 25), kappa_svm_imp[, 4], rep("complete", each = 25), rep("united_na_imp", each = 25))
kp_gbm_all_imp = cbind(rep("gbm", each = 25), kappa_gbm_imp[, 4], rep("complete", each = 25), rep("united_na_imp", each = 25))
kp_rf_all_imp = cbind(rep("rf", each = 25), kappa_rf_imp[, 4], rep("complete", each = 25), rep("united_na_imp", each = 25))

#dataframe con todos los resultados----
#("modelo", "kappa", "tipo de dato", "gestion de lances")
#dataframe de lances separados por modelo y tipo de dato usado----
separados_kp = data.frame(cbind(kp_svm_10_sep, kp_gbm_10_sep, kp_rf_10_sep, kp_svm_all_sep, kp_gbm_all_sep, kp_rf_all_sep))
colnames(separados_kp) = c("svm_reduced", "gbm_reduced", "rf_reduced", "svm_alldata", "gbm_alldata", "rf_alldata")

#data frame de lances unidos por modelo y tipo de dato----
unidos = data.frame(cbind(kp_svm_10_all, kp_gbm_10_all, kp_rf_10_all, kp_svm_all, kp_gbm_all, kp_rf_all, kp_svm_all_imp, kp_gbm_all_imp, kp_rf_all_imp))
colnames(unidos) = c("svm_reduced", "gbm_reduced", "rf_reduced", "svm_nadeleted", "gbm_nadeleted", "rf_nadeleted", 
                     "svm_naimputed", "gbm_naimputed", "rf_naimputed")  
#resultados formato tidy----
resultados_kp_tidy = data.frame(rbind(kp_svm_10_sep, kp_gbm_10_sep, kp_rf_10_sep, kp_svm_all_sep, kp_svm_all_sep, kp_gbm_all_sep, kp_rf_all_sep,
                                      kp_svm_10_all, kp_gbm_10_all, kp_rf_10_all, kp_svm_all, kp_gbm_all, kp_rf_all, kp_svm_all_imp,
                                      kp_gbm_all_imp, kp_rf_all_imp)) 
colnames(resultados_kp_tidy) = c("model", "kappa", "data", "set")
#graficos de resultados----
#datos reducidos, kappa de modelos comparando lances unidos y separados
gkp1 <- resultados_kp_tidy %>%
  filter(data == "reduced") %>%
  ggplot(aes(x = model, y = as.numeric(kappa), fill = set)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="Datos reducidos",
       x="Método",
       y="Kappa",
       fill = "Lances") +
  scale_fill_manual(labels=c("Separados", "Unidos"), values=c("gray46", "ghostwhite"))
gkp1 

#datos completos, kappa de modelos comparando lances unidos (eliminando NA y importando NA) y lances separados.
gkp2 <- resultados_kp_tidy %>%
  filter(data == "complete", set == c("separated", "united_na_rm")) %>%
  ggplot(aes(x = model, y = as.numeric(kappa), fill = set)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="Datos completos",
       x="Método",
       y="Kappa",
       fill = "Lances")
gkp2 + 
  scale_fill_manual(labels=c("separados", "unidos"), values = c("gray46", "ghostwhite"))


#datos unidos (eliminando NA), kappa de modelos comparando datos reducidos y completos
gkp3 <- resultados_kp_tidy %>%
  filter(set == c("united_na_rm", "united")) %>%
  ggplot(aes(x = model, y = as.numeric(kappa), fill = data)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="Lances unidos",
       x="Método",
       y="Kappa",
       fill = "datos") +
  scale_fill_manual(values=c("gray46", "ghostwhite"), labels = c("Completos", "Reducidos"))
gkp3 

#datos unidos (importando NA), kappa de modelos comparando datos reducidos y completos
gkp4 <- resultados_kp_tidy %>%
  filter(set == c("united_na_imp", "united")) %>%
  ggplot(aes(x = model, y = as.numeric(kappa), fill = data)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="United data, importing NA values",
       x="Method",
       y="kappa")
gkp4 + scale_fill_manual(values=c("gray46", "ghostwhite"))

#datos separados, kappa de modelos comparando datos reducidos y completos
gkp5 <- resultados_kp_tidy %>%
  filter(set == c("separated")) %>%
  ggplot(aes(x = model, y = as.numeric(kappa), fill = data)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="Separated data, removing NA values",
       x="Method",
       y="kappa")
gkp5 + scale_fill_manual(values=c("gray46", "ghostwhite"))



gkp6 <- resultados_kp_tidy %>%
  filter(set == c("united_na_imp", "united_na_rm")) %>%
  ggplot(aes(x = model, y = as.numeric(kappa), fill = set)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="Lances unidos y datos completos",
       x="Método",
       y="Kappa",
       fill = "Solucion aplicada") +
  scale_fill_manual(labels = c("Eliminación de valores NA", "Importación de valores NA"), values=c("gray46", "ghostwhite"))

gkp6 


















