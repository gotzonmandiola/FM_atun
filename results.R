#plot results
#crearemos un dataframe con todos los resultados----
#1. datos reducidos y lances separados
acc_svm_10_sep = cbind(rep("svm", each = 10), results_svm10_sep[, 1], rep("reduced", each = 10), rep("separated", each = 10))
acc_gbm_10_sep = cbind(rep("gbm", each = 10), results_gbm_10_sep[, 1], rep("reduced", each = 10), rep("separated", each = 10))
acc_rf_10_sep = cbind(rep("rf", each = 10), results_rf10_sep[, 1], rep("reduced", each = 10), rep("separated", each = 10))
#2. datos completos y lances separados
acc_svm_all_sep = cbind(rep("svm", each = 10), results_svm_all_sep[, 1], rep("complete", each = 10), rep("separated", each = 10))
acc_gbm_all_sep = cbind(rep("gbm", each = 10), results_gbm_all_sep[, 1], rep("complete", each = 10), rep("separatde", each = 10))
acc_rf_all_sep = cbind(rep("rf", each = 10), results_rf_all_sep[, 1], rep("complete", each = 10), rep("separatde", each = 10))
#3. datos reducidos y lances unidos
acc_svm_10_all = cbind(rep("svm", each = 25), accuracy_svm_10[, 4], rep("reduced", each = 25), rep("united", each = 25))
acc_gbm_10_all = cbind(rep("gbm", each = 25), accuracy_gbm_10[, 4], rep("reduced", each = 25), rep("united", each = 25))
acc_rf_10_all = cbind(rep("rf", each = 25), accuracy_rf_10[, 4], rep("reduced", each = 25), rep("united", each = 25))
#4. datos completos (eliminando NA) y lances unidos
acc_svm_all = cbind(rep("svm", each = 25), accuracy_svm_all[, 4], rep("completed_na_rm", each = 25), rep("united", each = 25))
acc_gbm_all = cbind(rep("gbm", each = 25), accuracy_gbm_all[, 4], rep("completed_na_rm", each = 25), rep("united", each = 25))
acc_rf_all = cbind(rep("rf", each = 25), accuracy_rf_all[, 4], rep("completed_na_rm", each = 25), rep("united", each = 25))
#5. datos completos (importando NA) y lances unidos
acc_svm_all_imp = cbind(rep("svm", each = 25), accuracy_svm_imp[, 4], rep("completed_na_imp", each = 25), rep("united", each = 25))
acc_gbm_all_imp = cbind(rep("gbm", each = 25), accuracy_gbm_imp[, 4], rep("completed_na_imp", each = 25), rep("united", each = 25))
acc_rf_all_imp = cbind(rep("rf", each = 25), accuracy_rf_imp[, 4], rep("completed_na_imp", each = 25), rep("united", each = 25))

#dataframe con todos los resultados----
#("modelo", "accuracy", "tipo de dato", "gestion de lances")
resultados_tidy = rbind(acc_svm_10_sep, acc_gbm_10_sep, acc_rf_10_sep, acc_svm_all_sep, acc_svm_all_sep, acc_gbm_all_sep, acc_rf_all_sep,
      acc_svm_10_all, acc_gbm_10_all, acc_rf_10_all, acc_svm_all, acc_gbm_all, acc_rf_all_imp, acc_svm_all_imp,
      acc_gbm_all_imp, acc_rf_all_imp)
colnames(resultados_tidy) = c("model", "accuracy", "data", "set")


#dataframe de lances separados por modelo y tipo de dato usado----
separados = data.frame(cbind(acc_svm_10_sep, acc_gbm_10_sep, acc_rf_10_sep, acc_svm_all_sep, acc_gbm_all_sep, acc_rf_all_sep))
colnames(separados) = c("svm_reduced", "gbm_reduced", "rf_reduced", "svm_alldata", "gbm_alldata", "rf_alldata")

#data frame de lances unidos por modelo y tipo de dato----
unidos = data.frame(cbind(acc_svm_10_all, acc_gbm_10_all, acc_rf_10_all, acc_svm_all, acc_gbm_all, acc_rf_all, acc_svm_all_imp, acc_gbm_all_imp, acc_rf_all_imp))
colnames(unidos) = c("svm_reduced", "gbm_reduced", "rf_reduced", "svm_nadeleted", "gbm_nadeleted", "rf_nadeleted", 
                     "svm_naimputed", "gbm_naimputed", "rf_naimputed")  
#tidy results



  
  
























  