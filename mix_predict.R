#modelo en datos mixtos
#cargar datos mixtos----
datos <- read.table(file = "datos/ts_fm_intersected_wide.csv", header = T, sep = ",", dec = ".")
datos %>% purrr::map_dbl(.f = ~ sum(is.na))
datos.sinna <- datos %>% select(-X98500)
names(datos)
datos <- datos.sinna %>% 
  filter(sp %in% c("mix")) %>%
  select(-c(ref, time, z.038, z.070, z.120, z.200))
#predict mixed data---- 
#using models created in the previous script
#calculate probability of class, by model, for set----
#separate set----
lance60 <- datos %>% filter(set == 60) %>% select(-set)
lance69 <- datos %>% filter(set == 69) %>% select(-set)

#predict by model----
pre_svm60 = predict(svm_model, newdata = lance60, type = "prob")
pre_gbm60 = predict(gbm_model, newdata = lance60, type = "prob")
pre_rf60 = predict(rf_model, newdata = lance60, type = "prob")
pre_svm69 = predict(svm_model, newdata = lance69, type = "prob")
pre_gbm69 = predict(gbm_model, newdata = lance69, type = "prob")
pre_rf69 = predict(rf_model, newdata = lance69, type = "prob")

#percent of species by each set and model----
skj_svm60 = nrow(filter(pre_svm60, skj > 0.8)) / nrow(pre_svm60) * 100
yft_svm60 = nrow(filter(pre_svm60, skj < 0.2)) / nrow(pre_svm60) * 100
other_svm60 = nrow(filter(pre_svm60, skj > 0.2, skj < 0.8)) / nrow(pre_svm60) * 100

skj_gbm60 = nrow(filter(pre_gbm60, skj > 0.8)) / nrow(pre_gbm60) * 100
yft_gbm60 = nrow(filter(pre_gbm60, skj < 0.2)) / nrow(pre_gbm60) * 100
other_gbm60 = nrow(filter(pre_gbm60, skj > 0.2, skj < 0.8)) / nrow(pre_gbm60) * 100

skj_rf60 = nrow(filter(pre_rf60, skj > 0.8)) / nrow(pre_rf60) * 100
yft_rf60 = nrow(filter(pre_rf60, skj < 0.2)) / nrow(pre_rf60) * 100
other_rf60 = nrow(filter(pre_rf60, skj > 0.2, skj < 0.8)) / nrow(pre_rf60) * 100

skj_svm69 = nrow(filter(pre_svm69, skj > 0.8)) / nrow(pre_svm69) * 100
yft_svm69 = nrow(filter(pre_svm69, skj < 0.2)) / nrow(pre_svm69) * 100
other_svm69 = nrow(filter(pre_svm69, skj > 0.2, skj < 0.8)) / nrow(pre_svm69) * 100

skj_gbm69 = nrow(filter(pre_gbm69, skj > 0.8)) / nrow(pre_gbm69) * 100
yft_gbm69 = nrow(filter(pre_gbm69, skj < 0.2)) / nrow(pre_gbm69) * 100
other_gbm69 = nrow(filter(pre_gbm69, skj > 0.2, skj < 0.8)) / nrow(pre_gbm69) * 100

skj_rf69 = nrow(filter(pre_rf69, skj > 0.8)) / nrow(pre_rf69) * 100
yft_rf69 = nrow(filter(pre_rf69, skj < 0.2)) / nrow(pre_rf69) * 100
other_rf69 = nrow(filter(pre_rf69, skj > 0.2, skj < 0.8)) / nrow(pre_rf69) * 100

#all prediction----
resultados = data.frame(skj_svm60, yft_svm60, other_svm60, skj_gbm60, yft_gbm60, other_gbm60, skj_rf60, yft_rf60, other_rf60,
           skj_svm69, yft_svm69, other_svm69, skj_gbm69, yft_gbm69, other_gbm69, skj_rf69, yft_rf69, other_rf69)


accuracy_allmodels %>%
  ggplot(aes(x = row.names()))+
  geom_point()+
  facet_wrap(~ accuracy_allmodels$modelo)

predict(gbm_model, newdata = datos)
predict(rf_model, newdata = datos)

#compare mix bio data
mix_data = read.table("datos\\mix_bio_data_tidy.csv", header = TRUE, sep = ",")

str(mix_data)

str(mix_data)
if(mix_data$set == "60") {
  skj60 = sum(mix_data$skj)/nrow()
}
  group_by(set)

group_by(mix_data, set)
#tengo que conseguir el porcentaje de cada especie en los datos originales


