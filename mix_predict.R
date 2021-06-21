#modelo en datos mixtos
#cargar datos mixtos----
datos <- read.table(file = "datos/ts_fm_intersected_wide.csv", header = T, sep = ",", dec = ".")
datos %>% purrr::map_dbl(.f = ~ sum(is.na))
datos.sinna <- datos %>% select(-X98500)
names(datos)
datos <- datos.sinna %>% 
  filter(sp %in% c("mix")) %>%
  select(-c(ref, set, time, z.038, z.070, z.120, z.200))
#predict mixed data---- 
#using models created in the previous script
#calculate probability of class
pre_svm = predict(svm_model, newdata = datos, type = "prob")
pre_svm %>%
  group_by(skj>0.5, yft>0.5) %>%
  

accuracy_allmodels %>%
  ggplot(aes(x = row.names()))+
  geom_point()+
  facet_wrap(~ accuracy_allmodels$modelo)

predict(gbm_model, newdata = datos)
predict(rf_model, newdata = datos)
