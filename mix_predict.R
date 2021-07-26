#modelo en datos mixtos
#cargar datos mixtos----
datos <- read.table(file = "datos/ts_fm_intersected_wide.csv", header = T, sep = ",", dec = ".")
datos %>% purrr::map_dbl(.f = ~ sum(is.na))
datos.sinna <- datos %>% select(-X98500)
names(datos)
datos <- datos.sinna %>% 
  filter(sp %in% c("mix")) %>%
  select(-c(ref, time, z.038, z.070, z.120, z.200))
#predict mixed data separating sets
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

#compare mix bio data, datos de lances mixtos tidy----
mix_data = read.table("datos\\mix_bio_data_tidy.csv", header = TRUE, sep = ",")
#porcentajes reales de cada especie en lances mixtos----
#60
mix_60_skj = mix_data %>%
  filter(set == 60) %>%
  filter(sp == "skj") %>%
  nrow() / nrow(filter(mix_data, set == 60)) * 100
mix_60_yft = mix_data %>%
  filter(set == 60) %>%
  filter(sp == "yft") %>%
  nrow() / nrow(filter(mix_data, set == 60)) * 100
mix_60_other = mix_data %>%
  filter(set == 60) %>%
  filter(sp == "other") %>%
  nrow() / nrow(filter(mix_data, set == 60)) * 100
#69
mix_69_skj = mix_data %>%
  filter(set == 69) %>%
  filter(sp == "skj") %>%
  nrow() / nrow(filter(mix_data, set == 69)) * 100
mix_69_yft = mix_data %>%
  filter(set == 69) %>%
  filter(sp == "yft") %>%
  nrow() / nrow(filter(mix_data, set == 69)) * 100
mix_69_other = mix_data %>%
  filter(set == 69) %>%
  filter(sp == "other") %>%
  nrow() / nrow(filter(mix_data, set == 69)) * 100
cbind(mix_60_skj, mix_60_yft, mix_60_other, mix_69_skj, mix_69_yft, mix_69_other)
#porcentaje de predicciones en cada modelo y lance----
#primero asignamos valores de umbral, es decir, que probabilidades queremos que se consideren una especie para clasificarlas como tal
umbral = c(0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.91, 0.95)
for (i in 1:length(umbral)) {
  skj_svm60[i] = nrow(filter(pre_svm60, skj > umbral[i])) / nrow(pre_svm60) * 100
  skj_gbm60[i] = nrow(filter(pre_gbm60, skj > umbral[i])) / nrow(pre_gbm60) * 100
  skj_rf60[i] = nrow(filter(pre_rf60, skj > umbral[i])) / nrow(pre_rf60) * 100
  
  skj_svm69[i] = nrow(filter(pre_svm69, skj > umbral[i])) / nrow(pre_svm69) * 100
  skj_gbm69[i] = nrow(filter(pre_gbm69, skj > umbral[i])) / nrow(pre_gbm69) * 100
  skj_rf69[i] = nrow(filter(pre_rf69, skj > umbral[i])) / nrow(pre_rf69) * 100
  
  yft_svm60[i] = nrow(filter(pre_svm60, yft > umbral[i])) / nrow(pre_svm60) * 100
  yft_gbm60[i] = nrow(filter(pre_gbm60, yft > umbral[i])) / nrow(pre_gbm60) * 100
  yft_rf60[i] = nrow(filter(pre_rf60, yft > umbral[i])) / nrow(pre_rf60) * 100
  
  yft_svm69[i] = nrow(filter(pre_svm69, yft > umbral[i])) / nrow(pre_svm69) * 100
  yft_gbm69[i] = nrow(filter(pre_gbm69, yft > umbral[i])) / nrow(pre_gbm69) * 100
  yft_rf69[i] = nrow(filter(pre_rf69, yft > umbral[i])) / nrow(pre_rf69) * 100
}

other_svm60 = 100-(skj_svm60 + yft_svm60)
other_gbm60 = 100-(skj_gbm60 + yft_gbm60)
other_rf60 = 100-(skj_rf60 + yft_rf60)

other_svm69 = 100-(skj_svm69 + yft_svm69)
other_gbm69 = 100-(skj_gbm69 + yft_gbm69)
other_rf69 = 100-(skj_rf69 + yft_rf69)

#tenemos los porcentajes obtenidos para cada especie dependiendo del valor de probabilidad asignado para cada uno. 
#Es decir, para umbral "x", tenemos los porcentajes de cada especie dependiendo de modelo y lance.
data.frame(cbind(umbral, skj_svm60, skj_gbm60 , skj_rf60, skj_svm69, skj_gbm69, skj_rf69, 
      yft_svm60, yft_gbm60, yft_rf60, yft_svm69, yft_gbm69, yft_rf69,
      other_svm60, other_gbm60, other_rf60, other_svm69, other_gbm69, other_rf69))

#aciertos de porcentaje de especie en cada lance dependiendo de modelo.----
#acierto = (%skjobtenido - %skjreal) / %skjreal
(skj_svm60 - mix_60_skj)/mix_60_skj

skjmix = data.frame(abs(cbind(skj_svm60, skj_gbm60 , skj_rf60, skj_svm69, skj_gbm69, skj_rf69) - mix_60_skj) / mix_60_skj)
skjmix = cbind(umbral, skjmix)
colnames(skjmix) = c("umbral", "svm 60", "gbm 60", "rf 60", "svm 69", "gbm 69", "rf 69")
yftmix = data.frame(abs(cbind(yft_svm60, yft_gbm60, yft_rf60, yft_svm69, yft_gbm69, yft_rf69) - mix_60_yft) / mix_60_yft)
yftmix = cbind(umbral, yftmix)
colnames(yftmix) = c("umbral", "svm 60", "gbm 60", "rf 60", "svm 69", "gbm 69", "rf 69")
othermix = data.frame(abs(cbind(other_svm60, other_gbm60, other_rf60, other_svm69, other_gbm69, other_rf69) - mix_60_other) / mix_60_other)
othermix = cbind(umbral, othermix)
colnames(othermix) = c("umbral", "svm 60", "gbm 60", "rf 60", "svm 69", "gbm 69", "rf 69")

#hacer graficos con estos valores de acierto para ver que valor es el optimo para clasificar especies. el que nos da = 0.
#Cuando haga el grafico elejir valro optimo.
str(skjmix)
aciertos = cbind(umbral, skjmix, yftmix, othermix)
str(aciertos)
aciertos %>%
  pivot_longer(cols = 2:19)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, colour = name))+
  geom_line()
#umbral optimo por especie----

skjmix %>%
  pivot_longer(cols = 2:7)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, colour = name))+
  geom_line() +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="SKJ",
       x="valor de umbral",
       y="Acierto",
       caption="Fuente: ",
       colour = "modelo y lance")

yftmix %>%
  pivot_longer(cols = 2:7)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, colour = name))+
  geom_line()+
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="YFT",
       x="valor de umbral",
       y="Acierto",
       caption="Fuente: ",
       colour = "modelo y lance")

othermix %>%
  pivot_longer(cols = 2:7)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, colour = name))+
  geom_line()+
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="OTHER",
       x="valor de umbral",
       y="Acierto",
       caption="Fuente: ",
       colour = "modelo y lance")
