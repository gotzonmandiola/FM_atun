#modelo en datos mixtos
#cargar datos mixtos----
#setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
datos <- read_delim(file = "datos/wide_data1000.csv", delim = ";")
datos.lr <- read_delim(file = "datos/wide_data1000.csv", delim = ";")
datos <- datos %>% select(-c(ref, set))
#si queremos hacerlo con todos los datos
# datos <- read.table(file = "datos/ts_fm_intersected_wide.csv", header = T, sep = ",", dec = ".")
# datos %>% purrr::map_dbl(.f = ~ sum(is.na))
# datos.sinna <- datos %>% select(-X98500)
# names(datos)
# datos <- datos.sinna %>% 
#   filter(sp %in% c("mix")) %>%
#   select(-c(ref, time, z.038, z.070, z.120, z.200))
#predict mixed data separating sets
#using models created in the previous script
#calculate probability of class, by model, for set----
#separate set----
# lance60 <- datos %>% filter(set == 60) %>% select(-set)
# lance69 <- datos %>% filter(set == 69) %>% select(-set)

#predict by model----

# pre_svm60 = predict(svm_model, newdata = lance60, type = "prob")
# pre_gbm60 = predict(gbm_model, newdata = lance60, type = "prob")
# pre_rf60 = predict(rf_model, newdata = lance60, type = "prob")
# pre_svm69 = predict(svm_model, newdata = lance69, type = "prob")
# pre_gbm69 = predict(gbm_model, newdata = lance69, type = "prob")
# pre_rf69 = predict(rf_model, newdata = lance69, type = "prob")

pre_svm_mix = predict(svm_model, newdata = datos, type = "prob") #aqui lo hacemos con todos los datos
pre_gbm_mix = predict(gbm_model, newdata = datos, type = "prob") #aqui lo hacemos con todos los datos
pre_rf_mix = predict(rf_model, newdata = datos, type = "prob") #aqui lo hacemos con todos los datos

#compare mix bio data, datos de lances mixtos tidy----
mix_data = read.table("datos\\mix_bio_data_tidy.csv", header = TRUE, sep = ",", dec = ".")
#porcentajes reales de cada especie en lances mixtos----
#60
# mix_60_skj = mix_data %>%
#   filter(set == 60) %>%
#   filter(sp == "skj") %>%
#   nrow() / nrow(filter(mix_data, set == 60)) * 100
# mix_60_yft = mix_data %>%
#   filter(set == 60) %>%
#   filter(sp == "yft") %>%
#   nrow() / nrow(filter(mix_data, set == 60)) * 100
# mix_60_other = mix_data %>%
#   filter(set == 60) %>%
#   filter(sp == "other") %>%
#   nrow() / nrow(filter(mix_data, set == 60)) * 100
#69
# mix_69_skj = mix_data %>%
#   filter(set == 69) %>%
#   filter(sp == "skj") %>%
#   nrow() / nrow(filter(mix_data, set == 69)) * 100
# mix_69_yft = mix_data %>%
#   filter(set == 69) %>%
#   filter(sp == "yft") %>%
#   nrow() / nrow(filter(mix_data, set == 69)) * 100
# mix_69_other = mix_data %>%
#   filter(set == 69) %>%
#   filter(sp == "other") %>%
#   nrow() / nrow(filter(mix_data, set == 69)) * 100
# cbind(mix_60_skj, mix_60_yft, mix_60_other, mix_69_skj, mix_69_yft, mix_69_other)

#*********************************
#MIX_sets
#*********************************

mix_skj = mean(c(mix_data %>%
      filter(set == 60) %>%
      filter(sp == "skj") %>%
      nrow() / nrow(filter(mix_data, set == 60)) * 100,
  mix_data %>%
    filter(set == 69) %>%
    filter(sp == "skj") %>%
    nrow() / nrow(filter(mix_data, set == 69)) * 100))

mix_yft = mean(c(mix_data %>%
                   filter(set == 60) %>%
                   filter(sp == "yft") %>%
                   nrow() / nrow(filter(mix_data, set == 60)) * 100,
                 mix_data %>%
                   filter(set == 69) %>%
                   filter(sp == "yft") %>%
                   nrow() / nrow(filter(mix_data, set == 69)) * 100))

mix_other = mean(c(mix_data %>%
                   filter(set == 60) %>%
                   filter(sp == "other") %>%
                   nrow() / nrow(filter(mix_data, set == 60)) * 100,
                 mix_data %>%
                   filter(set == 69) %>%
                   filter(sp == "other") %>%
                   nrow() / nrow(filter(mix_data, set == 69)) * 100))

#porcentaje de predicciones en cada modelo y lance----
#primero asignamos valores de umbral, es decir, que probabilidades queremos que se consideren una especie para clasificarlas como tal
umbral = c(0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.91, 0.95, 0.96, 0.97, 0.98, 0.99, 1)
skj_svm = c()
skj_gbm = c()
skj_rf = c()
yft_svm = c()
yft_gbm = c()
yft_rf = c()
for (i in 1:length(umbral)) {
   #skj_svm60[i] = nrow(filter(pre_svm60, skj > umbral[i])) / nrow(pre_svm60) * 100
   #skj_gbm60[i] = nrow(filter(pre_gbm60, skj > umbral[i])) / nrow(pre_gbm60) * 100
   #skj_rf60[i] = nrow(filter(pre_rf60, skj > umbral[i])) / nrow(pre_rf60) * 100
   
   #skj_svm69[i] = nrow(filter(pre_svm69, skj > umbral[i])) / nrow(pre_svm69) * 100
   #skj_gbm69[i] = nrow(filter(pre_gbm69, skj > umbral[i])) / nrow(pre_gbm69) * 100
   #skj_rf69[i] = nrow(filter(pre_rf69, skj > umbral[i])) / nrow(pre_rf69) * 100
   
   #yft_svm60[i] = nrow(filter(pre_svm60, yft > umbral[i])) / nrow(pre_svm60) * 100
   #yft_gbm60[i] = nrow(filter(pre_gbm60, yft > umbral[i])) / nrow(pre_gbm60) * 100
   #yft_rf60[i] = nrow(filter(pre_rf60, yft > umbral[i])) / nrow(pre_rf60) * 100
   
   #yft_svm69[i] = nrow(filter(pre_svm69, yft > umbral[i])) / nrow(pre_svm69) * 100
   #yft_gbm69[i] = nrow(filter(pre_gbm69, yft > umbral[i])) / nrow(pre_gbm69) * 100
   #yft_rf69[i] = nrow(filter(pre_rf69, yft > umbral[i])) / nrow(pre_rf69) * 100
  
  skj_svm[i] = nrow(filter(pre_svm_mix, skj > umbral[i])) / nrow(pre_svm_mix) * 100
  skj_gbm[i] = nrow(filter(pre_gbm_mix, skj > umbral[i])) / nrow(pre_gbm_mix) * 100
  skj_rf[i] = nrow(filter(pre_rf_mix, skj > umbral[i])) / nrow(pre_rf_mix) * 100
  
  yft_svm[i] = nrow(filter(pre_svm_mix, yft > umbral[i])) / nrow(pre_svm_mix) * 100
  yft_gbm[i] = nrow(filter(pre_gbm_mix, yft > umbral[i])) / nrow(pre_gbm_mix) * 100
  yft_rf[i] = nrow(filter(pre_rf_mix, yft > umbral[i])) / nrow(pre_rf_mix) * 100
}

 #other_svm60 = 100-(skj_svm60 + yft_svm60)
 #other_gbm60 = 100-(skj_gbm60 + yft_gbm60)
 #other_rf60 = 100-(skj_rf60 + yft_rf60)
 
 #other_svm69 = 100-(skj_svm69 + yft_svm69)
 #other_gbm69 = 100-(skj_gbm69 + yft_gbm69)
 #other_rf69 = 100-(skj_rf69 + yft_rf69)

other_svm = 100-(skj_svm + yft_svm)
other_gbm = 100-(skj_gbm + yft_gbm)
other_rf = 100-(skj_rf + yft_rf)

#tenemos los porcentajes obtenidos para cada especie dependiendo del valor de probabilidad asignado para cada uno. 
#Es decir, para umbral "x", tenemos los porcentajes de cada especie dependiendo de modelo y lance.
# data.frame(cbind(umbral, skj_svm60, skj_gbm60 , skj_rf60, skj_svm69, skj_gbm69, skj_rf69, 
#       yft_svm60, yft_gbm60, yft_rf60, yft_svm69, yft_gbm69, yft_rf69,
#       other_svm60, other_gbm60, other_rf60, other_svm69, other_gbm69, other_rf69))
data.frame(cbind(umbral, skj_svm, skj_gbm, skj_rf, yft_svm, yft_gbm, yft_rf, other_svm, other_gbm, other_rf))

#error de porcentaje de especie en cada lance dependiendo de modelo.----
#error = (%skjobtenido - %skjreal) / %skjreal
(skj_svm - mix_skj)/mix_skj

# skjmix = data.frame(abs(cbind(skj_svm60, skj_gbm60 , skj_rf60, skj_svm69, skj_gbm69, skj_rf69) - mix_60_skj) / mix_60_skj)
# skjmix = cbind(umbral, skjmix)
# colnames(skjmix) = c("umbral", "svm 60", "gbm 60", "rf 60", "svm 69", "gbm 69", "rf 69")
# yftmix = data.frame(abs(cbind(yft_svm60, yft_gbm60, yft_rf60, yft_svm69, yft_gbm69, yft_rf69) - mix_60_yft) / mix_60_yft)
# yftmix = cbind(umbral, yftmix)
# colnames(yftmix) = c("umbral", "svm 60", "gbm 60", "rf 60", "svm 69", "gbm 69", "rf 69")
# othermix = data.frame(abs(cbind(other_svm60, other_gbm60, other_rf60, other_svm69, other_gbm69, other_rf69) - mix_60_other) / mix_60_other)
# othermix = cbind(umbral, othermix)
# colnames(othermix) = c("umbral", "svm 60", "gbm 60", "rf 60", "svm 69", "gbm 69", "rf 69")

skjmix = data.frame(abs(cbind(skj_svm, skj_gbm, skj_rf) - mix_skj)) / mix_skj
skjmix = cbind(umbral, skjmix)
colnames(skjmix) = c("umbral", "svm", "gbm", "rf")

yftmix = data.frame(abs(cbind(yft_svm, yft_gbm, yft_rf) - mix_yft)) / mix_yft
yftmix = cbind(umbral, yftmix)
colnames(yftmix) = c("umbral", "svm", "gbm", "rf")

othermix = data.frame(abs(cbind(other_svm, other_gbm, other_rf) - mix_other)) / mix_other
othermix = cbind(umbral, othermix)
colnames(othermix) = c("umbral", "svm", "gbm", "rf")

#promedio del error minimo por valor de umbral

skjmix = skjmix %>%
  group_by(umbral) %>%
  mutate(skjmedia = mean(c(svm, gbm, rf))) 
min(skjmix[,5])
yftmix = yftmix %>%
  group_by(umbral) %>%
  mutate(yftmedia = mean(c(svm, gbm, rf)))
othermix = othermix %>%
  group_by(umbral) %>%
  mutate(othermedia = mean(c(svm, gbm, rf)))

mediaerror = cbind(umbral, skjmix[, 5], yftmix[, 5], othermix[, 5]) %>%
  group_by(umbral) %>%
  mutate(media = mean(c(skjmedia, yftmedia, othermedia)))
minerror = min(mediaerror[,5])
#hacer graficos con estos valores de acierto para ver que valor es el optimo para clasificar especies. el que nos da = 0.
#Cuando haga el grafico elejir valro optimo.
str(skjmix)
errores = cbind(umbral, skjmix, yftmix, othermix)
str(aciertos)
errores %>%
  pivot_longer(cols = 2:10)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, colour = name))+
  geom_line()
#umbral optimo por especie----

skjmix %>%
  pivot_longer(cols = 2:4)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, linetype = name))+
  geom_line() +
  ylim(0, 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="SKJ",
       x="valor de umbral",
       y="Error",
       linetype = "model")

yftmix %>%
  pivot_longer(cols = 2:4)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, linetype = name))+
  geom_line()+
  ylim(0, 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="YFT",
       x="valor de umbral",
       y="Error",
       linetype = "model")

othermix %>%
  pivot_longer(cols = 2:4)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, linetype = name))+
  geom_line()+
  ylim(0, 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="OTHER",
       x="valor de umbral",
       y="Error",
       linetype = "model")


