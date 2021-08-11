#para que funcione esto, primero ejecutar mix_predict.R
#uniendo los lances mixtos-----

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
       colour = "modelo y lance")

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
       colour = "modelo y lance")

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
       colour = "modelo y lance")

#separación de lances y calculo del error----
datos <- read_delim(file = "datos/wide_data1000.csv", delim = ";")
datos.lr <- read_delim(file = "datos/wide_data1000.csv", delim = ";")
datos <- datos %>% select(-c(ref, set))

lance60 <- datos %>% filter(set == 60) %>% select(-set)
lance69 <- datos %>% filter(set == 69) %>% select(-set)

#predict by model

pre_svm60 = predict(svm_model, newdata = lance60, type = "prob")
pre_gbm60 = predict(gbm_model, newdata = lance60, type = "prob")
pre_rf60 = predict(rf_model, newdata = lance60, type = "prob")
pre_svm69 = predict(svm_model, newdata = lance69, type = "prob")
pre_gbm69 = predict(gbm_model, newdata = lance69, type = "prob")
pre_rf69 = predict(rf_model, newdata = lance69, type = "prob")

#porcentajes reales de cada especie en lances mixtos
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

#porcentaje de predicciones en cada modelo y lance
#primero asignamos valores de umbral, es decir, que probabilidades queremos que se consideren una especie para clasificarlas como tal
umbral = c(0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.91, 0.95)
skj_svm60  = c()
skj_gbm60 = c()
skj_rf60 = c()
skj_svm69 = c()
skj_gbm69 = c()
skj_rf69 = c()
yft_svm60 = c()
yft_gbm60 = c()
yft_svm69 = c()
yft_gbm69 = c()
yft_rf69 = c()
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

#errores obtenidos con diferentes valores de umbral en cada lance
#60
skjmix60 = data.frame(abs(cbind(skj_svm60, skj_gbm60 , skj_rf60) - mix_60_skj) / mix_60_skj)
skjmix60 = cbind(umbral, skjmix60)
colnames(skjmix60) = c("umbral", "svm", "gbm", "rf")
yftmix60 = data.frame(abs(cbind(yft_svm60, yft_gbm60, yft_rf60) - mix_60_yft) / mix_60_yft)
yftmix60 = cbind(umbral, yftmix60)
colnames(yftmix60) = c("umbral", "svm", "gbm", "rf")
othermix60 = data.frame(abs(cbind(other_svm60, other_gbm60, other_rf60) - mix_60_other) / mix_60_other)
othermix60 = cbind(umbral, othermix60)
colnames(othermix) = c("umbral", "svm", "gbm", "rf")
#69
skjmix69 = data.frame(abs(cbind(skj_svm69, skj_gbm69 , skj_rf69) - mix_69_skj) / mix_69_skj)
skjmix69 = cbind(umbral, skjmix69)
colnames(skjmix69) = c("umbral", "svm", "gbm", "rf")
yftmix69 = data.frame(abs(cbind(yft_svm69, yft_gbm69, yft_rf69) - mix_69_yft) / mix_69_yft)
yftmix69 = cbind(umbral, yftmix69)
colnames(yftmix69) = c("umbral", "svm", "gbm", "rf")
othermix69 = data.frame(abs(cbind(other_svm69, other_gbm69, other_rf69) - mix_69_other) / mix_69_other)
othermix69 = cbind(umbral, othermix69)
colnames(othermix69) = c("umbral", "svm", "gbm", "rf")

#plotear errores por lances----
#60----
skjmix60 %>%
  pivot_longer(cols = 2:4)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, linetype = name))+
  geom_line() +
  ylim(0, 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="SKJ, set = 60",
       x="valor de umbral",
       y="Error",
       linetype = "model")

yftmix60 %>%
  pivot_longer(cols = 2:4)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, linetype = name))+
  geom_line()+
  ylim(0, 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="YFT, set = 60",
       x="valor de umbral",
       y="Error",
       linetype = "model")

othermix60 %>%
  pivot_longer(cols = 2:4)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, linetype = name))+
  geom_line()+
  ylim(0, 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="OTHER, set = 60",
       x="valor de umbral",
       y="Error",
       linetype = "model")

#69----
skjmix69 %>%
  pivot_longer(cols = 2:4)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, linetype = name))+
  geom_line() +
  ylim(0, 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="SKJ, set = 69",
       x="valor de umbral",
       y="Error",
       linetype = "model")

yftmix69 %>%
  pivot_longer(cols = 2:4)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, linetype = name))+
  geom_line()+
  ylim(0, 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="YFT, set = 69",
       x="valor de umbral",
       y="Error",
       linetype = "model")

othermix69 %>%
  pivot_longer(cols = 2:4)%>%
  arrange(name, umbral, value)%>%
  ggplot(aes(x = umbral, y = value, linetype = name))+
  geom_line()+
  ylim(0, 0.7) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), axis.line = element_line(colour="black"))+
  labs(title="OTHER, set = 69",
       x="valor de umbral",
       y="Error",
       linetype = "model")


prueba = skjmix69 %>%
  pivot_longer(cols = 2:4)%>%
  arrange(name, umbral, value)
