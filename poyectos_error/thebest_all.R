modelos <- list(gbm = gbm_model, rf = rf_model, svm = svm_model)

resultados_resamples <- resamples(modelos) #con la función resamples extraemos, de uno o varios modelos creados con train(), 
#las métricas obtenidas para cada repetición del proceso de validación, es decir,
#los resultados accuracy de cada repeticion (Rep) y particion (fold) en la CV. 
#otra manera es poner returnResamp = "final" en el control de entrenamiento del modelo. (no lo he probado)
resultados_resamples$values %>% head(10)

#ordenar los resultados en un cuadro por modelo, metrica a tener en cuenta y valor
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()

#visualizar el promedio de todos los resultados de cada modelo.
short_results = metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))

short_results
  
metricas_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1))
