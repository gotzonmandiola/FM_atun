#Gradient Boosting Machine

#1.leer los datos
library(tidyverse)
library(caret)
setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
datos <- read_delim(file = "wide_data1000.csv", delim = ";")
glimpse(datos)
str(datos)
datos <- datos %>% select(-c(ref, set))
#particion de datos (no hace falta hacer partición de datos, porque ya me lo hace cuando hago crossvalidation)
set.seed(123)
train <- createDataPartition(y = datos$sp, p = 0.8, list  = FALSE, times = 1)
datos_train <- datos[train, ]
datos_test  <- datos[-train, ]

#modelo de entrenamiento 
particiones = 5
repeticiones = 5

hiperparametros_gbm_4 = expand.grid(interaction.depth = c(2),
                                    n.trees = c(70),
                                    shrinkage = 0.1,
                                    n.minobsinnode = 10)

set.seed(123)
gbm_model_4 = train(sp ~ .,
                    data = datos_train,
                    method = "gbm",
                    tuneGrid = hiperparametros_gbm_4,
                    trControl = trainControl(
                      method = "repeatedcv", 
                      number = particiones, 
                      repeats = repeticiones,
                      verbose = TRUE
                    )
)

gbm_model_4
str(gbm_model_4)
ggplot(gbm_model_4, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo Gradient Boosting") +
  guides(color = guide_legend(title = "shrinkage"),
         shape = guide_legend(title = "shrinkage")) +
  theme_bw() +
  theme(legend.position = "bottom")

#creacion de modelo de predicción
prediccion <- predict(gbm_model_4, 
                      newdata = datos_test, 
                      type = "prob")
prediccion 

confusionMatrix(data = prediccion, reference = datos_test$sp)
str(prediccion)




