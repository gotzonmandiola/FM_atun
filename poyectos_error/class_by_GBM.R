#Gradient_boosting_machine -----------
#1.leer los datos
library(tidyverse)
library(caret)
setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
datos <- read_delim(file = "wide_data1000.csv", delim = ";")
glimpse(datos)
str(datos)
datos <- datos %>% select(-c(ref, set))


#modelo_1

particiones = 5
repeticiones = 5

set.seed(123)
gbm_model = train(sp ~ ., 
                   data = datos,
                   method = "gbm",
                   tuneLength = 5,
                   trControl = trainControl(
                     method = "repeatedcv", 
                     number = particiones, 
                     repeats = repeticiones,
                     verbose = FALSE
                   )
)
gbm_model

ggplot(gbm_model, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo Gradient Boosting") +
  guides(color = guide_legend(title = "shrinkage"),
         shape = guide_legend(title = "shrinkage")) +
  theme_bw() +
  theme(legend.position = "bottom")

#modelo_2

hiperparametros_gbm = expand.grid(interaction.depth = c(1, 2, 3),
                                  n.trees = c(80, 100, 120),
                                  shrinkage = 0.1,
                                  n.minobsinnode = 10)

set.seed(123)
gbm_model_2 = train(sp ~ .,
                    data = datos,
                    method = "gbm",
                    tuneGrid = hiperparametros_gbm,
                    trControl = trainControl(
                      method = "repeatedcv", 
                      number = particiones, 
                      repeats = repeticiones,
                      verbose = FALSE
                    )
)

gbm_model_2

ggplot(gbm_model_2, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo Gradient Boosting") +
  guides(color = guide_legend(title = "shrinkage"),
         shape = guide_legend(title = "shrinkage")) +
  theme_bw() +
  theme(legend.position = "bottom")


#modelo_3

hiperparametros_gbm_3 = expand.grid(interaction.depth = c(1, 2, 3),
                                  n.trees = c(60, 80, 100),
                                  shrinkage = 0.1,
                                  n.minobsinnode = 10)

set.seed(123)
gbm_model_3 = train(sp ~ .,
                    data = datos,
                    method = "gbm",
                    tuneGrid = hiperparametros_gbm_3,
                    trControl = trainControl(
                      method = "repeatedcv", 
                      number = particiones, 
                      repeats = repeticiones,
                      verbose = FALSE
                    )
)

gbm_model_3

ggplot(gbm_model_3, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo Gradient Boosting") +
  guides(color = guide_legend(title = "shrinkage"),
         shape = guide_legend(title = "shrinkage")) +
  theme_bw() +
  theme(legend.position = "bottom")

#modelo_4

hiperparametros_gbm_4 = expand.grid(interaction.depth = c(1, 2, 3),
                                    n.trees = c(65, 70, 80),
                                    shrinkage = 0.1,
                                    n.minobsinnode = 10)

set.seed(123)
gbm_model_4 = train(sp ~ .,
                    data = datos,
                    method = "gbm",
                    tuneGrid = hiperparametros_gbm_4,
                    trControl = trainControl(
                      method = "repeatedcv", 
                      number = particiones, 
                      repeats = repeticiones,
                      verbose = FALSE
                    )
)

gbm_model_4

ggplot(gbm_model_4, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo Gradient Boosting") +
  guides(color = guide_legend(title = "shrinkage"),
         shape = guide_legend(title = "shrinkage")) +
  theme_bw() +
  theme(legend.position = "bottom")

#modelo_5

hiperparametros_gbm_5 = expand.grid(interaction.depth = c(1, 2, 3),
                                     n.trees = c(67.5, 70, 70.5),
                                     shrinkage = 0.1,
                                     n.minobsinnode = 10)

set.seed(123)
gbm_model_5 = train(sp ~ .,
                    data = datos,
                    method = "gbm",
                    tuneGrid = hiperparametros_gbm_5,
                    trControl = trainControl(
                      method = "repeatedcv", 
                      number = particiones, 
                      repeats = repeticiones,
                      verbose = FALSE
                    )
)

gbm_model_5

ggplot(gbm_model_5, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo Gradient Boosting") +
  guides(color = guide_legend(title = "shrinkage"),
         shape = guide_legend(title = "shrinkage")) +
  theme_bw() +
  theme(legend.position = "bottom")


#el mejor modelo es el modelo_4

