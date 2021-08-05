#1.leer los datos
library(caret)
library(tidyverse)
setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
datos <- read_delim(file = "wide_data1000.csv", delim = ";")
glimpse(datos)
str(datos)
datos <- datos %>% select(-c(ref, set))

#modelo 1
particiones = 5
repeticiones = 5
hiperparametros <- expand.grid(sigma = c(0.0001, 0.001, 0.01),
                               C = c(1, 2))

set.seed(123)
svm_model = train(sp ~ ., 
                  data = datos,
                  method = "svmRadial",
                  tuneGrid = hiperparametros,
                  trControl = trainControl(
                    method = "repeatedcv", 
                    number = particiones, 
                    repeats = repeticiones,
                    verbose = TRUE
                  )
)
svm_model
set.seed(123)

ggplot(svm_model, type = "b",highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo SVM Radial", ylab = "accuracy") +
  theme_bw()

#modelo 2





