library(tidyverse)
library(caret)
library(MASS)
library(neuralnet)

datos <- read.csv("historic_sales.csv")
datos <- read.csv("historic_sales.csv", sep = ";", header = TRUE)

datos$customer_id <- NULL
set.seed(8)
datos_particionados <- createDataPartition(datos$volt, p=0.8, list=FALSE)
train_data <- datos[datos_particionados, ]
test_data <- datos[-datos_particionados, ]

logistic_regression <- glm(volt ~ ., data = train_data, family = binomial)
step_selection <- stepAIC(logistic_regression, trace = TRUE)

nn <- neuralnet(formula_nn, data = train_data, linear.output = FALSE)
plot(nn)


probabilities_nn <- predict(nn, test_data, type="response")

prediction_nn <- ifelse(probabilities_nn > 0.5,1,0)

test_data$volt==prediction_nn
accuracynn <- mean(prediction_nn==test_data$volt)*100