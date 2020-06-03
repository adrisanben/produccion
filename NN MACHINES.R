library(tidyverse)
library(caret)
library(MASS)
library(neuralnet)

datos <- read.csv("PROJECT_MillingMachine.csv")
datos <- read.csv("PROJECT_MillingMachine.csv", sep = ";", header = TRUE)

datos$machine_id = NULL

set.seed(11)

datos_particionados <- createDataPartition(datos$machine_failure, p=0.8, list=FALSE)
train_data <- datos[datos_particionados, ]
test_data <- datos[-datos_particionados, ]

logistic_regression <- glm(machine_failure ~ ., data = train_data, family = binomial)
step_selection <- stepAIC(logistic_regression, trace = TRUE)

formula_nn <- machine_failure~ abnormal_smell + abnormal_sound + improper_maintenance + oil_leak + innadequate_lubrication + corroded_parts + vibration_increase

nn <- neuralnet(formula_nn, data = train_data, linear.output = FALSE)
plot(nn)


probabilities_nn <- predict(nn, test_data, type="response")

prediction_nn <- ifelse(probabilities_nn > 0.5,1,0)

test_data$machine_failure==prediction_nn
accuracynn <- mean(prediction_nn==test_data$machine_failure)*100