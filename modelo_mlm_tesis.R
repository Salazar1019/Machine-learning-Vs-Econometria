#Llamado de paquetes############################################################
install.packages("stargazer")
library("readxl")

library("tidyverse")
library("ggplot2")
library("lmtest")
library("olsrr")

library("glmnet")
library("randomForest")
library("class")
library("stargazer")

#Importacion de datos###########################################################

datos <- read_excel("C:/Users/Home/Desktop/OneDrive - PUJ Cali/Tesis/datos_apartamentos_2019.xlsx")


datos$zona <- factor(datos$zona)
datos$estrato <- factor(datos$estrato)
datos$aream2sq <- datos$aream2**2

#View(datos)
#Análisis exploratorio##########################################################

colores <- c("norte" = "#E41A1C", "sur" = "#377EB8", "este" = "#4DAF4A", 
             "oeste" = "#984EA3", "centro" = "black")

ggplot(datos, aes(x = aream2, y = preciomillon, color = zona)) +
  geom_point() +
  scale_color_manual(values = colores) +
  labs(x = "Área (m²)", y = "Precio (millones)") +
  ggtitle("Relación entre Precio y Área por Zona")

ggplot(datos, aes(x = parqueaderos, y = preciomillon, color = zona)) +
  geom_point() +
  scale_color_manual(values = colores) +
  labs(x = "Parqueaderos", y = "Precio (millones)") +
  ggtitle("Relación entre Precio y Parqueaderos por Zona")

ggplot(datos, aes(x = banos, y = preciomillon, color = zona)) +
  geom_point() +
  scale_color_manual(values = colores) +
  labs(x = "Baños", y = "Precio (millones)") +
  ggtitle("Relación entre Precio y Baños por Zona")

ggplot(datos, aes(x = habitaciones, y = preciomillon, color = zona)) +
  geom_point() +
  scale_color_manual(values = colores) +
  labs(x = "Habitaciones", y = "Precio (millones)") +
  ggtitle("Relación entre Precio y Habitaciones por Zona")

ggplot(datos, aes(x = estrato, y = preciomillon, color = zona)) +
  geom_point() +
  scale_color_manual(values = colores) +
  labs(x = "Estrato", y = "Precio (millones)") +
  ggtitle("Relación entre Precio y Estrato por Zona")

ggplot(datos, aes(x = zona, y = aream2 , color = zona)) +
  geom_point() +
  scale_color_manual(values = colores) +
  labs(x = "Zona", y = "Area en m^2") +
  ggtitle("Relación entre zona y metros ")

mean(datos$preciomillon)
var(datos$preciomillon)

summary(datos)

#Modelo lineal múltiple#########################################################

modelo_mlm <- lm(formula = preciomillon ~ aream2 + aream2sq + zona + 
                   habitaciones + banos + parqueaderos + estrato + 
                   aream2 * estrato + aream2sq * estrato, data = datos)
ordenar <- summary(modelo_mlm)

stargazer(modelo_mlm, type = "latex")
step(modelo_mlm, direction = "both", trace = 1)

bptest(modelo_mlm)
resettest(modelo_mlm)
plot(modelo_mlm)

set.seed(123)
submuestra1 <- datos[sample(nrow(datos), 4000), ]
submuestra2 <- datos[-sample(nrow(datos), 1074), ]

#Modelo lineal múltiple para pronóstico#########################################

modelo_mlm2 <- lm(formula = preciomillon ~ aream2 + aream2sq + zona + 
                   habitaciones + banos + parqueaderos + estrato + 
                   aream2 * estrato + aream2sq * estrato , data = submuestra1)
summary(modelo_mlm2)

ols_plot_resid_fit(modelo_mlm2)
ols_plot_resid_hist(modelo_mlm2)
ols_test_normality(modelo_mlm2)

X_prueba <- data.frame(submuestra2[, c("aream2", "aream2sq", "zona", 
                                       "habitaciones", "banos", "parqueaderos", 
                                       "estrato")])
y_prueba <- submuestra2$preciomillon

predicciones <- predict(modelo_mlm2, newdata = X_prueba)

MSE_MLM <- mean((y_prueba - predicciones)^2)
MSE_MLM

#Modelo LASSO###################################################################

X <- model.matrix(preciomillon ~ aream2 + aream2sq + zona + habitaciones + 
                    banos + parqueaderos + estrato + aream2 * estrato + 
                    aream2sq * estrato, data = datos)[,-1]
Y <- datos$preciomillon

cv_model <- cv.glmnet(X, Y, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model)

lasso_model <- glmnet(X, Y, alpha = 1, lambda = best_lambda)
lasso_model

coef(lasso_model)
plot(lasso_model)

lasso_pred <- predict(lasso_model, newx=X)
mean(lasso_pred)
MSE_LASSO <- mean((Y - lasso_pred)^2)
MSE_LASSO

#Modelo de Bosque Aleatorio###################################################

n <- nrow(datos)
train_idx <- sample(1:n, size = floor(0.7 * n), replace = FALSE)
train <- datos[train_idx, ]
test <- datos[-train_idx, ]

rf_model <- randomForest(preciomillon ~ aream2 + aream2sq + zona +
                           habitaciones + banos + parqueaderos + estrato +
                           aream2 * estrato + aream2sq * estrato, data = train)
rf_model
importance(rf_model)

rf_pred <- predict(rf_model, newdata = test)

MSE_RF <- mean((test$preciomillon - rf_pred)^2)
MSE_RF

#k-NN para regresión############################################################

train_idx <- sample(1:nrow(datos),(nrow(datos)*0.7), replace = FALSE)
train <- datos[train_idx,]
test <- datos[-train_idx,]

vars_knn <- c("aream2", "habitaciones", "banos", "parqueaderos", "estrato", "zona")

knn_model <- knn(train[, vars_knn], test[, vars_knn], train[, "preciomillon"], 
                 k = 8)

MSE_knn <- mean((test$preciomillon - knn_model)^2)






