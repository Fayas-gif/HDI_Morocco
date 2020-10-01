
rm(list=ls()) #Clears the global environment
setwd("C:/Users/hp/OneDrive - Al Akhawayn University in Ifrane/QUANTITATIVE MTHDS") #setting the working directory
set.seed(1995) #set seed

#Loading the packages
library(tidyverse)
library(sf)
library(tmap)
library(spdep)
library(caret)

#Get rid of scientific notation
options(scipen = 999)

#Loading data
data_communes <- st_read("data_communes.shp")
cities <- st_read("Morocco_Cities.shp")

train <- as.data.frame(data_communes) %>%
  transmute("pptt" = LISA_p_04,
            "Four" = LISA_Fr_04,
            "Ten" = LISA_T_0,
            "Five" = LISA_Fv_04,
            "dvfm" = LISA_d_04,
            "cllp" = LISA_ll_04,
            "rntr" = LISA_r_04,
            "illt" = LISA_ll_04,
            "emp" = LISA_m_04,
            "elct" = LISA_lc_04,
            "DS" = DS,
            "Alg_Brd" = Alg_Brd,
            "HDI" = LISA_HDI_0)


test <- as.data.frame(data_communes) %>%
  transmute("pptt" = LISA_p_14,
            "Four" = LISA_Fr_14,
            "Ten" = LISA_T_1,
            "Five" = LISA_Fv_14,
            "dvfm" = LISA_d_14,
            "cllp" = LISA_ll_14,
            "rntr" = LISA_r_14,
            "illt" = LISA_ll_14,
            "emp" = LISA_m_14,
            "elct" = LISA_lc_14,
            "DS" = DS,
            "Alg_Brd" = Alg_Brd,
            "hdi" = LISA_HDI_1)

train[] <- as.numeric(factor(as.matrix(train)))
test[] <- as.numeric(factor(as.matrix(test)))

formula <- HDI ~  pptt + Four + Ten  + Five + dvfm + illt + cllp + rntr + emp + elct + DS + Alg_Brd  

ctrl <- trainControl(method ="repeatedcv", repeats = 3)

model_knn <- train(formula,
                   data = train,
                   trainControl = ctrl,
                   method = 'knn',
                   tuneLenght = 5)
predicted_KNN_HDI <- round(predict(model_knn,test))

model_rf <- train(formula,
                   data = train,
                   trainControl = ctrl,
                   method = 'rf',
                   tuneLenght = 5)
predicted_rf_HDI <- round(predict(model_rf,test))  

model_xgboost <- train(formula,
                  data = train,
                  trainControl = ctrl,
                  method = 'xgbTree',
                  tuneLenght = 5)
predicted_xgboost_HDI <- round(predict(model_xgboost,test))  

accuracy_rf <- mean(predicted_rf_HDI == test$hdi)
accuracy_knn <- mean(predicted_KNN_HDI == test$hdi)
accuracy_xgboost <- mean(predicted_xgboost_HDI == test$hdi)


data_communes$KNN_hdi <- round(predicted_KNN_HDI)
data_communes$RF_hdi <- round(predicted_rf_HDI)
data_communes$XGBOOST_hdi <-round(predicted_xgboost_HDI)


tm_shape(data_communes) +
  tm_polygons("LISA_HDI_1", title = "LISA clusters of HDI in 2014", 
              palette = c("red", "blue","white"))+ 
  tm_shape(cities) +
  tm_compass()+
  tm_dots(size = .2, label ="City") +
  tm_text("City", size = 1, fontface = "bold", shadow = T, auto.placement = .1)

tm_shape(data_communes) +
  tm_polygons("KNN_hdi", title = "Predicted LISA clusters of HDI in 2014 using KNN model", 
              palette = c("green", "yellow","white"), 
             style = "cat",
             labels = c("high-high", "low-low","not signif."))+
  tm_shape(cities) +
  tm_compass()+
  tm_dots(size = .2, label ="City") +
  tm_text("City", size = 1, fontface = "bold", shadow = T, auto.placement = .1)

tm_shape(data_communes) +
  tm_polygons("RF_hdi", title = "Predicted LISA clusters of HDI in 2014 using Random Forest model", 
              palette = c("red", "violet", "white"), style = "cat",
              labels = c("high-high", "low-low","not signif."))+ 
  tm_shape(cities) +
  tm_compass()+
  tm_dots(size = .2, label ="City") +
  tm_text("City", size = 1, fontface = "bold", shadow = T, auto.placement = .1)

tm_shape(data_communes) +
  tm_polygons("XGBOOST_hdi", title = "Predicted LISA clusters of HDI in 2014 using xgboost model", 
              palette = c("blue", "orange","white"), style = "cat",
              labels = c("high-high", "low-low","not signif."))+ 
  tm_shape(cities) +
  tm_compass()+
  tm_dots(size = .2, label ="City") +
  tm_text("City", size = 1, fontface = "bold", shadow = T, auto.placement = .1)