rm(list=ls()) #Clears the global environment
setwd("C:/Users/hp/OneDrive - Al Akhawayn University in Ifrane/Desktop/QUANTITATIVE MTHDS") #setting the working directory
set.seed(1995) #set seed

#Loading the packages
library(tidyverse)
library(ggplot2)
library(sf)
library(tmap)
library(dplyr)
library(spdep)
library(lmtest)

#Get rid of scientific notation
options(scipen = 999)

#Loading data
data_communes <- st_read("data_communes.shp")
cities <- st_read("Morocco_Cities.shp")

#Creating a set of data called change to store the value of change of each variable
#btwn 2004 and 2014
change <- data_communes %>% 
  transmute("geoID" = geoID,
            "pptt_c" = pptt_14 - pptt_04,
            "Four_c" = Four_14 - Four_04,
            "Ten_c" = Ten_14 - Ten_04,
            "Five_c" = Five_14 - Five_04,
            "dvfm_c" = dvfm_14 - dvfm_04,
            "cllp_c" = cllp_14 - cllp_04,
            "rntr_c" = rntr_14 - rntr_04,
            "illt_c" = illt_14 - illt_04,
            "emp_c" = emp_14 - emp_04,
            "elct_c" = elct_14 - elct_04,
            "DS" = DS,
            "Alg_Brd" = Alg_Brd,
            "HDI_c" = HDI_14 - HDI_04)


# This function computes the simplied anova from a linear model
simpleAnova <- function(object, ...) {
  # Compute anova table
  tab <- anova(object, ...)
  # Obtain number of predictors
  p <- nrow(tab) - 1
  # Add predictors row
  predictorsRow <- colSums(tab[1:p, 1:2])
  predictorsRow <- c(predictorsRow, predictorsRow[2] / predictorsRow[1])
  # F-quantities
  Fval <- predictorsRow[3] / tab[p + 1, 3]
  pval <- pf(Fval, df1 = p, df2 = tab$Df[p + 1], lower.tail = FALSE)
  predictorsRow <- c(predictorsRow, Fval, pval)
  # Simplified table
  tab <- rbind(predictorsRow, tab[p + 1, ])
  row.names(tab)[1] <- "Predictors"
  return(tab)
}

#Simple regression analysis:
model <- lm(HDI_c ~ pptt_c + Four_c + Ten_c + Five_c + dvfm_c + illt_c + emp_c +
              cllp_c + rntr_c + elct_c + DS + Alg_Brd ,change)
summary(model)

#Anova: testing the hypothesis of Beta coefficients are non zero:
simpleAnova(model)
confint(model)

#1-Assumption: (True)
#Calculate the mean of residuals:
mean(model$residuals)
t.test(model$residuals) #We are sure that the mean of residuals is zero!

#2-Assumption: 
#homoscedasticity: --> There's hedroscedasticity. So we cannot prove the Ass2 to be true
#Plot a scatter of expected values with residuals:
ggplot(model,aes(x= model$fitted.values, model$residuals))+
  geom_point()
#Testing the hedroscedasticity (alternative hypothesis) --> p.value is < alpha --> reject the homoscedasticity (null hypothesis)
bptest(model)

#3-Assumption:
#Independence: --> there's no special dependence in the map.
#Attach data together (residuals with geometry(maps)):
residuals <- as.data.frame(cbind("geoID" = data_communes$geoID,
                                 "residuals" = model$residuals))
data_communes <- right_join(data_communes, residuals, by = "geoID") 
#Mapping the residuals  
tm_shape(data_communes) +
  tm_polygons("residuals", title = "residuals simple linear regression", 
              palette = c( "yellow", "green", "white", "blue")) +
  tm_shape(cities) +
  tm_dots(size = .2, label ="City") +
  tm_text("City", size = 1, fontface = "bold", shadow = T, auto.placement = .1)

#4-Assumption:
#Testing for normality by plotting density:
plot(density(model$residuals))
#Hypothesis testing: H_a: non-normal distribution/ H_0: normal distribution:
shapiro.test(model$residuals)#p.value < alpha: reject the normal distribution hypothesis

#Conclusion: hypothesis test about the significance of the regression relationship and 
#the interval estimation results may not be valid:
