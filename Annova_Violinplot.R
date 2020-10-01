rm(list=ls()) #Clears the global environment
setwd("C:/Users/hp/OneDrive - Al Akhawayn University in Ifrane/Desktop/QUANTITATIVE MTHDS") #setting the working directory
set.seed(1995) #set seed

#Load the tidyverse package
library(tidyverse)
#Load the Pairviz package
library(PairViz)

#load the data
data_shape <- read.csv("morocco_data_regions.csv", header = TRUE)

#Organize the data means and variances according to regions 
region_info <- data_shape %>%
  group_by(region) %>%
  summarize(mean_hdi = mean(HDI_04), var_hdi= var(HDI_04))

#Visualization of the HDI means and variability of all the regions:
data_shape %>%
  ggplot(aes(x = region , y = HDI_04 , color = region))+
  geom_violin()+
  theme_grey()+
  ylab("HDI in 2004")+
  theme(legend.title = element_blank())+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(angle = 60 , hjust = 1))+
  stat_summary(fun.y = mean ,geom = "point" , shape = 23 , size = 2)

#Annova test assuming that the variances of the populations are equal:

res.aov1 <- aov (HDI_04 ~ region, data_shape) 
summary (res.aov1)

#Annova test not assuming that the variances of the populations are equal:

res.aov2 <- oneway.test(HDI_04~ region, data_shape)
res.aov2

#Testing two population means of all 12 regions together (testing all the possible pairs)
test<- pairwise.t.test(data_shape$HDI_04, data_shape$region,
                       p.adjust.method = "BH", pool.sd = FALSE)
test

#get the test results and create a p-value matrix
region <- with(data_shape, split(HDI_04, region))
regionNames <- names(region)
pvals <- test$p.value
weights <- pvals[!is.na(pvals)]
weights <- edge2dist(weights)
weights <- as.matrix(weights)
rownames(weights) <- regionNames
colnames(weigths) <- rownames(weights)
view(weights)


