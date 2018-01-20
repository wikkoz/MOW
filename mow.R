library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(alluvial)
library(extrafont)
library(magrittr)
library(fpc)
library(cluster) 
library(rpart)
library(rpart.plot)
library(factoextra)

#Przygotowanie danych
d1=read.table("data/student-mat.csv",sep=",",header=TRUE)
d2=read.table("data/student-por.csv",sep=",",header=TRUE)
d3<-rbind(d1,d2) 
d3norepeats<-d3 %>% distinct(school,sex,age,address,famsize,Pstatus,
                             Medu,Fedu,Mjob,Fjob,reason,
                             guardian,traveltime,studytime,failures,
                             schoolsup, famsup,activities,nursery,higher,internet,
                             romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)

alc_data = d3norepeats[ ,c('Dalc', 'Walc')]

#######################################
# Grupowanie (wybór 1 z nich)#
######################################

#########Wybór centrum

C1<-c(0,0)
C2<-c(5,5)
C3<-c(3,2)
C4<-c(2,3)
dist_euclidean <- dist(alc_data,method="euclidean")
dist_manhattan <- dist(alc_data,method="manhattan")

experiment_kmeans <- function(fit, iskmeans=TRUE) {
  f = fit$cluster
  km_stats_euclidean <- cluster.stats(dist_euclidean, f)
  km_stats_manhattan <- cluster.stats(dist_manhattan, f)
  cat("Euclidean: Average between: ", km_stats_euclidean$average.between, " Average within: ",
      km_stats_euclidean$average.within, " Siluetthe: ", km_stats_euclidean$avg.silwidth, " Dunn: ", 
      km_stats_euclidean$dunn, "\n")
  cat("Manhattan: Average between: ", km_stats_manhattan$average.between, " Average within: ",
      km_stats_manhattan$average.within, " Siluetthe: ", km_stats_manhattan$avg.silwidth, " Dunn: ", 
      km_stats_manhattan$dunn, "\n")
  return(fviz_cluster(fit, alc_data, geom = "point", palette = "Set2", ggtheme = theme_minimal()))
}

experiment_hclust <- function(method, metric) {
  hcut <- hcut(alc_data, k = 2, hc_method=method, hc_metric=metric)
  km_stats_euclidean <- cluster.stats(dist_euclidean, hcut$cluster)
  km_stats_manhattan <- cluster.stats(dist_manhattan, hcut$cluster)
  cat("Euclidean: Average between: ", km_stats_euclidean$average.between, " Average within: ",
      km_stats_euclidean$average.within, " Siluetthe: ", km_stats_euclidean$avg.silwidth, " Dunn: ", 
      km_stats_euclidean$dunn, "\n")
  cat("Manhattan: Average between: ", km_stats_manhattan$average.between, " Average within: ",
      km_stats_manhattan$average.within, " Siluetthe: ", km_stats_manhattan$avg.silwidth, " Dunn: ", 
      km_stats_manhattan$dunn, "\n")
  
  return(hcut)
}

#######################################
# Grupowanie - kmeans #
######################################

experiment_kmeans(kmeans(alc_data, centers = 2, nstart = 5))
experiment_kmeans(kmeans(alc_data, centers = rbind(C1, C2)))
experiment_kmeans(kmeans(alc_data, centers = rbind(C3, C4)))
experiment_kmeans(kmeans(alc_data, iter.max=100, centers = rbind(C1, C2)))
experiment_kmeans(kmeans(alc_data, iter.max=5, centers = rbind(C1, C2)))
experiment_kmeans(kmeans(alc_data, iter.max=100, algorithm = "Lloyd", centers = rbind(C1, C2)))
experiment_kmeans(kmeans(alc_data, iter.max=100, algorithm = "MacQueen", centers = 2))

#######################################
# Grupowanie - hierarchicczne #
######################################

hcut <- experiment_hclust("complete", "euclidian")
fviz_dend(hcut, show_labels = FALSE)
fviz_cluster(hcut, geom = "point", palette = "Set2", ggtheme = theme_minimal())

hcut <- experiment_hclust("ward.D", "euclidian")
fviz_dend(hcut, show_labels = FALSE)
fviz_cluster(hcut, geom = "point", palette = "Set2", ggtheme = theme_minimal())

hcut <- experiment_hclust("ward.D2", "euclidian")
fviz_dend(hcut, show_labels = FALSE)
fviz_cluster(hcut, geom = "point", palette = "Set2", ggtheme = theme_minimal())

hcut <- experiment_hclust("average", "euclidian")
fviz_dend(hcut, show_labels = FALSE)
fviz_cluster(hcut, geom = "point", palette = "Set2", ggtheme = theme_minimal())



#####################################
# NAJLEPSZE GRUPOWANIE #
######################################

fit <- kmeans(alc_data, centers = rbind(C1, C2))
# Połączenie danych
alc_clustered_data <- data.frame(d3norepeats, fit)

#####################################
# KLASYFIKACJA DRZEWEM #
######################################

set.seed(120) # 
sample <- sample.int(n = nrow(alc_clustered_data), size = floor(.75*nrow(alc_clustered_data)), replace = F)
train <- alc_clustered_data[sample, ]
test  <- alc_clustered_data[-sample, ]

experiment_cassification <- function (control) {
  classification <- rpart(train$fit ~ school + age + address + famsize + Pstatus +
                            Medu+Fedu+Mjob+Fjob+reason+
                            guardian+traveltime+studytime+failures+
                            schoolsup+ famsup+activities+nursery+higher+internet+
                            romantic + goout + sex,
                          method="class", data=train, control = control)
  
  rpart.plot(classification)
  
  printcp(classification)
  pred <- predict(classification, newdata = test, type="class") 
  tab <- table(pred, test$fit)
  (tab[1,1] + tab[2,2] )/ (tab[1,1] + tab[2,2] + tab[1,2] + tab[1,2]) *100 
}

experiment_cassification(rpart.control(minsplit = 1, cp = 0.01, xval = 10, maxdepth = 30))
experiment_cassification(rpart.control(minsplit = 20, cp = 0.001, xval = 10, maxdepth = 30))
experiment_cassification(rpart.control(minsplit = 10, cp = 0.001, xval = 10, maxdepth = 30))
experiment_cassification(rpart.control(minsplit = 20, cp = 0.01, xval = 10, maxdepth = 30))
experiment_cassification(rpart.control(minsplit = 20, cp = 0.01, xval = 10, maxdepth = 30))



