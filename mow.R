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
# Klasyfikatory (wybór 1 z nich)#
######################################


C1<-c(0,0)
C2<-c(5,5)
fit <- kmeans(alc_data, centers = rbind(C1, C2))

###ladny wykres
fviz_cluster(fit, alc_data, geom = "point", palette = "Set2", ggtheme = theme_minimal())

##sil
sil <- silhouette(fit$cluster, dist(alc_data))
fviz_silhouette(sil)


aggregate(alc_data,by=list(fit$cluster),FUN=mean)
fit <- fit$cluster

#hierarchiczne
d <- dist(alc_data, method = "euclidean")
tree <- hclust(d, method="complete") 
plot(tree) # 
fit <- cutree(tree, k=2)

hcut <- hcut(alc_data, k = 2, method="complete")


rect.hclust(tree, k=2, border="red")
fviz_cluster(hcut, geom = "point", palette = "Set2", ggtheme = theme_minimal())




# Połączenie danych
alc_clustered_data <- data.frame(d3norepeats, fit)

#######################################
# Wykres klasyfikaotra #
######################################
clusplot(alc_data, fit, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(alc_data, fit) 
str2=ggplot(alc_clustered_data, aes(x=Dalc, y=Walc)) + 
  geom_point(aes(colour=factor(fit)))+ scale_colour_hue(l=25,c=150)
grid.arrange(str2,nrow=2)


#######################################
# Statysyki klasyfikaotra #
######################################
Dist <- dist(alc_data,method="manhattan")
km_stats <- cluster.stats(Dist, fit)
km_stats


#####################################
# KLASYFIKACJA DRZEWEM #
######################################

set.seed(120) # 
sample <- sample.int(n = nrow(alc_heirar_data), size = floor(.75*nrow(alc_heirar_data)), replace = F)
train <- alc_clustered_data[sample, ]
test  <- alc_clustered_data[-sample, ]

classification <- rpart(train$fit ~ school + sex + age + address + famsize + Pstatus +
                          Medu+Fedu+Mjob+Fjob+reason+
                        guardian+traveltime+studytime+failures+
                        schoolsup+ famsup+activities+nursery+higher+internet+
              romantic + famrel + freetime + goout +  health + absences,
             method="class", data=train)

rpart.plot(classification)

printcp(classification)
pred <- predict(classification, newdata = test, type="class") 
table(pred, test$fit)

