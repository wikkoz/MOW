library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(alluvial)
library(extrafont)

d1=read.table("data/student-mat.csv",sep=",",header=TRUE)
d2=read.table("data/student-por.csv",sep=",",header=TRUE)
d3<-rbind(d1,d2) 
d3norepeats<-d3 %>% distinct(school,sex,age,address,famsize,Pstatus,
                             Medu,Fedu,Mjob,Fjob,reason,
                             guardian,traveltime,studytime,failures,
                             schoolsup, famsup,activities,nursery,higher,internet,
                             romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)

alc_data = d3norepeats[ ,c('Dalc', 'Walc')]

#Kmeans
fit <- kmeans(alc_data, 2)
aggregate(alc_data,by=list(fit$cluster),FUN=mean)
alc_kmeans_data <- data.frame(d3norepeats, fit$cluster)

library(cluster) 
clusplot(alc_data, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

str2=ggplot(alc_data, aes(x=Dalc, y=Walc)) + 
  geom_point(aes(colour=factor(fit$cluster)))+ scale_colour_hue(l=25,c=150)
grid.arrange(str2,nrow=2)

#hierarchiczne (rozne metody)
# Ward Hierarchical Clustering
d <- dist(alc_data, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete") 
plot(fit) # display dendogram
pijak <- cutree(fit, k=2) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=2, border="red")

alc_heirar_data <- data.frame(d3norepeats, pijak)

str2=ggplot(alc_heirar_data, aes(x=Dalc, y=Walc)) + 
  geom_point(aes(colour=factor(pijak)))+ scale_colour_hue(l=25,c=150)
grid.arrange(str2,nrow=2)


#####################################
# KLASYFIKACJA DŻEWEM #
######################################

#zmapować na liczby
#fir cluster na dyskretna

alc_kmeans_data$sex <- as.factor(alc_kmeans_data$sex)

library(rpart)
library(rpart.plot)
classification <- rpart(fit$cluster ~ school + sex + age + address + famsize + Pstatus + 
              romantic + famrel + freetime + goout +  health + absences,
             method="class", data=alc_kmeans_data)

rpart.plot(classification)

printcp(classification) # display the results 
plotcp(classification) # visualize cross-validation results 
plot(classification, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(classification, use.n=TRUE, all=TRUE, cex=.8)
