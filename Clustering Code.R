library(rgl)
library(dbscan)
library(plotly)
library(mclust)
data <- read.csv("./hpatel8.csv", header = FALSE)

distance_matrix<-dist(data, method = "euclidean")
hc<-hclust(distance_matrix,method="complete")
plot(hc)
max_diff<-(-1)
delta<-0
for (i in 2:length(hc$height)) {
  if(hc$height[i]-hc$height[i-1] > max_diff){
    max_diff = hc$height[i]-hc$height[i-1]
    delta<-((hc$height[i] + hc$height[i-1])/2)
  }
}
plot(hc, xlab = "Data Points") #---dendrogram plot-----
clusterCut <- cutree(hc, h = delta)
h_rgl<-plot3d(data, col = clusterCut)
bgplot3d({
  plot.new()
  title(main = 'Hierarchical', line = 3)
})
h_p<-plot_ly(x = data$V1,y = data$V2, z=data$V3, type = 'scatter3d', mode = 'markers', color = clusterCut, marker = list(size=3))%>%layout(title = "Hierarchical")
print(h_p)

#K-Means Deciding K and plotting Elbow Plot

sse<-c()
for (i in 2:10) {
  km <- kmeans(data, centers = i, iter.max = 100, nstart = 10)
  print(km)
  sse[i-1]=km$tot.withinss
}
print(sse)
plot(c(2:10), sse, xlab = "K Values", ylab = "SSE", main = "SSE vs K-Values")

#K-Means Clustering

km <- kmeans(data, centers = 3, iter.max = 100, nstart = 10)
col_c<-km$cluster
plot3d(data, col = col_c, pch =1)
bgplot3d({
  plot.new()
  title(main = 'K Means', line = 3)
})
k_p<-plot_ly(x = data$V1,y = data$V2, z=data$V3, type = 'scatter3d', mode = 'markers', color = col_c, marker = list(size=3))%>%layout(title = "K means with k = 3")
print(k_p)


#DB Scan

dist_matrix<-as.matrix(dist(data))
data_length<- length(data[,1])

#MinPts = 3

r<-c(1:data_length)

for (i in 1:data_length) {
  r[i] = sort(dist_matrix[i,])[4]
}
r<-sort(r, decreasing = TRUE)
plot(c(1:816), r, main = "Distance vs data points for minPts 3", pch=".")
db_r_p<-plot_ly(x = c(1:816), y = r, type = "scatter", mode = c("markers"))%>%layout(title = "Distance vs Data Points for minPts=3")
grid(10,10)
abline(h=10, col="red")

dbs<-dbscan(data, 86, 3)
tmp<-(dbs$cluster)+1
dbs_p<-plot_ly(x = data$V1,y = data$V2, z=data$V3, type = 'scatter3d', mode = 'markers', color = dbs$cluster, marker = list(size=3))%>%layout(title = "DBSCAN : Epsilon = 86, minPts=3")
print(dbs_p)

#MinPts = 4

for (i in 1:data_length) {
  r[i] = sort(dist_matrix[i,])[5]
}
r<-sort(r, decreasing = TRUE)
plot(c(1:816), r, main = "Distance vs data points for minPts 4")
dbs<-dbscan(data, 100, 4)
tmp<-(dbs$cluster)+1
dbs_p<-plot_ly(x = data$V1,y = data$V2, z=data$V3, type = 'scatter3d', mode = 'markers', color = dbs$cluster, marker = list(size=3))%>%layout(title = "DBSCAN : Epsilon = 100, minPts=4")
print(dbs_p)

#MinPts = 5

for (i in 1:data_length) {
  r[i] = sort(dist_matrix[i,])[6]
}
r<-sort(r, decreasing = TRUE)
plot(c(1:816), r, main = "Distance vs data points for minPts 5")
dbs<-dbscan(data, 110, 5)
tmp<-(dbs$cluster)+1
dbs_p<-plot_ly(x = data$V1,y = data$V2, z=data$V3, type = 'scatter3d', mode = 'markers', color = dbs$cluster, marker = list(size=3))%>%layout(title = "DBSCAN : Epsilon = 110, minPts=5")
print(dbs_p)

#MinPts = 6

for (i in 1:data_length) {
  r[i] = sort(dist_matrix[i,])[7]
}
r<-sort(r, decreasing = TRUE)
plot(c(1:816), r, main = "Distance vs data points for minPts 6")
dbs<-dbscan(data, 115, 6)
tmp<-(dbs$cluster)+1
dbs_p<-plot_ly(x = data$V1,y = data$V2, z=data$V3, type = 'scatter3d', mode = 'markers', color = dbs$cluster, marker = list(size=3))%>%layout(title = "DBSCAN : Epsilon = 115, minPts=6")
print(dbs_p)



