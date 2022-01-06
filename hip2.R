library(igraph)

penulis = as.matrix(read.csv("penulis.csv",header=FALSE, sep=","))
koneksi = as.matrix(read.csv("relasi.csv",header=FALSE, sep=","))
# print(koneksi[8493,8493]) #print colom 3

# jumlah paper yang telah ditulis
attr_pprCount = as.numeric(penulis[,3])
# 0: tidak aktif
# 1: kurang aktif
# 2: aktif
# 3: sangat aktif
attr_pprCount[attr_pprCount %in% c(1:5)] <- 0 
attr_pprCount[attr_pprCount %in% c(6:10)] <- 1
attr_pprCount[attr_pprCount %in% c(11:20)] <- 2
attr_pprCount[attr_pprCount %in% c(21:100)] <- 3
# typeof(attr_pprCount)

gkoneksi <- graph_from_adjacency_matrix(koneksi)
gkoneksi.pprCount <- attr_pprCount
# typeof(gkoneksi.pprCount)

# H2: Author yang telah menulis banyak Paper cenderung akan memiliki banyak relasi dengan author lainnya
#hitung degree centtrality
gkoneksi.degreeCent <- centr_degree(gkoneksi, mode="in")$res
# typeof(gkoneksi.degreeCent)
plot(gkoneksi.degreeCent, gkoneksi.pprCount, main="Scatterplot degree centrality ~ paper Count", 
  	xlab="degree centrality", ylab="pprCount", pch=1)

# gkoneksi.indegree <- degree(gkoneksi, mode = "in")
# length(gkoneksi.indegree)
# plot(gkoneksi.indegree, gkoneksi.pprCount, main="Scatterplot pprCoount ~ indegree", 
#   	xlab="indegree", ylab="pprCount", pch=1)

# check berdasarkan clustering coefficient or transitivity
gkoneksi.clustcoeff <- transitivity(gkoneksi, type="local", isolates = "zero")
plot(gkoneksi.clustcoeff, gkoneksi.pprCount, main="Scatterplot pprCoount ~ clustcoeff", 
  	xlab="clustcoeff", ylab="pprCount", pch=1)

data_cek <- as.data.frame(cbind(gkoneksi.pprCount, gkoneksi.degreeCent, gkoneksi.clustcoeff, as.numeric(penulis[,6])))
summary(data_cek)
colnames(data_cek) <- c('pprCount', 'degreeCent', 'clustcoeff', 'pubPerYear')

set.seed(100)
trainingRowIndex <- sample(1:nrow(data_cek), 0.8*nrow(data_cek))
trainingData <- data_cek[trainingRowIndex, ]
length(trainingData)
testData  <- data_cek[-trainingRowIndex, ]

lm1  <- lm(pprCount ~ degreeCent, data=trainingData,)
lm2  <- lm(pprCount ~ clustcoeff, data=trainingData)
lm3  <- lm(pprCount ~ degreeCent+clustcoeff, data=trainingData)
lm4  <- lm(pprCount ~ degreeCent+clustcoeff+pubPerYear, data=trainingData)

lm1.pred <- predict(lm1, testData)
lm2.pred <- predict(lm2, testData)
lm3.pred <- predict(lm3, testData)
lm4.pred <- predict(lm3, testData)

pred_data <- as.data.frame(cbind(testData$pprCount,lm1.pred,lm2.pred,lm3.pred, lm4.pred))
colnames(pred_data) <- c('pprCount', 'degreeCent', 'clustcoeff', 'degreeCent+clustcoeff', 'degreeCent+clustcoeff+pubPerYear')
boxplot(pred_data, main="Author relation")

library(stargazer) 
stargazer(lm1,lm2,lm3,lm4,type="text", 
	dep.var.labels=c("Author relation"),	
	column.labels=c("degreeCent","clustcoeff","degreeCent+clustcoeff", "degreeCent+clustcoeff+pubPerYear"),
	covariate.labels=c("degreeCent", "Clustering Coefficient", "pubPerYear"),
	omit.stat=c("LL","ser","f")) 