# Multivariate Analysis

# Cluster analysis: k-means
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
kmd <- read.table(file="kmeansdata.txt", header=TRUE, sep="\t")
attach(kmd)
names(kmd)
graphics.off()
plot(x,y,col=group,pch=16)

par(mfrow=c(2,2))
plot(x,y,pch=16,main='Ungrouped')
plot(x,y,col=group,pch=16,main='Tr ue groups (6)')
model <- kmeans(data.frame(x,y),6)
plot(x,y,col=model[[1]],main='kmea ns with 6 groups')
model <- kmeans(data.frame(x,y),4)
plot(x,y,col=model[[1]],main='kmea ns with 4 groups')
par(mfrow=c(1,1))

model <- kmeans(data.frame(x,y), 6)
table(model[[1]],group)

taxonomy <- read.table('taxonomy.txt',header=T)
attach(taxonomy) 
names(taxonomy)
pairs(taxonomy)

kmeans(taxonomy, 4)
kmeans(taxonomy[,-1],3)


# Principal components analysis
pgdata <- read.table("pgfull.txt",header=T)
names(pgdata)
plot(pgdata$AC,pgdata$AE)

install.packages("scatterplot3d", dependencies = TRUE)
library(scatterplot3d)
scatterplot3d(pgdata$AC,pgdata$AE,pgdata$AM,highlight.3d=T,pch=19)

pgd <- pgdata[,1:54]
model <- prcomp(pgd,scale=T)
plot(model$x[,1],model$x[,2],pch=19,xlab="PC1",ylab= "PC2")
summary(model)
plot(model)

biplot(model)

# Non-metric multidimensional scaling
install.packages("vegan")
library(vegan) 
library(MASS)
data(varespec)
varespec

# using vegan package
vare.dis <- vegdist(varespec)
vare.dis
vare.mds0 <- isoMDS(vare.dis)
vare.mds0
stressplot(vare.mds0, vare.dis)
ordiplot(vare.mds0, type="t")

vare.mds <- metaMDS(varespec, trace=FALSE)
vare.mds
ordiplot(vare.mds, type="t")
