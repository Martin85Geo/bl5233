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


# Interpretation and forecasting

# Indirect comparison

yv <- predict(model)[,1]
yv2 <- predict(model)[,2]
par(mfrow=c(2,2)) 
plot(pgdata$hay,yv,pch=16,xlab='biomass',ylab="PC 1")
plot(pgdata$pH,yv2,pch=16,xlab='soil pH',ylab="PC 2")
par(mfrow=c(1,1)) 

# Linear discriminant analysis
library(MASS)
taxa = taxonomy[,-1]
Taxon = taxonomy$Taxon
model <- lda(Taxon ~.,taxa)
plot(model,col=rep(1:4 ,each=30))
predict(model)

train <- sort(sample(1:120,60))
table(Taxon[train])

model2 <- lda(Taxon ~., taxa, subset=train)
predict(model2)

unused <- taxa[-train,]
predict(model,unused)$class

table(unused$Taxon)

# Direct comparison/constrained ordination

# Canonical correspondence analysis
library(vegan)
data(mite)
data(mite.env)
data(mite.xy)
plot(mite.env, gap=0,panel=panel.smooth)

model0 <- cca(mite)
model1 <- cca(mite, mite.env) # throws an error
model1 <- cca(mite ~ SubsDens + WatrCont + Substrate + Shrub + Topo, data=mite.env)

model0
model1

head(summary(model1))
plot(model1)

plot(model1, dis=c("wa","lc"))
ordispider(model1)

plot(procrustes(model0, model1))

# Model selection
m1 <- cca(mite ~ ., mite.env)
m0 <- cca(mite ~ 1, mite.env)

m <- step(m0, scope=formula(m1), test="perm")
mback <- step(m1, test="perm")

m <- ordistep(m0, scope=formula(m1))
m$anova

# Collinearlity
vif.cca(model1)

# Manual updating of a model
m <- update(m, . ~ . - Shrub)
drop1(m, test="perm")

# Conditioned on partial ordination
m <- cca(mite ~ SubsDens + WatrCont + Substrate + Shrub + Topo +
           Condition(mite.xy$x + mite.xy$y), data=mite.env)
m

mod <- varpart(mite, ~SubsDens, ~ Substrate +Shrub, data = mite.env)
mod
showvarparts(2)
