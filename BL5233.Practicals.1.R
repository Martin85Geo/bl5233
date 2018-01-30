# BL5233: Tutorial 1

# Exercise A 

# Load library
library(dplyr)

# Set working directory
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")

# Load text data
data <- read.table(file="datasetTutorial1corrected.txt", header=TRUE, sep="\t")

# Create new vector
newVar <- seq(1,nrow(data),1)

# Combine new vector with text data
data2 <- mutate(data, newVar)
# data2 < cbind(data1,newVar)

# Write combined data into a text file
write.table(data2, "data2.txt")

# Calculate mean and standard deviation of second column
mean(data2$Fruit, na.rm=T)
sd(data2$Fruit, na.rm=T)

# Subset new data
subFruit <- data2[data2$Fruit < 50,]
subGrazed <- subFruit[subFruit$Grazing == "Grazed",]

# Raise 2nd column to power of 3.5, then divide by log of 1st column, and round off result
Raised <- ((subGrazed$Fruit)^3.5 / log(subGrazed$Root))
Rounded <- round(Raised)

# Estimate the mean fruit weight for grazed versus ungrazed plants
Grazed <- data2[data2$Grazing=="Grazed", na.omit=TRUE] # Do this!
meanGrazed <- mean(Grazed$Fruit)
Ungrazed <- data2[data2$Grazing=="Ungrazed",]
meanUngrazed <- mean(Ungrazed$Fruit, na.omit=TRUE) # Do this! 

# 7. 
by(data$Fruit, data2$Grazing, mean)

# 8. 
plot(data2$Root, data2$Fruit, xlab="root", ylab="fruit weight")

# 9. 
howManyNAs <- function(x){
              apply(apply(x,2,is.na),2,sum)
              }
howManyNAs(data1)

# 10. 
scaledRoot <- scale(data2$Root, center=TRUE, scale=TRUE)

# 11. 
library(rpart)

# 12. pnbinom(30, size=0.9, mu=10, lower.tail=FALSE)
# 13. qnbinom(0.95, size=0.9, mu=10)
#     qnbinom(c(0.025,0.975),size=0.9, mu=10)
# 14. z <- rnbinom(1000, mu=10, size=0.9)
max <- max(z)
f <- factor(z, levels=0:maxz)

# 15.
plot(f)

# 16. obsprobs <- table(f)/1000
plot(obsprobs)

# 17.
tvals <- dnbinom(0:maxz, size=0.9, mu=10)
points(0:maxz, tvals)

#18.
# 19. 
library(dplyr)
data3 =  filter(data2, GRazing == "Grazed", Fruit <50)
# 20.
data4 = data2[order(-data$Fruit), ]
# arrange(data2, desc(Fruit))

# 21. 
data4 <- group_by(data2, Grazing)
data4summary <-summarize(data4, meanFruit = mean(Fruit, na.rm=TRUE))

# 22. 
data2 %>% arrange(desc(Fruit)) %>% group_by(Grazing) %>%




