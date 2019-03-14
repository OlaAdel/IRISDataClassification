library(datasets)
data(iris)
View(iris)
summary(iris) 

pie(table(iris$Species), main = "Pie Chart of the Iris data set Species", col = c("firebrick", "deepskyblue4", "goldenrod"), radius = 1)

barplot(table(iris$Sepal.Length), main = "Sepal Length", col = "deepskyblue4")
barplot(table(iris$Sepal.Width), main = "Sepal Width", col = "firebrick")
barplot(table(iris$Petal.Length), main = "Petal Length", col = "goldenrod")
barplot(table(iris$Petal.Width), main = "Petal Width", col = "gray35")

boxplot(Sepal.Length  ~ Species, iris, main = "Sepal Length", col = "deepskyblue4")
boxplot(Sepal.Width   ~ Species, iris, main = "Sepal Width", col = "firebrick")
boxplot(Petal.Length  ~ Species, iris, main = "Petal Length", col = "goldenrod")
boxplot(Petal.Width  ~ Species, iris, main = "Petal Width", col = "gray35")

plot(iris$Sepal.Length,iris$Sepal.Width, main=cor(iris$Sepal.Length,iris$Sepal.Width),xlab="Sepal Length", ylab="Sepal Width", pch=20,col = "goldenrod")
plot(iris$Sepal.Length,iris$Petal.Length, main=cor(iris$Sepal.Length,iris$Petal.Length),xlab="Sepal Length", ylab="Petal Length", pch=20,col = "deepskyblue4")
plot(iris$Sepal.Length,iris$Petal.Width, main=cor(iris$Sepal.Length,iris$Petal.Width),xlab="Sepal Length", ylab="Petal Width", pch=20,col = "firebrick")
plot(iris$Sepal.Width,iris$Petal.Length, main=cor(iris$Sepal.Width,iris$Petal.Length),xlab="Sepal Width", ylab="Petal Length", pch=20,col = "gray35")
plot(iris$Sepal.Width,iris$Petal.Width, main=cor(iris$Sepal.Width,iris$Petal.Width),xlab="Sepal Width", ylab="Petal Width", pch=20,col = "burlywood4")

hist(iris$Sepal.Length, col = "deepskyblue4", xlab = "Sepal Length", main ="Histogram of Sepal Length of Iris Data")
hist(iris$Sepal.Width, col = "firebrick", xlab = "Sepal Width", main ="Histogram of Sepal Width of Iris Data")
hist(iris$Petal.Length, col = "goldenrod", xlab = "Petal Length", main ="Histogram of Petal Length of Iris Data")
hist(iris$Petal.Width, col = "gray35", xlab = "Petal Width", main ="Histogram of Petal Width of Iris Data")

cor(iris[, 1:4])
cov(iris[, 1:4])
virginica <- iris[iris$Species == "virginica",] 

qqnorm(virginica$Sepal.Length, pch=20, col="goldenrod")
qqline(virginica$Sepal.Length, col="gray35")


versicolor <- iris[iris$Species == "versicolor",] 

qqnorm(versicolor$Sepal.Length, pch=20, col="deepskyblue4")
qqline(versicolor$Sepal.Length, col="firebrick")

t.test(virginica$Sepal.Length, versicolor$Sepal.Length, alternative = "greater")
iris.new<- iris[,c(1,2,3,4)]
iris.class<- iris[,"Species"]
head(iris.new)
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)
result<- kmeans(iris.new,3)
result$size
result$centers
result$cluster
par(mfrow=c(1,1), mar=c(5,4,2,2))
plot(iris.new[c(1,2)], col=result$cluster)
plot(iris.new[c(1,2)], col=iris.class)
plot(iris.new[c(3,4)], col=result$cluster)
plot(iris.new[c(3,4)], col=iris.class)

table(result$cluster,iris.class)

