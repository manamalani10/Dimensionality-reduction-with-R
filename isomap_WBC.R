#Data
#first import the dataset wdbc.data
data <- wdbc
str(data)
features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
names(data) <- c("id", "diagnosis", paste0(features,"_mean"), paste0(features,"_se"), paste0(features,"_worst"))

#Modifying dataset
wbc.data <- as.matrix(data[,c(3:32)])
row.names(wbc.data) <- data$id
diagnosis <- data$diagnosis

#Applying Isomap
D <- dist(wbc.data, method="euclidean")
library(vegan)
library(permute)
library(lattice)
model <- isomap(D, dims = 1, k = 5, mod = FALSE, plotResiduals = TRUE, verbose = TRUE)
summary(model)
plot(model)

#New dataset after applying Isomap
wbc.pcs <- model$points
wbc.pcs.df <- as.data.frame(wbc.pcs)
wbc.pcst <-wbc.pcs
wbc.pcst.df <- as.data.frame(wbc.pcst)
wbc.pcst.df <- cbind(wbc.pcs.df, diagnosis)
head(wbc.pcst.df)

#Splitting dataset
N <- nrow(wbc.pcst.df)
rv <- runif(N)
trainset <- wbc.pcst.df[rv <= 0.75,]
trainset.df <- as.data.frame(trainset)
testset <- wbc.pcst.df[rv > 0.75,]
testset.df <- as.data.frame(testset)
nrow(trainset.df)
nrow(testset.df)

#Applying SVM
library(e1071)
svmmodel <- svm(diagnosis~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, data = trainset.df, type = 'C-classification', kernel = "sigmoid", cost = 100) 
print(svmmodel)
summary(svmmodel)
plot(svmmodel,trainset.df, V1~V2)

#Using SVM to predict

##Train
pred <- predict(svmmodel, trainset.df)
pred

##Test
svm.predict <- predict(svmmodel, testset.df)
svm.predict

#Measuring Accuracy using Confusion Matrix
tab <- table(testset.df[, 11], svm.predict)
tab
acc <- sum(diag(tab))/sum(tab)
acc