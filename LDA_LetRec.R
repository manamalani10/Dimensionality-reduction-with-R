#LDA
#set (/Letter Recognition/LR_Dataset/) as current working directory
#Importing the dataset
dataset = read.csv("letter-recognition.data")

#Modifying Data
dataset = cbind(dataset[c(2:17)], dataset[1])
str(dataset)

#Splitting the dataset into Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$letter, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling
training_set[-17] = scale(training_set[-17])
test_set[-17] = scale(test_set[-17])

#Appling LDA
library(MASS)
lda = lda(formula = letter~., data = training_set)
training_set_lda = as.data.frame(predict(lda, training_set))
training_set_lda = cbind(training_set_lda[c(28:37)], training_set_lda[1])
test_set_lda = as.data.frame(predict(lda, test_set))
test_set_lda = cbind(test_set_lda[c(28:37)], test_set_lda[1])

#Fitting SVM to the Training set
library(e1071)
classifier = svm(formula = class~.,
                 data = training_set_lda,
                 type = "C-classification",
                 kernel = "linear")

#Predicting the Test set Results
pred = predict(classifier, newdata = test_set_lda[-11])

#Making the Confusion Matrix
cm = table(test_set_lda[, 11], pred)
cm
sum(diag(cm))/sum(cm)

#Bi-Plot
library(devtools)
dev.off()
#install_github("fawda123/ggord")
library(ggord)
ggord(lda, training_set_lda$class)