#PCA
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

#Appling PCA
library(caret)
library(e1071)
pca = preProcess(x = training_set[-17], method = 'pca', thresh = 0.90)
training_set_pca = predict(pca, training_set)
training_set_pca = cbind(training_set_pca[c(2:11)], training_set_pca[1])
test_set_pca = predict(pca, test_set)
test_set_pca = cbind(test_set_pca[c(2:11)], test_set_pca[1])

#Fitting SVM to the Training set
library(e1071)
classifier = svm(formula = letter~.,
                 data = training_set_pca,
                 type = "C-classification",
                 kernel = "linear")

#Predicting the Test set Results
pred = predict(classifier, newdata = test_set_pca[-11])

#Making the Confusion Matrix
cm = table(test_set_pca[, 11], pred)
cm
sum(diag(cm))/sum(cm)

#Ploting PCAs
library(ggfortify)
df <- training_set[-17]
autoplot(prcomp(df), data = training_set, colour = 'letter', loadings = TRUE,
         loadings.colour = 'black', loadings.label = TRUE, loadings.label.size = 5,
         loadings.label.colour = "blue")

#Bi-Plot
library(devtools)
dev.off()
#install_github("fawda123/ggord")
library(ggord)
ord <- prcomp(training_set[-17])
ggord(ord, training_set$letter)