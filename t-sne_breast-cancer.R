#first import breast cancer wiconsin data.csv as 'data'
dataset=read.csv("data.csv")
print(dataset)
dataset=dataset[,2:32]
dataset=dataset[c(2:31,1)]
#encoding M&B to (0,1) respectively
dataset$diagnosis=factor(dataset$diagnosis,levels = c("M","B"),labels = c(0,1))
library(caret)
library(e1071)
library(caTools)
set.seed(3)
#splitting the dataset into training data and test data
sample<-sample.split(dataset$diagnosis,SplitRatio = 0.7)
Train_dataset<-subset(dataset,sample==TRUE)
Test_dataset<-subset(dataset,sample==FALSE)
library(Rtsne)##(applying t-sne)##much better efficient method in R is to use Rtsne library
tsne_Train_dataset<-Rtsne(Train_dataset[,1:30],dims=2,initial_dims=30,perplexity=40)
print(tsne_Train_dataset)
tsne_Test_dataset<-Rtsne(Test_dataset[1:30],dims=2,initial_dims = 30,perplexity=40)
print(tsne_Test_dataset)
##applying classification without tsne
classifier<-svm(Train_dataset[-31],Train_dataset[31],type="C-classification")
y_pred<-predict(classifier,Test_dataset[-31])
print(y_pred)
cm<-table(Test_dataset[,31],y_pred)
print(cm)
acc<-sum(diag(cm))/sum(cm)
print(acc)
#applying classification with t-sne
classifier_tsne<-svm(tsne_Train_dataset$Y,Train_dataset[31],type = "C-classification")
y_pred_tsne<-predict(classifier_tsne,tsne_Test_dataset$Y)
print(y_pred_tsne)
cm_tsne<-table(Test_dataset[,31],y_pred_tsne)
print(cm_tsne)
acc_tsne<-sum(diag(cm_tsne))/sum(cm_tsne)
print(acc_tsne)
