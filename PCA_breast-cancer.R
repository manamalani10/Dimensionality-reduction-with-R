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
set.seed(42)
#splitting the dataset into training data and test data
sample<-sample.split(dataset$diagnosis,SplitRatio = 0.75)
Train_dataset<-subset(dataset,sample==TRUE)
Test_dataset<-subset(dataset,sample==FALSE)
#applying pca using preprocess function
pca<-preProcess(Train_dataset[-31],method =c("center","scale","pca"),pcaComp = 2)# PCA is requested but centering and scaling are not, the values will still be centered and scaled.So no need to mention centerin and scaling in pca differently 
Train_dataset_pca<-predict(pca,Train_dataset)
print(Train_dataset_pca)
Train_dataset_pca=Train_dataset_pca[c(2,3,1)]
print(Train_dataset_pca)
pca1<-preProcess(Test_dataset[-31],method =c("center","scale","pca"),pcaComp = 2)
Test_dataset_pca<-predict(pca1,Test_dataset)
print(Test_dataset_pca)
Test_dataset_pca<-Test_dataset_pca[c(2,3,1)]
#classification without pca
classifier<-svm(Train_dataset[-31],Train_dataset[31],type="C-classification")
y_pred<-predict(classifier,Test_dataset[-31])
print(y_pred)
cm<-table(Test_dataset[,31],y_pred)#prediction without pca
print(cm)
acc<-sum(diag(cm))/sum(cm)
print(acc)
#classification with pca
classifier_pca<-svm(Train_dataset_pca[-3],Train_dataset_pca[3],type = "C-classification")
y_pred_pca<-predict(classifier_pca,Test_dataset_pca[-3])
print(y_pred_pca)
cm_pca<-table(Test_dataset_pca[,3],y_pred_pca)
acc_pca<-sum(diag(cm_pca))/sum(cm_pca)
print(acc_pca)
print(cm_pca)##prediction(confusion matrix) outcome with pca
