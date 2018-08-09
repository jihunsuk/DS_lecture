rm(list=ls())

setwd("C:/Users/tjrwl/R workspace")

# Data import
# ,를 구분으로 읽어와 테이블 생성
rawdata = read.table("./Dataset.data", sep=",")

colnames(rawdata) = c("gender", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "ring")

# Data preprocessing
unique_gender = unique(rawdata$gender)
new_gender = matrix(0, nrow(rawdata), 1)

for (i in 1:length(unique_gender)) {
  tmp_idx = which(rawdata$gender == unique_gender[i])
  new_gender[tmp_idx] = i
}

rawdata = rawdata[, -1]

for (i in 1:ncol(rawdata)){
  rawdata[,i] = (rawdata[,i] - min(rawdata[,i])) / (max(rawdata[,i]) - min(rawdata[,i]))
}

prdata = cbind(rawdata, new_gender)

tmp_idx = union(which(prdata$new_gender == 2), which(prdata$new_gender == 3))
prdata = prdata[tmp_idx,]
prdata$new_gender[which(prdata$new_gender == 3)] = 0
prdata$new_gender = as.factor(prdata$new_gender) # 요인으로 설정

# data partition
trn_ratio = 0.7
trn_idx = sample(1:nrow(prdata), round(trn_ratio*nrow(prdata)))
tst_idx = setdiff(1:nrow(prdata), trn_idx)

trn_data = prdata[trn_idx,]
tst_data = prdata[tst_idx,]

# k-nn
library(class)
out_knn = knn(trn_data[,1:(ncol(trn_data)-1)], tst_data[,1:(ncol(tst_data)-1)],
              trn_data[,ncol(trn_data)], k=7, prob=TRUE) # prob=T옴RUE하면 확률도 나옴

# Logistic regression
model_lr = glm(new_gender ~., data=trn_data, family="binomial")
out_lr = predict(model_lr, tst_data)

# Decision Tree
library(rpart)
model_tree = rpart(new_gender ~., data = trn_data, control=rpart.control(minsplit=10))
out_tree = predict(model_tree,tst_data)
                       
plot(model_tree)
text(model_tree, use.n = TRUE)

# Neural Networks
library(nnet)

model_nn = nnet(new_gender ~., data = trn_data, size=10, linout=FALSE, maxit=300)
out_nn = predict(model_nn, tst_data, type="class") # 은닉노드 수 15


# NN시각화
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_updata.r')
library(reshape2)
library(reshape)
library(scales)
plot.nnet(model_nn)

# SVM
library(e1071)
model_svm = svm(new_gender ~., data=trn_data, type="nu-classification", kernel="radial", gamma=10, cost=100)
out_svm = predict(model_svm, tst_data)

target = tst_data[,ncol(tst_data)]
outs = cbind(target, out_knn)

tmp_idx1 = which(out_lr >= 0)
tmp_idx2 = which(out_lr < 0)
out_lr2 = out_lr
out_lr2[tmp_idx1] = 1
out_lr2[tmp_idx2] = 0
outs = cbind(outs,out_lr2)

out_tree2 = out_tree[,2]
tmp_idx1 = which(out_tree2 >= 0.5)
tmp_idx2 = which(out_tree2 < 0.5)
out_tree2[tmp_idx1] = 1
out_tree2[tmp_idx2] = 0
outs = cbind(outs, out_tree2)
outs = cbind(outs, out_nn, out_svm)

for (i in 1:ncol(outs)) {
  if (length(which(outs[,i] == 2)) == 0){
    next
  }
  tmp_idx1 = which(outs[,i] == 2)
  tmp_idx2 = which(outs[,i] == 1)
  
  outs[tmp_idx1, i] = 1
  outs[tmp_idx2, i] = 0
}

library(caret)
confusionMatrix(as.factor(outs[,2]), as.factor(outs[,1]))
confusionMatrix(as.factor(outs[,3]), as.factor(outs[,1]))-
confusionMatrix(as.factor(outs[,4]), as.factor(outs[,1]))
confusionMatrix(as.factor(outs[,5]), as.factor(outs[,1]))
confusionMatrix(as.factor(outs[,6]), as.factor(outs[,1]))


library(pROC)
plot(roc(tst_data$new_gender, out_lr, direction="<"),
     col="red", lwd=3, main="ROC")
