rm(list=ls())

setwd("C:/Users/tjrwl/R workspace")

data = read.csv("studentalcoholDataset.csv")
a = prcomp(data[-23], scale=T)[1]
#data = data[,prcomp(data, scale=T)[1]]

data = data[,-26]
data = data[,-25]
data = data[,-24]
data = data[,-22]
data = data[,-21]
data = data[,-20]
data = data[,-19]
data = data[,-18]
data = data[,-17]
data = data[,-16]

trn_ratio = 0.7
trn_idx = sample(1:nrow(data), round(trn_ratio*nrow(data)))
tst_idx = setdiff(1:nrow(data), trn_idx)

trn_data = data[trn_idx,]
tst_data = data[tst_idx,]

library(e1071)
model_svm = svm(alc ~ ., data = trn_data, kernel="radial", type="C-classification", gamma=0.1, cost=50, probability = TRUE)
x <- subset(tst_data, select = -alc)
y <- tst_data["alc"]
out_svm = predict(model_svm, tst_data)


example <- matrix(rbind(0, 22, 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 3, 3, 4), ncol=15, nrow=1)
example <- trn_data[1,]
test = predict(model_svm, example)
test
example2 <- matrix(rbind(1, 18, 0, 1, 4, 4, 1, 1, 0, 0, 3, 4, 3, 3, 3), ncol=15, nrow=1)
test2 = predict(model_svm, example2)
test2

#library(class)
#out_knn = knn(trn_data[,-23], tst_data[,-23],
#              trn_data[,23], k=5) #
#sum(out_knn==tst_data$alc)/NROW(out_knn)

library(nnet)
m = multinom(alc ~., data=trn_data)
out_lr = predict(m ,newdata = tst_data[, -23])
sum(out_lr==tst_data$alc)/NROW(out_lr)

# Decision Tree
library(rpart)
model_tree = rpart(alc ~., data = trn_data, control=rpart.control(minsplit=10))
out_tree = predict(model_tree,tst_data)
sum(round(out_tree)==tst_data$alc)/NROW(out_tree)

plot(model_tree)
text(model_tree, use.n = TRUE)

printcp(model_tree)
plotcp(model_tree)

ptree<-prune(model_tree, cp = model_tree$cptable[which.min(model_tree$cptable[,"xerror"]),"CP"])
plot(ptree)
text(ptree, cex=0.7)
pred_tree<-predict(ptree, tst_data, type='class')
confusionMatrix(pred_tree, tst_data$Alc)

library(caret)
target = tst_data["alc"]
outs = cbind(target, out_svm)
confusionMatrix(as.factor(outs[,1]), as.factor(outs[,2]))

plot(1:nrow(target["alc"]), jitter(target$alc, 1), col='blue', type='p', pch=4, xlab = "numberOfPeople", ylab = "Alc")
#points(1:nrow(target["alc"]), jitter(as.vector(out_svm),1), col="red")
grid()

