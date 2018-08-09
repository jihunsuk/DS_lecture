rm(list=ls())

# Load the "Toyota Corolla" dataset
setwd("C:/Users/tjrwl/R workspace")
rawdata = read.csv("toyota.csv")

# Remove uninformative features
rawdata$Id <- NULL
rawdata$Model <- NULL

#Preprocessing the data
unique_Fuel_Type = unique(rawdata$Fuel_Type)
unique_Fuel_Type_dummy = as.data.frame(matrix(0, nrow(rawdata), length(unique_Fuel_Type)-1))

for (i in 1:(length(unique_Fuel_Type)-1)){
  tmp = unique_Fuel_Type[i]
  tmp_idx = which(rawdata$Fuel_Type == tmp)
  unique_Fuel_Type_dummy[tmp_idx, i] = 1
  colnames(unique_Fuel_Type_dummy)[i] = sprintf("unique_Fuel_Type_%s", tmp)
}
prdata = rawdata
prdata$Fuel_Type <- NULL
prdata = cbind(unique_Fuel_Type_dummy, prdata)


unique_color = unique(rawdata$Color)
color_dummy = as.data.frame(matrix(0, nrow(rawdata), length(unique_color)-1))

for(i in 1:(length(unique_color)-1)){
  tmp = unique_color[i]
  tmp_idx = which(rawdata$Color == tmp)
  color_dummy[tmp_idx, i] = 1
  colnames(color_dummy)[i] = sprintf("color_%s", tmp)
}

prdata$Color <- NULL
prdata = cbind(color_dummy, prdata)

# Train a linear regression model with the data
trn_ratio = 0.7
trn_idx = sample(1:nrow(prdata), round(trn_ratio*nrow(prdata)))  #트레이닝 데이터인덱스
tst_idx = setdiff(1:nrow(prdata), trn_idx)      # 테스트 데이터인덱스

trn_data = prdata[trn_idx,] #트레이닝 데이터
tst_data = prdata[tst_idx,] # 테스트 데이터

fit_lr = lm(Price ~., data=trn_data)
pred_lr = predict(fit_lr, tst_data)

plot(tst_data$Price, pred_lr)
mse_lr = mean((tst_data$Price-pred_lr)^2)

# Train a stepwise linear regression model with the data
step_lr = step(fit_lr, direction="both")

pred_step = predict(step_lr, tst_data)
plot(tst_data$Price, pred_step)
mse_step = mean((tst_data$Price-pred_step)^2)

fit_lr
summary(fit_lr)
mse_lr

step_lr
summary(step_lr)
mse_step

