##useful package
library(readr)
library(randomForest)
library(dplyr)
library(caret)
library(VIM)
library(pROC)
library(boot)
##read data and basic process
user_format= read_csv("/Users/nino/Desktop/BAproject/data_format1/train_format1.csv")
user_info_format = read_csv("/Users/nino/Desktop/BAproject/data_format1/user_info_format1.csv")
user_info = merge(user_format,user_info_format,by = "user_id")
##missing value
aggr(user_info) #visulization
user_info = na.omit(user_info)
user_info$age_range_factor = as.factor(user_info$age_range)
user_info$gender_factor = as.factor(user_info$gender)
user_info$label_factor = as.factor(user_info$label)
##training testing split
set.seed(9)
in_train <- createDataPartition(y = user_info$label,
                                p = 3 / 4,
                                list = FALSE)
##read log info 
action_type_read = read_csv("/Users/nino/Desktop/BAproject/p0.csv")
action_type_info <- action_type_read[,-1]
colnames(action_type_info) <-  c("merchant_id", "user_id", "click", "cart", "purchase", "favourite" )
user_mer_final <- left_join(user_info,action_type_info, by = c('user_id', 'merchant_id'))
##missing value?
aggr(user_mer_final)
##split
user_final_tr <- user_mer_final[in_train, ]
user_final_test <- user_mer_final[-in_train, ]
##model1
model1 = glm(label_factor ~ age_range_factor + gender_factor, data = user_final_tr, family = "binomial")
summary(model1)
predict_prob_m1 = predict(model1, newdata = user_final_test, type = "response")
##model2
model2 <- glm(label_factor ~ age_range_factor + gender_factor + click + cart + purchase + favourite, 
              data = user_final_tr,
              family = "binomial")
summary(model2)
predict_prob_m2 = predict(model2, newdata = user_final_test, type = "response")
##AUC model 1
roc_m1 <- roc(user_final_test$label_factor, predict_prob_m1)
auc(roc_m1)
## AUC model 2
roc_m2 <- roc(user_final_test$label_factor, predict_prob_m2)
auc(roc_m2)
##prediction of the whole dataset
user_mer_final$predict_prob <- predict(model2, newdata = user_mer_final, type = "response")
## figure out best p value based on cost and revenue
findprofit = function(dataset, cost, value_repeat_buyer){
  profit_matrix = c(c(0,-value_repeat_buyer),c(-cost, value_repeat_buyer - cost))
  p_value = seq(0, 0.15, 0.005)
  profitPerThreshold = rep(0,length(p_value)) 
  n = 1
  for (i in p_value){
    prob_dummy = dataset$predict_prob > i
    classficationTable = table(truth=dataset$label,
                               predict=prob_dummy)
    if (length(colnames(classficationTable)) == 1 && colnames(classficationTable) == "FALSE"){
      profitPerThreshold[n] = sum(classficationTable * profit_matrix[c(1,2)])
    } else if (length(colnames(classficationTable)) == 1 && colnames(classficationTable) == "TRUE"){
      profitPerThreshold[n] = sum(classficationTable * profit_matrix[c(3,4)])
    } else{
      profitPerThreshold[n] = sum(classficationTable * profit_matrix)
    }
    n = n + 1
  }
  return (profitPerThreshold)
}


# estimate coupon cost & value_repeat_buyers
# double 11 sales = 31,000,000,000 USD
# double 11 # transactions = 1,480,000,000
# avg_paid = 21
# coupon cost = 0.1 * 21 = 2.1
# value_repeat_buyers = 0.4 * 21 = 8.4
cost = 2.1
value_repeat_buyer =  8.4
profit  = findprofit(user_mer_final,cost,value_repeat_buyer) 
max(profit)
benchmark = - cost * nrow(user_mer_final) + value_repeat_buyer * sum(user_mer_final$label == 1)
benchmark
amount_saved = max(profit) - benchmark
amount_saved
max_profit_index = which.max(profit)
p_value = seq(0, 0.15, 0.005)
p_threshold = p_value[max_profit_index]
p_threshold
prob_dummy = user_mer_final$predict_prob > p_threshold
classficationTable = table(truth=user_mer_final$label,
                           predict=prob_dummy)
classficationTable
# percentage amount saved 
100 - max(profit) / (benchmark) * 100

# saved for each transaction
saved_each_transact = amount_saved/ nrow(user_mer_final)
# apply it to all transactions
saved_each_transact * 1.48

