library(caret)

temp_data_train = mtcars
#> colnames(mtcars)
# [1] "mpg"  "cyl"  "disp" "hp"  
# [5] "drat" "wt"   "qsec" "vs"  
# [9] "am"   "gear" "carb"
 
colnames(temp_data_train)[1] = "Class"

s_tuneLength = 100
s_MLmethods_select = "rf"
s_number = 10 
s_repeats = 10



# set training / optimisation and control parameters
fitControl <- trainControl(method = "repeatedcv", number = s_number, repeats = s_repeats,  search = "random")#, summaryFunction = twoClassSummary) # MCC / twoClassSummary / prSummary / RRLab::MCCx
 

fit <- train(Class ~ ., data = temp_data_train,
	#metric=s_metric,
	method = s_MLmethods_select,
	tuneLength = s_tuneLength,
	trControl = fitControl,
	maximize = TRUE)
	
plot(fit)

pred = predict(fit,temp_data_train)

plot(x=temp_data_train$Class,y=pred)