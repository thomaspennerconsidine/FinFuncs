#REQUIRED PACKAHGED
#install.packages("xgboost")
#-_-_-_
#xgboost
#quantmod
#TTR
#-----END PACKAGES

#----

#BEGIN SCRIPT
#PACKAGE LOAD
library(quantmod)
library(TTR)
library(xgboost)
#END PACKAGE LOAD

#BEGIN SCRIPT

getSymbols("IBM", src = "yahoo")
# Read the stock data 
#ORIGINALBELOW

#$symbol = "ACC"
symbol = IBM
#fileName = paste(getwd(),"/",symbol,".csv",sep="") ; 
df = IBM #as.data.frame(read.csv(fileName))
colnames(df) = c("Open","Close","High", "Low", "Close","Adjusted")

# Define the technical indicators to build the model 
rsi = RSI(df$Close, n=14, maType="WMA")
adx = data.frame(ADX(df[,c("High","Low","Close")]))
sar = SAR(df[,c("High","Low")], accel = c(0.02, 0.2))
trend = df$Close - sar

# create a lag in the technical indicators to avoid look-ahead bias 
rsi = c(NA,head(rsi,-1)) 
adx$ADX = c(NA,head(adx$ADX,-1)) 
trend = c(NA,head(trend,-1))

# Create the target variable
price = df$Close-df$Open
class = ifelse(price > 0,1,0)



# Create a Matrix
model_df = data.frame(class,rsi,adx$ADX,trend)
model = matrix(c(class,rsi,adx$ADX,trend), nrow=length(class))
model = na.omit(model)
colnames(model) = c("class","rsi","adx","trend")






# Split data into train and test sets 
train_size = 2/3
breakpoint = nrow(model) * train_size

training_data = model[1:breakpoint,]
test_data = model[(breakpoint+1):nrow(model),]





# Split data training and test data into X and Y
X_train = training_data[,2:4] ; Y_train = training_data[,1]
class(X_train)[1]; class(Y_train)

X_test = test_data[,2:4] ; Y_test = test_data[,1]
class(X_test)[1]; class(Y_test)





# Train the xgboost model using the "xgboost" function
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
xgModel = xgboost(data = dtrain, nround = 5, objective = "binary:logistic")






# Using cross validation
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
cv = xgb.cv(data = dtrain, nround = 10, nfold = 5, objective = "binary:logistic")







# Make the predictions on the test data
preds = predict(xgModel, X_test)

# Determine the size of the prediction vector
print(length(preds))

# Limit display of predictions to the first 6
print(head(preds))





prediction = as.numeric(preds > 0.5)
print(head(prediction))






# Measuring model performance
error_value = mean(as.numeric(preds > 0.5) != Y_test)
print(paste("test-error=", error_value))







# View feature importance from the learnt model
importance_matrix = xgb.importance(model = xgModel)
print(importance_matrix)








# View the trees from a model
xgb.plot.tree(model = xgModel)

# View only the first tree in the XGBoost model
xgb.plot.tree(model = xgModel, n_first_tree = 1)