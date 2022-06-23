#NAME : Hariprasad K K
#REG NO : 19BCE7079
#Lab(L45+L46)
#timeSeries Algorithm

data=read.csv("Stock_prices")
data
data['t']=1:nrow(data)
data['t']
data['t^2']=data['t']*data['t']
data['t^2']
data$Month = substr(data$Month,4,5)
data$Month= as.factor(data$Month)
newdata<-one_hot(as.data.table(data))
colnames(newdata)<-c('Djan','Dfeb','Dmar','Dapr','Dmay','Djun','Djul','Daug','Dsep','Doct','Dnov','Ddec','Stock','t','t^2')
newdata<-data.frame(newdata)
col_names<-c(13, 14, 15,1,2,3,4,5,6,7,8,9,10,11,12)
newdata=newdata[,col_names]
data=newdata
validation = tail(data,20)

#here validation set is the newdata which is the last 20 rows

newdata=validation
tqq12
a=lm(newdata$Stock_prices~newdata$t)
b=lm(log(newdata$Stock_prices)~newdata$t)
c=lm(newdata$Stock_prices~newdata$t+newdata$t.2)
d=lm(newdata$Stock_prices~newdata$Djan+newdata$Dfeb+newdata$Dmar+newdata$Dapr+newdata$Dmay+newdata$Djun+newdata$Djul+newdata$Daug+newdata$Dsep+newdata$Doct+newdata$Dnov+newdata$Ddec)
e=lm(newdata$Stock_prices~newdata$t+newdata$t.2+newdata$Djan+newdata$Dfeb+newdata$Dmar+newdata$Dapr+newdata$Dmay+newdata$Djun+newdata$Djul+newdata$Daug+newdata$Dsep+newdata$Doct+newdata$Dnov+newdata$Ddec)
f=lm(log(newdata$Stock_prices)~newdata$Djan+newdata$Dfeb+newdata$Dmar+newdata$Dapr+newdata$Dmay+newdata$Djun+newdata$Djul+newdata$Daug+newdata$Dsep+newdata$Doct+newdata$Dnov+newdata$Ddec)

#Calculating RMSE for all the given equations

predictions=a %>% predict(data.frame(newdata$Stock_prices))
RMSE(predictions,newdata$Stock_prices)
predictions1=b %>% predict(data.frame(newdata))
RMSE(predictions1,newdata$Stock_prices)
predictions2=c %>% predict(data.frame(newdata$Stock_prices))
RMSE(predictions2,newdata$Stock_prices)
predictions3=d %>% predict(data.frame(newdata$Stock_prices))
RMSE(predictions3,newdata$Stock_prices)
predictions4=e %> predict(data.frame(newdata$Stock_prices))
RMSE(predictions4,newdata$Stock_prices)
predictions5=f %>% predict(data.frame(newdata$Stock_prices))
RMSE(predictions5,newdata$Stock_prices)

#Out of all equations e has least RMSE value.Hence equation e  is the most accurate predictionout of 

#Calculating RMSE for all the given equations
predictions=a %>% predict(data.frame(newdata$Stock_prices))
RMSE(predictions,newdata$Stock_prices)
predictions1=b %>% predict(data.frame(newdata))
RMSE(predictions1,newdata$Stock_prices)
predictions2=c %>% predict(data.frame(newdata$Stock_prices))
RMSE(predictions2,newdata$Stock_prices)
predictions3=d %>% predict(data.frame(newdata$Stock_prices))
RMSE(predictions3,newdata$Stock_prices)
predictions4=e %>% predict(data.frame(newdata$Stock_prices))
RMSE(predictions4,newdata$Stock_prices)
predictions5=f %>% predict(data.frame(newdata$Stock_prices))
RMSE(predictions5,newdata$Stock_prices)

#Out of all equations e has least RMSE value.Hence equation e  is the most accurate predictionout of all the given equations

#here newdata is the entire data with 160 rows
newdata=data
e=lm(newdata$Stock_prices~newdata$t+newdata$t.2+newdata$Djan+newdata$Dfeb+newdata$Dmar+newdata$Dapr+newdata$Dmay+newdata$Djun+newdata$Djul+newdata$Daug+newdata$Dsep+newdata$Doct+newdata$Dnov+newdata$Ddec)

#Using predict function to build the model

predictions4=e %>% predict(data.frame(newdata$Stock_prices))
RMSE(predictions4,newdata$Stock_prices)


