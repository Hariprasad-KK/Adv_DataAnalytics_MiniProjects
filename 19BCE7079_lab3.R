#NAME : Hariprasad K K
#REG NO : 19BCE7079
#LAB : L45+L46

#installing packages
install.packages("stringr")
install.packages("caTools")
#Packages
library(stringr)
library(caTools)

#Given data set
loans_data<-read.csv("loans_data.csv")

#top viwe of data
head(loans_data)

#finding the datatype of each column
mode(loans_data$Interest.Rate)
mode(loans_data$FICO.Range)
mode(loans_data$Employment.Length)
mode(loans_data$Debt.To.Income.Ratio)
mode(loans_data$Amount.Funded.By.Investors)
mode(loans_data$Amount.Requested)
mode(loans_data$Loan.Length)
mode(loans_data$Monthly.Income)
mode(loans_data$Open.CREDIT.Lines)
mode(loans_data$Revolving.CREDIT.Balance)
mode(loans_data$Inquiries.in.the.Last.6.Months)
mode(loans_data$Loan.Length)

#making them numeric
loans_data$Amount.Funded.By.Investors<-as.numeric(loans_data$Amount.Funded.By.Investors)
loans_data$Amount.Requested<-as.numeric(loans_data$Amount.Requested)
loans_data$Open.CREDIT.Lines<-as.numeric(loans_data$Open.CREDIT.Lines)
loans_data$Revolving.CREDIT.Balance<-as.numeric(loans_data$Revolving.CREDIT.Balance)
loans_data$Interest.Rate<-gsub("%$","",loans_data$Interest.Rate)
loans_data$Interest.Rate<-as.numeric(loans_data$Interest.Rate)
loans_data$Debt.To.Income.Ratio<-gsub("%$","",loans_data$Debt.To.Income.Ratio)
loans_data$Debt.To.Income.Ratio<-as.numeric(loans_data$Debt.To.Income.Ratio)
loans_data$Loan.Length<-gsub("months$","",loans_data$Loan.Length)
loans_data$Loan.Length<-as.numeric(loans_data$Loan.Length)

#checking whether they are numeric are not
mode(loans_data$Amount.Funded.By.Investors)
mode(loans_data$Amount.Requested)
mode(loans_data$Open.CREDIT.Lines)
mode(loans_data$Revolving.CREDIT.Balance)
mode(loans_data$Interest.Rate)
mode(loans_data$Debt.To.Income.Ratio)

#handling the employee.length column removing the special charcters
loans_data$Employment.Length<-gsub("<","",loans_data$Employment.Length)
loans_data$Employment.Length<-gsub("\\+","",loans_data$Employment.Length)

#handling fico.range column
loans_data$FICO.Range<-gsub("-","",loans_data$FICO.Range)

#finding na values in each column
sum(is.na(loans_data$Amount.Requested))
sum(is.na(loans_data$Amount.Funded.By.Investors))
sum(is.na(loans_data$Interest.Rate))
sum(is.na(loans_data$Loan.Length))
sum(is.na(loans_data$Loan.Purpose))
sum(is.na(loans_data$Debt.To.Income.Ratio))
sum(is.na(loans_data$State))
sum(is.na(loans_data$Home.Ownership))
sum(is.na(loans_data$Monthly.Income))
sum(is.na(loans_data$FICO.Range))
sum(is.na(loans_data$Open.CREDIT.Lines))
sum(is.na(loans_data$Revolving.CREDIT.Balance))
sum(is.na(loans_data$Inquiries.in.the.Last.6.Months))
sum(is.na(loans_data$Employment.Length))

#since all are having less NA values I haven't handled the na values
#splitting of datase
split=sample.split(loans_data$Interest.Rate,SplitRatio=0.75)
training_set=subset(loans_data,split==TRUE)
test_set=subset(loans_data,split==FALSE)
split

#regression
relation<-lm(training_set$Monthly.Income~training_set$Interest.Rate)
relation
pred_res<-as.data.frame(predict(relation,newdata = test_set))
pred_res
