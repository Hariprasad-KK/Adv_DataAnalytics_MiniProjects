#NAME : HARIPRASAD K K
#REG NO : 19BCE7079
#Slot : L45+L46
#Installing Libraries
install.packages("ggvis")
library('ggvis')
install.packages("tidyverse")
library('tidyverse')
install.packages("ggplot2")
library('ggplot2')

#Reading Data set
df <- read.csv("lab4_data.csv")
df

head(df)
class(df)
summary(df)

#Before assigning factors to string values
str(df)
levels(df$c27)  

#Assigning Factors to String Values
df$c10 <- as.factor(df$c10)
df$c9 <- as.factor(df$c9)
df$c7 <- as.factor(df$c7)
df$c27 <- as.factor(df$c27)
df$c27 <- as.factor(df$c12)

str(df)
levels(df$c10)
levels(df$c7)
levels(df$c9)
levels(df$c27)
levels(df$c12)


#computing the sums of matrix or array columns
colSums(is.na(df))

summary(df)

#Histograms
hist(df$c21)
hist(df$c29)
hist(df$c30)
hist(df$c31 , breaks=20)
hist(df$c32 , breaks=15)

#Dealing with NA values

#Since, the distribution of c29 and c31 is left-skewed. We will impute median values
median(na.omit((df$c29)))
median(na.omit((df$c31)))

df_data <- df
colSums(is.na(df_data))

# c29 replaced with Median
df_data$c29[is.na(df_data$c29)] <- median(na.omit((df$c29)))

# c31 replaced with Median
df_data$c31[is.na(df_data$c31)] <- median(na.omit((df$c31)))

colSums(is.na(df_data))

#Mode Function
get_mode <- function(x) {                 
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}
# c4 replaced with Mode
df_data$c4[is.na(df_data$c4)] <- get_mode(df$c4)

# c7 replaced with Mode
df_data$c7[is.na(df_data$c7)] <- get_mode(df$c7)

colSums(is.na(df_data))

#saving the new data frame
write.csv(df_data,"df_data.csv", quote = FALSE, row.names = TRUE)

data <- df_data

#Bar plots
counts <- table(df$c27, df$c7)
barplot(counts, main = '',legend = rownames(counts))

#Scatter plot
plot(df$c21, type= "p")

#Histogram
ggplot(df, aes(x = c29)) +geom_histogram()

#Box Plot
boxplot(df$c31, main = 'c31  Boxplot')
boxplot(df[,c(31,32)], main='Multiple Box plots')

# --- * PRE-PROCESSING * ---
input_dir = '../input/lab4_data'
list.files(input_dir)

#reusable functions
na_count <- function(data){
  sapply(data,function(x) sum(is.na(x)))    
}

#Data loaded in EDA
head(df)
summary(df$c29)
table(df$c9)
df$c9 <- factor(as.character(df$c9),levels=c("yes","no"))
head(df)

#Creating New Variables

#c33 was created by the average of c16 and c18
df$c33 <- (df$c16 + df$c18) /  2
head(df)

#c34 was created and filled with NA
df[,'c34'] <- NA
head(df)

#In c34 with respective c9 , yes and no are replaced with 1 and 0
df$c34[df$c9=='1'] <- 'yes'
df$c34[df$c9=='0'] <- 'no'
head(df)
df$c34 <- factor(df$c34,levels=c('1','0'))
head(df)
str(df)
names(df)
dim(df)
length(names(df))
sapply(df,function(x) sum(is.na(x)))
sum(is.na(df$c34))                                
names(df)
na_count(df)
summary(df$c31)
hist(df$c31)

#type check
is.numeric(df$c31)

#sort data
sort(df$c31)

min(df$c32,na.rm = TRUE)
max(df$c32,na.rm = TRUE)
dim(df)[[1]]
df1  <-  data.frame(seq_no=c(1:dim(df)[[1]]))
head(df1)
dim(df1)

#Merging Data sets
df <- cbind(df,df1)
df
dim(rbind(df,df))

#subset data
head(df[,names(df)[1:10]])
table(df$c34)
mean(df$c29,na.rm=TRUE)
head(subset(df,c29 > mean(c29,na.rm=TRUE),select=c(1,2,3)))