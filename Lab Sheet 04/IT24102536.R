#Exercise

setwd("C:/Users/IT24102536/Desktop/IT24102536")
# 1.import dataset
branch_data <-read.table("Exercise.txt",header=TRUE,sep = ",")
fix(branch_data)
attach(branch_data)

#2. Indentify variables
#Branch         ---   Categoricl(Nominal)
#Sales_X1       ---   Numeric (Ratio scale)
#Advertising_X2 ---   Numeric (Ratio scale)
#years_X3       ---   Numeric (Ratio scale)

#3.Boxplot for sales and interpretation 
boxplot(branch_data$Sales_X1, main="Boxplot for sales",outline = TRUE,outpch=8,horizontal=TRUE)

#4.five number summary and IQR for Advertising variables
summary(branch_data$Advertising_X)
fivenum(branch_data$Advertising_X2)
IQR(branch_data$Advertising)

#5.Check for outliners in years variables
find_outliers <- function(x){
  Q1<- quantile(x,0.25)
  Q3<- quantile(x,0.75)
  IQR_val <-Q3 -Q1
  
  lower <-Q1 - 1.5 * IQR_val
  upper <-Q3 + 1.5 * IQR_val
  
  outliers <-x[x < lower|x > upper]
  return(outliers)
}
find_outliers(branch_data$Years_X3)
