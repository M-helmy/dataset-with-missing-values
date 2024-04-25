# dataset-with-missing-values
# handling the dataset with imputation the missing values in R by single and multiple imputation. I use filling the missing values with standard mean for standard imputation, and for multiple imputation I used methods('pmm' , 'cart' , 'rf' , 'norm'
# dealing with missing data
mydata = read.csv("C:\\Users\\Helmy\\Downloads\\Fundamentals of data science R\\Lec 7-20240424T191014Z-001\\Lec 7\\Income.csv")
summary(mydata)
ols = lm(ID ~ ., data = mydata) 
summary(ols)   #st.error = 8.476, R2 = 0.2577

# imputate missing data by mean
attach(mydata)
# install.packages("Hmisc")
library("Hmisc")
new.income = impute(mydata$Income, mean)
new.age = impute(mydata$Age , mean)
new.data = cbind.data.frame (ID, new.income, new.age)
summary(new.data)
ols.new = lm(ID ~ ., data = new.data)
summary(ols.new)  #st.error = 7.783 , R2= 0.3161

# we will try same method by median
new.income2 = impute(mydata$Income , median)
new.age2 = impute(mydata$Age , median)

new.data2 = cbind.data.frame(ID, new.income2, new.age2)
ols.new2 = lm(ID ~ ., data = new.data2)
summary(ols.new2)  # st.error = 7.81 R2 = 0.3113

# by imuptating the missing data with mean it gives a better st.error

# now, we will handle the missing data with multiple imputation:

# 1- "pmm' method

# install.packages("mice")
library("mice")
# install.packages("randomForest")
library("randomForest")

PMM = mice(mydata , m= 5 , method= "pmm")
PMM_data = complete(PMM)
ols.pmm = lm(ID~., data = PMM_data)
summary(ols.pmm)  # st.error = 7.461 R2 = 0.3715

# 2- "cart" method

CRT = mice(mydata , m = 5 , method = "cart")
CRT_data = complete(CRT)
ols.crt = lm(ID~., data = CRT_data)
summary(ols.crt)  # st.error = 7.465 R2 = 0.3709

# 3- "rf" method

RF = mice(mydata , m= 5 , method = "rf")
RF_data = complete(RF)
ols.rf = lm(ID~., data = RF_data)
summary(ols.rf)  # st.error = 7.497 R2 = 0.3654

# 4- "norm" method

Bayes = mice(mydata , m = 5 , method = "norm")
Bayes_data = complete(Bayes)
ols.bayes = lm(ID~., data = Bayes_data)
summary(ols.bayes) # st.error = 7.805 R2 = 0.3122


AIC = AIC(ols.new ,ols.pmm, ols.crt, ols.rf, ols.bayes)
BIC = BIC(ols.new ,ols.pmm, ols.crt, ols.rf, ols.bayes)

matr = as.matrix(cbind(AIC, BIC)[,-c(1,3)])

# output :               AIC      BIC
#             ols.new   220.0387 225.7747
#             ols.pmm   217.4222 223.1581
#             ols.crt   217.4521 223.1880
#             ols.rf    217.7187 223.4546
#             ols.bayes 220.2163 225.9523

windows(400,500)

barplot(matr , beside = T, main = "handling missing data by imputation"
        ,ylim = c(0, 1.25*max(matr)), col = rainbow(5),
        args.legend = list(x = 'topright', ncol = 5) ,legend.text = c("Mean", "PMM", "CRT", "RF", "Bayes"))

# conclusion: PMM and CRT methods are better in impuation than other methods.
