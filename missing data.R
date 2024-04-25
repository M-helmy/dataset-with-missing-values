#dealing with missing data

data = data.frame(
  ID = 1:30,
  Income = c(1000, 13000, 5000, 4000, 4300, 45000, 20000, 43000, 48000, 29000,
             NA, 32000,9000,11000,7000,NA,61000,18000,43000 ,30000,
             70000,12000,NA,23000,16000,17000,NA,NA,35000,30000),
  Age = c(22,30,19,20,NA,33,51,28,45,44,31,23,12,56,NA,32,45,52,32,45,78,43,56,NA,37,49,62,57,51,33)

)
summary(data)
ols =lm(ID ~., data = data) # 8 observations were deleted as missing
summary(ols)  # st.error= 8.269 R2 = 0.2225

# impute missing with mean:

attach(data)
#install.packages("Hmisc")
library("Hmisc")
new.income = impute(data$Income, mean)
new.age = impute(data$Age , mean)
new.data = cbind.data.frame (ID, new.income, new.age)
summary(new.data) # no missing data 

ols.new = lm(ID ~ ., data = new.data)
summary(ols.new)  #st.error = 7.67  , R2= 0.2933

# we will try same method by median
new.income2 = impute(data$Income , median)
new.age2 = impute(data$Age , median)

new.data2 = cbind.data.frame(ID, new.income2, new.age2)
ols.new2 = lm(ID ~ ., data = new.data2)
summary(ols.new2)  # st.error = 7.691  R2 = 0.2895

# by imuptating the missing data with mean it gives a better st.error

#now, we will handle the missing data with multiple imputation:

# 1- "pmm' method
#install.packages("mice")
library("mice")
#install.packages("randomForest")
library("randomForest")

PMM = mice(data , m =5 , method ="pmm")
PMM_data = complete(PMM)
ols.pmm = lm(ID~., data = PMM_data)
summary(ols.pmm)  # st.error = 7.609 R2 = 0.3044

# 2- 'cart' method

crt = mice(data , m = 5 , nethod ='cart')
CRT_data = complete(crt)
ols.CRT = lm(ID~., data = CRT_data)
summary(ols.CRT)  # st.error = 7.559 R2 = 0.3137

# 3- 'rf' method

Rf = mice(data , m = 5 , method = 'rf')
Rf_data = complete(RF)
ols.RF = lm(ID~., data = Rf_data)
summary(ols.RF)  #st.error = 7.497 R2 = 0.3654

# 4- 'norm' method

Bayes = mice(data , m= 5 , method = 'norm')
Bayes_data = complete(Bayes)
ols.Bayes = lm(ID~., data = Bayes_data)
summary(ols.Bayes) #st.error = 7.45 R2 = 0.3331

AIC = AIC(ols.new, ols.pmm, ols.CRT, ols.RF ols.Bayes)
BIC = BIC(ols.new, ols.pmm, ols.CRT, ols.RF ols.Bayes)
mat = as.matrix(cbind(AIC, BIC) [,-c(1,3)])

windows(400,500) # show the graph in a separate window

barplot(mat , beside = T, main = "handling missing data by imputation"
        ,ylim = c(0, 1.25*max(mat)), col = rainbow(5),
        args.legend = list(x = 'topright', ncol = 5) ,legend.text = c("Mean", "PMM", "CRT", "RF", "Bayes"))


#concolusion Bayes method is the best practce to imputate missing values for these data
