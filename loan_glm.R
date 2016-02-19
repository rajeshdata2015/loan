train = read.csv("train.csv", header = TRUE)
test = read.csv("test.csv", header = TRUE)


train$tale = "1"
test$tale = "0"

test$Loan_Status = NA
data_v1 = rbind(train, test)
data_v1[data_v1==""] = NA
str(data_v1)


data_v2 = data_v1[,names(data_v1) %in% c("Loan_ID","Gender","Married", "Dependents","Education","Self_Employed"
                                         ,"Credit_History","Property_Area","Loan_Status","tale")]
data_v2 = data.frame(apply(data_v2,2,as.factor))

data_v3 = data_v1[,!names(data_v1) %in% c("Loan_ID","Gender","Married", "Dependents","Education","Self_Employed"
                                          ,"Credit_History","Property_Area","Loan_Status","tale")]

data_v3 = data.frame(apply(data_v3,2,as.numeric))

data_f = cbind(data_v2,data_v3)
str(data_f)
#data_f$Loan_Amount_Term = as.numeric(data_f$Loan_Amount_Term)
sapply(data_f,function(df){sum(is.na(df)) })

# imputation
data_f$Loan_Amount_Term[is.na(data_f$Loan_Amount_Term)== TRUE] = 360
data_f$Loan_Amount_Term = as.character(data_f$Loan_Amount_Term)
data_f$Loan_Amount_Term= substr(data_f$Loan_Amount_Term,1,2)
data_f$Loan_Amount_Term[data_f$Loan_Amount_Term == "35"] = "36"
data_f$Loan_Amount_Term = as.numeric(data_f$Loan_Amount_Term)

data_f$Education = as.character(data_f$Education)
data_f$Education[data_f$Education == "Not Graduate"] = "Not_Graduate"

data_f$Gender = as.character(data_f$Gender)
data_f$Gender[is.na(data_f$Gender) == TRUE] = "Other"

data_f$Married = as.character(data_f$Married)
data_f$Married[is.na(data_f$Married) == TRUE] = "No_Info"

data_f$Dependents = as.character(data_f$Dependents)
data_f$Dependents[is.na(data_f$Dependents) == TRUE] = "few"
data_f$Dependents[data_f$Dependents == "3+"] = "3plus"

data_f$Self_Employed = as.character(data_f$Self_Employed)
data_f$Self_Employed[is.na(data_f$Self_Employed) == TRUE] = "maybe"

data_f$Credit_History = as.character(data_f$Credit_History)
data_f$Credit_History[is.na(data_f$Credit_History) == TRUE] = "no_idea"
data_f$Credit_History[data_f$Credit_History == " 1"] = "1"
data_f$Credit_History[data_f$Credit_History == " 0"] = "0"
data_f$LoanAmount[is.na(data_f$LoanAmount)] = mean(data_f$LoanAmount,na.rm = TRUE)

data_f$Gender=as.factor(data_f$Gender)
data_f$Married = as.factor(data_f$Married)
data_f$Education = as.factor(data_f$Education)
data_f$Self_Employed = as.factor(data_f$Self_Employed)
data_f$Credit_History = as.factor(data_f$Credit_History)
data_f$Dependents=as.factor(data_f$Dependents)

data_f$am_ratio = (data_f$ApplicantIncome+data_f$CoapplicantIncome)/(data_f$LoanAmount*1000)
data_f$Loan_Amount_Term=as.numeric(data_f$Loan_Amount_Term)
data_f$ln_ratio = (data_f$LoanAmount*1000)/data_f$Loan_Amount_Term

########## test of independence
chisq.test(data_f$Gender,data_f$Married)
chisq.test(data_f$Gender,data_f$Dependents)
chisq.test(data_f$Gender,data_f$Education)#independent
chisq.test(data_f$Gender,data_f$Self_Employed)#independent
chisq.test(data_f$Gender,data_f$Credit_History)#independent
chisq.test(data_f$Married,data_f$Dependents)
chisq.test(data_f$Married,data_f$Credit_History)#independent
chisq.test(data_f$Dependents,data_f$Credit_History)#independent
chisq.test(data_f$Credit_History,data_f$Property_Area)#independent

#creating dummy variables for factors

for(t in unique(data_f$Gender)) {
  data_f[paste("gen_Dummy",t,sep="")] <- ifelse(data_f$Gender==t,1,0)
}
for(t in unique(data_f$Married)) {
  data_f[paste("mrd_Dummy",t,sep="")] <- ifelse(data_f$Married==t,1,0)
}
for(t in unique(data_f$Dependents)) {
  data_f[paste("dep_Dummy",t,sep="")] <- ifelse(data_f$Dependents==t,1,0)
}

for(t in unique(data_f$Education)) {
  data_f[paste("edu_Dummy",t,sep="")] <- ifelse(data_f$Education==t,1,0)
}

for(t in unique(data_f$Self_Employed)) {
  data_f[paste("self_emp_Dummy",t,sep="")] <- ifelse(data_f$Self_Employed==t,1,0)
}

for(t in unique(data_f$Credit_History)) {
  data_f[paste("credit_Dummy",t,sep="")] <- ifelse(data_f$Credit_History==t,1,0)
}
for(t in unique(data_f$Property_Area)) {
  data_f[paste("prprty_area_Dummy",t,sep="")] <- ifelse(data_f$Property_Area==t,1,0)
}

train_f = data_f[data_f$tale == 1,]
test_f = data_f[data_f$tale == 0,]
#train_f$Outlet_Identifier = NULL
set.seed(1234)
train_f$random <- runif(nrow(train_f))
train_70 <- train_f[train_f$random <= 0.7,] 
train_30 <- train_f[train_f$random > 0.7,] 

train_70$Loan_ID=NULL
train_70$random=NULL
train_70$tale = NULL
test_f$random = NULL
train_f$random =NULL
train_f$tale = NULL

gbm.formula = formula(Loan_Status~ln_ratio+am_ratio+Loan_Amount_Term+gen_DummyMale+gen_DummyFemale+gen_DummyOther+mrd_DummyNo+mrd_DummyYes
                      +mrd_DummyNo_Info+dep_Dummy0+dep_Dummy1+dep_Dummy2+dep_Dummy3plus
                      +dep_Dummyfew+edu_DummyGraduate+edu_DummyNot_Graduate+self_emp_DummyNo+self_emp_DummyYes
                      +self_emp_Dummymaybe+credit_Dummy1+credit_Dummy0+credit_Dummyno_idea
                      +prprty_area_DummyUrban+prprty_area_DummyRural+prprty_area_DummySemiurban)
train_f$Loan_Status = ifelse(train_f$Loan_Status == "Y",1,0)
train_70$Loan_Status = ifelse(train_70$Loan_Status == "Y",1,0)
train_f$Loan_ID = NULL
# with top 10 variables
library(gbm)
fit.gbm = gbm(gbm.formula,data=train_f,distribution = "gaussian", n.tree = 500
              ,shrinkage = 1,cv.folds = 20)
summary(fit.gbm)
pred = predict(fit.gbm,test_f)
pred = ifelse(pred>=0.68,"Y","N")

test_f$Loan_Status=pred
SampleSubmission = test_f[,c("Loan_ID","Loan_Status")]
write.csv(SampleSubmission,"SampleSubmission.csv",row.names = FALSE)

###################################################################33
