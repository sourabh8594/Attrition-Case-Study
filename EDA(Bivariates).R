

####################################

# EDA of Attrition , Bivariates 

####################################


setwd("C://Users//Sourabh Kushwaha//Documents//Attrition")
source("Attrition Case Study.R")

#A. C ---> C
#Attrition vs BusinessTravel
#Attrition vs Department
#Attrition vs Education
#Attrition vs EducationField
#Attrition vs EnvironmentSatisfaction
#Attrition vs Gender
#Attrition vs JobInvolvement
#Attrition vs JobLevel
#Attrition vs JobRole
#Attrition vs JobSatisfaction
#Attrition vs MaritalStatus
#Attrition vs Over18
#Attrition vs OverTime
#Attrition vs PerformanceRating
#Attrition vs RelationshipSatisfaction
#Attrition vs StockOptionLevel
#Attrition vs WorkLifeBalance


#1.Attrition vs BusinessTravel
# ----Summarization
# Comparative Proportion
prop.table(table(mydata$Attrition, mydata$BusinessTravel_label),1)
# ----Visualization
# Contingency Table
chi.businesstravel <- table(mydata$Attrition, mydata$BusinessTravel_label)
# ----ToH
# Chi-Square test
chi.test1<- chisq.test(chi.businesstravel)
chi.test1$observed
chi.test1$expected
chi.test1$residuals
sum((chi.test1$residuals)^2)
print(chi.test1)

#2.Attrition vs Department
# ----Summarization
# Comparative Proportion
prop.table(table(mydata$Attrition, mydata$Department_label), 1)
# ----Visualization
# Contingency Table
chi.Department <- table(mydata$Attrition, mydata$Department_label)
# ----ToH
# Chi-Square test
chi.test2<- chisq.test(chi.Department)
chi.test2$observed
chi.test2$expected
chi.test2$residuals
sum((chi.test2$residuals)^2)
print(chi.test2)


#3.Attrition vs Education
# ----Summarization
# Comparative Proportion
prop.table(table(mydata$Attrition_label, mydata$Education_label), 1)
# ----Visualization
# Contingency Table
chi.Education <- table(mydata$Attrition_label, mydata$Education_label)
# ----ToH
# Chi-Square test
chi.test3<- chisq.test(chi.Education)
chi.test3$observed
chi.test3$expected
chi.test3$residuals
sum((chi.test3$residuals)^2)
print(chi.test3)

#4.Attrition vs EducationField
# ----Summarization
# Comparative Proportion
prop.table(table(mydata$Attrition_label, mydata$EducationField_label), 1)
# ----Visualization
# Contingency Table
chi.EducationField <- table(mydata$Attrition_label, mydata$EducationField_label)
# ----ToH
# Chi-Square test
chi.test4<- chisq.test(chi.EducationField)
chi.test4$observed
chi.test4$expected
chi.test4$residuals
sum((chi.test4$residuals)^2)
print(chi.test4)

#5.Attrition vs EnvironmentSatisfaction
# ----Summarization
# Comparative Proportion
prop.table(table(mydata$Attrition_label, mydata$EnvironmentSatisfaction_label), 1)
# ----Visualization
# Contingency Table
chi.Environ <- table(mydata$Attrition_label, mydata$EnvironmentSatisfaction_label)
# ----ToH
# Chi-Square test
chi.test5<- chisq.test(chi.Environ)
chi.test5$observed
chi.test5$expected
chi.test5$residuals
sum((chi.test5$residuals)^2)
print(chi.test5)

#6.Attrition vs Gender
# ----Summarization
# Comparative Proportion
prop.table(table(mydata$Attrition_label, mydata$Gender), 1)
# ----Visualization
# Contingency Table
chi.Gender <- table(mydata$Attrition_label, mydata$Gender)
# ----ToH
# Chi-Square test
chi.test6<- chisq.test(chi.Gender)
chi.test6$observed
chi.test6$expected
chi.test6$residuals
sum((chi.test6$residuals)^2)
print(chi.test6)


#7.Attrition vs JobInvolvement
# ----Summarization
# Comparative Proportion
prop.table(table(mydata$Attrition_label, mydata$JobInvolvement_label), 1)
# ----Visualization
# Contingency Table
chi.JobInvolvement <- table(mydata$Attrition_label, mydata$JobInvolvement_label)
# ----ToH
# Chi-Square test
chi.test7<- chisq.test(chi.JobInvolvement)
chi.test7$observed
chi.test7$expected
chi.test7$residuals
sum((chi.test7$residuals)^2)
print(chi.test7)

#8.Attrition vs JobLevel
# ----Summarization
# Comparative Proportion
prop.table(table(mydata$Attrition, mydata$JobLevel), 1)
# ----Visualization
# Contingency Table
chi.JobLevel <- table(mydata$Attrition, mydata$JobLevel)
# ----ToH
# Chi-Square test
chi.test8<- chisq.test(chi.JobLevel)
chi.test8$observed
chi.test8$expected
chi.test8$residuals
sum((chi.test8$residuals)^2)
print(chi.test8)

#9.Attrition vs JobRole
prop.table(table(mydata$Attrition_label, mydata$JobRole_label), 1)
# ----Visualization
# Contingency Table
chi.JobRole <- table(mydata$Attrition_label, mydata$JobRole_label)
# ----ToH
# Chi-Square test
chi.test9<- chisq.test(chi.JobRole)
chi.test9$observed
chi.test9$expected
chi.test9$residuals
sum((chi.test9$residuals)^2)
print(chi.test9)


#10.Attrition vs JobSatisfaction
prop.table(table(mydata$Attrition_label, mydata$JobSatisfaction), 1)
# ----Visualization
# Contingency Table
chi.JobSatisfaction <- table(mydata$Attrition_label, mydata$JobSatisfaction)
# ----ToH
# Chi-Square test
chi.test10 <- chisq.test(chi.JobSatisfaction)
chi.test10$observed
chi.test10$expected
chi.test10$residuals
sum((chi.test10$residuals)^2)
print(chi.test10)

#11.Attrition vs MaritalStatus
prop.table(table(mydata$Attrition_label, mydata$MaritalStatus), 1)
# ----Visualization
# Contingency Table
chi.MaritalStatus <- table(mydata$Attrition_label, mydata$MaritalStatus)
# ----ToH
# Chi-Square test
chi.test11 <- chisq.test(chi.MaritalStatus)
chi.test11$observed
chi.test11$expected
chi.test11$residuals
sum((chi.test11$residuals)^2)
print(chi.test11)

#12.Attrition vs Over18
# Not significant

#13.Attrition vs OverTime
prop.table(table(mydata$Attrition_label, mydata$OverTime), 1)
# ----Visualization
# Contingency Table
chi.OverTime <- table(mydata$Attrition_label, mydata$OverTime)
# ----ToH
# Chi-Square test
chi.test13 <- chisq.test(chi.OverTime)
chi.test13$observed
chi.test13$expected
chi.test13$residuals
sum((chi.test13$residuals)^2)
print(chi.test13)

#14.Attrition vs PerformanceRating
prop.table(table(mydata$Attrition_label, mydata$PerformanceRating), 1)
# ----Visualization
# Contingency Table
chi.PerformanceRating <- table(mydata$Attrition_label, mydata$PerformanceRating)
# ----ToH
# Chi-Square test
chi.test14 <- chisq.test(chi.PerformanceRating)
chi.test14$observed
chi.test14$expected
chi.test14$residuals
sum((chi.test14$residuals)^2)
print(chi.test14)


#15.Attrition vs RelationshipSatisfaction
prop.table(table(mydata$Attrition_label, mydata$RelationshipSatisfaction), 1)
# ----Visualization
# Contingency Table
chi.RelationshipSatisfaction <- table(mydata$Attrition_label, mydata$RelationshipSatisfaction)
# ----ToH
# Chi-Square test
chi.test15 <- chisq.test(chi.RelationshipSatisfaction)
chi.test15$observed
chi.test15$expected
chi.test15$residuals
sum((chi.test15$residuals)^2)
print(chi.test15)

#16.Attrition vs StockOptionLevel
prop.table(table(mydata$Attrition_label, mydata$StockOptionLevel), 1)
# ----Visualization
# Contingency Table
chi.StockOptionLevel <- table(mydata$Attrition_label, mydata$StockOptionLevel)
# ----ToH
# Chi-Square test
chi.test16 <- chisq.test(chi.StockOptionLevel)
chi.test16$observed
chi.test16$expected
chi.test16$residuals
sum((chi.test16$residuals)^2)
print(chi.test16)

#17.Attrition vs WorkLifeBalance
prop.table(table(mydata$Attrition_label, mydata$WorkLifeBalance), 1)
# ----Visualization
# Contingency Table
chi.WorkLifeBalance <- table(mydata$Attrition_label, mydata$WorkLifeBalance)
# ----ToH
# Chi-Square test
chi.test17 <- chisq.test(chi.WorkLifeBalance)
chi.test17$observed
chi.test17$expected
chi.test17$residuals
sum((chi.test17$residuals)^2)
print(chi.test17)

#B. C --> Q

#1.Attrition vs Age
#2.Attrition vs DailyRate
#3.Attrition vs DistanceFromHome
#4.Attrition vs HourlyRate
#5.Attrition vs MonthlyIncome
#6.Attrition vs MonthlyRate
#7.Attrition vs NumCompaniesWorked
#8.Attrition vs PercentSalaryHike
#9.Attrition vs TotalWorkingYears
#10.Attrition vs TrainingTimesLastYear
#11.Attrition vs YearsAtCompany
#12.Attrition vs YearsInCurrentRole
#13.Attrition vs YearsSinceLastPromotion
#14.Attrition vs YearsWithCurrManager


#1.Attrition vs Age
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$Age), 1)
# ----Visualization
boxplot(mydata$Age~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$Age~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 1.38e-08, Null Hypothesis is rejected 
# Attrition is dependent on Age with 95% confidence

#2.Attrition vs DailyRate
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$DailyRate), 1)
# ----Visualization
boxplot(mydata$DailyRate~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$DailyRate~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 0.03004, Null Hypothesis is accepted 
# Attrition is independent on Age with 95% confidence

#3.Attrition vs DistanceFromHome
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$DistanceFromHome), 1)
# ----Visualization
boxplot(mydata$DistanceFromHome~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$DistanceFromHome~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 0.004137, Null Hypothesis is rejected 
# Attrition is dependednt on Distancefrom home with 95% confidence

#4.Attrition vs HourlyRate
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$HourlyRate), 1)
# ----Visualization
boxplot(mydata$HourlyRate~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$HourlyRate~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 0.7914, Null Hypothesis is accepted 
# Attrition is independednt on Hourly Rate with 95% confidence

#5.Attrition vs MonthlyIncome
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$MonthlyIncome), 1)
# ----Visualization
boxplot(mydata$MonthlyIncome~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$MonthlyIncome~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 4.434e-13, Null Hypothesis is rejected 
# Attrition is dependent on Monthly Income with 95% confidence

#6.Attrition vs MonthlyRate
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$MonthlyRate), 1)
# ----Visualization
boxplot(mydata$MonthlyRate~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$MonthlyRate~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 0.5653, Null Hypothesis is accepted 
# Attrition is independent on Monthly Rate with 95% confidence

#7.Attrition vs NumCompaniesWorked
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$NumCompaniesWorked), 1)
# ----Visualization
boxplot(mydata$NumCompaniesWorked~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$NumCompaniesWorked~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 0.1163, Null Hypothesis is accepted 
# Attrition is independent on Number of Companies worked with 95% confidence


#8.Attrition vs PercentSalaryHike
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$PercentSalaryHike), 1)
# ----Visualization
boxplot(mydata$PercentSalaryHike~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$PercentSalaryHike~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 0.6144, Null Hypothesis is accepted 
# Attrition is independent on Salary Hike with 95% confidence

#9.Attrition vs TotalWorkingYears
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$TotalWorkingYears), 1)
# ----Visualization
boxplot(mydata$TotalWorkingYears~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$TotalWorkingYears~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 1.16e-11, Null Hypothesis is rejected 
# Attrition is dependent on Total Working Years with 95% confidence

#10.Attrition vs TrainingTimesLastYear
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$TrainingTimesLastYear), 1)
# ----Visualization
boxplot(mydata$TrainingTimesLastYear~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$TrainingTimesLastYear~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 0.02036, Null Hypothesis is rejected 
# Attrition is dependent on Training Times Last Year with 95% confidence

#11.Attrition vs YearsAtCompany
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$YearsAtCompany), 1)
# ----Visualization
boxplot(mydata$YearsAtCompany~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$YearsAtCompany~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 2.286e-07, Null Hypothesis is rejected 
# Attrition is dependent on Years At Company with 95% confidence

#12.Attrition vs YearsInCurrentRole
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$YearsInCurrentRole), 1)
# ----Visualization
boxplot(mydata$YearsInCurrentRole~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$YearsInCurrentRole~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 3.187e-11, Null Hypothesis is rejected 
# Attrition is dependent on Years At Current Role with 95% confidence

#13.Attrition vs YearsSinceLastPromotion
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$YearsSinceLastPromotion), 1)
# ----Visualization
boxplot(mydata$YearsSinceLastPromotion~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$YearsSinceLastPromotion~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 0.1987, Null Hypothesis is Accepted 
# Attrition is independent on Years Since Last Promotion with 95% confidence

#14.Attrition vs YearsWithCurrManager
# ----Summarization
prop.table(table(mydata$Attrition_label, mydata$YearsWithCurrManager), 1)
# ----Visualization
boxplot(mydata$YearsWithCurrManager~mydata$Attrition_label,col = c(2,4))$stats
# ----ToH
# ------2 independent samples
# ------2 samplet test
t.test(mydata$YearsWithCurrManager~mydata$Attrition_label,alternative = "two.sided",conf.level = 0.95)
# p-value = 1.185e-10 , Null Hypothesis is Rejected 
# Attrition is dependent on Years With CurrManager with 95% confidence

#3. Q --> C


#D. Q --> Q ( Salary//Monthly Income  )

# 1.Age vs Monthly Income
# 2.DailyRate vs Monthly Income
# 3.DistanceFromHome vs Monthly Income
# 4.HourlyRate vs Monthly Income
# 5.MonthlyIncome vs Monthly Income 
# 6.MonthlyRate vs Monthly Income
# 7.NumCompaniesWorked vs Monthly Income 
# 8.PercentSalaryHike vs Monthly Income
# 9.TotalWorkingYears vs Monthly Income
# 10TrainingTimesLastYear vs Monthly Income
# 11.YearsAtCompany vs Monthly Income
# 12.YearsInCurrentRole vs Monthly Income
# 13.YearsSinceLastPromotion vs Monthly Income
# 14.YearsWithCurrManager vs Monthly Income


# 1.Age vs Monthly Income
# ----Summarization
# Correlation Coefficient
cor(mydata$Age,mydata$MonthlyIncome)
# ----Visualization
# Scatter plot
plot(mydata$Age,mydata$MonthlyIncome)
# Stratified Scatter plot
plot(mydata$Age, mydata$MonthlyIncome, pch = as.numeric(mydata$Age) + 15,
     col = mydata$Age)
# Matrix plot
names(mydata)
#pairs(mydata[,c(2,4,6,13,19)]) #20,21,24,29,30,32,33,34,35)])
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm1 <- lm(Age~MonthlyIncome, data = mydata)
summary(attrition.lm1)
# p-value < 0.05 , 2.2e-16 
# Ha hypothesis is accepted,
# Monthly Income is dependent on Age

# 2.DailyRate vs Monthly Income
# Scatter plot
plot(mydata$DailyRate,mydata$MonthlyIncome)
# Stratified Scatter plot
plot(mydata$DailyRate, mydata$MonthlyIncome, pch = as.numeric(mydata$DailyRate) + 15,
     col = mydata$DailyRate)
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm2 <- lm(DailyRate~MonthlyIncome, data = mydata)
summary(attrition.lm2)
# p-value = 0.7678
# Ho hypothesis is accepted,
# Monthly Income is independent on DailyRate

# 3.DistanceFromHome vs Monthly Income
# Scatter plot
plot(mydata$DistanceFromHome,mydata$MonthlyIncome)
# Stratified Scatter plot
plot(mydata$DistanceFromHome, mydata$MonthlyIncome, pch = as.numeric(mydata$DistanceFromHome) + 15,
     col = mydata$DistanceFromHome)
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm3 <- lm(DistanceFromHome~MonthlyIncome, data = mydata)
summary(attrition.lm3)
# p-value = 0.5145
# Ho hypothesis is accepted,
# Monthly Income is independent on DistanceFromHome

# 4.HourlyRate vs Monthly Income
# Scatter plot
plot(mydata$HourlyRate,mydata$MonthlyIncome)
# Stratified Scatter plot
plot(mydata$HourlyRate, mydata$MonthlyIncome, pch = as.numeric(mydata$HourlyRate) + 15,
     col = mydata$HourlyRate)
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm4 <- lm(HourlyRate~MonthlyIncome, data = mydata)
summary(attrition.lm4)
# p-value = 0.5451
# Ho hypothesis is rejected,
# Monthly Income is independent on Hourly Rate

# 5.MonthlyIncome vs Monthly Income 
#NA 

# 6.MonthlyRate vs Monthly Income
# Scatter plot
plot(mydata$MonthlyRate,mydata$MonthlyIncome)
# Stratified Scatter plot
#plot(mydata$MonthlyRate, mydata$MonthlyIncome, pch = as.numeric(mydata$MonthlyRate) +15,
#     col = mydata$MonthlyRate)
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm6 <- lm(MonthlyRate~MonthlyIncome, data = mydata)
summary(attrition.lm6)
# p-value: 0.1822
# Ho hypothesis is accepted,
# Monthly Income is independent on MOnthly rate.

# 7.NumCompaniesWorked vs Monthly Income
# Scatter plot
plot(mydata$NumCompaniesWorked,mydata$MonthlyIncome)
# Stratified Scatter plot
plot(mydata$NumCompaniesWorked, mydata$MonthlyIncome, pch = as.numeric(mydata$NumCompaniesWorked) + 15,
     col = mydata$NumCompaniesWorked)
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm7 <- lm(NumCompaniesWorked~MonthlyIncome, data = mydata)
summary(attrition.lm7)
# p-value = 8.41e-09
# Ho hypothesis is rejected,
# Monthly Income is dependent on Num Companies Worked

# 8.PercentSalaryHike vs Monthly Income
# Scatter plot
plot(mydata$PercentSalaryHike,mydata$MonthlyIncome)
# Stratified Scatter plot
plot(mydata$PercentSalaryHike, mydata$MonthlyIncome, pch = as.numeric(mydata$PercentSalaryHike) + 15,
     col = mydata$PercentSalaryHike)
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm8 <- lm(PercentSalaryHike~MonthlyIncome, data = mydata)
summary(attrition.lm8)
# p-value = 0.2961
# Ho hypothesis is accepted,
# Monthly Income is independent on Percent Salary Hike

# 9.TotalWorkingYears vs Monthly Income
# Scatter plot
plot(mydata$TotalWorkingYears,mydata$MonthlyIncome)
# Stratified Scatter plot
plot(mydata$TotalWorkingYears, mydata$MonthlyIncome, pch = as.numeric(mydata$TotalWorkingYears) + 15,
     col = mydata$TotalWorkingYears)
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm9 <- lm(TotalWorkingYears~MonthlyIncome, data = mydata)
summary(attrition.lm9)
# p-value = 2.2e-16
# Ho hypothesis is rejected,
# Monthly Income is dependent on Total Working years


# 10.TrainingTimesLastYear vs Monthly Income
# Scatter plot
plot(mydata$TrainingTimesLastYear,mydata$MonthlyIncome)
# Stratified Scatter plot
plot(mydata$TrainingTimesLastYear, mydata$MonthlyIncome, pch = as.numeric(mydata$TrainingTimesLastYear) + 15,
     col = mydata$TrainingTimesLastYear)
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm10 <- lm(TrainingTimesLastYear~MonthlyIncome, data = mydata)
summary(attrition.lm10)
# p-value = 0.405
# Ho hypothesis is accepted,
# Monthly Income is independent on Training Times Last Year

# 11.YearsAtCompany vs Monthly Income
# Scatter plot
plot(mydata$YearsAtCompany,mydata$MonthlyIncome)
# Stratified Scatter plot
plot(mydata$YearsAtCompany, mydata$MonthlyIncome, pch = as.numeric(mydata$YearsAtCompany) + 15,
     col = mydata$YearsAtCompany)
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm11 <- lm(YearsAtCompany~MonthlyIncome, data = mydata)
summary(attrition.lm11)
# p-value = 2.2e-16
# Ho hypothesis is rejected,
# Monthly Income is dependent on Years At Company

# 12.YearsInCurrentRole vs Monthly Income
# Scatter plot
plot(mydata$YearsInCurrentRole,mydata$MonthlyIncome)
# Stratified Scatter plot
plot(mydata$YearsInCurrentRole, mydata$MonthlyIncome, pch = as.numeric(mydata$YearsInCurrentRole) + 15,
     col = mydata$YearsInCurrentRole)
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm12 <- lm(YearsInCurrentRole~MonthlyIncome, data = mydata)
summary(attrition.lm12)
# p-value = 2.2e-16
# Ho hypothesis is rejected,
# Monthly Income is dependent on Years In current role

# 13.YearsSinceLastPromotion vs Monthly Income
# Scatter plot
plot(mydata$YearsSinceLastPromotion,mydata$MonthlyIncome)
# Stratified Scatter plot
plot(mydata$YearsSinceLastPromotion, mydata$MonthlyIncome, pch = as.numeric(mydata$YearsSinceLastPromotion) + 15,
     col = mydata$YearsSinceLastPromotion)
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm13 <- lm(YearsSinceLastPromotion~MonthlyIncome, data = mydata)
summary(attrition.lm13)
# p-value = 2.2e-16
# Ho hypothesis is rejected,
# Monthly Income is dependent on Years Since last promotion

# 14.YearsWithCurrManager vs Monthly Income
# Scatter plot
plot(mydata$YearsWithCurrManager,mydata$MonthlyIncome)
# Stratified Scatter plot
plot(mydata$YearsWithCurrManager, mydata$MonthlyIncome, pch = as.numeric(mydata$YearsWithCurrManager) + 15,
     col = mydata$YearsWithCurrManager)
# ----ToH
# t test for population slope
# y = beta0 + beta1*X
# Ho:beat1 = 0 ;  Ha: beta1 <> 0
attrition.lm14 <- lm(YearsWithCurrManager~MonthlyIncome, data = mydata)
summary(attrition.lm14)
# p-value = 2.2e-16
# Ho hypothesis is rejected,
# Monthly Income is dependent on Years With Current Manager
