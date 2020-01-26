

                        ##############################################

                                       #ATTRITION STUDY


                        ##############################################
                        
setwd("C://Users//Sourabh Kushwaha//Documents//Attrition")
library(xlsx)

# as there are multiple sheets & we have to work on sheet 1, hence sheet index = 1
mydata <- read.xlsx("Attrition.xlsx", sheetIndex = 1) 
str(mydata)

########################

#Step 1

########################

# Here we will check the Head & columns of the data which was imported earier.
# Also we will check that which column is Categorical or Quantatative.

str(mydata)

# As observed in the data Out of 35 rows, the following columns were found to be as Categorical :
#Attrition, BusinessTravel,Department, Education, Educationfeild, EnvironmentSatisfaction
#Gender, JobInvolvement, JobLevel, JobRole,JobSatisfaction, Marital Status, 
#Overtime, PeformanceRating,RelationshipSatisfaction,StockOptionLevel,WorkLifeBalance


#Also the follwing rows can be ignored as single data.
#Employee Count, Employee number, Over18, Standard hours, Employee number,
#

#Convert the data type to Categorical & labelling will be done accordinly.

mydata$Attrition <- as.factor(mydata$Attrition)
mydata$Education <- as.factor(mydata$Education)
mydata$EnvironmentSatisfaction <- as.factor(mydata$EnvironmentSatisfaction)
mydata$JobLevel <- as.factor(mydata$JobLevel)
mydata$JobSatisfaction <- as.factor(mydata$JobSatisfaction)
mydata$JobInvolvement <- as.factor(mydata$JobInvolvement)
mydata$PerformanceRating <- as.factor(mydata$PerformanceRating)
mydata$RelationshipSatisfaction <- as.factor(mydata$RelationshipSatisfaction)
mydata$StockOptionLevel <- as.factor(mydata$StockOptionLevel)
mydata$WorkLifeBalance <- as.factor(mydata$WorkLifeBalance)


# We will now rename the factors for Categorical data and leave the  Quantatative as it is.
mydata$Attrition_label <- factor(mydata$Attrition, labels = c("Not.Left","Left")) #for left & not left
mydata$BusinessTravel_label <- factor(mydata$BusinessTravel, labels = c("No","Less.Trav","Lot.Trav"))
mydata$Department_label <- factor(mydata$Department, labels = c("H.R","R&D","Sales"))
mydata$EnvironmentSatisfaction_label <- factor(mydata$EnvironmentSatisfaction, labels = c("Bad","Average","Good","V.G"))
mydata$Education_label <- factor(mydata$Education, labels = c("12th","Diploma","Grad","Masters","Ph.D"))
mydata$EducationField_label <- factor(mydata$EducationField, labels = c("H.R","Life.Sci","Marketing","Medical","Other","Tech.Deg"))
mydata$JobInvolvement_label <- factor(mydata$JobInvolvement, labels = c("Low","Mod","High","V.High"))
mydata$JobRole_label <- factor(mydata$JobRole, labels = c("Health.rep","H.R","Lab.Tech","Manager","Manufac.Dir","Research.Dir","Research.Sci","Sales.Exe","Sales.Rep"))
mydata$WorkLifeBalance_label <- factor(mydata$WorkLifeBalance, labels = c("Lowest","Moderate","Good","Highest"))
  
str(mydata)

########################

#Step 2 - EDA 

########################
# To visualize & summarize data we will perform EDA , based on univariate and bi-variate
#1. Univariate - Y (Attrition) vs all X ( Factors )

### We will take all the Categorical data first & perform Summarization & visualization
#Attrition, BusinessTravel,Department, Education, Educationfeild, EnvironmentSatisfaction
#Gender, JobInvolvement, JobLevel, JobRole,JobSatisfaction, Marital Status, 
#Overtime, PeformanceRating,RelationshipSatisfaction,StockOptionLevel,WorkLifeBalance

#A CATEGORICAL 


# 1.A.1- Attrition 
   #a.Summarization ( count,%, proportion, mode )
table(mydata$Attrition_label) #count 
prop.table(table(mydata$Attrition_label)) #proportion
round(prop.table(table(mydata$Attrition_label)) * 100,2) #pecentage
names(which.max(table(mydata$Attrition_label)))   #mode
   #b.Visualization (pie chart, bar chart) 
attrition.matrix <- as.matrix(table(mydata$Attrition_label))
barplot(attrition.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of Attrition",
        xlab = "Attrition", 
        ylab = "No. Of Employees",
        #        names.arg = c("A","N"),
        names.arg = levels(mydata$Attrition_label),
        col = c("red",3))
pie(attrition.matrix, radius = 1,
    main = "Piechart of Attrition",
    col = c("red","blue"),
    labels = levels(mydata$Attrition_label),
    clockwise = TRUE,init.angle = 0)


#1.A.2-BusinessTravel  
   #a.Summarization ( count,%, proportion, mode )
table(mydata$BusinessTravel_label) #count 
prop.table(table(mydata$BusinessTravel_label)) #proportion
round(prop.table(table(mydata$BusinessTravel_label)) * 100,2) #pecentage
names(which.max(table(mydata$BusinessTravel_label)))   #mode
   #b.Visualization (pie chart, bar chart) 
BusinessTravel.matrix <- as.matrix(table(mydata$BusinessTravel_label))
barplot(BusinessTravel.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of BussinessTravel",
        xlab = "BussinessTravel", 
        ylab = "No. Of Employees",
        names.arg = levels(mydata$BusinessTravel_label),
        col = c("green","cyan","blue"))
pie(BusinessTravel.matrix, radius = 1,
    main = "Piechart of Bussiness",
    col = c("red","blue","green"),
    labels = levels(mydata$BusinessTravel_label),
    clockwise = TRUE,init.angle = 0)

#1.A.3-Department  
  #a.Summarization ( count,%, proportion, mode )
table(mydata$Department_label) #count 
prop.table(table(mydata$Department_label)) #proportion
round(prop.table(table(mydata$Department_label)) * 100,2) #pecentage
names(which.max(table(mydata$Department_label)))   #mode
 #b.Visualization (pie chart, bar chart) 
Department.matrix <- as.matrix(table(mydata$Department_label))
barplot(Department.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of Department",
        xlab = "Department", 
        ylab = "No. Of Employees",
        names.arg = levels(mydata$Department_label),
        col = c("black","maroon","cyan"))
pie(Department.matrix, radius = 1,
    main = "Piechart of Department",
    col = c("black","maroon","cyan"),
    labels = levels(mydata$Department_label),
    clockwise = TRUE,init.angle = 0)

#1.A.4-Education,   
    #a.Summarization ( count,%, proportion, mode )
table(mydata$Education_label) #count 
prop.table(table(mydata$Education_label)) #proportion
round(prop.table(table(mydata$Education_label)) * 100,2) #pecentage
names(which.max(table(mydata$Education_label)))   #mode
   #b.Visualization (pie chart, bar chart) 
Department.matrix <- as.matrix(table(mydata$Department_label))
barplot(Department.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of Department",
        xlab = "Department", 
        ylab = "No. Of Employees",
        names.arg = levels(mydata$Department_label),
        col = c("black","maroon","cyan"))
pie(Department.matrix, radius = 1,
    main = "Piechart of Department",
    col = c("black","maroon","cyan"),
    labels = levels(mydata$Department_label),
    clockwise = TRUE,init.angle = 0)

#1.A.5-Educationfeild,   
    #a.Summarization ( count,%, proportion, mode )
table(mydata$EducationField_label) #count 
prop.table(table(mydata$EducationField_label)) #proportion
round(prop.table(table(mydata$EducationField_label)) * 100,2) #pecentage
names(which.max(table(mydata$EducationField_label)))   #mode
    #b.Visualization (pie chart, bar chart) 
EducationField.matrix <- as.matrix(table(mydata$EducationField_label))
barplot(EducationField.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of EducationField",
        xlab = "EducationField", 
        ylab = "No. Of Employees",
        names.arg = levels(mydata$EducationField_label),
        col = c("black","maroon","cyan","green","blue","red"))
 

#1.A.6-EnvironmentSatisfaction,   
#a.Summarization ( count,%, proportion, mode )
table(mydata$EnvironmentSatisfaction_label) #count 
prop.table(table(mydata$EnvironmentSatisfaction_label)) #proportion
round(prop.table(table(mydata$EnvironmentSatisfaction_label)) * 100,2) #pecentage
names(which.max(table(mydata$EnvironmentSatisfaction_label)))   #mode
#b.Visualization (pie chart, bar chart) 
EnvironmentSatisfaction.matrix <- as.matrix(table(mydata$EnvironmentSatisfaction_label))
barplot(EnvironmentSatisfaction.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of EnvironmentSatisfaction",
        xlab = "EnvironmentSatisfaction", 
        ylab = "No. Of Employees",
        names.arg = levels(mydata$EnvironmentSatisfaction_label),
        col = c("black","maroon","cyan","green","blue","red"))
pie(EnvironmentSatisfaction.matrix, radius = 1,
    main = "Piechart of EnvironmentSatisfaction",
    col = c("black","maroon","cyan","green"),
    labels = levels(mydata$EnvironmentSatisfaction_label),
    clockwise = TRUE,init.angle = 0)

#1.A.7- Gender
      #a.Summarization ( count,%, proportion, mode )
table(mydata$Gender) #count 
prop.table(table(mydata$Gender)) #proportion
round(prop.table(table(mydata$Gender)) * 100,2) #pecentage
names(which.max(table(mydata$Gender)))   #mode
      #b.Visualization (pie chart, bar chart) 
Gender.matrix <- as.matrix(table(mydata$Gender))
barplot(Gender.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of Gender",
        xlab = "Gender", 
        ylab = "No. Of Employees",
        names.arg = levels(mydata$Gender),
        col = c("black","maroon"))
pie(Gender.matrix, radius = 1,
    main = "Piechart of Gender",
    col = c("black","maroon"),
    labels = levels(mydata$Gender),
    clockwise = TRUE,init.angle = 0)

#1.A.8- JobInvolvement
    #a.Summarization ( count,%, proportion, mode )
table(mydata$JobInvolvement_label) #count 
prop.table(table(mydata$JobInvolvement_label)) #proportion
round(prop.table(table(mydata$JobInvolvement_label)) * 100,2) #pecentage
names(which.max(table(mydata$JobInvolvement_label)))   #mode
   #b.Visualization (pie chart, bar chart) 
JobInvolvement.matrix <- as.matrix(table(mydata$JobInvolvement_label))
barplot(JobInvolvement.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of JobInvolvement",
        xlab = "JobInvolvement", 
        ylab = "No. Of Employees",
        names.arg = levels(mydata$JobInvolvement_label),
        col = c("black","maroon","blue","green"))
pie(JobInvolvement.matrix, radius = 1,
    main = "Piechart of Jobinvolvement",
    col = c("black","maroon","blue","green"),
    labels = levels(mydata$JobInvolvement_label),
    clockwise = TRUE,init.angle = 0)

#1.A.9- JobLevel
      #a.Summarization ( count,%, proportion, mode )
table(mydata$JobLevel) #count 
prop.table(table(mydata$JobLevel)) #proportion
round(prop.table(table(mydata$JobLevel)) * 100,2) #pecentage
names(which.max(table(mydata$JobLevel)))   #mode
      #b.Visualization (pie chart, bar chart) 
JobLevel.matrix <- as.matrix(table(mydata$JobLevel))
barplot(JobLevel.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of JobLevel",
        xlab = "JobLevel", 
        ylab = "No. Of Employees",
        names.arg = levels(mydata$JobLevel),
        col = c("red","cyan","black","green","grey"))
pie(JobLevel.matrix, radius = 1,
    main = "Piechart of JobLevel",
    col = c("red","cyan","black","green","grey"),
    labels = levels(mydata$JobLevel),
    clockwise = TRUE,init.angle = 0)

#1.A.10- JobRole
#a.Summarization ( count,%, proportion, mode )
table(mydata$JobRole_label) #count 
prop.table(table(mydata$JobRole_label)) #proportion
round(prop.table(table(mydata$JobRole_label)) * 100,2) #pecentage
names(which.max(table(mydata$JobRole_label)))   #mode
#b.Visualization (pie chart, bar chart) 
Jobrole.matrix <- as.matrix(table(mydata$JobRole_label))
barplot(Jobrole.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of Jobrole",
        xlab = "JobLevel", 
        ylab = "No. Of Employees",
        names.arg = levels(mydata$JobRole_label),
        col = c("red","cyan","black","green","grey","white","purple","maroon","blue"))
pie(Jobrole.matrix, radius = 1,
    main = "Piechart of Jobrole",
    col = c("red","cyan","black","green","grey","white","purple","maroon","blue"),
    labels = levels(mydata$JobRole_label),
    clockwise = TRUE,init.angle = 0)

#1.A.11- Job Satisfaction
#a.Summarization ( count,%, proportion, mode )
table(mydata$JobSatisfaction) #count 
prop.table(table(mydata$JobSatisfaction)) #proportion
round(prop.table(table(mydata$JobSatisfaction)) * 100,2) #pecentage
names(which.max(table(mydata$JobSatisfaction)))   #mode
#b.Visualization (pie chart, bar chart) 
JobSatisfaction.matrix <- as.matrix(table(mydata$JobSatisfaction))
barplot(JobSatisfaction.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of Job Satisfaction",
        xlab = "JobLevel", 
        ylab = "No. Of Employees",
        names.arg = levels(mydata$JobSatisfaction),
        col = c("red","cyan","black","green"))
pie(JobSatisfaction.matrix, radius = 1,
    main = "Rating of JobSatisfaction",
    col = c("red","cyan","black","green"),
    labels = levels(mydata$JobSatisfaction),
    clockwise = TRUE,init.angle = 0)

#1.A.12- Marital Status 
#a.Summarization ( count,%, proportion, mode )
table(mydata$MaritalStatus) #count 
prop.table(table(mydata$MaritalStatus)) #proportion
round(prop.table(table(mydata$MaritalStatus)) * 100,2) #pecentage
names(which.max(table(mydata$MaritalStatus)))   #mode
#b.Visualization (pie chart, bar chart) 
MaritalStatus.matrix <- as.matrix(table(mydata$MaritalStatus))
barplot(MaritalStatus.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of Marital Status",
        xlab = "MaritalStatus", 
        ylab = "No. Of Employees",
        names.arg = levels(mydata$MaritalStatus),
        col = c("red","black","green"))
pie(MaritalStatus.matrix, radius = 1,
    main = "Piechart of Marital Status",
    col = c("red","black","green"),
    labels = levels(mydata$MaritalStatus),
    clockwise = TRUE,init.angle = 0)


#1.A.13- Overtime 
#a.Summarization ( count,%, proportion, mode )
table(mydata$OverTime) #count 
prop.table(table(mydata$OverTime)) #proportion
round(prop.table(table(mydata$OverTime)) * 100,2) #pecentage
names(which.max(table(mydata$OverTime)))   #mode
#b.Visualization (pie chart, bar chart) 
Overtime.matrix <- as.matrix(table(mydata$OverTime))
barplot(Overtime.matrix,  beside = TRUE, 
        space = 0.5,
        main = "Barplot of Overtime",
        xlab = "Overtime", 
        ylab = "No. Of Employees",
        names.arg = levels(mydata$OverTime),
        col = c("red","green"))
pie(Overtime.matrix, radius = 1,
    main = "Piechart of Overtime",
    col = c("red","green"),
    labels = levels(mydata$OverTime),
    clockwise = TRUE,init.angle = 0)

#1.A.14 PeformanceRating
table(mydata$PerformanceRating)
prop.table(table(mydata$PerformanceRating))
round(prop.table(table(mydata$PerformanceRating))*100,2)
names(which.max(table(mydata$PerformanceRating)))
#b.Visualization (pie chart, bar chart) 
performancerating.matrix <- table(mydata$PerformanceRating)
barplot(performancerating.matrix, beside = TRUE,
        space = 0.5,
        main = "Barplot of Performance Rating",
        xlab = "Performance Rating",
        ylab = "No Of Employees",
        col = c("blue","grey"))
pie(performancerating.matrix, radius = 1,
    main = "Pie chart of Performance",
    col = c("blue","grey"),
    labels = levels(mydata$PerformanceRating),
    clockwise = TRUE, init.angle = 0)

#1.A.15 RelationshipSatisfaction
table(mydata$RelationshipSatisfaction)
prop.table(table(mydata$RelationshipSatisfaction))
round(prop.table(table(mydata$RelationshipSatisfaction))*100,2)
names(which.max(table(mydata$RelationshipSatisfaction)))
#b.Visualization (pie chart, bar chart)
RelationshipSatisfaction.matrix <- table(mydata$RelationshipSatisfaction)
barplot(RelationshipSatisfaction.matrix, beside = TRUE,
        space = 0.5,
        main = "Barplot of ReationshipSatisfaction",
        xlab = "Relationshipsatisfaction",
        ylab = "No Of Employees",
        col = c("Red","grey","orange","white"))
pie(RelationshipSatisfaction.matrix, radius = 1,
    clockwise = TRUE, init.angle = 0,
    col=c("light blue","light green","light cyan","light grey"),
    labels = levels(mydata$RelationshipSatisfaction))

#1.A.16 StockOptionLevel
table(mydata$StockOptionLevel)
prop.table(table(mydata$StockOptionLevel))
round(prop.table(table(mydata$StockOptionLevel))*100,2)
names(which.max(table(mydata$StockOptionLevel)))
#b.Visualization (pie chart, bar chart) 
StockOptionLevel.matrix <- table(mydata$StockOptionLevel)
barplot(StockOptionLevel.matrix, beside = TRUE,
        space = 0.5,
        main = "Barplot oof Stock Option",
        xlab = "Stockoption",
        ylab = "No Of Employee",
        col = c("orange","cyan","black","blue"))
pie(StockOptionLevel.matrix, radius = 1,
    clockwise = TRUE, init.angle = 0,
    col = c("orange","cyan","black","blue"),
    labels = levels(mydata$StockOptionLevel))

#1.A.17 WorkLifeBalance
table(mydata$WorkLifeBalance_label)
prop.table(table(mydata$WorkLifeBalance_label))
round(prop.table(table(mydata$WorkLifeBalance_label))*100,2)
names(which.max(table(mydata$WorkLifeBalance_label)))
#b.Visualization (pie chart, bar chart)
WorkLifeBalance_label.matrix <- table(mydata$WorkLifeBalance_label)
barplot(WorkLifeBalance_label.matrix, beside=TRUE,
        main = "Barplot of Work Life Balance",
        xlab = "Ratiing of Work Life Balance",
        ylab = "No of Employees",
        col = c("Grey","white","light green","black"))
pie(WorkLifeBalance_label.matrix, radius = 1,
    col = c("Grey","white","light green","black"),
    labels = levels(mydata$WorkLifeBalance_label),
    init.angle = 0, clockwise = TRUE)


#B.EDA for QUANTATATIVE Data

#1.B.1 Age 
table(is.na(mydata$Age))
mean(mydata$Age, na.rm = TRUE)
median(mydata$Age)
min(mydata$Age)
max(mydata$Age)
range(mydata$Age)
sd(mydata$Age)
var(mydata$Age)
IQR(mydata$Age)
fivenum(mydata$Age)
summary(mydata$Age)
# ----Visualization
png(filename = "Age.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$Age, 
     breaks = c(10,20,30,40,50,60),
     ylim = c(0,700),
     main = "Histogram of Age",
     xlab = "Age",
     ylab = "No of Employee",
     col = 1:8)
abline(v = mean(mydata$Age), col = "blue")
text(36,400,"mean", col="blue")
abline(v = median(mydata$Age), col = "green")
text(38, 500, "mean+2sd", col = "green")
boxplot(mydata$Age,
        main = "Boxplot of Age",
        xlab = "Age",ylab = "No of Employee")
dotchart(mydata$Age,
         main = "Dotchat of Age",
         xlab = "Age",ylab = "No of Employee",
         col = 1:8)
plot(density(mydata$Age),
     main = "Density plot of Age",
     xlab = "Age",
     col = 1:8)   
dev.off()
boxplot(mydata$Age, col = "red")$stats
boxplot(mydata$Age, col = "red")$out

#1.B.2 DailyRate
table(is.na(mydata$DailyRate))
mean(mydata$DailyRate)
median(mydata$DailyRate)
min(mydata$DailyRate)
max(mydata$DailyRate)
range(mydata$DailyRate)
IQR(mydata$DailyRate)
fivenum(mydata$DailyRate)
# ----Visualization
png(filename = "Daily Rate.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$DailyRate,
     main = "Histogram of DailyRate",
     ylim = c(0,140),
     xlab = "DailyRate",
     ylab = "No of Employee",
     col = 1:8)
abline(v = mean(mydata$DailyRate), col = "Black")
text(800,140,"Mean = 802", col = "Black")
text(900,120,"Median = 802.4", col = "Blue")
boxplot(mydata$DailyRate,
        main="Box Plot of Daily Rate",
        xlab="Daily Rate",
        ylab="No Of Employee",
        ylim = c(0,1600),
        col = "Red")
dotchart(mydata$DailyRate,
         main = "Dotchat of Daily Rate",
         xlab = "Daily Rate",ylab = "No of Employee",
         ylim = c(0,1600),
         col = 1:8)
plot(density(mydata$DailyRate),
     main="Density Plot of Daily Rate",
     xlab="Daily Rate")
dev.off()
boxplot(mydata$DailyRate, col = "red")$stats
boxplot(mydata$DailyRate, col = "red")$out    

#1.B.3 Distance from home 
table(is.na(mydata$DistanceFromHome))
mean(mydata$DistanceFromHome, na.rm = TRUE)
median(mydata$DistanceFromHome)
min(mydata$DistanceFromHome)
max(mydata$DistanceFromHome)
range(mydata$DistanceFromHome)
sd(mydata$DistanceFromHome)
var(mydata$DistanceFromHome)
IQR(mydata$DistanceFromHome)
fivenum(mydata$DistanceFromHome)
summary(mydata$DistanceFromHome)
#-------visualization
png(filename = "DistanceFromHome.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$DistanceFromHome,
     main = "Histogram of Distance From Home",
     ylim = c(0,500),
     xlab = "DistanceFromHome",
     ylab = "No of Employee",
     col = 1:8)
abline(v = mean(mydata$DistanceFromHome), col = "Blue")
text(10,300,"Mean = 9.2", col = "Blue")
abline(v = median(mydata$DistanceFromHome), col = "Black")
text(7,400,"Median = 7", col = "Black")
boxplot(mydata$DistanceFromHome,
        main="Box Plot of DistanceFromHome",
        xlab="DistanceFromHome",
        col = "Red")
dotchart(mydata$DistanceFromHome,
         main = "Dotchat of DistanceFromHome",
         xlab = "Daily Rate",ylab = "No of Employee",
         col = 1:8)
plot(density(mydata$DistanceFromHome),
     main="Density Plot of DistanceFromHome",
     xlab="DistanceFromHome")
dev.off()
boxplot(mydata$DistanceFromHome, col = "red")$stats
boxplot(mydata$DistanceFromHome, col = "red")$out      

#1.B.4 Employee count 
table(is.na(mydata$EmployeeCount))
mean(mydata$EmployeeCount, na.rm = TRUE)
range(mydata$EmployeeCount)
#data of Employee count not required as it is representing a single entity.

#1.B.5 Employee number
table(is.na(mydata$EmployeeNumber))
mean(mydata$EmployeeNumber, na.rm = TRUE)
range(mydata$EmployeeNumber)
# eployee number doesn't have any significant existance 

#1.B.6 Hourly rate 
table(is.na(mydata$HourlyRate))
mean(mydata$HourlyRate)
median(mydata$HourlyRate)
min(mydata$HourlyRate)
max(mydata$HourlyRate)
range(mydata$HourlyRate)
IQR(mydata$HourlyRate)
fivenum(mydata$HourlyRate)
#------visualization 
png(filename = "Hourlyrate.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$HourlyRate,
     main = "Histogram of Hourly Rate",
     xlab = "Hourly Rate",
     ylab = "No of employee",
     ylim = c(0,140),
     col = 1:8)
abline(v = mean(mydata$HourlyRate))
text(65,130,"Mean = 65.89")
  boxplot(mydata$HourlyRate,
          main="Box Plot of HourlyRate",
          xlab="HourlyRate",
          ylab="No of Employee",
          col = "Red")
dotchart(mydata$HourlyRate,
         main = "Dotchat of HourlyRate",
         xlab = "HourlyRate",ylab = "No of Employee",
         col=1:10)
plot(density(mydata$HourlyRate),
     main="Density Plot of HourlyRate",
     xlab="HourlyRate")
dev.off()
boxplot(mydata$HourlyRate, col = "red")$stats
boxplot(mydata$HourlyRate, col = "red")$out 


#1.B.7 Monthly Income
table(is.na(mydata$MonthlyIncome))
mean(mydata$MonthlyIncome)
median(mydata$MonthlyIncome)
min(mydata$MonthlyIncome)
max(mydata$MonthlyIncome)
range(mydata$MonthlyIncome)
IQR(mydata$MonthlyIncome)
fivenum(mydata$MonthlyIncome)
#------visualization 
png(filename = "MonthlyIncome.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$MonthlyIncome,
     main = "Histogram of MonthlyIncome",
     xlab = "MonthlyIncome",
     ylab = "No of employee",
     ylim = c(0,600),
     col = 1:8)
abline(v = mean(mydata$MonthlyIncome))
text(10000,600,"Mean = 6502.9")
abline(v = median(mydata$MonthlyIncome, col = "Blue"))
text(4000,400,"Meadian = 4919", col = "blue")
boxplot(mydata$MonthlyIncome,
        main="Box Plot of MonthlyIncome",
        xlab="MonthlyIncome",
        ylab="No of Employee",
        col = "Red")
dotchart(mydata$MonthlyIncome,
         main = "Dotchat of MonthlyIncome",
         xlab = "MonthlyIncome",ylab = "No of Employee",
         col=1:10)
plot(density(mydata$MonthlyIncome),
     main="Density Plot of MonthlyIncome",
     xlab="MonthlyIncome")
dev.off()
boxplot(mydata$MonthlyIncome, col = "red")$stats
boxplot(mydata$MonthlyIncome, col = "red")$out 

#1.B.8 Monthly rate
table(is.na(mydata$MonthlyRate))
mean(mydata$MonthlyRate)
median(mydata$MonthlyRate)
min(mydata$MonthlyRate)
max(mydata$MonthlyRate)
range(mydata$MonthlyRate)
IQR(mydata$MonthlyRate)
fivenum(mydata$MonthlyRate)
#------visualization 
png(filename = "MonthlyRate.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$MonthlyRate,
     main = "Histogram of MonthlyRate",
     xlab = "MonthlyRate",
     ylab = "No of employee",
     ylim = c(0,150),
     col = 1:8)
abline(v = mean(mydata$MonthlyRate))
text(17000,150,"Mean = 14313.3")
boxplot(mydata$MonthlyRate,
        main="Box Plot of MonthlyRate",
        xlab="MonthlyRate",
        ylab="No of Employee",
        col = "Red")
dotchart(mydata$MonthlyRate,
         main = "Dotchat of MonthlyRate",
         xlab = "MonthlyRate",ylab = "No of Employee",
         col=1:10)
plot(density(mydata$MonthlyRate),
     main="Density Plot of MonthlyRate",
     xlab="MonthlyRate")
dev.off()
boxplot(mydata$MonthlyRate, col = "red")$stats
boxplot(mydata$MonthlyRate, col = "red")$out

#1.B.9 NumCompanies worked 
table(is.na(mydata$NumCompaniesWorked))
mean(mydata$NumCompaniesWorked)
median(mydata$NumCompaniesWorked)
min(mydata$NumCompaniesWorked)
max(mydata$NumCompaniesWorked)
range(mydata$NumCompaniesWorked)
IQR(mydata$NumCompaniesWorked)
fivenum(mydata$NumCompaniesWorked)
#------visualization 
png(filename = "NumCompaniesWorked.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$NumCompaniesWorked,
     main = "Histogram of NumCompaniesWorked",
     xlab = "NumCompaniesWorked",
     ylab = "No of employee",
     ylim = c(0,800),
     col = 1:8)
abline(v = mean(mydata$NumCompaniesWorked))
text(4,800,"Mean = 2.69")
abline(v = median(mydata$NumCompaniesWorked),col ="Blue")
text(2,700,"Median = 2",col = "Blue")
boxplot(mydata$NumCompaniesWorked,
        main="Box Plot of NumCompaniesWorked",
        xlab="NumCompaniesWorked",
        ylab="No of Employee",
        col = "Red")
dotchart(mydata$NumCompaniesWorked,
         main = "Dotchat of NumCompaniesWorked",
         xlab = "NumCompaniesWorked",ylab = "No of Employee",
         col=1:10)
plot(density(mydata$NumCompaniesWorked),
     main="Density Plot of NumCompaniesWorked",
     xlab="NumCompaniesWorked")
dev.off()
boxplot(mydata$NumCompaniesWorked, col = "red")$stats
boxplot(mydata$NumCompaniesWorked, col = "red")$out


#1.B.10 PercentageSalaryHike
table(is.na(mydata$PercentSalaryHike))
mean(mydata$PercentSalaryHike)
median(mydata$PercentSalaryHike)
min(mydata$PercentSalaryHike)
max(mydata$PercentSalaryHike)
range(mydata$PercentSalaryHike)
IQR(mydata$PercentSalaryHike)
fivenum(mydata$PercentSalaryHike)
#------visualization 
png(filename = "PercentSalaryHike.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$PercentSalaryHike,
     main = "Histogram of PercentSalaryHike",
     xlab = "PercentSalaryHike",
     ylab = "No of employee",
     ylim = c(0,450),
     col = 1:8)
abline(v = mean(mydata$PercentSalaryHike))
text(16,400,"Mean = 15.2")
boxplot(mydata$PercentSalaryHike,
        main="Box Plot of PercentSalaryHike",
        xlab="PercentSalaryHike",
        ylab="No of Employee",
        col = "Red")
dotchart(mydata$PercentSalaryHike,
         main = "Dotchat of PercentSalaryHike",
         xlab = "PercentSalaryHike",ylab = "No of Employee",
         col=1:10)
plot(density(mydata$PercentSalaryHike),
     main="Density Plot of PercentSalaryHike",
     xlab="PercentSalaryHike")
dev.off()
boxplot(mydata$PercentSalaryHike, col = "red")$stats
boxplot(mydata$PercentSalaryHike, col = "red")$out


#1.B.11 StandardHours 
table(is.na(mydata$StandardHours))
mean(mydata$StandardHours)
median(mydata$StandardHours)
min(mydata$StandardHours)
max(mydata$StandardHours)
#single data , not required || no change in data 

#1.B.12 Total working years
table(is.na(mydata$TotalWorkingYears))
mean(mydata$TotalWorkingYears)
median(mydata$TotalWorkingYears)
min(mydata$TotalWorkingYears)
max(mydata$TotalWorkingYears)
range(mydata$TotalWorkingYears)
IQR(mydata$TotalWorkingYears)
fivenum(mydata$TotalWorkingYears)
#------visualization 
png(filename = "TotalWorkingYears.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$TotalWorkingYears,
     main = "Histogram of TotalWorkingYears",
     xlab = "TotalWorkingYears",
     ylab = "No of employee",
     ylim = c(0,700),
     col = 1:8)
abline(v = mean(mydata$TotalWorkingYears))
text(15,700,"Mean = 11.27")
abline(v= median(mydata$TotalWorkingYears), col = "Blue")
text(10,500,"Median = 10",col = "Blue")
boxplot(mydata$TotalWorkingYears,
        main="Box Plot of TotalWorkingYears",
        xlab="TotalWorkingYears",
        ylab="No of Employee",
        col = "Red")
dotchart(mydata$TotalWorkingYears,
         main = "Dotchat of TotalWorkingYears",
         xlab = "TotalWorkingYears",ylab = "No of Employee",
         col=1:10)
plot(density(mydata$TotalWorkingYears),
     main="Density Plot of TotalWorkingYears",
     xlab="TotalWorkingYears")
dev.off()
boxplot(mydata$TotalWorkingYears, col = "red")$stats
boxplot(mydata$TotalWorkingYears, col = "red")$out


#1.B.13 TrainingTimeLastYear 
table(is.na(mydata$TrainingTimesLastYear))
mean(mydata$TrainingTimesLastYear)
median(mydata$TrainingTimesLastYear)
min(mydata$TrainingTimesLastYear)
max(mydata$TrainingTimesLastYear)
range(mydata$TrainingTimesLastYear)
IQR(mydata$TrainingTimesLastYear)
fivenum(mydata$TrainingTimesLastYear)
#------visualization 
png(filename = "TrainingTimesLastYear.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$TrainingTimesLastYear,
     main = "Histogram of TrainingTimesLastYear",
     xlab = "Training Time Last Year in Days",
     ylab = "No of employee",
     xlim = c(0,6),
     ylim = c(0,600),
     col = 1:7)
abline(v = mean(mydata$TrainingTimesLastYear))
text(3.5,600,"Mean = 2.79")
abline(v= median(mydata$TrainingTimesLastYear), col = "Blue")
text(3.8,500,"Median = 3",col = "Blue")
boxplot(mydata$TrainingTimesLastYear,
        main="Box Plot of TrainingTimeLastYear",
        xlab="TrainingTimeLastYear",
        ylab="No of Employee",
        col = "Red")
dotchart(mydata$TrainingTimesLastYear,
         main = "Dotchat of TrainingTimeLastYear",
         xlab = "TrainingTimeLastYear",ylab = "No of Employee",
         col=1:10)
plot(density(mydata$TrainingTimesLastYear),
     main="Density Plot of TrainingTimeLastYear",
     xlab="TrainingTimeLastYear")
dev.off()
boxplot(mydata$TrainingTimesLastYear, col = "red")$stats
boxplot(mydata$TrainingTimesLastYear, col = "red")$out


#1.B.14 YearsAtCompany
table(is.na(mydata$YearsAtCompany))
mean(mydata$YearsAtCompany)
median(mydata$YearsAtCompany)
min(mydata$YearsAtCompany)
max(mydata$YearsAtCompany)
range(mydata$YearsAtCompany)
IQR(mydata$YearsAtCompany)
fivenum(mydata$YearsAtCompany)
#------visualization 
png(filename = "YearsAtCompany.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$YearsAtCompany,
     main = "Histogram of Years At Company",
     xlab = "YearsAtCompany",
     ylab = "No of employee",
     col = 2:8)
abline(v = mean(mydata$YearsAtCompany))
text(12,790,"Mean = 7 Years")
abline(v= median(mydata$YearsAtCompany), col = "Blue")
text(6,600,"Median = 5",col = "Blue")
boxplot(mydata$YearsAtCompany,
        main="Box Plot of YearsAtCompany",
        xlab="YearsAtCompany",
        ylab="No of Employee",
        col = "Red")
dotchart(mydata$YearsAtCompany,
         main = "Dotchat of YearsAtCompany",
         xlab = "YearsAtCompany",ylab = "No of Employee",
         col=1:10)
plot(density(mydata$YearsAtCompany),
     main="Density Plot of YearsAtCompany",
     xlab="YearsAtCompany")
dev.off()
boxplot(mydata$YearsAtCompany, col = "red")$stats
boxplot(mydata$YearsAtCompany, col = "red")$out


#1.B.15 YearsInCurrentRole
table(is.na(mydata$YearsInCurrentRole))
mean(mydata$YearsInCurrentRole)
median(mydata$YearsInCurrentRole)
min(mydata$YearsInCurrentRole)
max(mydata$YearsInCurrentRole)
range(mydata$YearsInCurrentRole)
IQR(mydata$YearsInCurrentRole)
fivenum(mydata$YearsInCurrentRole)
#------visualization 
png(filename = "YearsInCurrentRole.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$YearsInCurrentRole,
     main = "Histogram of Years In CurrentRole",
     xlab = "Years In CurrentRole",
     ylab = "No of employee",
     xlim = c(0,20),
     ylim = c(0,700),
     col = 2:8)
abline(v = mean(mydata$YearsInCurrentRole))
text(7,700,"Mean = 4.22")
abline(v= median(mydata$YearsInCurrentRole), col = "Blue")
text(5,500,"Median = 3",col = "Blue")
boxplot(mydata$YearsInCurrentRole,
        main="Box Plot of YearsInCurrentRole",
        xlab="YearsInCurrentRole",
        ylab="No of Employee",
        col = "Red")
dotchart(mydata$YearsInCurrentRole,
         main = "Dotchat of YearsInCurrentRole",
         xlab = "YearsInCurrentRole",ylab = "No of Employee",
         xlim = c(0,20),
         col=1:10)
plot(density(mydata$YearsInCurrentRole),
     main="Density Plot of YearsInCurrentRole",
     xlab="YearsInCurrentRole")
dev.off()
boxplot(mydata$YearsInCurrentRole, col = "red")$stats
boxplot(mydata$YearsInCurrentRole, col = "red")$out

#1.B.16 YearsSinceLastPromotion
table(is.na(mydata$YearsSinceLastPromotion))
mean(mydata$YearsSinceLastPromotion)
median(mydata$YearsSinceLastPromotion)
min(mydata$YearsSinceLastPromotion)
max(mydata$YearsSinceLastPromotion)
range(mydata$YearsSinceLastPromotion)
IQR(mydata$YearsSinceLastPromotion)
fivenum(mydata$YearsSinceLastPromotion)
#------visualization 
png(filename = "YearsSinceLastPromotion.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$YearsSinceLastPromotion,
     main = "Histogram of Years Since Last Promotion",
     xlab = "Years Since Last Promotion",
     ylab = "No of employee",
     ylim = c(0,1000),
     col = 2:8)
abline(v = mean(mydata$YearsSinceLastPromotion))
text(4.5,900,"Mean = 2.18")
abline(v= median(mydata$YearsSinceLastPromotion), col = "Blue")
text(2,700,"Median = 1",col = "Blue")
boxplot(mydata$YearsSinceLastPromotion,
        main="Box Plot of Years Since Last Promotion",
        xlab="Years Since Last Promotion",
        ylab="No of Employee",
        col = "Red")
dotchart(mydata$YearsSinceLastPromotion,
         main = "Dotchat of Years Since Last Promotion",
         xlab = "Years Since Last Promotion",ylab = "No of Employee",
         col=1:10)
plot(density(mydata$YearsSinceLastPromotion),
     main="Density Plot of Years Since Last Promotion",
     xlab="Years Since Last Promotion")
dev.off()
boxplot(mydata$YearsSinceLastPromotion, col = "red")$stats
boxplot(mydata$YearsSinceLastPromotion, col = "red")$out

#1.B.17 YearsWithCurrManager
table(is.na(mydata$YearsWithCurrManager))
mean(mydata$YearsWithCurrManager)
median(mydata$YearsWithCurrManager)
min(mydata$YearsWithCurrManager)
max(mydata$YearsWithCurrManager)
range(mydata$YearsWithCurrManager)
IQR(mydata$YearsWithCurrManager)
fivenum(mydata$YearsWithCurrManager)
#------visualization 
png(filename = "YearsWithCurrManager.png",height = 480, width = 960, units = "px")
par(mfrow = c(2,2))
hist(mydata$YearsWithCurrManager,
     main = "Histogram of Years With Current Manager",
     xlab = "Years With Current Manager",
     ylab = "No of employee",
     ylim = c(0,800),
     col = 1:8)
abline(v = mean(mydata$YearsWithCurrManager))
text(5,800,"Mean = 4.12")
abline(v= median(mydata$YearsWithCurrManager), col = "Blue")
text(3,600,"Median = 3",col = "Blue")
boxplot(mydata$YearsWithCurrManager,
        main="Box Plot of Years With Current Manager",
        xlab="Years With Current Manager",
        ylab="No of Employee",
        col = "Red")
dotchart(mydata$YearsWithCurrManager,
         main = "Dotchat of Years With Current Manager",
         xlab = "Years With Current Manager",ylab = "No of Employee",
         col=1:10)
plot(density(mydata$YearsWithCurrManager),
     main="Density Plot of Years With Current Manager",
     xlab="Years With Current Manager")
dev.off()
boxplot(mydata$YearsWithCurrManager, col = "red")$stats
boxplot(mydata$YearsWithCurrManager, col = "red")$out