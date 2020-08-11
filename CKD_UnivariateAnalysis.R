library(readxl)
options(scipen=99)
Chronic_Kidney_Disease_Dataset <- read_excel("Desktop/Chronic Kidney Disease Dataset.xls", 
                                               sheet = "All Data")
View(Chronic_Kidney_Disease_Dataset) 
Kidney <- Chronic_Kidney_Disease_Dataset
rm(Chronic_Kidney_Disease_Dataset)

library(dplyr)
CK<- Kidney %>% select ("Age",
                        "Female", 
                        "Racegrp", 
                        "Educ", 
                        "Unmarried", 
                        "Income", 
                        "CareSource", 
                        "Insured", 
                        "BMI", 
                        "Dyslipidemia",
                        "PVD",
                        "Activity",
                        "PoorVision",
                        "Smoker", 
                        "Hypertension",
                        "Fam Hypertension",
                        "Diabetes",
                        "Fam Diabetes",
                        "Stroke",
                        "CVD", 
                        "Fam CVD",
                        "CHF",
                        "Anemia",
                        "CKD")

#Screening variable
str(CK$CKD)
CK$CKD <- factor(CK$CKD)
table(CK$CKD)
#
#0    1 
#5536  464 
#Removing null values from the target variable
CK<- CK[-which(is.na(CK$CKD)), ]
#6000 obs

#Structure of the table
str(CK)

cols <- c("Female", 
          "Racegrp", 
          "Educ",
          "Unmarried",  
          "Income",
          "CareSource", 
          "Insured", 
          "Dyslipidemia",
          "PVD",
          "Activity",
          "PoorVision",
          "Smoker", 
          "Hypertension",
          "Fam Hypertension",
          "Diabetes",
          "Fam Diabetes",
          "Stroke",
          "CVD", 
          "Fam CVD",
          "CHF",
          "Anemia",
          "CKD")

CK[cols] <- lapply(CK[cols], factor)
str(CK)



#Age
library(psych)
describe(CK$Age)
plot(density(CK$Age))
summary(aov(CK$Age~CK$CKD))
#p-value is significant
which(is.na(CK$Age))
#None


#Age category
CK$Agecat[CK$Age<44] <- 1
CK$Agecat[CK$Age>=44 & CK$Age<65] <- 2
CK$Agecat[CK$Age>=65] <- 3
CK$Agecat <- factor(CK$Agecat)
table(CK$Agecat)
#   1    2    3 
#1946 1392  952 
CK$Agecat <- relevel(CK$Agecat, ref= "1")

#Racegrp
CK$Racegrp <- relevel(CK$Racegrp, ref= "white")

#CareSource
CK$CareSource <- relevel(CK$CareSource, ref= "noplace")

#BMI
CK$BMIcat[CK$BMI < 18.5] <- "Underweight"
CK$BMIcat[CK$BMI >= 18.5 & CK$BMI < 25] <- "Normal"
CK$BMIcat[CK$BMI >= 25 & CK$BMI < 30] <- "Overweight"
CK$BMIcat[CK$BMI >= 30] <- "Obese"
CK$BMIcat <- factor(CK$BMIcat)
table(CK$BMIcat)
CK$BMIcat <- relevel(CK$BMIcat, ref= "Normal")

#Activity
CK$Activity <- relevel(CK$Activity, ref= "1")


#Fam hypertension and hypertension
CK$hyper[CK$Hypertension==0 & CK$`Fam Hypertension`==0] <- "None"
CK$hyper[CK$Hypertension==0 & CK$`Fam Hypertension`==1] <- "Only in fam "
CK$hyper[CK$Hypertension==1 & CK$`Fam Hypertension`==0] <- "Only individual"
CK$hyper[CK$Hypertension==1 & CK$`Fam Hypertension`==1] <- "In both"
table(CK$hyper)
CK$hyper <- factor(CK$hyper)
CK$hyper <- relevel(CK$hyper, ref= "None")
#In both        None    Only in fam    Only individual 
#462            2041           614              1173 


#Fam diabetes and diabetes
CK$db[CK$Diabetes==0 & CK$`Fam Diabetes`==0] <- "None"
CK$db[CK$Diabetes==0 & CK$`Fam Diabetes`==1] <- "Only in fam "
CK$db[CK$Diabetes==1 & CK$`Fam Diabetes`==0] <- "Only individual"
CK$db[CK$Diabetes==1 & CK$`Fam Diabetes`==1] <- "In both"
table(CK$db)
#In both        None    Only in fam    Only individual 
#294            2775            1056               165 
CK$db <- factor(CK$db)
CK$db <- relevel(CK$db, ref= "None")


#Fam CVD and CVD
CK$cvd[CK$CVD==0 & CK$`Fam CVD`==0] <- "None"
CK$cvd[CK$CVD==0 & CK$`Fam CVD`==1] <- "Only in fam "
CK$cvd[CK$CVD==1 & CK$`Fam CVD`==0] <- "Only individual"
CK$cvd[CK$CVD==1 & CK$`Fam CVD`==1] <- "In both"
table(CK$cvd)
#In both        None    Only in fam  Only individual 
#93            2623            1417             157 
CK$cvd <- factor(CK$cvd)
CK$cvd <- relevel(CK$cvd, ref= "None")




#Female
CK$Female <- factor(CK$Female)
table(CK$Female)
#Male: 2865, Female: 3135
chisq.test(CK$Female, CK$CKD)
#Insignificant


#Racegrp
CK$Racegrp <- factor(CK$Racegrp)
table(CK$Racegrp)
#black hispa other white 
#1078  1758   190  2974 
chisq.test(CK$Racegrp, CK$CKD)
#Significant
which(
  is.na(CK$Racegrp))

#Educ
CK$Educ <- factor(CK$Educ)
table(CK$Educ)
which(is.na(CK$Educ))
#15 observations- removing
chisq.test(CK$Educ, CK$CKD)
#p-value is significant
CK <- CK[-which(is.na(CK$Educ)),]
#5985 obs

#Unmarried
CK$Unmarried <- factor(CK$Unmarried)
table(CK$Unmarried)
which(is.na(CK$Unmarried))
#Approx 300 null values
chisq.test(CK$Unmarried, CK$CKD)
#p-value is significant
#Removing 300 values
CK <- CK[-which(is.na(CK$Unmarried)),]
#5686 obs

#Income
str(CK$Income)
table(CK$Income)
which(is.na(CK$Income))
#800 values
chisq.test(CK$Income, CK$CKD)
#p-value is significant
#Removing 800 values
CK <- CK[-which(is.na(CK$Income)),]
#4947 obs

#CareSource
str(CK$CareSource)
CK$CareSource <- factor(CK$CareSource)
which(is.na(CK$CareSource))
#2 values
chisq.test(CK$CareSource, CK$CKD)
#p-value is significant
CK <- CK[-which(is.na(CK$CareSource)),]
#4945 obs

#Insured
str(CK$Insured)
CK$Insured <- factor(CK$Insured)
which(is.na(CK$Insured))
chisq.test(CK$Insured, CK$CKD)
#p-value is significant
CK <- CK[-which(is.na(CK$Insured)),]
#4935 obs

#BMI
str(CK$BMI)
describe(CK$BMI)
is.na(CK$BMI)
which(is.na(CK$BMI))
summary(aov(CK$BMI~CK$CKD))
#Insignificant

#DBP
str(CK$DBP)
describe(CK$DBP)
which(is.na(CK$DBP))
summary(aov(CK$DBP~CK$CKD))
CK <- CK[-which(is.na(CK$DBP)),]
#4730 obs

#Dyslipidemia
str(CK$Dyslipidemia)
CK$Dyslipidemia <- factor(CK$Dyslipidemia)
table(CK$Dyslipidemia)
which(is.na(CK$Dyslipidemia))
chisq.test(CK$Dyslipidemia,CK$CKD)
#Insignificant

#PVD
str(CK$PVD)
CK$PVD <- factor(CK$PVD)
table(CK$PVD)
which(is.na(CK$PVD))
chisq.test(CK$PVD,CK$CKD)
#Significant

#Activity
str(CK$Activity)
CK$Activity <- factor(CK$Activity)
table(CK$Activity)
which(is.na(CK$Activity))
chisq.test(CK$Activity,CK$CKD)
#Significant
#Removing null values
CK <- CK[-which(is.na(CK$Activity)),]
#4725 obs

#PoorVision
str(CK$PoorVision)
CK$PoorVision <- factor(CK$PoorVision)
table(CK$PoorVision)
which(is.na(CK$PoorVision))
chisq.test(CK$PoorVision,CK$CKD)
#Significant
#Removing null values
CK <- CK[-which(is.na(CK$PoorVision)),]
#4507 obs

#Smoker
str(CK$Smoker)
CK$Smoker <- factor(CK$Smoker)
table(CK$Smoker)
which(is.na(CK$Smoker))
chisq.test(CK$Smoker,CK$CKD)
#Significant
#No null values

#Hypertension
str(CK$Hypertension)
CK$Hypertension <- factor(CK$Hypertension)
table(CK$Hypertension)
which(is.na(CK$Hypertension))
chisq.test(CK$Hypertension,CK$CKD)
#Significant
#Removing null values
CK <- CK[-which(is.na(CK$Hypertension)),]
#4467 obs

#Fam Hypertension
str(CK$`Fam Hypertension`)
CK$`Fam Hypertension` <- factor(CK$`Fam Hypertension`)
table(CK$`Fam Hypertension`)
which(is.na(CK$`Fam Hypertension`))
chisq.test(CK$`Fam Hypertension`,CK$CKD)
#Significant
#No null values

#Diabetes
str(CK$Diabetes)
CK$Diabetes <- factor(CK$Diabetes)
table(CK$Diabetes)
which(is.na(CK$Diabetes))
chisq.test(CK$Diabetes,CK$CKD)
#Significant
#No null values

#Fam Diabetes
str(CK$`Fam Diabetes`)
CK$`Fam Diabetes` <- factor(CK$`Fam Diabetes`)
table(CK$`Fam Diabetes`)
which(is.na(CK$`Fam Diabetes`))
chisq.test(CK$`Fam Diabetes`,CK$CKD)
#Insignificant

#Stroke
str(CK$Stroke)
CK$Stroke <- factor(CK$Stroke)
table(CK$Stroke)
which(is.na(CK$Stroke))
chisq.test(CK$Stroke,CK$CKD)
#Significant
#Removing null values
CK <- CK[-which(is.na(CK$Stroke)),]

#CVD
str(CK$CVD)
CK$CVD <- factor(CK$CVD)
table(CK$CVD)
which(is.na(CK$CVD))
chisq.test(CK$CVD,CK$CKD)
#Significant
#Removing null values
CK <- CK[-which(is.na(CK$CVD)),]
#4459 obs

#Fam CVD
str(CK$`Fam CVD`)
CK$`Fam CVD` <- factor(CK$`Fam CVD`)
table(CK$`Fam CVD`)
which(is.na(CK$`Fam CVD`))
chisq.test(CK$`Fam CVD`,CK$CKD)
#Slightly Significant
#Removing null values
CK <- CK[-which(is.na(CK$`Fam CVD`)),]
#4271 obs

#CHF
str(CK$CHF)
CK$CHF <- factor(CK$CHF)
table(CK$CHF)
which(is.na(CK$CHF))
chisq.test(CK$CHF,CK$CKD)
#Significant
#Removing null values
CK <- CK[-which(is.na(CK$CHF)),]
#4260 obs


#Anemia
str(CK$Anemia)
CK$Anemia <- factor(CK$Anemia)
table(CK$Anemia)
which(is.na(CK$Anemia))
chisq.test(CK$Anemia,CK$CKD)
#Insignificant

#Female, BMI, Dyslipidemia, Fam Diabetes, Anemia are insiginificant
#4260 obs of 25 var

df = subset(CK, select = -c(Female, BMI, Dyslipidemia, `Fam Diabetes`, Anemia) )
#Removing 5 insignificant columns

write.csv(df, file= "CKData.csv")

