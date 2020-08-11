library(readxl)
Chronic_Kidney_Disease_Dataset <- read_excel("Data/Chronic Kidney Disease Dataset.xlsx", 
                                             sheet = "All Data")
View(Chronic_Kidney_Disease_Dataset)
c <- Chronic_Kidney_Disease_Dataset
rm(Chronic_Kidney_Disease_Dataset)
#Setting shorter alias

#Screening the target variable: Removing nulls
c <- c[!(is.na(c$CKD)),]
c$Dyslipidemia

library(dplyr)
#Selecting variables 
ck <- c %>% select ("Age",
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
rm(c)


#------------------------------------------------------------------
#Structuring

install.packages("funModeling")
library(funModeling)
df_status(ck)
#All are numeric

cols <- c("Female", 
          "Racegrp", 
          "Educ",
          "Unmarried",  
          "Income",
          "CareSource", 
          "Insured", 
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

ck[cols] <- lapply(ck[cols], factor)
str(ck)

#Age category
ck$Agecat[ck$Age<44] <- 1
ck$Agecat[ck$Age>=44 & ck$Age<65] <- 2
ck$Agecat[ck$Age>=65] <- 3
ck$Agecat <- factor(ck$Agecat)
table(ck$Agecat)
#   1    2    3 
#1946 1392  952 
ck$Agecat <- relevel(ck$Agecat, ref= "1")

#Racegrp
ck$Racegrp <- relevel(ck$Racegrp, ref= "white")

#CareSource
ck$CareSource <- relevel(ck$CareSource, ref= "noplace")

#BMI
ck$BMIcat[ck$BMI < 18.5] <- "Underweight"
ck$BMIcat[ck$BMI >= 18.5 & ck$BMI < 25] <- "Normal"
ck$BMIcat[ck$BMI >= 25 & ck$BMI < 30] <- "Overweight"
ck$BMIcat[ck$BMI >= 30] <- "Obese"
ck$BMIcat <- factor(ck$BMIcat)
table(ck$BMIcat)
#Normal       Obese  Overweight Underweight 
#1781        1809        2109          95 
ck$BMIcat <- relevel(ck$BMIcat, ref= "Normal")

#Activity
ck$Activity <- relevel(ck$Activity, ref= "1")

#-----------------------------------------------------------------

#Bivariate
options(scipen=90)

#Significant
chisq.test(ck$Agecat, ck$CKD)             #p-value < 0.00000000000000022
chisq.test(ck$Racegrp, ck$CKD)            #p-value = 0.000000000000001843
chisq.test(ck$Educ, ck$CKD)               #p-value = 0.000005362
chisq.test(ck$Unmarried, ck$CKD)          #p-value = 0.000002014
chisq.test(ck$Income, ck$CKD)             #p-value = 0.00000000003581
chisq.test(ck$CareSource, ck$CKD)         #p-value = 0.0000000000001243
chisq.test(ck$Insured, ck$CKD)            #p-value = 0.000000000002503
chisq.test(ck$PVD, ck$CKD)                #p-value < 0.00000000000000022
chisq.test(ck$Activity, ck$CKD)           #p-value < 0.00000000000000022
chisq.test(ck$PoorVision, ck$CKD)         #p-value = 0.00000000000009865
chisq.test(ck$Smoker, ck$CKD)             #p-value = 0.0000002147
chisq.test(ck$Hypertension, ck$CKD)       #p-value < 0.00000000000000022
chisq.test(ck$`Fam Hypertension`, ck$CKD) #p-value = 0.0005041
chisq.test(ck$Stroke, ck$CKD)             #p-value < 0.00000000000000022
chisq.test(ck$CHF, ck$CKD)                #p-value < 0.00000000000000022
chisq.test(ck$Diabetes, ck$CKD)           #p-value < 0.00000000000000022
chisq.test(ck$CVD, ck$CKD)                #p-value < 0.00000000000000022
chisq.test(ck$`Fam CVD`, ck$CKD)          #p-value = 0.006518
chisq.test(ck$Anemia, ck$CKD)             #p-value = 0.00003015


#Insignificant
chisq.test(ck$Female, ck$CKD)             #p-value = 0.2846
chisq.test(ck$BMIcat, ck$CKD)             #p-value = 0.2952
chisq.test(ck$`Fam Diabetes`, ck$CKD)     #p-value = 0.1971
chisq.test(ck$Dyslipidemia, ck$CKD)       #p-value = 0.9507


df = subset(ck, select = -c(Female, BMI, `Fam Diabetes`, BMIcat, Age, Dyslipidemia))

df1 <- na.omit(df)
#Removing null values

#----------------------------------------------------------------Model
log <- glm(CKD~., data=df1, family = "binomial")
summary(log)
str(df1)

#Coefficients:
#                     Estimate Std. Error z value             Pr(>|z|)    
#(Intercept)         -3.8756275  0.7855510  -4.934          0.000000807 ***
#Racegrpblack        -0.39685    0.19695  -2.015             0.043904 *  
#Racegrphispa        -0.94509    0.20177  -4.684           0.00000281 ***
#Educ1               -0.25517    0.15457  -1.651             0.098775 .  
#PVD1                 0.62703    0.20341   3.083             0.002052 ** 
#Activity2           -0.29134    0.14766  -1.973             0.048496 *  
#Activity3           -0.67332    0.25499  -2.641             0.008275 ** 
#Activity4           -1.03053    0.60852  -1.693             0.090363 .  
#Hypertension1        0.76552    0.16478   4.646           0.00000339 ***
#Diabetes1            0.57130    0.16218   3.523             0.000427 ***
#CVD1                 0.62945    0.23759   2.649             0.008066 ** 
#Anemia1              1.01353    0.45939   2.206             0.027366 *  
#Agecat2              1.20814    0.32394   3.730             0.000192 ***
#Agecat3              2.74994    0.31929   8.613 < 0.0000000000000002 ***


str(df1)
mod <- glm(CKD~. , data=df1, family = "binomial")
library(epiDisplay)
logistic.display(mod)


library(epiDisplay)
logistic.display(log)
