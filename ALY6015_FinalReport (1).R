library(tidyverse)
library(psych)
library(readr)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(GGally)
library(ggpubr)
library(fastDummies)
library(ggiraphExtra)
library(reshape2)
library(corrplot)
library(caTools)
library(car)
library(ggcorrplot)
library(MASS)
library(vtable)
library(officer)
library(flextable)
library(patchwork)
library(stargazer)
library(MASS)
library(leaps)
library(furniture)
library(knitr)
library(glmnet)
library(Metrics)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tidyr)

old_census <- read.csv("/Users/kushagrabubna/Downloads/nov22pub.csv")
old_census <- old_census %>% drop_na()

census_data <- read.csv("/Users/kushagrabubna/Downloads/nov22pub.csv")


census_data <- na.omit(census_data) 
census_data

ncol <- ncol(census_data)
nrow <- nrow(census_data)

for (i in 1:nrow){
  census_data$employment_status[i] <- if(census_data$pemlr[i] == 1){
    "Employed-At Work"
  }else if(census_data$pemlr[i] == 2){
    "Employed-Absent"
  }else if(census_data$pemlr[i] == 3){
    "Unemployed- On Layoff"
  }else if(census_data$pemlr[i] == 4){
    "Unemployed- Looking"
  }else if(census_data$pemlr[i] == 5){
    "Retired"
  }else if(census_data$pemlr[i] == 6){
    "Disabled"
  }else{
    "Other"
  }
}

for (i in 1:nrow){
  census_data$Marital_Status[i] <- if(census_data$pemaritl[i] == 1){
    "Married-Spouse Present"
  }else if(census_data$pemaritl[i] == 2){
    "Married-Spouse Absent"
  }else if(census_data$pemaritl[i] == 3){
    "Widowed"
  }else if(census_data$pemaritl[i] == 4){
    "Divorced"
  }else if(census_data$pemaritl[i] == 5){
    "Separated"
  }else{
    "Never Married"
  }
}

for (i in 1:nrow){
  census_data$occupation[i] <- if(census_data$prmjocc1[i] == 1){
    "Management and Business"
  }else if(census_data$prmjocc1[i] == 2){
    "Professional Occupation"
  }else if(census_data$prmjocc1[i] == 3){
    "Service Occupation"
  }else if(census_data$prmjocc1[i] == 4){
    "Sales Department"
  }else if(census_data$prmjocc1[i] == 5){
    "Office and Administrative Job"
  }else if(census_data$prmjocc1[i] == 6){
    "Farming, Fishing and Forestry"
  }else if(census_data$prmjocc1[i] == 7){
    "Construction and Extraction"
  }else if(census_data$prmjocc1[i] == 8){
    "Installation, Maintainence and Repair"
  }else if(census_data$prmjocc1[i] == 9){
    "Production Occupation"
  }else if(census_data$prmjocc1[i] == 10){
    "Transportation"
  }else{
    "Armed Forces"
  }
}
for (i in 1:nrow){
  census_data$marital_status[i] <- if(census_data$pemaritl[i] <= 2){
    "Married"
  }else if(census_data$prmjocc1[i] == 3){
    "Widowed"
  }else if(census_data$prmjocc1[i] == 4){
    "Divorced"
  }else if(census_data$prmjocc1[i] == 5){
    "Separated"
  }else {
    "Never Married"
  }
}
census_data$gender <- ifelse(census_data$pesex == 1, "Male","Female")

census_data$disability <- ifelse(census_data$prdisflg == 1, "Disabled","Not-Disabled")

census_data$health <- ifelse(census_data$pedisrem == 1, "Healthy", "Not-Healthy")

census_data$certified <- ifelse(census_data$pecert1 == 1, "Certified", "Not-Certified")

for (i in 1:nrow){
  census_data$edu_status[i] <- if(census_data$peeduca[i] == 31){
    "Less than 1 Grade"
  }else if(census_data$peeduca[i] == 32){
    "1, 2, 3 or 4 Grade"
  }else if(census_data$peeduca[i] == 33){
    "5 or 6 Grade"
  }else if(census_data$peeduca[i] == 34){
    "7 or 8 Grade"
  }else if(census_data$peeduca[i] == 35){
    "9 Grade"
  }else if(census_data$peeduca[i] == 36){
    "10 Grade"
  }else if(census_data$peeduca[i] == 37){
    "11 Grade"
  }else if(census_data$peeduca[i] == 38){
    "12 Grade "
  }else if(census_data$peeduca[i] == 39){
    "Grad Diploma"
  }else if(census_data$peeduca[i] == 40){
    "College"
  }else if(census_data$peeduca[i] == 41){
    "Associate Degree- Occupational"
  }else if(census_data$peeduca[i] == 42){
    "Associate Degree- Academic"
  }else if(census_data$peeduca[i] == 43){
    "Bachelor's Degree"
  }else if(census_data$peeduca[i] == 44){
    "Master's Degree"
  }else if(census_data$peeduca[i] == 45){
    "Professional School"
  }else{
    "Doctorate Degree"
  }
}

for (i in 1:nrow){
  census_data$edu_newstatus[i] <- if(census_data$peeduca[i] == 31){
    "Elementary Education"
  }else if(census_data$peeduca[i] == 32){
    "Elementary Education"
  }else if(census_data$peeduca[i] == 33){
    "Elementary Education"
  }else if(census_data$peeduca[i] == 34){
    "Elementary Education"
  }else if(census_data$peeduca[i] == 35){
    "High School"
  }else if(census_data$peeduca[i] == 36){
    "High School"
  }else if(census_data$peeduca[i] == 37){
    "High School"
  }else if(census_data$peeduca[i] == 38){
    "High School"
  }else if(census_data$peeduca[i] == 39){
    "College"
  }else if(census_data$peeduca[i] == 40){
    "College"
  }else if(census_data$peeduca[i] == 41){
    "College"
  }else if(census_data$peeduca[i] == 42){
    "College"
  }else if(census_data$peeduca[i] == 43){
    "Graduation"
  }else if(census_data$peeduca[i] == 44){
    "Graduation"
  }else if(census_data$peeduca[i] == 45){
    "Doctorate"
  }else{
    "Doctorate"
  }
}


for (i in 1:nrow){
  census_data$Edu_Level[i] <- if(census_data$peeduca[i] <= 38){
    "High School"
  }else if(census_data$peeduca[i] == 39 ){
    "College"
  }else if(census_data$peeduca[i] == 40){
    "College"
  }else if(census_data$peeduca[i] == 41){
    "College"
  }else if(census_data$peeduca[i] == 42){
    "College"
  }else {
    "Higher Education"
  }
}
for (i in 1:nrow){
  census_data$Fam_income[i] <- if(census_data$hefaminc[i] == 1){
    sample(2000:5000,1)
  }else if(census_data$hefaminc[i] == 2){
    sample(5000:7499,1)
  }else if(census_data$hefaminc[i] == 3){
    sample(7500:9999,1)
  }else if(census_data$hefaminc[i] == 4){
    sample(10000:12499,1)
  }else if(census_data$hefaminc[i] == 5){
    sample(12500:14999,1)
  }else if(census_data$hefaminc[i] == 6){
    sample(15000:19999,1)
  }else if(census_data$hefaminc[i] == 7){
    sample(20000:24999,1)
  }else if(census_data$hefaminc[i] == 8){
    sample(25000:29999,1)
  }else if(census_data$hefaminc[i] == 9){
    sample(30000:34999,1)
  }else if(census_data$hefaminc[i] == 10){
    sample(35000:39999,1)
  }else if(census_data$hefaminc[i] == 11){
    sample(40000:49999,1)
  }else if(census_data$hefaminc[i] == 12){
    sample(50000:59999,1)
  }else if(census_data$hefaminc[i] == 13){
    sample(60000:74999,1)
  }else if(census_data$hefaminc[i] == 14){
    sample(75000:99999,1)
  }else if(census_data$hefaminc[i] == 15){
    sample(100000:149999,1)
  }else{
    sample(150000:300000,1)
  }
}

for (i in 1:nrow){
  census_data$ethnicity[i] <- if(census_data$ptdtrace[i] == 01){
    "White"
  }else if(census_data$ptdtrace[i] == 02){
    "Black"
  }else if(census_data$ptdtrace[i] == 03){
    "American Indian"
  }else if(census_data$ptdtrace[i] == 04){
    "Asian"
  }else if(census_data$ptdtrace[i] == 05){
    "Hawaiian"
  }else if(census_data$ptdtrace[i] == 06){
    "White-Black"
  }else if(census_data$ptdtrace[i] == 07){
    "White-American Indian"
  }else if(census_data$ptdtrace[i] == 08){
    "White-Asian"
  }else if(census_data$ptdtrace[i] == 09){
    "White-Hawaiian"
  }else if(census_data$ptdtrace[i] == 10){
    "Black-American Indian"
  }else if(census_data$ptdtrace[i] == 11){
    "Black-Asian"
  }else if(census_data$ptdtrace[i] == 12){
    "Black-Hawaiian"
  }else if(census_data$ptdtrace[i] == 13){
    "American Indian-Asian"
  }else if(census_data$ptdtrace[i] == 14){
    "American Indian-Hawaiian"
  }else if(census_data$ptdtrace[i] == 15){
    "Asian-Hawaiian"
  }else if(census_data$ptdtrace[i] == 16){
    "W-B-AI"
  }else if(census_data$ptdtrace[i] == 17){
    "W-B-A"
  }else if(census_data$ptdtrace[i] == 18){
    "W-B-H"
  }else if(census_data$ptdtrace[i] == 19){
    "W-AI-A"
  }else if(census_data$ptdtrace[i] == 20){
    "W-AI-H"
  }else if(census_data$ptdtrace[i] == 21){
    "W-A-H"
  }else if(census_data$ptdtrace[i] == 22){
    "B-AI-A"
  }else if(census_data$ptdtrace[i] == 23){
    "W-B-AI-A"
  }else if(census_data$ptdtrace[i] == 24){
    "W-AI-A-H"
  }else if(census_data$ptdtrace[i] == 25){
    "Other 3 combinations"
  }else{
    "Other 4 combinations"
  }
}

for (i in 1:nrow){
  census_data$region[i] <- if(census_data$gereg[i] == 1){
    "Northeast"
  }else if(census_data$gereg[i] == 2){
    "Midwest"
  }else if(census_data$gereg[i] == 3){
    "South"
  }else{
    "West"
  }
}

for (i in 1:nrow){
  census_data$metro_status[i] <- if(census_data$gtmetsta[i] == 1){
    "Metropolitian"
  }else if(census_data$gtmetsta[i] == 2){
    "Non-Metropolitian"
  }else{
    "Not Identified"
  }
}

furniture::table1(census_data,
                   "Gender" = gender, "Metropolitian Status" = metro_status,
                  splitby = ~region,
                  test = TRUE,
                  na.rm = TRUE,
                  format_number = TRUE
) -> tab11
tab11


tab12 <- census_data %>%                              
          group_by(employment_status) %>% 
          summarize(min = min(Fam_income),
                    q1 = quantile(Fam_income, 0.25),
                    median = median(Fam_income),
                    mean = mean(Fam_income),
                    q3 = quantile(Fam_income, 0.75),
                    max = max(Fam_income))
tab12

tab13 <- census_data %>%                              
  group_by(edu_status) %>% 
  summarize(min = min(Fam_income),
            q1 = quantile(Fam_income, 0.25),
            median = median(Fam_income),
            mean = mean(Fam_income),
            q3 = quantile(Fam_income, 0.75),
            max = max(Fam_income))
tab13


tab14 <- census_data %>%                              
  group_by(occupation) %>% 
  summarize(
            q1 = quantile(pehruslt, 0.25),
            median = median(pehruslt),
            mean = mean(pehruslt),
            q3 = quantile(pehruslt, 0.75),
            max = max(pehruslt))
tab14[-1,]

tab15 <- census_data %>%                              
  group_by(edu_newstatus) %>% 
  summarize(min = min(Fam_income),
            q1 = quantile(Fam_income, 0.25),
            median = median(Fam_income),
            mean = mean(Fam_income),
            q3 = quantile(Fam_income, 0.75),
            max = max(Fam_income))
tab15

ggplot(census_data, aes(x=gender, y= Fam_income))+
  geom_bar(stat = "summary", width = 0.5, fill="tomato3") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20)) +
  labs(title = "Males dominates the census with average income",
       caption="Source: CPS Survey Nov 2022") +
       xlab("Gender") +
       ylab("Average Income")

g <- ggplot(census_data, aes(edu_status))
g + geom_bar(aes(fill=employment_status), width = 0.5) + 
  theme(axis.text.x = element_text(angle=60, vjust=0.6, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20),
        legend.text = element_text(size = 15)) +
  labs(title="Diploma Graduates are most employed compared to other degree holders ", 
       caption="Source: CPS Survey Nov 2022") +
  scale_fill_discrete(name = "Employment Status") +
       xlab("Education Level") +
       ylab("Population")

g1 <- ggplot(census_data, aes(region))
g1 + geom_bar(aes(fill=ethnicity), width = 0.5) + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20),
        legend.text = element_text(size = 15)) +
  labs(title="Whites dominates the ethnicity in all regions across United States", 
       caption="Source: CPS Survey Nov 2022") +
        scale_fill_discrete(name = "Ethnicity") +
       xlab("Region") +
       ylab("Population") +
  guides(fill = guide_legend(ncol = 1))


ggplot(census_data, aes(x=Marital_Status, y= Fam_income,fill=region))+
  geom_bar(stat = "summary", width = 0.5, position = 'dodge') +
  theme(axis.text.x = element_text(angle=45, vjust=0.6, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20),
        legend.text = element_text(size = 15)) +
  labs(title = "Married couples with spouse dominates the census with average income",
       caption="Source: CPS Survey Nov 2022") +
  scale_fill_discrete(name = "Region") +
  xlab("Marital Status") +
  ylab("Average Income")

tab11df <- as.data.frame(tab11)

#flextable(tab11df) %>% save_as_docx(path = 'flextbale1.docx')
#flextable(tab12) %>% save_as_docx(path = 'flextbale2.docx')
#flextable(tab13) %>% save_as_docx(path = 'flextbale3.docx')
#flextable(tab14) %>% save_as_docx(path = 'flextbale4.docx')
flextable(tab15) %>% save_as_docx(path = 'flextbale5.docx')

newCensus12 <- data.frame(census_data$Fam_income,census_data$peeduca,census_data$prmjocc1,census_data$ptdtrace,census_data$gereg,census_data$gtmetsta,census_data$hrnumhou,census_data$prtage,census_data$pesex,census_data$pemaritl,census_data$prdisflg,census_data$ptnmemp1,census_data$pedisrem,census_data$pecert1)
colnames(newCensus12) <- c("Fam_income","Edu_Level","Occupation","Ethnicity","Region","Metro_status","Total_people","Age","Gender","Marital_Status","Disability","Total_Paid_Employees","Health","Certified")
Corrrrr<- cor(newCensus12)
corrplot(Corrrrr, 
         method = 'circle', 
         ) # Correlation plot for six important variables

newCensus <- data.frame(census_data$Fam_income,census_data$Edu_Level,census_data$occupation,census_data$ethnicity,census_data$region,census_data$metro_status,census_data$hrnumhou,census_data$prtage,census_data$gender,census_data$marital_status,census_data$disability,census_data$ptnmemp1,census_data$health,census_data$certified)
colnames(newCensus) <- c("Fam_income","Edu_Level","Occupation","Ethnicity","Region","Metro_status","Total_people","Age","Gender","Marital_Status","Disability","Total_Paid_Employees","Health","Certified")

# Regression model
lm1 <- lm(Fam_income~Edu_Level+occupation+ethnicity+region+metro_status+hrnumhou+prtage+gender+marital_status+disability+ptnmemp1+health+certified, data = census_data)
summary(lm1)


# Ridge regression model
set.seed(123)
trainIndex <- sample(x=nrow(newCensus),size = nrow(newCensus)*0.7)
trainData <- newCensus[trainIndex,]
testData <- newCensus[-trainIndex,]

train_x <- model.matrix(Fam_income~. , trainData)[,-1]
test_x <- model.matrix(Fam_income~. , testData)[,-1]

train_y <- trainData$Fam_income
test_y <- testData$Fam_income

set.seed(123)
lambda <- cv.glmnet(train_x, train_y, alpha=0 , nfolds = 10)
plot(lambda)

lambdaMin <- lambda$lambda.min
lambda1Se <- lambda$lambda.1se

lambdaMin
lambda1Se

# Fitting Ridge model based on lambda
model <- glmnet(train_x,train_y, alpha = 0)
plot(model, xvar = "lambda")

model <- glmnet(train_x,train_y, alpha = 1)
plot(model, xvar = "lambda")

# Model for lambda min
modelMinRidge <- glmnet(train_x,train_y, alpha = 0, lambda = lambdaMin)
coef(modelMinRidge)

train_predict_Ridge <- predict(modelMinRidge, newx = train_x)

train_rmse_Ridge <- rmse(train_y, train_predict_Ridge)
train_rmse_Ridge


# Lasso Regression Model
set.seed(123)
trainIndex1 <- sample(x=nrow(newCensus),size = nrow(newCensus)*0.7)
trainData1 <- newCensus[trainIndex1,]
testData1 <- newCensus[-trainIndex1,]

train_x1 <- model.matrix(Fam_income~. , trainData1)[,-1]
test_x1 <- model.matrix(Fam_income~. , testData1)[,-1]

train_y1 <- trainData1$Fam_income
test_y1 <- testData1$Fam_income

set.seed(123)
lambda1 <- cv.glmnet(train_x1, train_y1, alpha=1 , nfolds = 10)
plot(lambda1)

lambdaMin1 <- lambda1$lambda.min
lambda1Se1 <- lambda1$lambda.1se

lambdaMin1
lambda1Se1

modelMinRidge1 <- glmnet(train_x,train_y, alpha = 1, lambda = lambdaMin1)
coef(modelMinRidge1)

train_predict <- predict(modelMinRidge1, newx = train_x)
train_rmse <- rmse(train_y, train_predict)
train_rmse

predictV <- predict(lm1, newx = trainx)
predict_rmse <- rmse(census_data$Fam_income, predictV)
predict_rmse




