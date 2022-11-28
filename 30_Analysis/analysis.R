library(ISLR2)
library(kableExtra)
library(stargazer)
library(ggplot2)
library(pROC)
library(e1071) # confusion matrix
library(caret)
library(tidyverse)
library(leaps)
library(arm)
library(nnet)
library(knitr)
library(MASS)
library(summarytools)

df_raw <- read.csv('ds_salaries.csv')
drop <- c("X","salary","salary_currency","work_year")
df = df_raw[,!(names(df_raw) %in% drop)]
#df$work_year <- as.factor(df$work_year)
df$experience_level <- as.factor(df$experience_level)
df$employment_type <- as.factor(df$employment_type)
df$company_size <- as.factor(df$company_size)
df$remote_ratio <- factor(df$remote_ratio,
                          levels=c(0, 50, 100),
                          labels=c("No Remote", "Hybrid", "Full Remote"))
df$company_location <- factor(df$company_location,
                              levels=c("AE","AS","AT","AU","BE","BR","CA","CH","CL","CN",
                                       "CO","CZ","DE","DK","DZ","EE","ES","FR","GB","GR",
                                       "HN","HR","HU","IE","IL","IN","IQ","IR","IT","JP",
                                       "KE","LU","MD","MT","MX","MY","NG","NL","NZ","PK",
                                       "PL","PT","US","VN","RO","RU","SG","SI","TR","UA"),
                              labels=c("AS","OC","EU","OC","EU","SA","NA","EU","SA","AS",
                                       "SA","EU","EU","EU","AF","EU","EU","EU","EU","EU",
                                       "NA","EU","EU","EU","AS","AS","AS","AS","EU","AS",
                                       "AF","EU","EU","EU","NA","AS","AF","EU","OC","AS",
                                       "EU","EU","NA","AS","EU","EU","AS","EU","AS","NA"))
df$employee_residence <- factor(df$employee_residence,
                                levels=c("AE","AR","AT","AU","BE","BG","BO","BR","CA","CH",
                                         "CL","CN","CO","CZ","DE","DK","DZ","EE","ES","FR",
                                         "GB","GR","HK","HN","HR","HU","IE","IN","IQ","IR",
                                         "IT","JE","JP","KE","LU","MD","MT","MX","MY","NG",
                                         "NL","NZ","PH","PK","PL","PR","PT","RO","RS","RU",
                                         "SG","SI","TN","TR","UA","US","VN"),
                                labels=c("AS","OC","EU","OC","EU","EU","SA","SA","NA","EU",
                                         "SA","AS","SA","EU","EU","EU","AF","EU","EU","EU",
                                         "EU","EU","AS","NA","EU","EU","EU","AS","AS","AS",
                                         "EU","EU","AS","AF","EU","EU","EU","NA","AS","AF",
                                         "EU","OC","AS","AS","EU","NA","EU","EU","EU","EU",
                                         "AS","EU","AF","AS","EU","NA","AS"))
df$job_title <- factor(df$job_title,
                       levels=c("3D Computer Vision Researcher","AI Scientist","Analytics Engineer","Applied Data Scientist","Applied Machine Learning Scientist","BI Data Analyst","Big Data Architect","Big Data Engineer","Business Data Analyst","Cloud Data Engineer","Computer Vision Engineer","Computer Vision Software Engineer","Data Analyst","Data Analytics Engineer","Data Analytics Lead","Data Analytics Manager","Data Architect","Data Engineer","Data Engineering Manager","Data Science Consultant","Data Science Engineer","Data Science Manager","Data Scientist","Data Specialist","Director of Data Engineering","Director of Data Science","ETL Developer","Finance Data Analyst","Financial Data Analyst","Head of Data","Head of Data Science","Head of Machine Learning","Lead Data Analyst","Lead Data Engineer","Lead Data Scientist","Lead Machine Learning Engineer","Machine Learning Developer","Machine Learning Engineer","Machine Learning Infrastructure Engineer","Machine Learning Manager","Machine Learning Scientist","Marketing Data Analyst","ML Engineer","NLP Engineer","Principal Data Analyst","Principal Data Engineer","Principal Data Scientist","Product Data Analyst","Research Scientist","Staff Data Scientist"),
                       labels=c("Data Engineer","Data Scientist","Data Engineer","Data Scientist","Data Scientist","Data Analyst","Data Engineer","Data Engineer","Data Analyst","Data Engineer","Data Engineer","Data Engineer","Data Analyst","Data Analyst","Data Analyst","Data Analyst","Data Engineer","Data Engineer","Data Engineer","Data Scientist","Data Scientist","Data Scientist","Data Scientist","Data Scientist","Data Engineer",
                                "Data Scientist","Data Engineer","Data Analyst","Data Analyst","Data Analyst","Data Scientist","Machine Learning Engineer",'Data Analyst','Data Engineer','Data Scientist',"Machine Learning Engineer","Machine Learning Engineer","Machine Learning Engineer","Machine Learning Engineer","Machine Learning Engineer",
                                "Machine Learning Engineer","Data Analyst","Machine Learning Engineer","Machine Learning Engineer","Data Analyst","Data Engineer","Data Scientist","Data Analyst","Data Scientist","Data Scientist"))
df<-df[!(df$company_location=="OC" | df$company_location=="AF" | df$company_location=="SA"),]
df<-df[!(df$employee_residence=="OC" | df$employee_residence=="AF" | df$employee_residence=="SA"),]
df= na.omit(df)
df <- droplevels(df)

smp_size <- floor(0.75 * nrow(df))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

ggplot(df, aes(x=remote_ratio, y=salary_in_usd, fill=remote_ratio)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Greens") +
  labs(x="Remote Working Status",y="Salary") + 
  theme_classic() + theme(legend.position="none")
