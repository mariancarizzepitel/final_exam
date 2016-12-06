library(tidyverse)
library(apaTables)

# CREATE ANALYTIC DATA SET ---------------------------------------------------------

# load data
raw_data <- read_csv(file="exam_data_f16.csv",na=c("","NA","-999"))

# plug out age 
age <- raw_data$age 

# turn categorical variables into factors 
categorical_variables <- select(raw_data, gender)
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1,"Female"=2) 

# subsets of data for each scale 
agree_items <- select (raw_data, A1, A2, A3, A4, A5) 
conscient_items <- select (raw_data, C1, C2, C3, C4, C5)
jobperf_items <- select (raw_data, JP1, JP2, JP3, JP4, JP5)

# look for out of range data
psych::describe(agree_items) # has out of range data
psych::describe(conscient_items)
psych::describe(jobperf_items)

# convert out of range data to NA
is_bad_value <- agree_items<1 | agree_items>6 #take out "bad value", out of range data 
agree_items[is_bad_value] <- NA 

# fix reverse keyed items 
agree_items <- mutate(agree_items,A1=7-A1)
conscient_items <- mutate(conscient_items,C4=7-C4)
conscient_items <- mutate(conscient_items,C5=7-C5)
jobperf_items <- mutate(jobperf_items,JP1=7-JP1)
jobperf_items <- mutate(jobperf_items,JP2=7-JP2)

# score data 
agreeableness <- psych::alpha(as.data.frame(agree_items),check.keys = FALSE)$scores
conscientiousness <- psych::alpha(as.data.frame(conscient_items),check.keys = FALSE)$scores
performance <- psych::alpha(as.data.frame(jobperf_items),check.keys = FALSE)$scores

# create analytic data set
analytic_data <- cbind(agreeableness,conscientiousness,performance,categorical_variables,age)

# save csv 
write_csv(analytic_data,path="analytic_data.csv")
# ---------------------------------------------------------------------------------


# TABLES AND GRAPHS  ---------------------------------------------------------
# Table 1: Correlation tbale 
analytic_data_no.gender <- select(analytic_data, -gender) #delete gender column 
apa.cor.table(analytic_data_no.gender,filename="Table1.doc",table.number=1)
psych::alpha(analytic_data_no.gender) # cronbach's alpha 

# Figure 1: Checking for non-linear relations 
psych::pairs.panels(as.data.frame(analytic_data_no.gender),lm=TRUE)
# ---------------------------------------------------------------------------------



# MULTIPLE REGRESSION - COMPLETE DATA ---------------------------------------------------------

# regression analysis 
reg1 <- lm(performance ~ conscientiousness, data=analytic_data)
reg2 <- lm(performance ~ conscientiousness + agreeableness, data=analytic_data)
apa.reg.table(reg1,reg2,filename="Table2_regression.doc", table.number =2)
# ---------------------------------------------------------------------------------



# MULTIPLE REGRESSION - GENDER -------------------------------------------------------

# create separate subsets for males and females
analytic_data_males <- filter(analytic_data,gender=="Male")
analytic_data_females <- filter(analytic_data,gender=="Female")

# multiple regression for males 
reg1_males <- lm(performance ~ conscientiousness, data=analytic_data_males)
reg2_males <- lm(performance ~ conscientiousness + agreeableness, data=analytic_data_males)
apa.reg.table(reg1_males,reg2_males,filename="Table3_regression_males.doc", table.number =3)

# multiple regression for females 
reg1_females <- lm(performance ~ conscientiousness, data=analytic_data_females)
reg2_females <- lm(performance ~ conscientiousness + agreeableness, data=analytic_data_females)
apa.reg.table(reg1_females,reg2_females,filename="Table4_regression_females.doc", table.number =4)
