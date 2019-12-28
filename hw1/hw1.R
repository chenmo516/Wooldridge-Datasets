library(DBI)
library(RSQLite)

con <- dbConnect(SQLite(),'wooldridge2.db')
dbListTables(con)
wpull <- function(strinput) {
  con <- dbConnect(SQLite(),'wooldridge2.db')
  dt <- dbReadTable(con,strinput)
  dt <- data.table(strinput)
  print(dbReadTable(con,'bwght_labels'))
  dbDisconnect(con)
  return(dt)
}

wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}

wage1 <- wpull('wage1') ## wage1 table
summary(wage1)
# Q1
##i
average_educ <- mean(wage1$educ)
average_educ
# The average education is 12.56274
max_edu <- max(wage1$educ)
max_edu
# The highest level of education is 18 years
min_edu <- min(wage1$educ)
min_edu
# The lowest level of education is 0 years

##ii
average_mean <- mean(wage1$wage)
average_mean
boxplot(wage1$wage,horizontal = T,main = "Average Hourly Wage",xlab = "Average Wage")
# Average hourly wage is $5.908992
# The wage is low as the average hourly wage as there are many outliers in the boxplot

##iii
#CPI for 1976= 56.9
#CPI for 2000= 218.056

##iv
hourly_wages <- average_mean*(218.056/56.9)
hourly_wages
# Average hourly wage of $22.64484 seems reasonable in terms of 2010.

##iv
female_count<-nrow(subset(wage1,wage1$female==1))
male_count<-nrow(subset(wage1,wage1$female==0))
female_count
male_count
# There are 252 women and 274 men in wage1 sample

#Q2
meap01 <- wpull('meap01') ## meap01 table
summary(meap01)
##i
largest_math4 <- max(meap01$math4)
smallest_math4 <- min(meap01$math4)
largest_math4
smallest_math4
# The largest value of math4 is 100
# The smallest value of math4 is 0
range_value <- range(meap01$math4)
range_value
# The range makes sense as the math4 value is in percentange and range is from 0-100
# The range makes sense because the attribute math pass rate is a percentage

##ii
total_schools <- nrow(meap01)
math_pass <- nrow(subset(meap01,meap01$math4==100))
math_pass
# 38 schools have a perfect pass rate on the math test
pass_percentage = (math_pass/nrow(meap01))*100
pass_percentage
# It is 2.08% of the total sample

##iii
pass_percentage_50 <- nrow(subset(meap01,meap01$math4==50))
pass_percentage_50
# 17 schools have math pass rates of exactly 50%

##iv
avg_math4 = mean(meap01$math4)
avg_read4 = mean(meap01$read4)
avg_math4
avg_read4
# The reading test is harder to pass as it has a low average pass rate(60.06188%) compared to the math test(71.909%)

##v
corr_value<- cor(meap01$math4,meap01$read4)
corr_value
# There is a strong relation between math and reading(0.8427281). High math scores indicate high reading scores

##vi
avg_exppp <- mean(meap01$exppp)
avg_exppp
sd_exppp <- sd(meap01$exppp)
sd_exppp
#since the sd is $1091.89, it has a wide variation

##vii
schoolA <- 6000
schoolB <- 5500
percentage_diff <- ((schoolA-schoolB)/schoolB)*100
percentage_diff
log_difference <- 100*(log(schoolA)-log(schoolB))
log_difference
# School A's spending School B's by 9.09% and 8.70% in natural logs
# The difference percentage is higher than the percentage calculated using natural logs

#Q3
data_401k <- wpull('401k') ## meap01 table
summary(data_401k)
avg_participation<-mean(data_401k$prate)
avg_participation
# The average participation rate is 87.36291%
avg_match<-mean(data_401k$mrate)
avg_match
# The average match rate is 0.7315124

##ii
l_r_model <- lm(prate~mrate,data=data_401k)
summary(l_r_model)
nrow(data_401k)
#The equation form of the model is : prate=83.0755 + 5.8611mrate
# R Squared is 0.0747
# The sample size of the dataset is 1534

##iii
summary(l_r_model)
# according to the linear regression equation, even with no match rate there is a 83.07% prate
# the equation implies that there is a 5.8% increase in prate with 1 increase in mrate

#iv
prate_3.5<- 83.0755+(5.8611*3.5)
prate_3.5
#the prate increases to 103.58% which is an impossible case

##v
cov(data_401k$prate,data_401k$mrate)
#Scatter Plot
plot(data_401k$mrate,data_401k$prate,main ="Scatterplot between match rate and participation rate",xlab="Match Rate",ylab="Participation Rate",col="black",abline(l_r_model,col="violet"))
# As per the plot above, we can say that the variation in prate with mrate is not linear
# It is observed that there is a high prate density near 100% , so irrespective of mrate value ,prate is 100%.
# The low value of R-squared indicates that mrate explains only 7.41% variation in prate


##Q4
ceosal2 <- wpull('ceosal2')
summary(ceosal2)

#i
avg_sal <- mean(ceosal2$salary)
avg_sal
# The average salary is $865.8644 thousand
avg_ten <- mean(ceosal2$ceoten)
avg_ten
# The average tenure is 7.7954802 years

##ii
nrow(subset(ceosal2,ceosal2$ceoten==0))
# 5 CEOs are in their first year
max(ceosal2$ceoten)
# The longest tenure in the given sample is 37 years

#iii
s_r_model <- lm(log(salary)~ceoten,data=ceosal2)
summary(s_r_model)
## the equation form of the model is : ln(salary)=6.505498 + 0.009724ceoten
salary_predicted <- exp(predict(s_r_model, data.table(ceoten=c(ceosal2$ceoten+1))))
summary(salary_predicted)
actual_salary <- exp(predict(s_r_model, data.table(ceoten=c(ceosal2$ceoten))))
summary(actual_salary)
percent_difference <- ((salary_predicted-actual_salary)/actual_salary)*100
percent_difference
mean(percent_difference)
# From the above, the salary for every one 1 year increase in CEO tenure increases by 0.977106%

##Q5
wage2 <- wpull('wage2')
summary(wage2)
mean(wage2$wage)
# The average salary is 957.9455
mean(wage2$IQ)
# The average IQ is 101.2824
sd(wage2$IQ)
# The standard deviation of IQ is 15.05264

#ii
sr_model_iq_wage <- lm(wage~IQ,data=wage2)
summary(sr_model_iq_wage)
predicted_wage <- predict(sr_model_iq_wage,data.table(IQ=c(wage2$IQ+15)))
actual_wage <- predict(sr_model_iq_wage,data.table(IQ=c(wage2$IQ)))
wage_difference <- predicted_wage-actual_wage
mean(wage_difference)
# wage increases by 124.546 with 15 point increase in IQ

#iii
iq_wage_model <- lm(log(wage)~IQ,data=wage2)
summary(iq_wage_model)
predicted_wage_new <- predict(iq_wage_model,data.table(IQ=c(wage2$IQ+15)))
actual_wage_new <- predict(iq_wage_model,data.table(IQ=c(wage2$IQ)))
wage_difference_percent <- predicted_wage_new-actual_wage_new
mean(wage_difference_percent)
# From the above calculation there is a 13.21073% increase in increase in IQ by 15 points

##Q6
meap93 <- wpull('meap93')
summary(meap93)

#i
math_expend_model <- lm(math10~expend,data=meap93)
summary(math_expend_model)
predicted_math_expend <- predict(math_expend_model,data.table(expend=c(meap93$expend+1)))
actual_math_expend <- predict(math_expend_model,data.table(expend=c(meap93$expend)))
expend_difference_percent <- ((predicted_math_expend-actual_math_expend)/actual_math_expend)*100
mean(expend_difference_percent)
# Each additional dollar spent has a diminishing effect on math pass rate

##ii
math_log_expend_model <- lm(math10~log(expend),data=meap93)
summary(math_log_expend_model)
predicted_math_log_expend <- predict(math_log_expend_model,data.table(expend=c((meap93$expend)*(1+0.1))))
actual_math_log_expend <- predict(math_log_expend_model,data.table(expend=c(meap93$expend)))
expend_difference <- predicted_math_log_expend-actual_math_log_expend
mean(expend_difference)
#Percent change in math pass is 1.0640
# β1 value(log(expend)) is 11.164
# (11.164)*(10/100) is approximately equal to 1.0640 and therefore β1/10 is the percentage point change in math10 given a 10% increase in expend

#iii
summary(math_log_expend_model)
nrow(meap93)
# Estimated equation is math10 = -69.341+ 11.164*(log(expend))
#Sample size is 408
#R squared values is 0.02966

#iv
mean(expend_difference)
## Increase in 10% if expenditure increases the math10 by 1.064081

#v
max(meap93$math10)
max(actual_math_log_expend)
#the largest value is only 66.7 which is below 100 and the largest possible value in the predicted model is 30.15376 which is also below 100
# there is nothing to worry in this case

##Q7
hprice1 <-wpull('hprice1')
summary(hprice1)

#i
price_model <- lm(price~bdrms+sqrft,data=hprice1)
summary(price_model)
# equation of the model is price = -19.31500 + 15.19819bdrms + 0.12844sqrft

#ii
actual_price_bdrms <- predict(price_model,data.table(bdrms=c(hprice1$bdrms),sqrft=c(hprice1$sqrft)))
predicted_price_bdrms <- predict(price_model,data.table(bdrms=c(hprice1$bdrms+1),sqrft=c(hprice1$sqrft)))
price_difference <- predicted_price_bdrms-actual_price_bdrms
mean(price_difference)
# the estimated increase in price for a house with one more bedroom, holding square footage constant is $15198.19

#iii
predicted_price_bdrms_new <- predict(price_model,data.table(bdrms=c(hprice1$bdrms+1),sqrft=c(hprice1$sqrft+140)))
price_difference_new <- predicted_price_bdrms_new-actual_price_bdrms
mean(price_difference_new)
comparison_value <- mean(price_difference_new) - mean(price_difference)
comparison_value
# The estimated price for additional room with extra 140 sqrft is $33179.26 which is $17981.07 higher than the previous estimated price of one additional room with constant squarefoot

#iv
summary(price_model)
# R-Squared value is 0.6319 and the percentage of the variation in price is 63.19%

#v
predicted_price_value <- predict(price_model,data.table(bdrms=c(4),sqrft=c(2438)))
predicted_price_value
# the predicted selling price for this house from the OLS regression line is approximately $354605.2

#vi
residual_price <- 300-predicted_price_value
residual_price
# The residual price is -$54605.25 and hence we can conclude that the buyer underpaid for the house

#Q8
summary(ceosal2)
head(ceosal2)

#i
annual_salary_model <- lm(log(salary)~log(mktval)+log(sales),data=ceosal2)
summary(annual_salary_model)
# From the above model, the equation of the model is log(salary) = 4.62092+ 0.10671log(mktval)+ 0.16213log(sales)

#ii
annual_salary_model_new <- lm(log(salary)~log(mktval)+log(sales)+log(profits),data=ceosal2)
## log(profits) : NaNs produced
range(ceosal2$profits)
## As log(profits) have negative values we cannot clearly define the model
annual_salary_model_proper <- lm(log(salary)~log(mktval)+log(sales)+profits,data=ceosal2)
summary(annual_salary_model_proper)
#From the above model, these firm performance variables explain only 29.93% change in CEO salaries and thus these variables does not explain
# most of the variation in CEO salaries

#iii
annual_salary_model_ceoten <- lm(log(salary)~log(mktval)+log(sales)+profits+ceoten,data=ceosal2)
summary(annual_salary_model_ceoten)
predicted_percent_return <- exp(predict(annual_salary_model_ceoten,data.table(mktval=c(ceosal2$mktval),sales=c(ceosal2$sales),profits=c(ceosal2$profits),ceoten=c(ceosal2$ceoten+1))))
actual_percent_return <- exp(predict(annual_salary_model_ceoten,data.table(mktval=c(ceosal2$mktval),sales=c(ceosal2$sales),profits=c(ceosal2$profits),ceoten=c(ceosal2$ceoten))))
difference_percent_return <- ((predicted_percent_return-actual_percent_return)/actual_percent_return)*100
mean(difference_percent_return)
# the estimated percentage return for another year of CEO tenure, holding other factors fixed is 1.17532%

#iv
cor(log(ceosal2$mktval),ceosal2$profits)
# The correlation value comes to 0.7768976, hence we can say that they both are co-related as the value is less than 1
# From the correlation we can say that log(mktval) has negligible effect on the CEO salary

#Q9
attend <- wpull('attend')

##i
min(attend$atndrte)
# The minimum value of attendance rate is 6.25%
max(attend$atndrte)
# The minimum value of attendance rate is 100%
mean(attend$atndrte)
# The average of attendance rate is 81.70956%
min(attend$priGPA)
# The minimum prior GPA is 0.857
max(attend$priGPA)
# The maximum prior GPA is 3.93
mean(attend$priGPA)
# The average mean of prior GPA is 2.586775
min(attend$ACT)
# The minimum ACT score is 13
max(attend$ACT)
# The minimum ACT score is 32
mean(attend$ACT)
# The average of ACT score is 22.51029

##ii
attendrte_model <- lm(atndrte~priGPA+ACT,data=attend)
summary(attendrte_model)
# The equation of this model is atndrte = 75.700+17.261priGPA-1.717ACT
# The intercept determines that 75.7% of students attend classes without prior GPA and ACT

##iii
attendrte_model
# The coefficient of pri GPA indicates that there is 17.261% increase in attendance with increase in 1 GPA
# The coefficient of ACT indicates that there is a 1.717% decrease in attendance with increase in 1 ACT score which is a surprise as the attendance rate usually increases

##iv
attendrte_model_new <- predict(attendrte_model,data.table(priGPA=c(3.65),ACT=c(20)))
attendrte_model_new
# The attendance rate goes to 104.3705% which is impossible as the max value is 100%
sample_student <- attend[priGPA==3.65 & ACT==20]
sample_student
# There is one student with 87.5% attendance rate.

##v
studentA <- predict(attendrte_model,data.table(priGPA=c(3.1),ACT=c(21)))
studentB <- predict(attendrte_model,data.table(priGPA=c(2.1),ACT=c(26)))
percent_difference_students <- studentA - studentB
percent_difference_students
# The predicted difference in attendance rate of studentA and studentB is 25.84336%

#Q10

htv <- wpull('htv')
summary(htv)

#i
range(htv$educ)
# The range of the sample is 6-20
grade_12_count <- nrow(subset(htv,htv$educ==12))
sample_size_educ <- nrow(htv)
percent_educ <- (grade_12_count/sample_size_educ)*100
percent_educ
# About 41.62602% of men have completed 12th grade but no higher grade
mean(htv$educ)
mean(htv$fatheduc)
mean(htv$motheduc)
# The men's level of education(13.0374) is higher compared to their father's education(12.44715) and their mother's education(12.17805)

##ii
educ_model <- lm(educ~motheduc+fatheduc,data=htv)
summary(educ_model)
# The equation of the model is educ = 6.96435 + 0.30420motheduc + 0.19029fatheduc
# 24.93% of sample variation in education is explained by these factors
# The coefficient of mother's education indicates that the men's education increases by 0.30420 for every 1 increase in mother's education

#iii
educ_ability_model <- lm(educ~motheduc+fatheduc+abil,data=htv)
summary(educ_ability_model)
# The equation of the model is educ = 8.44869+0.18913motheduc + 0.11109fatheduc + 0.50248abil
actual_abil_model<- predict(educ_ability_model,data.table(motheduc=c(htv$motheduc),fatheduc=c(htv$fatheduc),abil=c(htv$abil)))
predicted_abil_model<- predict(educ_ability_model,data.table(motheduc=c(htv$motheduc),fatheduc=c(htv$fatheduc),abil=c(htv$abil+1)))
difference_abil<- predicted_abil_model-actual_abil_model
mean(difference_abil)
# Ability explains the variation in education keeping parents' education as constant as the R-squared increases from 0.2493 to 0.4275.

#iv
educ_ability_model_new <- lm(educ~motheduc+fatheduc+abil+I(abil^2),data=htv)
summary(educ_ability_model_new)
p<-D(expression(0.190126*motheduc+0.108939*fatheduc+0.401462*abil+0.050599*abil^2),"abil")
p
min_abil_value <- Re(polyroot(c(0.401462,0.050599 *2)))
min_abil_value
min_educ_value<- predict(educ_ability_model_new,data.table(motheduc=c(0),fatheduc=c(0),abil=c(min_abil_value)))
min_educ_value
D(p,"abil")
# The minimum value of abil, abil* is -3.967094
# We do indeed have a minimum since the secodn derivative results in a positive number(0.050599 * 2)

#v
htv_sample_abil <- subset(htv,htv$abil<min_abil_value)
nrow(htv_sample_abil)
nrow(htv)
# Only 15 records out of 1230 records have ability less than the minimum ability value
# Therefore it is only a small fraction of men in the sample have ability less than the value calculated above

#vi
plot_educ_model<- ggplot(htv,aes(x=abil,y=educ))+geom_point()
plot_educ_model <- plot_educ_model + scale_x_continuous(name="Ability") + scale_y_continuous(name="Education Years")
plot_educ_model

plot_educ_model_new <- plot_educ_model + geom_line(aes(y=predict(educ_ability_model_new,data.table(motheduc=c(12.18),fatheduc=c(12.45),abil=c(htv$abil)))),color="green",size=1)
plot_educ_model_new