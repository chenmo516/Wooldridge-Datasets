#Q1

##i
# We can interpret that from the equation that for every 1% increase in the expenditure of A, the vote count of A increases by 6.08136%
vote1 <- wpull('vote1')
vote_model <- lm(voteA~log(expendA)+log(expendB)+prtystrA,data=vote1)
summary(vote_model)
vote_model_new <- lm(voteA~log(expendA)+I(log(expendA)-log(expendB))+prtystrA,data=vote1)
summary(vote_model_new)

##ii
#Null Hypothesis is true. It states that a 1% increase in A's expenditures is offset by a 1% increase in B's expenditures. Alternative Hypothesis Ha : B1 + B2 != 0

##iii
#The equation is of the form voteA = 45.08788 + 6.08136log(expendA)-6.61563log(expendB)+0.15201prtystrA
#The expenditure of A and B affect the outcome because of the t-stat value
#The coefficients of log(expendA) and log(expendB) have opposite signs and therefore we can use these results to test the hypothesis in part 2

##iv
#The equation is of the form voteA = 45.08788 - 0.53427log(expendA)+6.61563I(log(expendA)-log(expendB))+0.15201prtystrA
#The coefficient of log(expendA) now becomes -0.53427 and a t-stat value of -1.002

##v
# From the above we can conclude that the null hypothesis does not get rejected

##Q2
#i
lawsch85 <- wpull('lawsch85')
salary_model <- lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank,data=lawsch85)
summary(salary_model)
lawsch85$LSAT[which(is.na(lawsch85$LSAT))]<-median(lawsch85$LSAT,na.rm = TRUE)
lawsch85$GPA[which(is.na(lawsch85$GPA))]<-median(lawsch85$GPA,na.rm = TRUE)
# The Null Hypothesis states that there is no change in median starting salary with respect to the rank of law school. So H0 = 0, Ha > 0 or Ha < 0. Since it is two-sided test at 95% significance we can use thershold value of 1.98. Now |-9.429|>1.98 we reject the hypothesis

#ii
salary_model_new <- lm(log(salary)~ LSAT + GPA + log(libvol) + log(cost) + rank, data = lawsch85)
summary(salary_model_new)
salary_model_new2 <- lm(log(salary)~ log(libvol) + log(cost) + rank, data = lawsch85)
summary(salary_model_new2)
Fstat1<- ((0.8446-0.8221)/2)/((1-0.8446)/135)
Fstat1
#The t-statistic on LSAT is 1.065 and that on GPA is 2.749 which are significant at 95% confidence level. The F-statistics for LSAT and GPA is 9.773166 and so jointly they are very significant

#iii
lawsch85$clsize[which(is.na(lawsch85$clsize))]<-median(lawsch85$clsize,na.rm = TRUE)
lawsch85$faculty[which(is.na(lawsch85$faculty))]<-median(lawsch85$faculty,na.rm = TRUE)
summary(lm(log(salary)~ LSAT + GPA + log(libvol) + log(cost) + rank + clsize + faculty, data = lawsch85))
Fstat2<- ((0.8471-0.8446)/2)/((1-0.8471)/133)
Fstat2
#The F-statistic for class size and faculty size is 1.087312. So, jointly these 2 factors are not so significant

#iv
# Factors like infrastructure, quality of professors, less media influence thereby people not knowing about it might influence the rank of the law school

##Q3
hprice1 <- wpull('hprice1')
price_model <- lm(log(price)~sqrft+bdrms,data=hprice1)
summary(price_model)
tidy(price_model)
confidence_interval <- 150*0.000379+0.0289
confidence_interval
#i
#θ1=0.08575
price_model_new <- lm(log(price)~ I(sqrft - 150 * bdrms) + bdrms, data = hprice1)
summary(price_model_new)

#ii
#β2=CI - 150β1. log(price)= β0 + β1(sqrft - 150bdrms) + CIbdrms + u


#iii
#Using part 2 of the equation the standard error is 0.0268 and using the obtained standard error
#the confidence interval is between 0.03258(3.258%) and 0.1390(13.9022%)
tidy(price_model_new)
bedrooms_model <-lm(log(price)~ I(sqrft - 150 * bdrms) + bdrms, data = hprice1)
confint(bedrooms_model)

#Q4
wage2 <- wpull('wage2')
std_wage_equation <- lm(log(wage)~educ+exper+tenure,data=wage2)
summary(std_wage_equation)
tidy(std_wage_equation)
std_wage_equation_new <- lm(log(wage)~educ+exper+I(tenure+exper),data=wage2)
summary(std_wage_equation_new)
tidy(std_wage_equation_new)
#i
# From the above, we can conclude that null hypothesis that another year of general workforce experience has the same effect on
# ln[wage] as another year of tenure with the current employer

#ii
# From the abive models thw significance levle is at 5% and also the p-value of the t-value goes to 68.1%
# and hence we fail to reject null hypothesis

#Q5
data_401Ksubs <- wpull('401Ksubs')
single_person_households <- data_401Ksubs[data_401Ksubs$fsize==1,]
single_person_households_number <- nrow(single_person_households)
single_person_households_number
#i
# There are 2017 single-person households in the dataset

#ii
net_financial_wealth_model <- lm(nettfa~inc+age,data=single_person_households)
summary(net_financial_wealth_model)
# The equation is of the form nettfa = -43.03981 + 0.79932inc + 0.84266age + u
# As per the equation we can see that for every increase in age by 1 the net financial assets increase by 0.84266($842.66)
# and for every increase in annual income by 1($1000), the net financial asset increase by 0.79932($799.32)

#iii
# The intercept does not make sense as it goes to a negative value
# which means that it gives the net financial value of people with negative age which is impossible
# There are no surprises in the slope values

#iv
tidy(net_financial_wealth_model)
t_stat_null <- (0.84266-1)/0.09202
t_stat_null
# The t-stat under the null is: (0.84266-1)/0.09202= -1.7098. p-value is 0.9562758

#v
pt(t_stat_null,2014)
nrow(single_person_households)
sph_income_model <- lm(nettfa~inc,data=single_person_households)
summary(sph_income_model)
cor(single_person_households$age,single_person_households$inc)
# The estimated coefficient on inc has increased, it is now 0.8207.This is because it is not a perfect model and there is some covariance in errors between 2 independent variables.

#Q6
kielmc <- wpull('kielmc')
kielmc_81<-kielmc[kielmc$year==1981,]
house_price_model <- lm(log(price)~log(dist),data=kielmc_81)
summary(house_price_model)
#i
#The equation is:- log(price)=8.04716+0.36488log(dist)
# The causal effect of dist on price means that Beta1>=0, hence positive sign.
# From the equation, we can conclude that every 1% increase in distance from the incinerator, the price increases by 0.36488

#ii
house_price_model_new <- lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age,dat=kielmc_81)
summary(house_price_model_new)
# The effect of the incinerator on price has decreased drastically and not so significant too. The conflict with the above result is because there are other factors like area and baths which are much more significant

#iii
hpm_new1 <- lm(log(price)~log(dist)+log(intst)+I((log(intst))^2)+log(area)+log(land)+rooms+baths+age,dat=kielmc_81)
summary(hpm_new1)
# The coefficient -0.11 becomes significant now which means that as the distance from the home to the interstate increases, the price decreases. Also, the significant level of inst has changed greatly.

#iv
hpm_new_2 <- lm(log(price)~I((log(dist))^2)+log(dist)+log(intst)+I((log(intst))^2)+log(area)+log(land)+rooms+baths+age,dat=kielmc_81)
summary(hpm_new_2)
#The square of log(dist) is not so significant as it's coefficient is -0.036418 and also the t-value is -0.331

#Q7
#i
wage1 <- wpull('wage1')
wage_model <- lm(log(wage)~educ+exper+I(exper^2),data=wage1)
summary(wage_model)
# The equation is of the form log(wage)=0.1263226+0.0906207educ+0.0409731exper-0.0007121exper^2

#ii
# exper^2 is statistically significant at 1% level as the t-value is -6.141 with a p-value of 0

#iii
wage_5yr <- 100*(0.0409 - 2*0.0007*4) * 1
wage_5yr
wage_20yr <- 100*(0.0409 - 2*0.0007*19) * 1
wage_20yr
# The estimated return to 5th year is 3.54% and the estimated return to 20th year is 1.43%

#iv
wage_lowering <- (0.0409)/(2*0.0007)
wage_lowering
nrow(wage1[which(wage1$exper>=round(wage_lowering,0)),])
# When percentage change is 0%,exper=29.21429. There are 121 people having more experience in this sample

#Q8
#i
wage_model_q8 <-lm(log(wage)~educ+exper+I(educ*exper), data=wage2)
summary(wage_model_q8)
# Holding exper fixed,the difference in log(wage) for another year of education is 0.04725277 which is equal to the (0.044050+0.003203) i.e. Beta1+Beta3*exper

#ii
predict(wage_model_q8,data.table(educ=2,exper=1))-predict(wage_model_q8,data.table(educ=1,exper=1))
0.044050+0.003203
# Null hypothesis is B3=0. This means that people with more education and more experience earns more. Alternative hypothesis is B3>0

#iii
wage_model_q8_nh <-lm(log(wage)~educ+exper+I(educ*exper), data=wage2)
wage_model_q8_nh_new <-lm(log(wage)~educ+exper, data=wage2)
pt(abs(0.025),931)
wage_model_q8_2 <- lm(log(wage)~educ+exper+I(educ*(exper-10)), data=wage2)
# B3 has t-value of 2.095 which is greater than the critical value.Hence, B3-0 is not justified

#iv
wage_model_q8_new <-lm(log(wage)~educ+exper+I(educ*(exper-10)), data =wage2)
confint(wage_model_q8_new)
# The equation is: log(wage)=5.949455+0.076080educ-0.021496exper+0.003203educ*(exper-10).Theta(C)=0.076080 and the standard error is 0.006615. 95% CI is 0.06309 to 0.08906

#Q9

#i
library(Deriv)
gpa2 <- wpull('gpa2')
sat_model <- lm(sat~hsize+I(hsize^2),data=gpa2)
summary(sat_model)
# The equation is of the form  sat=997.981+ 19.814hsize -2.131hsize^2 . The quadratic term is statistically significant with a t-value of -3.881 and P(>t) almost 0

#ii
p<-D(expression(997.981+ 19.814*hsize -2.131*hsize^2),"hsize")
p
hsizemax <- Re(polyroot(c(19.814,-2.131 *2)))
hsizemax
# The optimal value for high school size is hsizemax=4.648991 or 465. It is the maximum value of hsize which is obtained by first derivative of the regression equation wrt hsize

#iii
# No. This data is only for those students who have taken the SAT test and got in to the high school.

#iv
sat_model_new <-lm(log(sat)~hsize+I(hsize^2),data=gpa2)
summary(sat_model_new)
q<-D(expression(6.8960291+ 0.0196029*hsize -0.0020872*hsize^2),"hsize")
q
hsize_log <- Re(polyroot(c(0.0196029,-0.0020872 *2)))
hsize_log
# Estimated equation: log(sat)=6.8960291+0.0196029hsize -0.0020872hsize^2 . Yes, there is a difference of 470-465=5 for optimal class size compared to the previous model

#Q10
#i
hprice1 <- wpull('hprice1')
hprice_model<-lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=hprice1)
summary(hprice_model)
# Equation: log(price)=-1.29704+0.16797log(lotsize)+0.70023log(sqrft)+0.03696bdrms

#ii
predicted_hprice_model <- predict(hprice_model,data.table(lotsize=20000,sqrft=2500,bdrms=4))
predicted_hprice_average <- mean(predicted_hprice_model)
exp_pre_model <- exp(predict(hprice_model,data.table(lotsize=20000,sqrft=2500,bdrms=4)))*exp(0.1846/2)
exp_pre_average <- mean(exp_pre_model)
predicted_hprice_average
exp_pre_average
# The predicted value of log(price) is 5.992899 . The predicted value of price is 439.307 for same values of explanatory variables

#iii
hprice_model_new <- lm(price~lotsize+sqrft+bdrms,data=hprice1)
summary(hprice_model_new)
# The new model price=A+B(lotsize)+C(sqrft)+D(bdrms)+u is better as it has a R-squared of 67.24% compared to the previous model which had a R-squared of 18.46%


























