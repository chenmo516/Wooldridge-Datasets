#Q1
#from ps5.R
n <- 500
set.seed(75080)

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 50
y   <- -100*z+ 1100 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt1 <- data.table('id'=1:500,'sat'=y,'income'=x,'group'=rep(1,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 80
y   <- -80*z+ 1200 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt2 <- data.table('id'=501:1000,'sat'=y,'income'=x,'group'=rep(2,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 30
y   <- -120*z+ 1000 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt3 <- data.table('id'=1001:1500,'sat'=y,'income'=x,'group'=rep(3,n))

dtable <- merge(dt1    ,dt2, all=TRUE)
dtable <- merge(dtable ,dt3, all=TRUE)

ggplot(dtable,aes(x=income,y=sat,color=as.factor(group)))+geom_point()
# There are three tables containing id, sat, income and group.
# There's a positive correlation between sat and income i.e as sat
# increases there is an increase in income

#Q2
dtable$group <- as.factor(dtable$group)
dtable_model <- lm(sat~income,data=dtable)
summary(dtable_model)
#The equation is of the form sat = 950.8914 + 2.7923*income
# In the above model, as the income increases, there is a 2.7923 increase in sat
dtable_model1 <- lm(sat~income+group-1,data=dtable)
summary(dtable_model1)
#The equation is of the form sat = -20.173*income + 2111.255*group1 + 2812.183*group2 + 1605.304*group3
# In the above model, an increase in income, decreases the sat by 20.173
dtable_model_group_1 <- lm(sat~income,data=dtable[group==1])
summary(dtable_model_within_1)
#The equation is of the form sat = 2095.3391 - 19.8534*income
# In the above model, an increase in income, decreases the sat by 19.8534
dtable_model_group_2 <- lm(sat~income,data=dtable[group==2])
summary(dtable_model_within_2)
#The equation is of the form sat = 2505.1204 - 16.3257*income
# In the above model, an increase in income, decreases the sat by 16.3257
dtable_model_group_3 <- lm(sat~income,data=dtable[group==3])
summary(dtable_model_within_3)
#The equation is of the form sat = 1722.484 - 24.027*income
# In the above model, an increase in income, decreases the sat by 24.027
# In the first model, the equation is formed without taking group into consideration
# In the second model, the groups are fixed into a single variable
# In the last three models, the groups are splitted into separate entities.

#Q3
library(party)
plot(ctree(sat~income,data=dtable))
plot(ctree(sat~group,data=dtable))
plot(ctree(sat~income+group,data=dtable))
#These plots give us the media SAT scores of people with different income levels.
#They also tell us the median SAT scores of people in various groups.

#Q4
library(glmtree)
plot(glmtree(sat~income|group,data=dtable))
#The estimates are very similar to the estimates we got by running the models separately from the above problems

#Q5
wss <- kmeans.wss(dtable[,.(income,sat)])
wss
eratio(wss)
plot.wss(wss)
kmeans_model <- kmeans(dtable[,.(income,sat)],2,nstart=10)
kmeans_model$centers
dtable$kgroup1 <- as.factor(kmeans_model$cluster)
#income       sat
#1 60.76117 1198.0552
#2 43.16161  964.6751

#Q6
table(dtable$kgroup1,dtable$group)
wss <- hclust.wss(dtable[,.(income,sat)])
wss
eratio(wss)
plot.wss(wss)
hclust_model <- hclust(dist(dtable[,.(income,sat)]))
dtable$hgroup <- as.factor(cutree(hclust_model,4))
table(dtable$hgroup,dtable$group)
#K means is effective in 297 out of 500 data points for group 1, 456 in group 2, and 384 in group 3.
#If we do hierarchical clustering, the optimal number of groups is 4

#Q7
model_2 <-lm(sat~income+kgroup1-1,data=dtable)
summary(model_2)
# The equation is of the form sat = 0.6112*income+1160.9197*kgroup11 + 938.2959*kgroup12
model_3 <-lm(sat~income+hgroup-1,data=dtable)
summary(model_3)
# The equation is of the form sat = 1.396e-01*income+1160.9197*kgroup11 + 938.2959*kgroup12
#For within effects model, the relationships in the generated data model and the relationship in k means clustering model are different.
#Hierarchical clustering also does not give same results as the earlier data generation process.

#Q8
wss <- kmeans.wss(dtable[,.(income)])
wss
eratio(wss)
plot.wss(wss)
kmeans_model2 <- kmeans(dtable[,.(income)],3,nstart=10)
kmeans_model2$centers
dtable$kgroup2 <- as.factor(kmeans_model2$cluster)
table(dtable$kgroup2,dtable$group)
summary(lm(sat~income+kgroup2-1,data=dtable))
#For group 1, k means clustering identifies 489 out of 500 data points correctly.
#For group 2, is is 500 out of 500. The k means accuracy for group 2 is 100 %.
#For group 3, k means clustering correctly identifies 484 out of 500 data points
accuracy <- ((489+500+484)/1500)*100
accuracy
# With the overall accuracy of 98.2 %, taking only the income variable significantly increases the accuracy of the estimation now.
# The relationships obtained from the k means estimation is very similar to the relationships obtained in data generation process.

#Q9
library(pracma)
wss <- kmeans.wss(dtable[,.(scale(income),scale(sat))])
wss
eratio(wss)
plot.wss(wss)
kmeans_model3 <- kmeans.scale(dtable[,.(income,sat)],3)
kmeans_model3$centers
kmeans_model3$means
dtable$kgroup3 <- as.factor(kmeans_model3$cluster)
table(dtable$kgroup3,dtable$group)
summary(lm(sat~income+kgroup3-1,data=dtable))
#371 out of 500 for group 1, 500 out of 500 for group 2, 330 out of 500 for group 3.
#The overall accuracy in the case is roughly 80%.


































































