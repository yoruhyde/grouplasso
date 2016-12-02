# library(glmnet)
# 
# age <- c(4,8,7,12,6,9,10,14,7) 
# gender <- c(1,0,1,1,1,0,1,0,0) ; gender<-as.factor(gender)
# bmi_p <- c(0.86,0.45,0.99,0.84,0.85,0.67,0.91,0.29,0.88) 
# m_edu <- c(0,1,1,2,2,3,2,0,1); m_edu<-as.factor(m_edu)
# p_edu <- c(0,2,2,2,2,3,2,0,0); p_edu<-as.factor(p_edu)
# f_color <- c("blue", "blue", "yellow", "red", "red", "yellow", "yellow", "red", "yellow")
# asthma <- c(1,1,0,1,0,0,0,1,1)
# 
# f_color <- as.factor(f_color)
# xfactors <- model.matrix(asthma ~ gender + m_edu + p_edu + f_color)[,-1]
# x <- as.matrix(data.frame(age, bmi_p, xfactors))
# 
# #note alpha =1 for lasso only and can blend with ridge penalty down to alpha=0 ridge only
# glmmod<-glmnet(x,y=as.factor(asthma),alpha=1,family='binomial')


library(glmnet);library(bit64);library(data.table)
setwd("C:\\Users\\yuemeng1\\Desktop\\LASSO")

# marketdim=c("dmanum")
# timedim=c("week")
# dep=c("sales")


data=fread("input_data.csv")
var.group=fread("input_var.csv")
date.group=fread("input_date_lkup.csv")

data[,week:=as.Date(week,"%m/%d/%Y")]

# select from all variables (multiple groups)
# pool regression
# regression by dma



indepvar=c("twi","twi_lag1","twi_lag2","ggtrend","ggtrend_lag1","ggtrend_lag2","wt")
# indepvar=c("twi","twi_lag1","twi_lag2")

# variable selection by using LASSO regression (pool regression)
lasso_run=pool_lasso_reg(dep="sales_i",
                 actual="sales",
                 w="sales_m",
                 date.var="week",
                 date.start="2015-01-01",
                 date.end="2015-02-21",
                 group="dmanum",
                 data=data,
                 indepvar=indepvar,
                 is.plot=T,is.weights = T,is.intercept = T,is.multithread = F,spec=rep("localhost",4))

plot(lasso_run$fit)

coef_select=lasso_run$coef
coef_all=lasso_run$coef_all

# post calc for LASSO reg
a=post_calc_lasso(data=data,
                  fit=lasso_run,
                  var.group,
                  date.group,
                  group=c("group1"),
                  is.output=F,
                  is.graph=T)

runApp(a$app)

# any weighted linear regression needed? --No
# Loop by DMA
lasso_dma=loop_lasso_reg(dep="sales_i",
                         actual="sales",
                         w="sales_m",
                         date.var="week",
                         date.start="2015-01-01",
                         date.end="2015-02-21",
                         group="dmanum",
                         data=data,
                         indepvar=indepvar,
                         is.plot=F,is.weights = T,is.intercept = T,is.multithread = F,spec=rep("localhost",4))


a=post_calc_lasso(data=data,
                  fit=lasso_dma,
                  var.group,
                  date.group,
                  group=c("group1"),
                  is.output=F,
                  is.graph=T)

runApp(a$app)


# lmtest=lm(y~x)
# coef(lmtest)
# 
# # x=matrix(rnorm(100*20),100,20)
# # y=rnorm(100)
# # g2=sample(1:2,100,replace=TRUE)
# # g4=sample(1:4,100,replace=TRUE)
# # fit1=glmnet(x,y)
# # predict(fit1,newx=x[1:5,],s=c(0.01,0.005))
# # predict(fit1,type="coef")
# # fit2=glmnet(x,g2,family="binomial")
# # predict(fit2,type="response",newx=x[2:5,])
# # predict(fit2,type="nonzero")
# # fit3=glmnet(x,g4,family="multinomial")
# # predict(fit3,newx=x[1:3,],type="response",s=0.01)
# 
# write.csv(aaaa,"coef matrix.csv",row.names=F)
