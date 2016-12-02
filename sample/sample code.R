
library(glmnet);library(bit64);library(data.table)
setwd("C:\\Users\\yuemeng1\\Desktop\\code\\grouplasso\\data")


data=fread("input_data.csv")
var.group=fread("input_var.csv")
date.group=fread("input_date_lkup.csv")

data[,week:=as.Date(week,"%m/%d/%Y")]



indepvar=c("twi","twi_lag1","twi_lag2","ggtrend","ggtrend_lag1","ggtrend_lag2","wt")

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
