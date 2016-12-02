#' Use pool lasso regression to do variable selection,
#' If a variable has two very stong lags, then delete one of these 2 lag to let more variables in
#' @export
pool_lasso_reg = function(dep,actual,w,date.var,date.start,date.end,group,data,indepvar,
                          is.plot=T,is.weights = T,is.intercept = T,is.multithread = F,spec=rep("localhost",4)) {
  # dep="sales"
  # actual="sales"
  # w="sales_m"
  # date.var="week"
  # date.start="2015-01-01"
  # date.end="2015-02-21"
  # group=c("dmanum")
  # data=data
  # indepvar=c("twi","twi_lag1","twi_lag2","ggtrend","ggtrend_lag1","ggtrend_lag2","wt","wt_lag1","wt_lag2")
  # is.plot=T
  # is.weights = T
  # is.intercept = T
  # is.multithread = F
  # spec=rep("localhost",4)

  require(data.table);require(RcppEigen)

  # format independent variables and dependent variables
  index=data[[date.var]]>=date.start & data[[date.var]]<=date.end
  x=as.matrix(data[index,c(indepvar),with=F])
  y=as.matrix(data[index,c(dep),with=F])

  # lasso regression
  if(is.intercept & is.weights) {
    glmmod=glmnet(x,y,alpha=1,family='gaussian',weights = data[[w]][index],lower.limits=0)
  } else if (!is.intercept & is.weights) {
    glmmod=glmnet(x,y,alpha=1,family='gaussian',intercept = F,weights = data[[w]][index],lower.limits=0)
  } else if (is.intercept & !is.weights) {
    glmmod=glmnet(x,y,alpha=1,family='gaussian',lower.limits=0)
  } else if (!is.intercept & !is.weights) {
    glmmod=glmnet(x,y,alpha=1,family='gaussian',intercept = F,lower.limits=0)
  }
  cv.glmmod <- cv.glmnet(x,y,alpha=1,type.measure="deviance",nfolds=5)
  if(is.plot)  plot(glmmod,xvar="lambda",label=T)

  coefmatrix=predict(glmmod,type="coef")
  coefmatrix=as.data.table(t(as.matrix(coefmatrix)))

  # select the coefficient when no duplicated variables in the model
  coef_select=coefmatrix[,c(indepvar),with=F]
  for(i in 1:ncol(coef_select)) {
    varanme=var.group[variable==colnames(coef_select)[i]]$variable_name
    temp=paste(colnames(coef_select)[i],":=ifelse(",colnames(coef_select)[i],"==0,'0',varanme)")
    expr=parse(text=temp)
    coef_select[,eval(expr)]
  }

  vec=numeric()
  for(i in 1:nrow(coef_select)) {
    tempv=unlist(coef_select[i,])
    temp=sum(duplicated(tempv[!tempv %in% c("0")]))
    vec=c(vec,temp)
  }
  coef_select=cbind(coef_select,vec)

  #index to choose the final model
  index_final=which(coef_select$vec!=0)[1]-1
  if(is.na(index_final)) {
    index_final=nrow(coef_select)
    sucks_model_index=index_final
  } else {
    sucks_model_index = which(coef_select$vec!=0)[1]
  }

  coefmatrix[,lambda:=glmmod$lambda]

  #coefficient and lmbda to be output
  coef=t(coefmatrix[c(index_final,sucks_model_index)])
  # coef$variable=rownames(coef)
  coef=as.data.table(coef,keep.rownames = T)
  setnames(coef,c("rn","model_select","model_one_more_var"))

  coef=coef[(model_select+model_one_more_var)!=0]
  lmd_select=coef[rn=="lambda",model_select]
  coef=coef[rn!="lambda"]
  # if (lmd_select >cv.glmmod$lambda.1se) {
  #   print("Warning: Your lambda is too large, may fit poor.")
  # } else if (lmd_select <cv.glmmod$lambda.min) {
  #   print("Warning: Your lambda is too small.")
  # } else {
  #   print("lambda is in the range of cross validation values range.")
  # }
  lambda_check=data.table(lambda=lmd_select,lambda_min=cv.glmmod$lambda.min,lambda_lse=cv.glmmod$lambda.1se)
  # lmd_lse=cv.glmmod$lambda.1se

  # coeficient where min mse
  # coef(glmmod,s=cv.glmmod$lambda.1se)
  return(list(coef=coef,coef_all=coefmatrix,fit=cv.glmmod,lambda=lambda_check,date.start=date.start,date.end=date.end,actual=actual,
              date.var=date.var,dep=dep,w=w,group=group,is.dma=FALSE))

}
