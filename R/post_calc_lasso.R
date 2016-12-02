#' @export
post_calc_lasso=function(data,fit=lasso_run,var.group,date.group,group,is.output=F,is.graph=T) {
  # data=data
  # fit=lasso_dma
  # group=c("group1")
  # is.output=F
  # is.graph=T

  require(shiny);require(rCharts);require(DT)

  date.start=fit$date.start
  date.end=fit$date.end
  date.var=fit$date.var
  if (!fit$is.dma) {
    coef=fit$coef[,c("rn","model_select"),with=F]
    coef=coef[model_select!=0]
    varnames=coef$rn
    coef=coef$model_select

    varnames=replace(varnames,varnames=="(Intercept)","int")
    # varnames=varnames[!varnames %in% c("(Intercept)")]

    index=data[[date.var]]>=date.start & data[[date.var]]<=date.end
    data=data.table(int=rep(1,nrow(data)),data)
    X=data[index,c(varnames,fit$date.var,fit$group,fit$dep,fit$actual,fit$w),with=F]
    X$yhat=(as.matrix(X[,varnames,with=F]) %*% coef) * X[[fit$w]]
    for (i in 1:length(varnames)){
      X[[varnames[i]]]=coef[i]*X[[fit$w]]*X[[varnames[i]]]
    }
    # X$res=X[[fit$actual]]-X$yhat
    # decomp.date=X[,lapply(.SD,sum),by=c(date.var),.SDcols=c(varnames,"yhat","res",fit$actual)]
    # R2=1-(sum((X[["res"]]/X[[fit$w]])^2))/sum((X[[fit$dep]]-mean(X[[fit$dep]]))^2)
    # mape=mean(abs(decomp.date$res/decomp.date[[fit$actual]]))
    #
    # output=f_decomp_output(decomp=X,fit,date.start,date.end,date.var,var.group,date.group,group,is.output,is.graph)
    #
    # return(list(R2=R2,mape=mape,decomp=output$decomp,con=output$con,app=output$app))

  } else {
    coef=fit$coef
    coef[is.na(coef)]=0
    varnames=coef$rn
    coef=melt.data.table(coef,id.vars=c("rn"))
    coef[rn=="(Intercept)",rn:="int"]

    varnames=replace(varnames,varnames=="(Intercept)","int")
    setnames(coef,"value","coefficient")

    index=data[[date.var]]>=date.start & data[[date.var]]<=date.end
    data=data.table(int=rep(1,nrow(data)),data)
    X=data[index,c(varnames,fit$date.var,fit$group),with=F]
    X=melt.data.table(X,id.vars=c(fit$group,fit$date.var))
    setnames(X,c(fit$group,"variable"),c("variable","rn"))
    X[,variable:=paste(fit$group,variable,sep="_")]
    X=merge(X,coef,by=c("rn","variable"),all.x=T)
    X[,value:=value*coefficient]
    X[,coefficient:=NULL]
    X=dcast.data.table(X,variable+week~rn)
    setnames(X,"variable",fit$group)

    final=data[index,c(fit$date.var,fit$group,fit$dep,fit$actual,fit$w),with=F]
    setnames(final,fit$group,"variable")
    final[,variable:=paste(fit$group,variable,sep="_")]
    setnames(final,"variable",fit$group)

    X=merge(X,final,by=c(fit$group,fit$date.var),all=T)
    for (i in 1:length(varnames)){
      X[[varnames[i]]]=X[[fit$w]]*X[[varnames[i]]]
    }
    temp=paste("yhat:=",paste(varnames,collapse="+"),sep="")
    expr=parse(text=temp)
    X[,eval(expr)]

  }

  X$res=X[[fit$actual]]-X$yhat
  decomp.date=X[,lapply(.SD,sum),by=c(date.var),.SDcols=c(varnames,"yhat","res",fit$actual)]
  R2=1-(sum((X[["res"]]/X[[fit$w]])^2))/sum((X[[fit$dep]]-mean(X[[fit$dep]]))^2)
  mape=mean(abs(decomp.date$res/decomp.date[[fit$actual]]))

  output=f_decomp_output(decomp=X,fit,date.start,date.end,date.var,var.group,date.group,group,is.output,is.graph)
  return(list(R2=R2,mape=mape,decomp=output$decomp,con=output$con,app=output$app))

}
