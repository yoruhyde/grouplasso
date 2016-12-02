#' @export
loop_lasso_reg = function(dep,actual,w,date.var,date.start,date.end,group,data,indepvar,
                          is.plot=F,is.weights = F,is.intercept = T,is.multithread = F,spec=rep("localhost",4)) {

  # dep="sales"
  # actual="sales_i"
  # w="sales_m"
  # indepvar=indepvar
  # date.var="week"
  # date.start="2015-01-01"
  # date.end="2015-02-21"
  # group="dmanum"
  # data=data
  # is.plot=F
  # is.weights = T
  # is.intercept = T
  # is.multithread=F
  # spec=rep("localhost",4)

  require(data.table);require(RcppEigen)
  comb <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  }

  loop=unique(data[[group]])

  if (is.multithread){
    if (is.null(spec)) stop("Error: spec cannot be null if is.multithread is True.") else{
      cl=makeCluster(spec,type="SOCK",outfile="")
      registerDoSNOW(cl)
      temp.result=foreach(i=1:length(loop),.multicombine = T,.combine = "comb",.init = list(list(),list(),list()),
                          .packages=c("data.table","RcppEigen")) %dopar%
                          {
                            print(paste(i," Cross section: ",loop[i],sep=""))
                            index=(1:nrow(data))[data[[group]]==loop[i]]
                            temp=data[index]
                            fit=pool_lasso_reg(dep=dep,actual=actual,w=w,date.start=date.start,date.end=date.end,
                                               date.var=date.var,data=temp,indepvar=indepvar,group=group,is.plot=is.plot,
                                               is.weights = is.weights,is.intercept=is.intercept,is.multithread=is.multithread,spec=spec)
                            coef=fit$coef
                            coef[,c("model_one_more_var"):=NULL]
                            coef=coef[model_select!=0]
                            setnames(coef,"model_select",paste(group,loop[i],sep="_"))
                            # coef[is.na(coef)]=0
                            la
                            list(coef,fit,group=loop[i])
                          }
      stopCluster(cl)
    }
  }else{
    temp.result=foreach(i=1:length(loop),.multicombine = T,.combine = "comb",.init = list(list(),list(),list(),list()),
                        .packages=c("data.table","RcppEigen")) %do%
                        {
                          print(paste(i," Cross section: ",loop[i],sep=""))
                          index=(1:nrow(data))[data[[group]]==loop[i]]
                          temp=data[index]
                          fit=pool_lasso_reg(dep=dep,actual=actual,w=w,date.start=date.start,date.end=date.end,
                                      date.var=date.var,data=temp,indepvar=indepvar,group=group,is.plot=is.plot,
                                      is.weights = is.weights,is.intercept=is.intercept,is.multithread=is.multithread,spec=spec)
                          coef=fit$coef
                          coef[,c("model_one_more_var"):=NULL]
                          coef=coef[model_select!=0]
                          setnames(coef,"model_select",paste(group,loop[i],sep="_"))
                          coef_all=fit$coef_all
                          lmd=fit$lambda
                          lmd[,fit$group:=loop[i]]
                          # coef[is.na(coef)]=0
                          list(coef,coef_all,lmd,group=loop[i])
                        }
  }
  coef.matrix=Reduce(function(...) merge(...,by=c("rn"),all=T),temp.result[[1]])
  lmd.matrix=do.call("rbind",temp.result[[3]])
  # fit=temp.result[[2]]
  # return(list(coef=coef.matrix,fit=fit,group=temp.result[[3]]))
  return(list(coef=coef.matrix,coef.list=temp.result[[2]],lambda=lmd.matrix,date.var=date.var,date.start=date.start,date.end=date.end,dep=dep,actual=actual,w=w,group=group,is.dma=TRUE))
}
