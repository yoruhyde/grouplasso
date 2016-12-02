#' @export
f_decomp_output=function(decomp,fit,date.start,date.end,date.var,var.group,date.group,group,is.output=F,is.graph=T){
  #decomp=X
  
  index=decomp[[fit$date.var]]>=fit$date.start & decomp[[fit$date.var]]<=fit$date.end
  decomp=decomp[index]
  expr=paste(c(fit$date.var,fit$group),collapse = ",")
  expr=paste("order(",expr,")")
  decomp=decomp[eval(parse(text=expr))]
  
  date.group[[fit$date.var]]=as.Date(date.group[[fit$date.var]],"%m/%d/%Y")
  index=var.group$variable %in% names(decomp)
  var.group=var.group[index]
  decomp=merge(decomp,date.group,by=c(date.var),all.x=T)
  dim.var=group[group %in% names(var.group)]
  dim.date=group[group %in% names(date.group)]
  
  f_group=function(x){
    #x=unique(var.group[[dim.var]])[1]
    dim=var.group$variable[var.group[[dim.var]]==x]
    expr=paste(x,"=",paste(dim,collapse = "+"))
    expr=parse(text=paste("list(",expr,")"))
    decomp[,eval(expr)]
  }
  var.name=unique(var.group[[dim.var]])
  temp=lapply(var.name,f_group)
  temp=Reduce(cbind,temp)
  
  index=var.group$variable %in% names(decomp)
  temp.result=cbind(decomp[,!c(var.group$variable[index]),with=F],temp)
  setnames(temp.result,c(fit$actual,"yhat","res"),c("Actual","Predicted","Residual"))
  forplot=temp.result[,c(fit$date.var,fit$group,"Actual","Predicted","Residual",var.name),with=F]
  forplot.date=temp.result[,lapply(.SD,sum),by=c(fit$date.var),.SDcols=c("Actual","Predicted","Residual",var.name)]
  forplot.date[[fit$group]]="All"
  forplot=rbind(forplot.date,forplot)
  
  # calc contribution
  if (length(dim.date)>0){
    temp.con=temp.result[,lapply(.SD,sum),by=c(dim.date),.SDcols=c("Actual","Predicted","Residual",var.name)]
  }else{
    temp.con=temp.result[,lapply(.SD,sum),.SDcols=c("Actual","Predicted","Residual",var.name)]
  }
  temp.con[[fit$group]]="All"
  temp.con.all=temp.result[,lapply(.SD,sum),by=c(fit$group,dim.date),.SDcols=c("Actual","Predicted","Residual",var.name)]
  temp.con=rbind(temp.con,temp.con.all)
  f_con=function(temp){
    actual=temp$decomp[temp$variable=="Actual"]
    temp=temp[,contribution:=decomp/actual]
    temp
  }
  
  temp.con$Actual=as.numeric(temp.con$Actual)
  temp.con=melt.data.table(temp.con,id.vars=c(fit$group,dim.date),measure.vars=names(temp.con)[!names(temp.con) %in% c(fit$group,dim.date)],value.name="decomp")
  temp.i=unique(temp.con[,c(fit$group,dim.date),with=F],by=NULL)
  
  f_con_loop=function(x) {
    temp=merge(temp.i[x],temp.con,by=c(fit$group,dim.date),all.x=T)
    f_con(temp)
  }
  temp.con=Reduce(rbind,lapply(1:nrow(temp.i),f_con_loop))
  setnames(temp.con,c("variable","decomp","contribution"),c("Variable","Decomp","Contribution"))
  
  # output
  if(is.output){
    write.csv(forplot,"output_forplot_all.csv",row.names=F)
    write.csv(temp.con,"output_contribution.csv",row.names=F)
  }
  
  # plots
  if (is.graph) return(list(con=temp.con,decomp=forplot,app=rshiny(temp.con,forplot,fit))) else return(list(con=temp.con,decomp=forplot))
}