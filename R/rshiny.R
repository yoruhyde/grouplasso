#' @export
line.hchart=function(x,data,fit){
  # x="All"
  # data=forplot
  temp=data[data[[fit$group]]==x,c(fit$date.var,"Actual","Predicted","Residual"),with=F]
  temp[[fit$date.var]] <- paste("#!", as.numeric(temp[[fit$date.var]])*86400000, "!#")
  a <- Highcharts$new()
  a$chart(zoomType='x')
  a$xAxis(type='datetime',labels=list(format= '{value:%m/%d/%Y}',rotation=45,align='left'))
  a$yAxis(title=list(text="AvP"),min=min(temp$Residual)*1.05)
  a$series(data=toJSONArray2(temp[,c(fit$date.var,"Actual"),with=F], json = F, names = F),name="Actual",type="line",color='#058DC7')
  a$series(data=toJSONArray2(temp[,c(fit$date.var,"Predicted"),with=F], json = F, names = F),name="Predicted",type="line",color='#FF9655')
  a$series(data=toJSONArray2(temp[,c(fit$date.var,"Residual"),with=F], json = F, names = F),name="Residual",type="column",color="black")
  a$tooltip(pointFormat= '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:,.0f}</b> <br/>',
            shared= T,xDateFormat='%m/%d/%Y'
  )
  a$exporting(enabled=T)
  a$legend(align="center",verticalAlign= "top")
  a
}


#' @export
rshiny=function(temp.con,forplot,fit){
  server <- function(input, output) {
    output$avp=renderChart2(line.hchart(input$group,forplot,fit))
    options(DT.options=list(pageLength=10,autoWidth=T,searching = F))
    output$con=renderDataTable(datatable(temp.con[temp.con[[fit$group]]==input$group & (!Variable %in% c("Actual","Predicted"))][order(-Decomp)],
                                         rownames=F)%>%
                                 formatCurrency("Decomp",currency="")%>%formatPercentage(c("Contribution","Decomp"),2))
  }
  
  ui <- fluidPage(
    selectizeInput("group", multiple = F,label=NULL,choices=as.list(unique(forplot[[fit$group]])),selected = "All"),
    fluidRow(
      # column(8,showOutput("avp","highcharts")),
      # column(4,offset = 0,dataTableOutput("con",width="100%"))
      showOutput("avp","highcharts"),
      br(),
      dataTableOutput("con",width="50%")
    )
  )
  
  shinyApp(ui = ui, server = server)
}