library(shiny)
library(shinydashboard)
library(dplyr)
library(magrittr)
library(plotly)
library(dygraphs)
library(tidyquant)
library(lubridate)
library(forecast)
library(Hmisc)


options(warn=-1)

sample_data = read.csv('sample data.csv', stringsAsFactors = FALSE)
sample_data$month = factor(sample_data$month, levels = month.name)
sample_data$day = factor(sample_data$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))



ui = navbarPage("BI ASSISTANT",id="nav",
  tabPanel("Dashboard",
  dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      
      singleton(tags$head(
        tags$script(src="//cdnjs.cloudflare.com/ajax/libs/annyang/1.4.0/annyang.min.js"),
        tags$script(src="//code.responsivevoice.org/responsivevoice.js"),
        includeScript('init.js'),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }",
                   ".dygraph-label.dygraph-title { font-weight: normal; }"
        )
      )
      )
      
      
    ),
    
    fluidRow(
      
      valueBoxOutput("quantity", width = 4),
      valueBoxOutput("sales", width = 4),
      valueBoxOutput("profit", width = 4)
    ),
    
    fluidRow(
      column(width = 6,
             box(solidHeader = TRUE, width = 12,
                 collapsible = TRUE,color="purple", dygraphOutput("trend", height = 200)),
             box(solidHeader = TRUE, width = 6,
                 collapsible = TRUE,color="purple", plotlyOutput("salesbar1", height = 200), height = 250),
             box(solidHeader = TRUE, width = 6,
                 collapsible = TRUE,color="purple", plotlyOutput("sankey", height = 200, width = "100%"), height = 250)
             
             
             
             
             
      ),
      column(width = 6,
             box(solidHeader = TRUE, width = 12,
                 collapsible = TRUE,color="purple", plotlyOutput("hmap", height = 200)),
             box(solidHeader = TRUE, width = 12,
                 collapsible = TRUE,color="purple", plotlyOutput("regression", height = 200), height = 250)
      ))
    
    
    
    
    
    
  ) 
  
  
  
  
  
)  ),

tabPanel("How to Use",
         
       
         
         htmlOutput("pdf")
         

))







server = function (input, output, session)
{
  
  
  output$quantity = renderValueBox({
    
    
    
    
    valueBox(sum(sample_data$quantity)
             , "Quantity", icon = icon("signal"),
             color = "purple"
    )
    
    
  })  
  
  
  
  output$sales = renderValueBox({
    
    
    
    valueBox(round(sum(sample_data$sales),0)
             , "Sales", icon = icon("signal"),
             color = "purple"
    )
    
    
  }) 
  
  
  output$profit = renderValueBox({
    
    
    
    valueBox(round(sum(sample_data$profit),0)
             , "Profit", icon = icon("signal"),
             color = "purple"
    )
    
    
  })
  
  output$trend = renderDygraph({
    sample_data$date = as.Date(sample_data$date, format = "%m/%d/%y")
    met = as.character(input$metric3)
    orderdate = "date"
    quant_ts = sample_data[,c(orderdate, met)]
    colnames(quant_ts)[2] = met
    quant_ts_grouped = quant_ts %>% group_by(date) %>% summarise_all(sum)
    date = seq.Date(as.Date(min(quant_ts_grouped$date)), as.Date(max(quant_ts_grouped$date)), by = "day")
    join_dates = as.data.frame(date)
    new_ts = merge(join_dates, quant_ts_grouped, by = 'date', all.x = TRUE)
    new_ts[is.na(new_ts)] = 0
    min_year = year(min(quant_ts_grouped$date))
    min_month = month(min(quant_ts_grouped$date))
    min_day = day(min(quant_ts_grouped$date))
    
    new_ts_for = ts(new_ts[2], start = c(min_year,min_month,min_day), frequency = 365.25)
    h = input$h
    h = as.numeric(h)
    
    fit_aic = Inf
    fit_order = c(0,0,0)
    for (p in 1:4) for (d in 0:1) for (q in 1:4) {
      fitcurrent_aic = AIC(arima(new_ts_for, order=c(p, d, q),method="ML"))
      if (fitcurrent_aic < fit_aic) {
        fit_aic = fitcurrent_aic
        fit_order <- c(p, d, q)
      }
    }
    
    
    
    fit = Arima(new_ts_for, order = fit_order,method="ML")
    pred = forecast(fit, h = h)
    actuals = pred$x
    lower = pred$lower[,2]
    upper = pred$upper[,2]
    point_forecast = pred$mean
    max_new_ts = max(new_ts$date)
    max_new_ts_num = as.numeric(max_new_ts)
    max_plus = as.numeric(max_new_ts_num+h)
    min_new_ts = min(new_ts$date)
    se = seq(as.numeric(min_new_ts), as.numeric(max_plus))
    se = as.Date(se)
    all = cbind(actuals, lower, upper, point_forecast, se)
    
    all = xts(all, order.by = as.Date(se))
    all = xts(all, order.by = index(all))
    
    all$se = NULL
    
    chart_title = paste0(capitalize(met), " Trend Over Time")
    dygraph(all, main = chart_title) %>% dyAxis('y', met) %>% dyRangeSelector(dateWindow = c(max_new_ts-h, max_new_ts+h))  %>% dySeries("actuals", label = "Actual") %>% dySeries(c("lower", "point_forecast", "upper"), label = "Predicted")
    
  })  
  
  
  
  
  output$salesbar1 =  
    renderPlotly({
      
      
      col = input$dimension
      col = tolower(col)
      col = as.character(col)
      met = input$metric
      met = tolower(met)
      met = as.character(met)
      nd = aggregate(sample_data[,met]~sample_data[,col], data = sample_data, FUN = sum)
      colnames(nd) = c("dimension", "metric")
      nd = nd %>% arrange(desc(metric))
      nd$dimension = reorder(x=nd$dimension, X = nd$metric, FUN = sum)
      
      sum_of = "Sum Of "
      by = " by "
      
      plot_ly(nd, x = ~metric, y = ~dimension, type = "bar", orientation = "h", text = ~dimension, textposition = 'auto', textfont = list(color = 'rgb(255,255,255)'),marker = list(color = 'rgb(95,92,168)', line = list(color = 'rgb(255,255,255)'))) %>% layout(title = paste0(sum_of,capitalize(met),by,capitalize(col)),xaxis = list(title = ""), yaxis = list(title = "", showticklabels = FALSE)) 
      
    }) 
  
  
  output$regression =  
    renderPlotly({
      
      
      met1 = input$metric1
      met1 = as.character(met1)
      met2 = input$metric2
      met2 = as.character(met2)
      met5 = input$metric5
      met5 = as.character(met5)
      dim5 = input$dimension5
      dim5 = as.character(dim5)
      
      sample_data_filtered = sample_data[,c(met1,met2,met5,dim5)]
      sample_data_filtered[,4] = as.factor(sample_data_filtered[,4])
      
      sample_data_final = sample_data_filtered %>% group_by(sample_data_filtered[,4]) %>% summarise_if(is.numeric,sum)
      
      colnames(sample_data_final) = c("dimension", "metric1", "metric2", "metric5")
      
   
      vs = " Vs. "
      by = " By "
      
      plot_ly(sample_data_final, x = ~sample_data_final$metric1, y = ~sample_data_final$metric2, type = 'scatter', size = ~sample_data_final$metric5, color = ~sample_data_final$dimension, mode = 'markers', text = ~paste0(capitalize(met5), ": ",sample_data_final$metric5), marker = list(opacity = 0.5, symbol = 'circle')) %>% layout(title = paste0(capitalize(met1),vs,capitalize(met2),by, capitalize(dim5), " [Sizing by ", capitalize(met5),"]"),xaxis = list(title = met1), yaxis = list(title = met2), showlegend = FALSE) 
      
    }) 
  
  output$hmap =  
    renderPlotly({
      
      met4 = as.character(input$metric4)
      dim1 = as.character(input$dimension1)
      dim2 = as.character(input$dimension2)
      
      sample_data_filtered = sample_data[,c(dim1,dim2,met4)]
      
      sample_data_filtered[,1] = as.factor(sample_data_filtered[,1])
      sample_data_filtered[,2] = as.factor(sample_data_filtered[,2])
      
      new_df = sample_data_filtered %>% group_by(sample_data_filtered[,1], sample_data_filtered[,2]) %>% summarise_if(is.numeric, sum)
      
      colnames(new_df) = c("Dimension 1", "Dimension 2", "Metric")
      
      sum_of = "Sum Of "
      by = " by "
      and = " and "
      plot_ly(x = new_df$`Dimension 1`, y = new_df$`Dimension 2`, z = new_df$Metric, type = "heatmap") %>% layout(title = paste0(sum_of,capitalize(met4),by,capitalize(dim1),and,capitalize(dim2)))
      
    })
  

  
  output$sankey = renderPlotly({
    
    dim3 = as.character(input$dimension3)
    dim4 = as.character(input$dimension4)
    
    sample_data_filter = sample_data[,c(dim3,dim4)]
    sample_data_sankey = sample_data_filter %>% group_by(sample_data_filter[,1],sample_data_filter[,2] ) %>% summarise(count=n())
    
    colnames(sample_data_sankey) = c("label1", "label2", "count")
    
    node_list1 = data.frame(label1 = c(sample_data_sankey$label1) %>% unique())
    node_list1$index1 = index(node_list1)-1
    node_list2 = data.frame(label2 = c(sample_data_sankey$label2) %>% unique())
    node_list2$index2 = seq(max(node_list1$index)+1,max(node_list1$index)+length(node_list2$label))
    
    new_df1 = merge(sample_data_sankey, node_list1, by = 'label1')
    
    new_df2 = merge(new_df1, node_list2, by = 'label2')
    
    sank = "Sankey: "
    an = " And "
    
    
    plot_ly(type = "sankey", orientation = "h",
            node = list(label = c(sample_data_sankey$label1, sample_data_sankey$label2) %>% unique()),
            link = list(
              source =  new_df2$index1,
              target = new_df2$index2,
              value =  new_df2$count)) %>% layout(title=paste0(sank,capitalize(dim3),an,capitalize(dim4)))
    
    }) 
  
  output$pdf <- renderUI({
    tags$iframe(style="height:700px; width:100%; scrolling=yes",src="Instructions.pdf")
  })
  
  
} 


shinyApp(ui = ui, server = server)