
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(shiny)
library(plotly)
vg <- read_csv("https://raw.githubusercontent.com/jochenzu/nintendo-sales/refs/heads/main/nintendosales.csv")
vg



vg$Year <- as.numeric(vg$Year)
vg<-na.omit(vg)
year_sale <- vg %>% 
  group_by(Year) %>%
  summarise(total = sum(Global_Sales, na.rm = TRUE))
yearsale_plot<- ggplot(year_sale,aes(total, as.factor(Year), fill = total)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") + 
  scale_alpha_continuous(range = c(0.1, 1)) +
  scale_x_continuous(expand = c(0, 3)) +
  theme(panel.grid.major.y = element_blank())
yearsale_plot


ggplot(vg) +
  geom_point(aes(EU_Sales, NA_Sales))+
  xlim(0,20)+
  ylim(0,20)



reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}

vg$Year <- as.numeric(vg$Year)
vg<-na.omit(vg)
vg$Year <- as.factor(vg$Year)

histogram <- function(data) {
  year_sale <- data %>% 
    group_by(Year) %>%
    summarise(total = sum(Global_Sales, na.rm = TRUE))
  ggplot(year_sale,aes(total,Year,fill = total))+
    geom_bar(stat = "identity", width = 1,aes(alpha = total)) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") + 
    scale_alpha_continuous(range = c(0.1, 1)) +
    scale_x_continuous(expand = c(0, 0, 0.1, 0.1)) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )+
    labs(title = 'Nintendo Games Total Sales Per Year', x = 'Tatal sales (in Millions)', y = 'Year') +
    theme(plot.title = element_text(hjust = 0.5))
}



scatterplot <- function(data, selected_) {
  ggplot(data, aes(color= 'tomato')) +
    geom_point(aes(log10(EU_Sales), log10(NA_Sales),alpha = as.numeric(selected_)))+
    scale_alpha(range = c(0.05, 0.6))+
    theme(legend.position = "none") +
    labs(title = 'Sales in Europe and North America') +
    theme(plot.title = element_text(hjust = 0.5))
}



library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("united"),
  plotOutput("histogram", brush = brushOpts("plot_brush", direction = "y")),
  plotOutput("scatterplot"),
  dataTableOutput("table"))

server <- function(input, output) {
  selected <- reactiveVal(rep(TRUE, nrow(vg)))
  
  observeEvent(
    input$plot_brush,
    selected(reset_selection(vg, input$plot_brush))
  )
  
  output$histogram <- renderPlot(histogram(vg))
  output$scatterplot <- renderPlot(scatterplot(vg, selected()))
  output$table <- renderDataTable(filter(vg, selected()))
  #outpu$ridgeplot<-renderPlot(ridgeplot(vg), selected())
  #output$table <- renderDataTable(data_table(vg, selected()))
}


shinyApp(ui, server)

