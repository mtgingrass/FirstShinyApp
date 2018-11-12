# Pre-process Data --------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(DT)

# Clean Data --------------------------------------------------------------
source("clean.R")

# Dashboard Header --------------------------------------------------------
header <- dashboardHeader(title = "SE Home")

# Sidebar -----------------------------------------------------------------
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dashboard",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  
  ## Links
  menuItem(
    "DevGin",
    icon = icon("send", lib = 'glyphicon'),
    badgeLabel = "new",
    
    href = "http://devgin.com/"
  ),

  menuItem(
    "Mark Gingrass",
    icon = icon("send", lib = 'glyphicon'),
    badgeLabel = "new",
    
    href = "http://markgingrass.com"
  ),
  
  ## Slider Input Years
  
  sliderInput(
    "obs",
    "Choose Years:",
    min = 2014,
    max = 2019,
    value = c(2014, 2019),
    sep = ""
  ),
  
  ## Dropdown PM Listing
  selectInput("PM_ID", "PM:",
              c("", PM_list))
))

# Tab1 --------------------------------------------------------------------
overview_row0 <- fluidRow(titlePanel("SE Performance Review"), textOutput("PM_ID"))

# Row 1 -------------------------------------------------------------------
overview_row1 <- fluidRow(
  valueBoxOutput("LeftValueBox"),
  valueBoxOutput("MiddleValueBox"),
  valueBoxOutput("RightValueBox")
)

# Row 2 -------------------------------------------------------------------
overview_row2 <- fluidRow(
  box(
    title = "Met/Miss Counts by Year & Cycle Type",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("LeftBox", height = "250px")
  ),
  box(
    title = "Met/Miss Counts by Year & Ctgry",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("RightBox", height = "250px")
  )
)

# Row 3 -------------------------------------------------------------------
overview_row3 <- fluidRow(
  box(
    title = "Met/Miss Counts by Year & Letter Type",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("BottomBox", height = "300px"),
    width = 12
  )
)


# TAB 2 -------------------------------------------------------------------
# TAB 2 ROW 1 -------------------------------------------------------------
data_table_row1 <- fluidRow(DT::dataTableOutput("mytable"))


# TAB 3 -------------------------------------------------------------------
# TAB 3 ROW 1 -------------------------------------------------------------

stats_row1 <- fluidRow(
  box(
    title = "Box Plot of Days Past Due per Cycle Type",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("tab3_top", height = "300px"),
    width = 12
  )
)


stats_row2 <- fluidRow(
  box(
    title = ">= 2nd Outliers Removed",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("tab3_left", height = "300px"),
    width = 4
  ),

  box(
    title = "1st Outliers Removed",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("tab3_middle", height = "300px"),
    width = 4
  ),
  
  box(
    title = "ACK or RTA Outliers Removed",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("tab3_right", height = "300px"),
    width = 4
  )
)





# MAIN PANEL --------------------------------------------------------------
mp <- mainPanel(tabsetPanel(
  tabPanel("Overview", overview_row1, overview_row2, overview_row3),
  tabPanel("Data Table", data_table_row1),
  tabPanel("Stats", stats_row1, stats_row2)
))

# Body --------------------------------------------------------------------
body <- dashboardBody(mp)
#completing the ui part with dashboardPage

ui <-
  dashboardPage(title = 'SE Performance Review', header, sidebar, body, skin =
                  'red')

# SERVER ------------------------------------------------------------------
# Server Interface --------------------------------------------------------
filter_pm <- ""
# create the server functions for the dashboard

server <- function(input, output) {
  # Updated data based on selected filters
  
  filtered_data <-
    reactive(
      se_report_filtered(
        my_filter = input$PM_ID,
        start_year = input$obs[1],
        end_year = input$obs[2]
      )
    )
  
  #creating the valueBoxOutput content
  
  output$LeftValueBox <- renderValueBox({
    LeftValueBox <- filtered_data()
    LeftValueBox <- LeftValueBox[[1]]
    total_missed <- sum(LeftValueBox$Status, na.rm = TRUE)
    valueBox(
      formatC(total_missed, format = "d", big.mark = ','),
      "Total Missed:",
      icon = icon("stats", lib = 'glyphicon'),
      color = "red"
    )
  })
  
  
  output$MiddleValueBox <- renderValueBox({
    MiddleValueBoxData <- filtered_data()
    MiddleValueBoxData <- MiddleValueBoxData[[1]]
  
    total_met <-
      dim(MiddleValueBoxData)[1] - sum(MiddleValueBoxData$Status, na.rm = TRUE)
    
    valueBox(
      formatC(total_met, format = "d", big.mark = ','),
      
      'Total Met',
      icon = icon("stats", lib = 'glyphicon'),
      color = "green"
    )
  })
  
  # RIGHT VALUE BOX ---------------------------------------------------------
  output$RightValueBox <- renderValueBox({
    ValueBoxData <- filtered_data()
    ValueBoxData <- ValueBoxData[[1]]
    ValueBoxData <-
      100 * (1 - sum(ValueBoxData$Status, na.rm = TRUE) / dim(ValueBoxData)[1])
    valueBox(
      formatC(
        paste0(sprintf("%.2f", ValueBoxData), "%"),
        format = "f",
        big.mark = ',',
        digits = 4
      )
      
      ,
      "Achievement",
      icon = icon("thumbs-up", lib = 'font-awesome'),
      color = "green"
    )
  })
  
  #creating the plotOutput content
  output$LeftBox <- renderPlot({
    BoxData <- filtered_data()
    BoxData <- BoxData[[1]]
    BoxPlot <- ggplot(BoxData, aes(x = Status, fill = Cycle)) +
      geom_bar() +
      facet_grid(. ~ Fiscal.Year) +
      labs(x = 'MISS / MET', y = 'Count') +
      ggtitle('Miss and Met Counts by Year') +
      theme_bw() +
      theme(
        legend.position = 'bottom',
        panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank()
        
      ) + scale_x_continuous(breaks = seq(0, 1), labels = c("MET", "MISS"))
    
    BoxPlot
  })
  
  output$BottomBox <- renderPlot({
    BoxData <- filtered_data()
    BoxData <- BoxData[[1]]
    BoxPlot <-
      ggplot(BoxData, aes(x = Status, fill = Letter.Type)) +
      geom_bar() +
      facet_grid(. ~ Fiscal.Year) +
      labs(x = 'MISS / MET', y = 'Count') +
      ggtitle('Miss and Met Counts by Year') +
      theme_bw() +
      theme(
        legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) + scale_x_continuous(breaks = seq(0, 1), labels = c("MET", "MISS"))
    
    BoxPlot
  })
  
  output$mytable <- DT::renderDataTable({
    dt_data <- filtered_data()
    dt_data <- dt_data[[1]]
    dt_data %>%
      DT::datatable(
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
  })
  
  output$RightBox <- renderPlot({
    BoxData <- filtered_data()
    BoxData <- BoxData[[1]]
    
    BoxPlot <- ggplot(BoxData, aes(x = Status, fill = Ctgry.Name)) +
      geom_bar() +
      facet_grid(. ~ Fiscal.Year) +
      labs(x = 'MISS / MET', y = 'Count') +
      ggtitle('Miss and Met Counts by Year and Letter Type') +
      theme_bw() +
      theme(
        legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) + scale_x_continuous(breaks = seq(0, 1), labels = c("MET", "MISS"))
    
    BoxPlot
  })
  
  output$tab3_top <- renderPlot({
    BoxData <- filtered_data()
    BoxData <- BoxData[[1]]
    
    BoxPlot <- BoxData %>%
      filter(Status == 1) %>%
      ggplot(aes(y = diff_days)) +
      geom_boxplot() +
      facet_wrap( ~ as.factor(Cycle)) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    BoxPlot
  })
  
  output$tab3_left <- renderPlot({
    BoxData <- filtered_data()
    BoxData <- BoxData[[1]]

    df <- BoxData %>%
      filter(Status == 1) %>%
      filter(Cycle == ">= 2nd Cycle Action")
    
    df <- df %>%
      ggplot(aes(y = diff_days)) + geom_boxplot(outlier.shape = NA) +
      scale_y_continuous(limits = quantile(df$diff_days, c(0.1, 0.9))) +
      facet_wrap( ~ as.factor(Cycle)) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    df
  })
  

  output$tab3_middle <- renderPlot({
    BoxData <- filtered_data()
    BoxData <- BoxData[[1]]
    
    df <- BoxData %>%
      filter(Status == 1) %>%
      filter(Cycle == "1st Cycle Action")
    
    df <- df %>%
      ggplot(aes(y = diff_days)) + geom_boxplot(outlier.shape = NA) +
      scale_y_continuous(limits = quantile(df$diff_days, c(0.1, 0.9))) +
      facet_wrap( ~ as.factor(Cycle)) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    df
  })
  
  
  
  output$tab3_right <- renderPlot({
    BoxData <- filtered_data()
    BoxData <- BoxData[[1]]
    
    df <- BoxData %>%
      filter(Status == 1) %>%
      filter(Cycle == "ACK or RTA")
    
    df <- df %>%
      ggplot(aes(y = diff_days)) + geom_boxplot(outlier.shape = NA) +
      scale_y_continuous(limits = quantile(df$diff_days, c(0.1, 0.9))) + 
      facet_wrap( ~ as.factor(Cycle))
    
    df
  })
}

#run/call the shiny app
shinyApp(ui, server)