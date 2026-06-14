install.packages("shiny")
install.packages("readxl")
install.packages("lmomco")
library(shiny)
library(readxl)
library(lmomco)

# UI部分
ui <- fluidPage(
  titlePanel("L-Moment Design Rainfall Depth Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Excel File", accept = c(".xlsx")),
      numericInput("ey", "Enter EY value (1/return period):", value = 0.01, min = 0.0001, max = 1, step = 0.0001),
      actionButton("calculate", "Calculate Design Rainfall Depth"),
      textOutput("rainfallDepthOutput")
    ),
    
    mainPanel(
      tableOutput("contents")
    )
  )
)

# Server部分
server <- function(input, output) {
  
  # 读取并显示上传的Excel数据
  data <- reactive({
    req(input$file1)
    df <- read_excel(input$file1$datapath, range = "A2:A3744")
    df <- na.omit(df)
    as.numeric(df[[1]])
  })
  
  output$contents <- renderTable({
    req(data())
    head(data(), n = 3744)  # 显示前10行数据
  })
  
  # 计算并显示设计降雨深度
  observeEvent(input$calculate, {
    req(data(), input$ey)
    
    # 计算L-矩
    lmom <- lmom.ub(data())
    
    # 选择GEV分布
    gev_params <- pargev(lmom)
    
    # 计算设计降雨深度
    design_rainfall <- quagev(1 - input$ey, gev_params)
    
    output$rainfallDepthOutput <- renderText({
      paste("设计降雨深度为:", round(design_rainfall, 2), "mm")
    })
  })
}

# 启动Shiny应用
shinyApp(ui = ui, server = server)
