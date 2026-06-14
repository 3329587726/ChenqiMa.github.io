library(readxl)
library(shiny)
library(ggplot2)

# 设置Shiny应用的UI
ui <- fluidPage(
  titlePanel("PDS Methods"),  # 修改表头
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Excel File",
                accept = c(".xlsx")),
      numericInput("ey", "Enter EY value:", value = 0, min = 0, step = 0.01),
      textOutput("betaValue"),
      textOutput("alphaValue"),
      textOutput("epsValue"),
      textOutput("zValue"),
      textOutput("fValue"),
      textOutput("rainfallDepthCalcValue"),
      textOutput("warningMessage") 
    ),
    mainPanel(
      tableOutput("contents"),
      plotOutput("cdfPlot")
    )
  )
)

# 设置Shiny应用的服务器逻辑
server <- function(input, output) {
  # 读取上传的Excel文件
  data <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath, sheet = "Rd")
  })
  
  # 显示Excel文件中的内容
  output$contents <- renderTable({
    data()
  })
  
  # 当EY大于等于0.5时显示提示信息
  output$warningMessage <- renderText({
    req(input$ey)
    if (input$ey <= 0.5) {
      return("Due to EY being smaller than 0.5, it is recommended to use the AMS method.")
    }
  })
  
  # 计算alpha值
  alpha <- reactive({
    req(data())   
    data()[90, 1] 
  })
  Alpha <- rainfall_values[90]
  output$alphaValue <- renderText({
    paste("Alpha Value:", alpha())
  })
  
  
  #计算eps值
 
  Eps <- 0.5 * (1 - (mean(rainfall_values) - Alpha)^2 / var(rainfall_values))
  Beta <- (mean(rainfall_values) - Alpha) / (1 - Eps)
  output$epsValue <- renderText({
    paste("Eps Value:", round(eps(), 4))
  })
  
  # 计算beta值
  beta <- reactive({
    req(eps(), data(), alpha())
    values <- data()[, 1]
    avg <- mean(values)
    (avg - alpha()) / (1 - eps())
  })
  
  output$betaValue <- renderText({
    paste("Beta Value:", round(beta(), 4))
  })
  
  
  # Calculate F Value
  output$fValue <- renderText({
    req(input$ey, eps())  # Ensure EY and Eps are available
    F <- 1 - input$ey * 30 / 90
    paste("F Value:", round(F, 4))
  })
  
  # Calculate z Value
  output$zValue <- renderText({
    req(F, eps())  
    F <- 1 - input$ey * 30 / 90  
    z <- ifelse(F == 0, NA, ((1 - F)^(-eps()) - 1) / eps()) 
    paste("z Value:", round(z, 4))
  })
  
  # Calculate Design Rainfall Depth
  output$rainfallDepthCalcValue <- renderText({
    req(data(), betaValue(), alpha(), zValue()) 
    values <- data()[, 1]
    stdev <- sd(values, na.rm = TRUE)  
    z <- ((1 - F)^(-eps()) - 1) / eps()  
    designRainfallDepth <- z * stdev + alpha()  
    paste("Design Rainfall Depth (mm):", round(designRainfallDepth, 4))
  })
  
  
  # Render CDF plot and calculate F and design rainfall depth
  output$cdfPlot <- renderPlot({
    req(data(), input$ey > 0.5, betaValue())  # Ensure all prerequisites are met
    df <- data()
    beta <- betaValue()
    
    if (!is.na(beta) && ncol(df) >= 1 && nrow(df) >= 90) {
      mean_value <- mean(df[[1]][1:90], na.rm = TRUE)
      alpha <- data()[90, 1]  # Directly access the value
      eps <- epsValue()  # Assuming epsValue is another reactive expression defined elsewhere
      
      # Define the range for R values
      rainfall_depth <- 10:219  # R1=10, R2=11, ..., R210=219
      
      # Calculate z values
      z_values <- sapply(rainfall_depth, function(R) {
        (R - alpha) / beta
      })
      
      # Calculate F values based on the given conditions
      F_values <- sapply(z_values, function(z) {
        if (z > 0.1) {
          max(0, 1 - (1 + eps * z) ** (-1 / eps))
        } else {
          0
        }
      })
      
      # Combine data for plotting
      cdf_data <- data.frame(Rainfall_Depth = rainfall_depth, F = F_values)
      
      # Plot using ggplot2
      ggplot(cdf_data, aes(x = Rainfall_Depth, y = F)) +
        geom_point(color = "blue") +
        labs(title = "Cumulative Distribution Function (CDF)",
             x = "Rainfall Depth", y = "F") +
        theme_minimal()
    } else {
      print("The selected sheet does not contain enough columns or rows.")
    }
  })
}
  
  # 启动Shiny应用
  shinyApp(ui = ui, server = server)
 