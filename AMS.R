library(readxl)
library(shiny)
library(ggplot2)

# 设置Shiny应用的UI
ui <- fluidPage(
  titlePanel("AMS Methods"),  # 修改表头
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Excel File",
                accept = c(".xlsx")),
      numericInput("ey", "Enter EY value:", value = 0, min = 0, step = 0.01),
      textOutput("betaValue"),
      textOutput("alphaValue"),
      textOutput("aepValue"),
      textOutput("fValue"),
      textOutput("rainfallDepthCalcValue"),
      textOutput("warningMessage")  # 添加提示信息
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
    if (input$ey >= 0.5) {
      return("Due to EY being greater than or equal to 0.5, it is recommended to use the PDS method.")
    }
  })
  
  # 计算beta值
  betaValue <- reactive({
    req(data(), input$ey < 0.5)
    df <- data()
    if (ncol(df) >= 2 && nrow(df) >= 10) {
      std_dev <- sd(df[[2]][1:10], na.rm = TRUE)
      beta <- (sqrt(6) * std_dev) / pi
      return(beta)
    } else {
      return(NA)
    }
  })
  
  output$betaValue <- renderText({
    if (input$ey < 0.5) {
      beta <- betaValue()
      if (!is.na(beta)) {
        paste("Beta Value:", round(beta, 4))
      } else {
        "The selected sheet does not contain enough columns or rows."
      }
    }
  })
  
  # 计算alpha值
  output$alphaValue <- renderText({
    if (input$ey < 0.5) {
      req(data())
      df <- data()
      beta <- betaValue()
      if (!is.na(beta)) {
        mean_value <- mean(df[[2]][1:10], na.rm = TRUE)
        alpha <- mean_value - 0.5772 * beta
        paste("Alpha Value:", round(alpha, 4))
      } else {
        "The selected sheet does not contain enough columns or rows."
      }
    }
  })
  
  # 计算AEP值
  output$aepValue <- renderText({
    if (input$ey < 0.5) {
      req(input$ey)
      ey <- input$ey
      if (ey < 0.1) {
        aep <- ey
      } else {
        aep <- 1 - exp(-ey)
      }
      paste("AEP Value:", round(aep, 4))
    }
  })
  
  # 计算并显示F值
  output$fValue <- renderText({
    if (input$ey < 0.5) {
      req(input$ey)
      ey <- input$ey
      if (ey < 0.1) {
        aep <- ey
      } else {
        aep <- 1 - exp(-ey)
      }
      F <- 1 - aep
      paste("F Value:", round(F, 4))
    }
  })
  
  # 计算并显示降雨深度值
  output$rainfallDepthCalcValue <- renderText({
    if (input$ey < 0.5) {
      req(data(), input$ey)
      df <- data()
      beta <- betaValue()
      if (!is.na(beta) && ncol(df) >= 2 && nrow(df) >= 10) {
        mean_value <- mean(df[[2]][1:10], na.rm = TRUE)
        alpha <- mean_value - 0.5772 * beta
        
        ey <- input$ey
        if (ey < 0.1) {
          aep <- ey
        } else {
          aep <- 1 - exp(-ey)
        }
        F <- 1 - aep
        rainfall_depth <- alpha - beta * log(-log(F))
        paste("Rainfall Depth (mm):", round(rainfall_depth, 4))
      } else {
        "The selected sheet does not contain enough columns or rows."
      }
    }
  })
  
  # 绘制CDF散点图
  output$cdfPlot <- renderPlot({
    if (input$ey < 0.5) {
      req(data(), input$ey)
      df <- data()
      beta <- betaValue()
      if (!is.na(beta) && ncol(df) >= 2 && nrow(df) >= 10) {
        mean_value <- mean(df[[2]][1:10], na.rm = TRUE)
        alpha <- mean_value - 0.5772 * beta
        
        rainfall_depth <- 10:219  # R1=10, R2=11, ..., R210=219
        F_values <- sapply(rainfall_depth, function(R) {
          exp(-exp(-(R - alpha) / beta))
        })
        
        cdf_data <- data.frame(Rainfall_Depth = rainfall_depth, F = F_values)
        
        ggplot(cdf_data, aes(x = Rainfall_Depth, y = F)) +
          geom_point(color = "blue") +
          labs(title = "Cumulative Distribution Function (CDF)",
               x = "Rainfall Depth",
               y = "F") +
          theme_minimal()
      }
    }
  })
}

# 启动Shiny应用
shinyApp(ui = ui, server = server)
