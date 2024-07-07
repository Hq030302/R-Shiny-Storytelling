library(shiny)
library(shinydashboard)
library(DT)

# UI
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Dashboard Kelompok H",
                                    titleWidth = 300),
                    ## Sidebar content
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Data", tabName = "data", icon = icon("table")),
                        menuItem("Analisis", tabName = "analisis", icon = icon("chart-bar")),
                        menuItem("Interpretasi", tabName = "interpretasi", icon = icon("comment"))
                      )
                    ),
                    ## Body content
                    dashboardBody(
                      tabItems(
                        # First tab content (Data)
                        tabItem(tabName = "data", 
                                h1("Input Dataset"),
                                fileInput('file1', h3("Silakan upload file Immunotherapy dalam csv"))
                        ),
                        # Second tab content (Visualisasi)
                        tabItem(tabName = "analisis",
                                h1("Dataset"),
                                h4("Dataset yang kami gunakan terdiri dari 8 variabel dengan jumlah data sebanyak 90 data."),
                                # dataset table
                                DTOutput("view_data"), 
                                tags$hr(),
                                
                                # stat. deksriptif
                                h1("Statistika Deskriptif"),
                                verbatimTextOutput("summary"),
                                tags$hr(),
                                
                                # plot
                                h1("Visualisasi"),
                                ## plot 1 
                                h3("Histogram"),
                                selectInput("var1", # select x variable
                                            label = "Pilih variabel untuk visualisasi histogram",
                                            choices = c("age",                 
                                                        "Time",               
                                                        "Number_of_Warts",
                                                        "Area",
                                                        "induration_diameter"),
                                            selected = "age"),
                                sliderInput("bins1", # select bins
                                            label = "Number of bins:",
                                            min = 1,
                                            max = 30,
                                            value = 15),
                                plotOutput("plot1"),
                                tags$hr(),
                                ## plot 2
                                h3("Scatterplot"),
                                selectInput("var2", # select x variable
                                            label = "Pilih variabel independen X untuk visualisasi scatterplot",
                                            choices = c("Time",               
                                                        "Number_of_Warts",
                                                        "Area",
                                                        "induration_diameter"),
                                            selected = "Time"),
                                selectInput("var3", # select y variable
                                            label = "Variabel dependen Y untuk visualisasi scatterplot",
                                            choices = c("age"),
                                            selected = "age"),
                                plotOutput("plot2"),
                        ),
                        # Third tab content (Interpretasi)
                        tabItem(tabName = "interpretasi",
                                h1("Interpretasi"),
                                h2("1. Histogram"),
                                h3("Berdasarkan histogram yang telah dibuat, dapat diketahui bahwa:"),
                                h4("a. Variabel age tidak berdistribusi normal karena tidak berbentuk bell-shaped, dan persebaran datanya cenderung miring ke kanan."),
                                h4("b. Variabel Time bisa dibilang berdistribusi normal karena bentuknya hampir menyerupai bell-shaped, dan persebaran datanya bisa dibilang cukup merata."),
                                h4("c. Variabel Number_of_Warts tidak berdistribusi normal karena tidak berbentuk bell-shaped, dan persebaran datanya cenderung miring ke kanan."),
                                h4("d. Variabel Area tidak berdistribusi normal karena tidak berbentuk bell-shaped, dan persebaran datanya cenderung miring ke kanan."),
                                h4("d. Variabel induration_diameter tidak berdistribusi normal karena tidak berbentuk bell-shaped, dan persebaran datanya cenderung acak dan saling berjauhan."),
                                h2("2. Scatter plot"),
                                h3("Berdasarkan scatter plot yang telah dibuat dengan menetapkan variabel age sebagai variabel dependen Y, dapat diketahui bahwa:"),
                                h4("a. Variabel age dengan variabel Time memiliki hubungan korelasi positif yang lemah karena datanya cenderung menyebar dan tidak mendekati garis. Kedua variabel saling mempengaruhi satu sama lain, namun tidak signifikan."),
                                h4("b. Variabel age dengan variabel Number_of_Warts tidak memiliki hubungan korelasi karena datanya ada yang berkumpul dan ada yang menyebar, dan garisnya tidak konsisten. Kedua variabel tidak saling mempengaruhi satu sama lain."),
                                h4("c. Variabel age dengan variabel Area tidak memiliki hubungan korelasi karena datanya cenderung menyebar dan garisnya tidak konsisten. Kedua variabel tidak saling mempengaruhi satu sama lain."),
                                h4("d. Variabel age dengan variabel induration_diameter tidak memiliki hubungan korelasi karena datanya cenderung menyebar dan garisnya tidak konsisten. Kedua variabel tidak saling mempengaruhi satu sama lain."),)
                      )
                    )
)


# SERVER
server <- function(input, output) {
  observe({
    # read input data file
    file1 = input$file1
    if (is.null(file1)) {
      return(NULL)
    }
    data1 = read.csv(file1$datapath, sep = ";")
    
    # output dataset table
    output$view_data <- renderDT(
      {data1}, options=list(scrollX = TRUE)
    )
    
    # output stat. deskriptif
    ## summary 1
    output$summary <- renderPrint({
      summary(data1)
    })
    ## summary 2

    
    # output plot
    ## plot 1
    output$plot1 <- renderPlot({
      #input variable
      if(input$var1=='age'){
        i<-2
      }
      if(input$var1=='Time'){
        i<-3
      }
      if(input$var1=='Number_of_Warts'){
        i<-4
      }
      if(input$var1=='Area'){
        i <-6
      }
      if(input$var1=='induration_diameter'){
        i <-7 
      }
      x_plot1 <- data1[, i]
      #referring input bins 
      bins1 <- seq(min(x_plot1), max(x_plot1), length.out = input$bins1 + 1)
      #producing histogram as output
      hist(x_plot1, breaks = bins1, col = 'blue', main=paste("Histogram", input$var1), border = 'white', xlab=input$var1)
      hist
    })
    
    ## plot 2
    output$plot2 <- renderPlot({
      #input variable x
      if(input$var2=='age'){
        j <-2
      }
      if(input$var2=='Time'){
        j <-3
      }
      if(input$var2=='Number_of_Warts'){
        j <-4
      }
      if(input$var2=='Area'){
        j <-6
      }
      if(input$var2=='induration_diameter'){
        j <-7 
      }
      x_plot2 <- data1[, j]
      #input variable y
      if(input$var3=='age'){
        k <-2
      }
      if(input$var3=='Time'){
        k <-3
      }
      if(input$var3=='Number_of_Warts'){
        k <-4
      }
      if(input$var3=='Area'){
        k <-6
      }
      if(input$var3=='induration_diameter'){
        k <-7 
      }
      y_plot2 <- data1[, k]
      # scatterplot as output
      scatter.smooth(x_plot2, y_plot2, main = paste("Scatterplot",input$var2,"and",input$var3),
           xlab = input$var2, ylab = input$var3,
           pch = 19, frame = FALSE)
    })
  })
}

shinyApp(ui=ui, server=server)