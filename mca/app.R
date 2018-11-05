library(shiny)
library(FactoMineR)
data(tea)
tea<- tea[,1:15]

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny App for MCA"),
  
  # Sidebar with different widgets depending on the selected tab
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1",
                       checkboxInput("deffile","Use default dataset (tea)?",TRUE),
                       conditionalPanel(condition="!input.deffile",
                                        fileInput('file', 'Choose file to upload',
                                                  accept = c('text/csv',
                                                             'text/comma-separated-values',
                                                             '.csv'))),
                       hr("Remember that MCA can only be used with categorical data, indicate the variables used for MCA below:"),
                       h6("(All variables will be included by default if leave a blank)"),
                       textInput('variable','Enter a vector (comma delimmited) indicating the variables used for MCA (categorical only)',
                                 "")),
      conditionalPanel(condition = "input.tabselected==2",
                       numericInput("nPC", "Number of PCs", min = 2, value =  5)),
      conditionalPanel(condition = "input.tabselected==3",
                       numericInput("xpc3", "PC on X-axis", min = 1, value = 1),
                       numericInput("ypc3", "PC on Y-axis", min = 1, value = 2)),
      conditionalPanel(condition = "input.tabselected==4",
                       numericInput("xpc4", "PC on X-axis", min = 1, value = 1),
                       numericInput("ypc4", "PC on Y-axis", min = 1, value = 2)),
      conditionalPanel(condition = "input.tabselected==5",
                       numericInput("xpc5", "PC on X-axis", min = 1, value = 1),
                       numericInput("ypc5", "PC on Y-axis", min = 1, value = 2))
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data", value = 1, 
                           dataTableOutput("Table")),
                  tabPanel("Eigenvalues", value = 2, 
                           plotOutput("ScreePlot"),
                           h5("Summary Table"),
                           tableOutput("Eigentable")),
                  tabPanel("Individuals", value = 3, 
                           plotOutput("IndPlot")),
                  tabPanel("Categories", value = 4, 
                           plotOutput("CatPlot"),
                           h5("Contributions"),
                           tableOutput("Ctrcat")),
                  tabPanel("Variables", value = 5, 
                           plotOutput("VarPlot")),
                  id = "tabselected"
      )
    )
  )
)


server <- function(input, output) {
  data <- reactive({
    if(input$deffile){
      data = tea
    }
    else{
      infile = input$file
      data = read.csv(infile$datapath,header=TRUE)
    }
    if(input$variable == ""){
      data_used = data
    }
    else{
      data_used = data[, c(as.numeric(unlist(strsplit(input$variable,','))))]
    }
    return(data_used)
  })
  
  mca<- reactive({
    MCA(data(), ncp = input$nPC)
  })
  
  output$Table<- renderDataTable({
    data_display<- cbind.data.frame(name = rownames(data()), data())
    return(data_display)
  })
  
  output$ScreePlot <- renderPlot({
    barchart<- barplot(mca()$eig[,1], las = 1, border = NA, names.arg = 1:length(mca()$eig[,1]),
                       ylim =c(0, 1.1* ceiling(max(mca()$eig[,1]))), ylab = "value",
                       xlab = "Eigenvalues (associated to the PCs)", main = "Scree plot")
    points(barchart, mca()$eig[,1], pch = 19, col = "gray50")
    lines(barchart, mca()$eig[,1], lwd = 2, col = "gray50")
  })
  
  output$Eigentable <- renderTable({
    eig = cbind.data.frame(name = rownames(mca()$eig), mca()$eig)
    colnames(eig) = c('dim', 'eigenvalue', 'percentage', 'cumulative')
    eig
  })
  
  output$IndPlot <- renderPlot({
    plot(mca(), axes = c(input$xpc3, input$ypc3) ,choix = "ind", invisible = "var")
  })
  
  output$CatPlot <- renderPlot({
    plot(mca(), axes = c(input$xpc4, input$ypc4) ,choix = "ind", invisible = "ind")
  })
 
  output$Ctrcat <- renderTable({
    ctr = mca()$var$contrib
    ctr = cbind.data.frame(Category = rownames(ctr), ctr)
    ctr
  })
  
  output$VarPlot <- renderPlot({
    plot(mca(), axes = c(input$xpc5, input$ypc5), choix = "var")
  })
  
}

shinyApp(ui = ui, server = server)





