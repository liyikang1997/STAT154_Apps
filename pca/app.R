library(shiny)
library(FactoMineR)


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny App for PCA"),
  
  # Sidebar with different widgets depending on the selected tab
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1",
                       checkboxInput("deffile","Use default dataset (mtcars)?",TRUE),
                       conditionalPanel(condition="!input.deffile",
                                        fileInput('file', 'Choose file to upload',
                                                  accept = c('text/csv',
                                                             'text/comma-separated-values',
                                                             '.csv'))),
                       hr("Remember that PCA can only be used with quantitative data, indicate the variables used for PCA below:"),
                       h6("(All variables will be included by default if leave a blank)"),
                       textInput('variable','Enter a vector (comma delimmited) indicating the variables used for PCA (quantitative only)',
                                 "")),
      conditionalPanel(condition = "input.tabselected==2",
                       numericInput("nPC", "Number of PCs", min = 2, value =  5),
                       checkboxInput('standardize', label = 'standardize',value = TRUE)),
      conditionalPanel(condition = "input.tabselected==3",
                       numericInput("xpc3", "PC on X-axis", min = 1, value = 1),
                       numericInput("ypc3", "PC on Y-axis", min = 1, value = 2)),
      conditionalPanel(condition = "input.tabselected==4",
                       numericInput("xpc4", "PC on X-axis", min = 1, value = 1),
                       numericInput("ypc4", "PC on Y-axis", min = 1, value = 2))
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
                           
                  tabPanel("Variables", value = 4, 
                           plotOutput("VarPlot"),
                           h5("Contributions"),
                           tableOutput("Ctrvar")),
                  id = "tabselected"
      )
    )
  )
)


server <- function(input, output) {
  data=reactive({
    if(input$deffile){
      data=mtcars
    }
    else{
      infile=input$file
      data=read.csv(infile$datapath,header=TRUE)
    }
    if(input$variable == ""){
      data_used = data
    }
    else{
      data_used = data[, c(as.numeric(unlist(strsplit(input$variable,','))))]
    }
    return(data_used)
  })
  
  
  pca<- reactive({
    PCA(data(), scale.unit = input$standardize, ncp = input$nPC)
  })
  
  output$Table<- renderDataTable({
    data_display<- cbind.data.frame(name = rownames(data()), data())
    return(data_display)
  })
  
  output$ScreePlot <- renderPlot({
    barchart<- barplot(pca()$eig[,1], las = 1, border = NA, names.arg = 1:length(pca()$eig[,1]),
                       ylim =c(0, 1.1* ceiling(max(pca()$eig[,1]))), ylab = "value",
                       xlab = "Eigenvalues (associated to the PCs)", main = "Scree plot")
    points(barchart, pca()$eig[,1], pch = 19, col = "gray50")
    lines(barchart, pca()$eig[,1], lwd = 2, col = "gray50")
  })
  
  output$Eigentable <- renderTable({
    eig = cbind.data.frame(name = rownames(pca()$eig), pca()$eig)
    colnames(eig) = c('dim', 'eigenvalue', 'percentage', 'cumulative')
    return(eig)
  })
  
  output$IndPlot <- renderPlot({
    plot(pca(), axes = c(input$xpc3, input$ypc3) ,choix = "ind")
  })
  
  output$VarPlot <- renderPlot({
    plot(pca(), axes = c(input$xpc4, input$ypc4), choix = "var")
  })
  
  output$Ctrvar <- renderTable({
    ctr = pca()$var$contrib
    ctr = cbind.data.frame(variable = rownames(ctr), ctr)
    return(ctr)
  })
}

shinyApp(ui = ui, server = server)





