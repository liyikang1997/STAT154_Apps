#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mvtnorm)
library(MASS)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny App for Linear discriminant analysis with 2 classes"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("mean1",'Enter a vector (comma delimmited) indicating the Mean for class 1', value = '0,0'),
      textInput("mean2",'Enter a vector (comma delimmited) indicating the Mean for class 2', value = '2,2'),
      textInput("sigma1",'Enter a vector (comma delimmited) indicating the variance for class 1', value = '2,-1'),
      textInput("sigma2",'Enter a vector (comma delimmited) indicating the variance for class 2', value = '1,0'),
      numericInput("size1", "Size of class 1", min = 1, value = 100),
      numericInput("size2", "Size of class 2", min = 1, value = 100)),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("LdaPlot"),
      verbatimTextOutput("boundary")
    )
  )
)


server <- function(input, output) {
  
  x<- reactive({
    mean1<- c(as.numeric(unlist(strsplit(input$mean1,','))))
    mean2<- c(as.numeric(unlist(strsplit(input$mean2,','))))
    sigma1<-  cbind(c(as.numeric(unlist(strsplit(input$sigma1,',')))), rev(c(as.numeric(unlist(strsplit(input$sigma1,','))))))
    sigma2<-  cbind(c(as.numeric(unlist(strsplit(input$sigma2,',')))), rev(c(as.numeric(unlist(strsplit(input$sigma2,','))))))
    
    X1 = rmvnorm(input$size1, mean = mean1, sigma = sigma1)
    X2 = rmvnorm(input$size2, mean = mean2,  sigma = sigma2)
    X = rbind(X1, X2)
  })
  
  
  labels<- reactive({
    labels = c(rep(1, input$size1), rep(2, input$size2))
  })
  
  table<- reactive({
    table<- as.data.frame(cbind(X = x()[,1], Y = x()[,2], Class = labels()))
  })
  
  boundary<- reactive({
    lda<- lda(Class~., data = table())
    
    g1<- lda$means[1,]
    g2<- lda$means[2,]
    n1<- input$size1
    n2<- input$size2
    n<- n1+n2
    W1<- (1/(n1-1))*t(x()[1:n1,])%*%x()[1:n1,]
    W2<- (1/(n2-1))*t(x()[n1+1:n, ])%*%x()[n1+1:n, ]
    W<- (n1-1)*W1/(n-1) + (n2-1)*W2/(n-1) 
    W_inv<- inv(W)
  
    f<- -0.5*(g1+g2)*W_inv*(g1-g2)+(g1-g2)*W_inv%*%x()
    })
  
  
  output$LdaPlot<- renderPlot({
    plot(x()[labels() == 1,], col = 'red', xlab="X-axis", ylab="Y-axix", xlim=c(min(x()[,1])-1, max(x()[,1])+1),  ylim=c(min(x()[,2])-1, max(x()[,2])+1))
    points(x()[labels() == 2,], col = 'blue')
  })
  
  output$boundary<- renderPrint(
    boundary()
  )
  
}
# Run the application 
shinyApp(ui = ui, server = server)

