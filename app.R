#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

pingjun <- function(n_sample = 2000,n_batch = 200,n_test = 3000,distribution = "norm",n_bind = 100)
{
  if(distribution == "norm")
  {a = rnorm(n_sample,0,1)}
  
  
  if(distribution =="binomial" )
  {a = rbinom(n_sample,100,0.5)}
  if(distribution =="random")
  {a = runif(n_sample,1,100)}
  k = c()
  for(i in 1:n_test)
  {
    sample1 = sample(1:n_sample,size = n_batch )
    n = a[sample1]
    k = c(k,mean(n))
  }
  l = c(1:n_test)
  k = data.frame(l,k)
  #return(k)
  #n_bind = (max(k$k)-min(k$k))/n_bind
  
  ggplot(k,aes(k,y =..density..))+geom_histogram(bins = n_bind ,show.legend = FALSE)+geom_density(size=1,colour = "Black")+labs(x = "arithmetic mean", y = "number")  + theme_minimal()
  #return (qplot(k,data = k,geom = "histogram",binwidth = (max(k$k)-min(k$k))/n_bind,xlim = c(min(k$k),max(k$k))))
} 
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Central Limit Theorem test"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 2,
                     max = 200,
                     value = 100),
         sliderInput("n_batch",
                     "Size of batch:",
                     min = 2,
                     max = 2000,
                     value = 200),
         sliderInput("n_test",
                     "Sample number:",
                     min = 10,
                     max = 5000,
                     value = 1000),
         sliderInput("n_sample",
                     "Size of the whole samples:",
                     min = 100,
                     max = 5000,
                     value = 1000),
          selectInput("dist", "The distribution type:", 
                       choices=c("norm","binomial","random"))
         
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      pingjun(n_sample = input$n_sample,n_batch = input$n_batch,n_test = input$n_test,distribution = input$dist,n_bind = input$bins)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

