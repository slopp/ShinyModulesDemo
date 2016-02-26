library(shiny)
library(e1071) 
library(ggplot2)

#---------------------
# Define Clusters
#---------------------

iris.true <- iris[,5]
iris.train <- iris[,-5] # no cheating

# Naive Bayes
classifier <- naiveBayes(iris.train, iris.true)
nb.clusters <- factor(as.integer(predict(classifier,iris.train)))

#kmeans (k=3)
km.clusters <- factor(kmeans(iris.train,3)$cluster)

iris.result <- cbind(iris.train, nb.clusters, km.clusters)

#---------------------
# Application Code
#---------------------


ui <- fluidPage(
   titlePanel("Clustering Comparison Tool"),
   column(width = 6, 
          fluidRow(h1("K Means"),
                   plotOutput("kmeans.plot")
                   ),
          fluidRow(selectInput("kmeans.x", label = "X Variable", choices=colnames(iris.train)),
                   selectInput("kmeans.y", label = "Y Variable", choices=colnames(iris.train))
                   )
          ),
   column(width=6,
          fluidRow(h1("Naive Bayes"),
                   plotOutput("nb.plot")
                   ),
          fluidRow(selectInput("nb.x", label="X Variable", choices=colnames(iris.train)),
                   selectInput("nb.y", label="Y Variable", choices=colnames(iris.train))
                   )
    )
)

server <- shinyServer(function(input, output){
  output$kmeans.plot<- renderPlot({
    ggplot(iris.result,aes_string(x=input$kmeans.x, y=input$kmeans.y,color='km.clusters')) + geom_point()
  })
  
  output$nb.plot <- renderPlot({
    ggplot(iris.result,aes_string(x=input$nb.x, y=input$nb.y, color='nb.clusters')) + geom_point()
  })
  
})   

shinyApp(ui, server)
  
