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
# Module Code
#---------------------


clusterUI <- function(id, train){
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot")),
    selectInput(ns("x"), label = "X Variable", choices=colnames(train)),
    selectInput(ns("y"), label = "Y Variable", choices=colnames(train))
  )
  
}

cluster <- function(input, output,session,result, column){
  output$plot<- renderPlot({
    ggplot(result,aes_string(x=input$x, y=input$y,color=column)) + geom_point()
  })
  
}

#---------------------
# Application Code
#---------------------


ui <- fixedPage(
  titlePanel("Clustering Comparison Tool"),
  column(width = 6, 
            h1("K Means"),
            clusterUI("kmeans", iris.train)
         
  ),
  column(width=6,
            h1("Naive Bayes"),
            clusterUI("nb", iris.train)
  )
)

server <- function(input, output){
  callModule(cluster,"kmeans", iris.result, "km.clusters")
  callModule(cluster, "nb", iris.result, "nb.clusters")
}

shinyApp(ui, server)
