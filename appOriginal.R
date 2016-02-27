library(shiny)
library(e1071) 
library(ggplot2)

#---------------------
# Define Clusters
#---------------------

iris.true <- iris[,5]
iris.train <- iris[,-5] # no cheating

# Naive Bayes
nb.classifier <- naiveBayes(iris.train, iris.true)
nb.clusters <- factor(as.integer(predict(nb.classifier,iris.train)))

# SVM
svm.classifier <- svm(iris.train, iris.true)
svm.clusters <- factor(as.integer(predict(svm.classifier,iris.train)))

iris.result <- cbind(iris.train, nb.clusters, svm.clusters)

#---------------------
# Application Code
#---------------------


ui <- fluidPage(
   titlePanel("Classification Comparison Tool"),
   p("SVM and Naive Bayes algorithms were used to cluster the iris dataset. Here we are plotting the 'fitted' results."),
   column(width = 6, 
          fluidRow(h1("Support Vector Machine"),
                   plotOutput("svm.plot")
                   ),
          fluidRow(selectInput("svm.x", label = "X Variable", choices=colnames(iris.train)),
                   selectInput("svm.y", label = "Y Variable", choices=colnames(iris.train))
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
  output$svm.plot<- renderPlot({
    ggplot(iris.result,aes_string(x=input$svm.x, y=input$svm.y,color='svm.clusters')) + geom_point()
  })
  
  output$nb.plot <- renderPlot({
    ggplot(iris.result,aes_string(x=input$nb.x, y=input$nb.y, color='nb.clusters')) + geom_point()
  })
  
})   

#shinyApp(ui, server)
  
