 rm(list=ls())
 cat("\014") 

# Setting working directory

# setwd("D:/WPI/DS501/Case Study 3/SJA_App/SJA_App")

# Load Libraries

library(shiny)
library(tidyverse)
library(rsconnect)
library(data.table)

# Load Data

data0 = req(read.csv(file = "data/StudentsPerformance.csv"))

# Filtering to just numeric data

data1 = req(data0[,6:8])

# Filtering to just non numeric data

data2 = req(data0[,1:5])

# Isolating the names for both data sets

vars1 = names(data1)
vars2 = names(data2)

# User interface

ui <- fluidPage(

# Adding the title for the application
  
    headerPanel('Analysis of Performance of Students'), 
    
# Dividing the Application into multiple tabs

    tabsetPanel(
      
# Titling the First Tab
      
      tabPanel("Score Factors",

# Creating the side panel with a select input option
            sidebarPanel(selectInput('histo', 'Variable 1', vars2)),

# Creating the Main Panel with both a plot and a table to summarize the plot
      
      mainPanel("Plot of Factors", plotOutput(outputId = "distPlot"),
        h3("Table of Variable 1"),tableOutput("table"))),

# Naming the second tab that has the machine learning involved
      
      tabPanel("k-Mean Cluster",
               
# Adding three inputs to the side panel that change the cluster as well as the axis'

              sidebarPanel(
      selectInput('xcol', 'X Variable', vars1),
      selectInput('ycol', 'Y Variable', vars1, selected = vars1[[2]]),
      sliderInput(inputId = "clusters",
                  label = "Cluster Count", min = 1, max = 9, value = 3)),

# Adding the plot that is resultant of the inputs above

      mainPanel("k-Mean Plot", plotOutput("plot1"))),
      
# Adding the third tab which is a histogram of the scores

      tabPanel("Score Histogram",
               
# Adding two inputs to create the histogram

      sidebarPanel(selectInput('xcol1', 'X Variable', vars1),
      sliderInput(inputId = "bins",label = "Number of bins", min = 1, max = 100,
                             value = 30)),
      
# Adding the plot that changes from the inputs as well as summary of the data that the plot shows

      mainPanel(plotOutput(outputId = "histoplot"),
                h3("Statistics of the X-Variable"),
                verbatimTextOutput("summary"))),
                 
# Adding the final tab that contains all the text related to the app and data

      tabPanel("Description of Data/App",
               
               mainPanel(
                 h1("Home Work 6 - Case Study 3"),
                 h2("Dataset Used"),
                 a("Students Performance in Exams", href= "https://www.kaggle.com/datasets/spscientist/students-performance-in-exams"),
                 h2("Algorithm Selected for the Above Dataset"),
                 p("Clustering was selected for the dataset above."),
                 h2("Mathematical/Statistical Details of the Algorithm"),
                 p("As mentioned above, the machine learning algorithm that was
                   selected was clustering. More specifically, k-mean clustering
                   was selected. It is one of the simpler types of machine 
                   learning algorithms. It is unsupervised which means human 
                   interaction is not needed to teach the machine. The algorithm 
                   uses the data to infer the outputs. A number of clusters is
                   put into the algorithm where it then calculates the same 
                   number of centroids. The data is then sorted into clusters
                   based on which centroid the data point is closest to. The algorithm 
                   starts by selecting random centroids and goes through an
                   iterative process to find the optimal centroids based on the 
                   averaging of the data. That is the machine learning aspect of
                   the algorithm."),
                 h2("Dataset and Machine Learning/Modeling Methodology"),
                 p("The data set that was selected is what appears to be from a 
                   standardized test, which may be the state version (MCAS for MA)
                   or the SATs.
                   It includes the gender of students, race/ethnicity which is depicted as a 
                   letter A-E, the highest level of parental education,
                   whether or not they get free lunch provided by the institution,
                   if they participated in a test preperation course, and the results for 
                   reading, mathematics, and writing. The data is analyzed 
                   through many different methods. There are histograms to 
                   analyze the scores as well as statistics on those scores. 
                   There is a histogram showing the distribution of categorical
                   values such as gender, race, etc. Then there is the machine
                   learning k-mean cluster that can have the axis change based 
                   on what the user wants. For more information for the k-mean 
                   clustering, refer to the paragraph above. Every plot has an
                   option to change the inputs to give the user a choice to 
                   better analyze the data."),
                 h2("Analysis of Data"),
                 h3("Data Collected"),
                 p("The main data was acquired from the website linked above.
                   Through analysis, more data can be acquired that may be 
                   important to school administrators or teachers. For example,
                   when setting the k-mean plot variables to reading and writing
                   scores
                   and maximizing the clusters to nine, it is clear that there
                   is a linear relationship between the two scores
                   based on the distribution of the clusters and the centroid 
                   location.
                   When analyzing the education of the parents in conjunction with
                   basic 
                   mathematics, the highest level of parental education was high 
                   school or less for nearly 
                   38% of the students. So, 62% of the students had a parent that
                   had at least participated in a college course. If this 
                   application were 
                   to be taken a step further, it would be interesting to see
                   statistics of the clusters that are formed using the k-mean. 
                   What are the education levels of the lowest scoring cluster 
                   and do they have to pay for lunch?
                   Are boys or girls more likely to score higher in reading, 
                   writing, or mathematics? With the ability to analyze the data
                   in the cluster, it would make the machine learning algorithm 
                   that much more useful."),
                 h3("Why the topic is important to me"),
                 p("This topic is important to me because my significant other 
                   is a teacher. She speaks about the programs she uses at work. 
                   I showed her what I wanted to do and she performed a role 
                   of a stakeholder in the creation of the application. So this
                   application became useful to her as it allows her to identify 
                   and target specific students based on individual need."),
                 h3("How the data was analyzed"),
                 p("The data was analyzed in many ways. The two main ways 
                 however, 
                   were using the machine learning k-mean clustering algorithm
                   and histograms."),
                 h3("What was found in the data"),
                 p("What was found from the data can be interpreted differently
                 from person to 
                   person. It matters what you are looking for. In the previous
                   two case studies a question was asked and code was written 
                   to answer that question. However, in this case study, code is
                   written to give the user the opportunity to explore the data
                   themselves and make assumptions and inferences from the data
                   as they see fit.")))))

# Server logic

server <- function(input, output) {
  
# Creating the reactive data for the k-mean plot

  selectedData <- reactive({
    data1[, c(req(input$xcol), req(input$ycol))]})
  
# Creating the reactive data for the score histogram
  
   selectedData3 <- reactive({
    data1[, req(input$xcol1)]})
  
# Creating the reactive cluster for the k-mean plot
   
   clusters <- reactive({
    kmeans(req(selectedData()), req(input$clusters))})
  
# Creating the plot for the k-Mean plot with different colors as each cluster is added
   
   output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
# Adding other variables to the plot
     
    par(mar = c(5.1, 4.1, 0, 1))
    plot(req(selectedData()),
         col = req(clusters()$cluster),
         pch = 20, cex = 3)
    points(req(clusters()$centers), pch = 4, cex = 4, lwd = 4)})
  
# Creating the summary for the third tab the score histogram
   
  output$summary <- renderPrint({summary(req(selectedData3()))})
  
# Creating the data and table for the first tab which is the categories plot  
  
  selectedData2 <- reactive({data.table(table(req(data2[, req(input$histo)])))})
  
# Creating the plot for the variable categories tab
  
  output$distPlot <- renderPlot({
    ggplot() + geom_col(data = req(selectedData2()), aes(x = V1, y = N))
    })
  
# Creating the table for the variable tab to summarize the data
 
  output$table <- renderTable(req(selectedData2()))
  
# Creating the histogram plot for the score histogram
  
  output$histoplot <- renderPlot({
    
    bins <- seq(min(req(selectedData3())), max(req(selectedData3())), length.out = req(input$bins) + 1)
    
    hist(req(selectedData3()), breaks = bins, col = "#75AADB", border = "white",
         xlab = "Score out of 100",
         main = "Histogram of Scores")})
  
  }

# Run app 
 
shinyApp(ui=ui, server=server)

