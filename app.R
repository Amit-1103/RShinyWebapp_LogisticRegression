library(DT)
library(shiny)
library(shinythemes)
library(summarytools)
library(ggplot2)
library(GGally)
library(Amelia)
library(ROCR)
library(pscl)
library(caret)
library(e1071)
library(tidyverse)
library(rmarkdown)

ui <- fluidPage(shinythemes::themeSelector(),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                    titlePanel(
                        h1("Logistic Regression", align = "center")
                        ),
                         sidebarPanel(" ",
                             fileInput(
                                 "dataset",
                                 "Choose a CSV file",
                                 multiple = FALSE,
                                 accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv'),
                                 width = NULL,
                                 buttonLabel = "Browse",
                                 placeholder = "No file selected"
                             ),
                             checkboxInput("header", "Header", TRUE),
                             uiOutput("predicted_columns"),
                             br(),
                             uiOutput("inde_columns"),
                             radioButtons("format", "Download report:", c("HTML", "PDF", "Word"),inline = TRUE),
                             downloadButton("downloadReport"),
                             width = 3
                   ),
                         mainPanel(
                             tabsetPanel(
                             tabPanel("About",
                                      br(),
                                      h3("About Logistic Regression",align="center"),
                                      br(),
                                      uiOutput("about")),
                             tabPanel("Data",
                                      h3("Data Table", align = "center"),
                                      dataTableOutput("data")),
                             tabPanel("Summary",
                                      h3("Structure of Data Frame", align = "center"),
                                      br(),
                                      verbatimTextOutput("struc"),
                                      br(),
                                      h3("Summary of Data Frame", align = "center"),
                                      br(),
                                      verbatimTextOutput("summary")),
                             tabPanel("Descriptive Statistics",
                                      h3("Descriptive Statistics of Data", align = "center"),
                                      br(),
                                      verbatimTextOutput("des")),
                             tabPanel("Visualizations",
                                      h3("Pair Plots", align = "center"),
                                      br(),
                                      plotOutput("visual")),
                             tabPanel("Regression",
                                      h3("Model Summary", align = "center"),
                                      br(),
                                      verbatimTextOutput("regression"),
                                      br(),
                                      h3("Coefficients", align = "center"),
                                      br(),
                                      verbatimTextOutput("coeff"),
                                      br(),
                                      h3("ANOVA table", align = "center"),
                                      br(),
                                      verbatimTextOutput("anova")),
                             tabPanel("Assessment",
                                      br(),
                                      fileInput(
                                          "testdata",
                                          "Choose a test data (.CSV) file",
                                          multiple = FALSE,
                                          accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv'),
                                          width = NULL,
                                          buttonLabel = "Browse",
                                          placeholder = "No file selected"),
                                      br(),
                                      h3("Confusion Matrix",align="center"),
                                      verbatimTextOutput("con_matrix")),
                             tabPanel("Prediction",
                                      br(),
                                      textInput("predict",label = "Enter other attributes to predict
                                                (seperated by comma)", placeholder="Enter here"),
                                      br(),
                                      h3("Predicted Output"),
                                      verbatimTextOutput("predictv")),
                             tabPanel("Performance",
                                      br(),
                                      h3("Performance instance plot",align="center"),
                                      br(),
                                      h4("True positive vs False positive rate",align="center"),
                                      plotOutput("assess")),
                             tabPanel("Contact Us",
                                      br(),
                                      h3("Contact details",align="center"),
                                      br(),
                                      uiOutput("contact")))
                         )
)

server <- function(input, output,session) {
    options(shiny.sanitize.errors = TRUE)
    df <- reactive(read.csv(input$dataset$datapath, header = input$header))
    dtf <- reactive(subset(df(), select = c(input$columns,input$ind_columns)))
    dtf2 <- reactive(na.omit(dtf()))
    dtf3 <- reactive(subset(dtf2(), select = c(input$ind_columns)))
    td <- reactive(read.csv(input$testdata$datapath))
    td2 <- reactive(na.omit(td()))
    
    output$data <-  renderDataTable(
        dtf2(),
        filter = 'top',
        server = TRUE,
        options = list(lengthMenu = c(10,15,20,25,50), pageLength = 15)
    )
    output$predicted_columns <- renderUI({
        colnam <- colnames(df())
        selectInput("columns", "Choose Prediction variable", 
                           choices  = colnames(df()),
                            selected = colnam[1])
    })
    output$inde_columns <- renderUI({
       colnam <- colnames(df())
        selectInput("ind_columns", "Choose Independent variables", 
                    choices  <- colnam[!(colnam %in% input$columns)],
                    multiple = T,
                    selected = colnam[!(colnam %in% input$columns)])
    })
    output$struc <- renderPrint({
        str(dtf2())
    })
    output$summary <- renderPrint({
        dfSummary(dtf2())
    })
    output$des <- renderPrint({
        descr(dtf2())
    })
    
    output$visual <- renderPlot({
        ggpairs(dtf2(),
                aes(colour=as.factor(dtf2()[,input$columns])))
    })
    
    output$regression <- renderPrint({
        model <- glm(dtf2()[,input$columns] ~ ., family=binomial(link = "logit"), data=dtf3())
        summary(model)
    })
    
    output$anova <- renderPrint({
        model2 <- glm(dtf2()[,input$columns] ~ ., family=binomial(link = "logit"), data=dtf3())
        anova(model2, test="Chisq")
    })
    
    output$coeff <- renderPrint({
        model3 <- glm(dtf2()[,input$columns] ~ ., family=binomial(link = "logit"), data=dtf3())
        model3
    })
   
    output$con_matrix <- renderPrint({
        model5 <- glm(dtf2()[,input$columns] ~ ., family=binomial(link = "logit"), data=dtf3())
        predicted_result <- predict(model5,newdata=td2(),type='response')
        predicted_result2 <- ifelse(predicted_result > 0.5,1,0)
        confusionMatrix(data=as.factor(predicted_result2), reference=as.factor(td2()[,input$columns]))
    })
    
    output$predictv <- renderPrint({
        pre_inp <- reactive(as.numeric(strsplit(input$predict,",")[[1]]))
        temp <- dtf3()[1,]
        temp[1,] <- pre_inp()
        pd <- reactive(temp)
        model6 <- glm(dtf2()[,input$columns] ~ ., family=binomial(link = "logit"), data=dtf3())
        predicted_results <- predict(model6,newdata = pd(),type='response')
        predicted_results2 <- ifelse(predicted_results > 0.5,1,0)
        sprintf("%f",predicted_results2[1])
    })
    
    output$assess <- renderPlot({
        model4 <- glm(dtf2()[,input$columns] ~ ., family=binomial(link = "logit"), data=dtf3())
        p <- predict(model4, newdata=td2(), type="response")
        pr <- prediction(p, td2()[,input$columns])
        prf <- performance(pr, measure = "tpr", x.measure = "fpr")
        plot(prf)
    })
    
    output$about <- renderUI({
        h4(p("Logistic Regression, also known as Logit Regression or Logit Model, 
          is a mathematical model used in statistics to estimate (guess) the 
          probability of an event occurring having been given some previous data. 
          Logistic Regression works with binary data, where either the event happens
          (1) or the event does not happen (0).",br(),br(),
        p("Like all regression analyses, the logistic regression is a predictive analysis.
          Logistic regression is used to describe data and to explain the relationship 
          between one dependent binary variable and one or more nominal, ordinal, interval
          or ratio-level independent variables.",br(),br(),
        p("Here, the response variables can be categorical or continuous, as the model 
         does not strictly require continuous data. To predict group membership, logistic
         regression uses the log odds ratio rather than probabilities and an iterative
         maximum likelihood method rather than a least squares to fit the final model.
         This means the researcher has more freedom when using logistic regression and the method may 
         be more appropriate for nonnormally distributed data or when the samples 
         have unequal covariance matrices.",br(),br(),
        p("Thus, Logistic regression models the probabilities for classification 
         problems with two possible outcomes. It's an extension of the linear
         regression model for classification problems.")))))
    })
    
    output$contact <- renderUI({
       h4(br(),br(),p("Name: Sofin Wadhwaniya", br(),
          p("Email address: sofinwadhwaniya@gmail.com", br(),br(),br(),
            p("Name: Het Mehta", br(),
              p("Email address: hetmehta61@gmail.com")))))
    })    

    output$downloadReport <- downloadHandler(
        filename = function() {
            paste("Report", sep = ".", switch(
                input$format, PDF = "pdf", HTML = "html", Word = "docx"
            ))
        },
        content = function(file) {
            src <- normalizePath("report1.Rmd")
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, "report1.Rmd", overwrite = TRUE)
            out <- render("report1.Rmd", switch(
                input$format,
                PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ))
            file.rename(out, file)
        }
    )
}

shinyApp(ui = ui, server = server)
