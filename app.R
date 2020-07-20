
library(shiny)
library(broom)
library(gt)
library(tidyverse)
library(shinythemes)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),

    # Application title
    titlePanel("eLMo"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file","Select file", accept = ".csv"),
            hr(),
            # selectInput(inputId = "dv",
            #             label = "DV",
            #             choices = names(df)),
            # hr(),
            # selectInput(inputId = "iv1",
            #             label = "IV",
            #             choices = names(df)),
            # selectInput(inputId = "iv2",
            #             label = "IV",
            #             choices = c("none",names(df))),
            uiOutput("varselect1"),
            selectInput("type",label="Type",choices = c("numeric","categorical")),
            uiOutput("varselect2"),
            uiOutput("varselect3"),
            hr(),
            selectInput("plotting",label="Plot",choices = c("none","box","scatter")),
            uiOutput("varselect4"),
            hr(),
            actionButton("go", "Go")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("lm"),
           plotOutput("plot")
           
        )
    )
)

server <- function(input, output) {
    
    df <- reactive({
        file <- input$file
        if (is.null(file)) {
            
            return(NULL)
        }
        read_csv(file$datapath) 
    })
    
    output$varselect1 <- renderUI({
        cols <- names(df())
        selectInput("dv", "Dependent variable",choices=cols)  
        
    })
    output$varselect2 <- renderUI({
        cols <- names(df())
        selectInput("iv1", "Independent variable 1",choices=cols)  
        
    })
    output$varselect3 <- renderUI({
        cols <- names(df())
        selectInput("iv2", "Independent variable 2",choices=c("none",cols)) 
        
    })
    output$varselect4 <- renderUI({
        cols <- names(df())
        selectInput("facet", "Facet",choices=c("none",cols)) 
        
    })
    mod  <-  eventReactive(input$go, {
        if (input$iv2 == "none" & input$type == "numeric") {
            lm(reformulate(termlabels = c(input$iv1), response = input$dv),data=df())
        }
        else if (input$iv2 != "none" & input$type == "numeric") {
            lm(reformulate(termlabels = c(input$iv1,input$iv2), response = input$dv),data=df())
        }
        else if (input$iv2 == "none" & !input$type == "numeric") {
            glm(reformulate(termlabels = c(input$iv1), response = input$dv),
               family="binomial",data=df())
        }
        else if (input$iv2 != "none" & !input$type == "numeric") {
            glm(reformulate(termlabels = c(input$iv1,input$iv2), response = input$dv),
               family="binomial",data=df())
        }
        
        })
    
    plot <- eventReactive(input$go, {
        
        if(input$plotting == "none"){}
        
        else if(input$plotting == "scatter") {
            
            df() %>% 
                ggplot(aes_string(input$iv1,input$dv)) +
                theme_minimal(base_size=14) +
                geom_point(alpha= .7) +
                
                if(input$facet != "none") {
                    facet_wrap(~get(input$facet))
                } else{NULL}
            
        }
        
        else if(input$plotting == "box") {
            df() %>% 
                ggplot(aes_string(input$iv1,input$dv,group=input$iv1)) +
                theme_minimal(base_size=14) +
                geom_point(alpha= .2) +
                geom_boxplot(alpha=0,outlier.shape = NA) +
                if(input$facet != "none") {
                    facet_wrap(~get(input$facet))
                } else{NULL}
        }
  
    })
    
    output$lm <- renderTable({
        mod() %>% 
            tidy() %>% 
            mutate_if(is.numeric,round,3) %>%
            gt()
    })
    
    output$plot <- renderPlot({
        plot()
        
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
