
library(shiny)
library(broom)
library(gt)
library(tidyverse)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cerulean"),

    # Application title
    titlePanel("Linear Modelling"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file","Select file", accept = ".csv"),
            selectInput("sep",label="Separator",choices = c(",",";")),
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
            actionButton("go", "Go")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("lm")
           
        )
    )
)

server <- function(input, output) {
    
    df <- reactive({
        file <- input$file
        if (is.null(file)) {
            
            return(NULL)
        }
        
        if (input$sep == ",") {
            read_csv(file$datapath) 
        }
        else if (input$sep == ";") {
            read_csv2(file$datapath)  
        }
    })
    
    output$varselect1 <- renderUI({
        cols <- names(df())
        selectInput("dv", "DV",choices=cols)  
        
    })
    output$varselect2 <- renderUI({
        cols <- names(df())
        selectInput("iv1", "IV 1",choices=cols)  
        
    })
    output$varselect3 <- renderUI({
        cols <- names(df())
        selectInput("iv2", "IV 2",choices=c("none",cols)) 
        
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
    
    output$lm <- renderTable({
        mod() %>% 
            tidy() %>% 
            mutate_if(is.numeric,round,3) %>%
            gt()
    })

    }

# Run the application 
shinyApp(ui = ui, server = server)
