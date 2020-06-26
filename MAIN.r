library(shiny)

setwd("D:/Documents/150/project2020/latest_042020/")
source("FUNCTIONS.r")

# Define UI ----
ui <- fluidPage(
  
  #CSS STYLESHEET
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  ),
  br(),
  #BODY
  navbarPage("CMSC 150 Project",
            tabPanel(
              "Polynomial Regression",
              tags$div(class="sidebar", checked=NA,
              titlePanel("Estimation: Polynomial Regression"),
              sidebarLayout(
                sidebarPanel(
                  fileInput("prinputfile", "INPUT DATA",
                            accept = c(".csv")),
                  tableOutput("prdatacontents")
                ),
                mainPanel(
                  fluidRow(
                    column(
                      3,
                      helpText("Degree of your polynomial function."),
                      numericInput("n", "Degree", min = 1, value = 1)
                    ),
                    column(3,
                           helpText("Independent or x value for the estimate."),
                           numericInput("ind", "Independent", value = 0)),
                    column(3,
                           helpText("Estimated value using the polynomial function."), 
                           strong("Estimate"),
                           verbatimTextOutput("prest")),
                    column(3,
                           checkboxInput("pr_check", "Show graph", value=FALSE)),
                    br(),
                    actionButton("pr_action", strong("Get Estimate"))
                  ),
                  br(),
                  strong("Function"),
                  verbatimTextOutput("prfxn"),
                  br(),
                  strong("Graph"),
                  helpText("Green circles represent the ordered pairs from the csv input.",
                           "Various color of line represent the functions per interval."),
                  plotOutput("prgraph")
                )
              )
            )
            ),
            tabPanel("Optimization", 
                     sidebarLayout(
                       sidebarPanel(width=3,
                                    tags$div(
                                      class="sidebar", checked=NA,
                                      checkboxInput("show_init_tab", "Show initial tableau", value=FALSE),
                                      actionButton("optimize_button", strong("OPTIMIZE"), width = '100%'),
                                      hr(),
                                      splitLayout(
                                        tags$div(class="step", checked=NA,
                                                 column(3,h4("STEP:")),
                                                 column(9,numericInput("step_input", NULL,value=0))
                                        )
                                      ),
                                      actionButton("update_button", strong("UPDATE"), width = '100%'),
                                      verticalLayout(
                                        tags$div(class="ans", checked=NA,
                                                 column(8,h4("MINIMUM COST: ")),
                                                 column(4,h4(strong(textOutput("min_cost"))))
                                        )
                                      )
                                    )
                       ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Table", 
                                    tags$div(class="main", checked=NA,
                                             column(2,
                                                    numericInput("s1","DENVER", value = 200),
                                                    numericInput("s2","PHOENIX", value = 200),
                                                    numericInput("s3","DALLAS", value = 200),
                                                    tags$div(class="sup",checked=NA,h4("SUPPLY")),
                                                    tags$div(class="dem",checked=NA,h4("DEMAND"))
                                             ),
                                             column(2, 
                                                    numericInput("x1","Sacramento", value = 5),
                                                    numericInput("x6","", value = 6),
                                                    numericInput("x11","", value = 3),
                                                    br(),
                                                    numericInput("d1"," ", value=100)
                                             ),
                                             column(2, 
                                                    numericInput("x2","Salt Lake City", value = 6),
                                                    numericInput("x7","", value = 7),
                                                    numericInput("x12","", value = 5),
                                                    br(),
                                                    numericInput("d2"," ", value=100)
                                             ),
                                             column(2,
                                                    numericInput("x3","Albuquerque", value = 7),
                                                    numericInput("x8","", value = 8),
                                                    numericInput("x13","", value = 7),
                                                    br(),
                                                    numericInput("d3"," ", value=100)
                                             ),
                                             column(2,
                                                    numericInput("x4","Chicago", value = 8),
                                                    numericInput("x9","", value = 9),
                                                    numericInput("x14","", value = 11),
                                                    br(),
                                                    numericInput("d4"," ", value=100)
                                             ),
                                             column(2,
                                                    numericInput("x5","	New York City", value = 9),
                                                    numericInput("x10","", value = 10),
                                                    numericInput("x15","", value = 13),
                                                    br(),
                                                    numericInput("d5"," ", value=100)
                                             )
                                             
                                    )
                           ),
                           tabPanel("Tableau",
                                    tags$div(class="tableaupanel", checked=NA,
                                             h4("Initial Tableau"),
                                             tableOutput("init_table"),
                                             h4("Solution of Current Tableau"),
                                             tableOutput("solution_table"),
                                             h4("Current Tableau"),
                                             tableOutput("current_table")
                                    )
                           )
                         )
                       )
                     )
                     
            )
            )
)
  


# SERVER LOGIC ----
server <- function(input, output) {

  #put reactive expression for simplex
  #RETURNS a list 'res'
  #call this variable whenever calling an output..
  simplex_reactive <- eventReactive(input$optimize_button,{
    supply = c(input$s1, input$s2, input$s3)
    demand = c(input$d1, input$d2, input$d3, input$d4, input$d5)
    data = c(input$x1,input$x2,input$x3,input$x4,input$x5,input$x6,input$x7,input$x8,input$x9,input$x10,input$x11,input$x12,input$x13,input$x14,input$x15)
    
    #CALL SIMPLEX BACKEND FROM MAIN.r
    SimplexBackend(supply, demand, data)
  })
  
  update_reactive <- eventReactive(input$update_button,{
    if(is.na(simplex_reactive())) return (NULL)
    
    step = input$step_input
    tablist = (simplex_reactive())$tablist
    if(step>length(tablist)) return (NULL)
    if(step<1) return (NULL)
    
    return(list(curr_tab = tablist[step], sol_tab = SolutionTable(tablist[[step]])))
  })
  
  # ------------------ OUTPUT VARIABLES
  output$solution_table <- renderTable({
    if(is.na(simplex_reactive())) return (NULL)
    
    res = simplex_reactive()
    update_reactive()$sol_tab
  })
  
  output$init_table <- renderTable({
    if(is.na(simplex_reactive())) return (NULL)
    
    if(input$show_init_tab == FALSE) return (NULL)
    (simplex_reactive())$init_tab
  })
  
  output$current_table <- renderTable({
    if(is.na(simplex_reactive())) return (NULL)
    
    update_reactive()$curr_tab
  })
  
  output$min_cost <- renderText({
    if(is.na(simplex_reactive())) return ("No feasible solution.")
    (simplex_reactive())$mincost
  })
  
  #------ POLY REG
  prdata <- reactive({
    infile <- input$prinputfile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    dt <- read.csv(infile$datapath, header = FALSE)
    names(dt) = c("x", "y")
    return(dt)
  })
  
  output$prdatacontents <- renderTable({
    prdata()
  })
  
  
  pr <- eventReactive(input$pr_action, {
    if (is.null(prdata())) {
      return (NULL)
    }
    data <- prdata()
    
    n <- input$n
    x <- data[, 1]
    y <- data[, 2]
    
    # return(PolynomialRegression(x,y,n))
    return(PolynomialRegression(data[, 1], data[, 2], input$n))    
  })
  
  output$prgraph <- renderPlot({
    if (is.null(prdata()) || is.null(pr())) {
      return (NA)
    }
    
    if(input$pr_check == TRUE){
      data <- prdata()
      PR <- pr()
      
      x <- data[, 1]
      y <- data[, 2]
      
      plot(x, y, col="green")
      lines(PR$f(min(x):max(x)), type='l', col="red")
      title(xlab="x", col.lab=rgb(0,0.5,0))
      title(ylab="y", col.lab=rgb(0,0.5,0))
    }
  })
  
  output$prfxn <- renderText({
    if (is.null(pr())) {
      return (NULL)
    }
    PR <- pr()
    PR$s
  })
  
  output$prest <- renderText({
    if (is.null(pr())) {
      return (NULL)
    }
    PR <- pr()
    PR$f(input$ind)
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)
