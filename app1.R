library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("N-Quiz-A-tor"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Choose Excel File",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

      # Horizontal line ----
      tags$hr(),
      # Horizontal line ----
      tags$hr(),

      # Input: Text ----
      textInput(inputId = "answer", label = "Term"),
      
      # Input: MASH
      actionButton(inputId = "mash", label =  "SUBMIT")
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      tableOutput("contents"),
      textOutput("myanswer"),
      # Horizontal line ----
      tags$hr(),
      h3("Last Wrong Definition"),
      tableOutput("lastwrong"),
      # Horizontal line ----
      tags$hr(),
      textOutput("score_x"),
      textOutput("score_y")
    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  require(dplyr)
  dfx <- NULL

  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    df <- read_excel(input$file1$datapath)
    x <- dim(df)[[1]]
    dfx <<- df <- sample_n(df, size = x, replace=FALSE)
    ans <<- df$Term[[1]]
    defin <<- df$Definition[[1]]
    df <- select(df, Week,  Definition)
    return(df[1,])

  })
  
  
  x <- 0
  y <- 0
  output$myanswer <- renderText("Select File using widget to your left.")
  
  if(exists("ans")) output$myanswer <- renderText("Write your answer and click submit.")
  
  
  
  observeEvent(input$mash, {
    
    #create object for clicked polygon
    
    answer <- isolate(input$answer)
    
    # output$myanswer <-renderText(ifelse(
    #   tolower(answer) == tolower(ans), "CORRECT!", paste("Sorry, incorrect - I was looking for", ans)))
    # 
    xz <- ans
    
    if(tolower(answer) == tolower(ans)) output$myanswer <-renderText("Correct")
    if(tolower(answer) != tolower(ans)){
      definx <- defin
      output$myanswer <-renderText(paste("Incorrect, I wanted", xz))
      output$lastwrong <- renderTable(data.frame(Definition = definx, Term = xz))
    }
    
    updateTextInput(session, "answer", value = "")
    
    x <- get("x", pos = -1)
    y <- get("y", pos = -1)  
    
    x <<- x <- x + as.numeric(tolower(answer) == tolower(ans))
    y <<- y <- y + as.numeric(tolower(answer) != tolower(ans))
    
#    browser()
    output$score_x <-renderText(paste("Total Correct = ", x))
    output$score_y <-renderText(paste("Total Incorrect = ", y))
    
    df <- sample_n(dfx, size = 1, replace=FALSE)
    ans <<- df$Term[[1]]
    defin <<- df$Definition[[1]]
    output$contents <- renderTable(select(df, Week, Definition))
    
  }) #END OBSERVE EVENT
  

 
  
}

  

# Create Shiny app ----
shinyApp(ui, server)


