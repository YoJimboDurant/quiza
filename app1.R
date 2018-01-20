library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Uploading Files"),

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
      actionButton(inputId = "mash", label =  "MASH ME!")
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      tableOutput("contents"),
      textOutput("myanswer")
    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  require(dplyr)

  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    df <- read_excel(input$file1$datapath)
    x <- dim(dfx)[[2]]
    dfx <<- df <- sample_n(df, size = x, replace=FALSE)
    
    df <- select(df, Week,  Definition)
    return(df[1,])

  })
  
  output$myanswer <- renderText("Select File using widget to your left.")
  
  observeEvent(input$mash, {
    
    #create object for clicked polygon
    output$myanswer <-renderText(isolate(input$answer))

    
  }) #END OBSERVE EVENT
  
  
  
}

  

# Create Shiny app ----
shinyApp(ui, server)


