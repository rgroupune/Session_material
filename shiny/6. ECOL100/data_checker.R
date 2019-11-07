# THis script makes a shiny app to allow ECOL100 students to upload their leaf mfeasurement data.
# It checks whetehr the data is properly formatted, and if it is, 
# The students can upload their data to Sabine's Google Docs. 
# Laste edited 18 July 2019

# Load some libraries
library(shiny)
library(googlesheets)
#gs_auth()

# REad in some helper functions
source("helper_functions.R")
options(shiny.sanitize.errors = F)

# Define UI for data upload app ----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  title = "ECOL100: Enter your leaf data",
  titlePanel("ECOL100: Enter your leaf data here"),
  sidebarLayout(
    sidebarPanel(
      p(tags$b('INSTRUCTIONS:')),
      p("Your data must be uploaded as a .csv (Comma-Separated Values) file. You can create a .csv file by using 'Save As...' in Microsoft Excel's File menu. Your dataset must contain six columns:"),
      tags$ol(
        tags$li("The first column indicates the number of the individual tree/shrub (1-5) from which you collected the leaf."),
        tags$li("The second column contains the number of the leaf collected from each individual (1-10)."),
        tags$li("The third and fourth columns contain the length and width of each leaf (in cm). Lengths and Widths should have one digit after decimal point."),
        tags$li("The fifth column contains the calculated leaf area (Length x Width), in cm", tags$sup(2), "presented with two digits after the decimal point."),
        tags$li("The sixth column contains the height of each tree, in m, presented with one digit after the decimal point.")
      ),
      p("To avoid errors, use the template provided on Moodle."),
      tags$hr(),
      fileInput("file1", "Choose CSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      numericInput("student_number", "Provide your Student Number", "", min = 0),
      textInput("last_name", "Provide your Last Name", ""),
      tags$hr(),
      p("Check over the preview of your data (on the right)."),
      p("Once you are satisfied that it is correct, click the 'Submit' button, below. It may take a minute before you see the confirmation message. Be patient! The message will contain your password."),
      actionButton("submit", "Submit", class = 'btn-primary')
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tableOutput("contents")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  #    Until the correct data is provided, disable the submit button
  observe({
    shinyjs::toggleState("submit", !is.null(input$student_number) && input$student_number != "" && !is.null(input$last_name) && input$last_name != "")
  })
  
  df <- reactive({
    # input$file1 will be NULL initially. After the user selects  and uploads a file, head of that data file by default, or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files, having a comma separator causes `read.csv` to error
    tryCatch({
      df <- read.csv(input$file1$datapath, header = T, sep = ",")
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
    )
    df <- df[!apply(df, 1, function(x){all(is.na(x))}),] # drop any rows that contain only NAs
    df <- df[,!apply(df, 2, function(x){all(is.na(x))})] # drop any columns that contain only NAs
    validate( # initial (first round) validation
      need(nrow(df) == 50,                'ERROR DETECTED: Your dataset should contain 50 rows, plus the column header. Check your data, and try again.\n'),
      need(ncol(df) == 6,                 'ERROR DETECTED: Your dataset should contain 6 columns: Tree number, Leaf number, Length, Width, Leaf area, and Tree height. Check your data, and try again.\n')
    )
    validate(# subsequent (second round) detailed validation
      need(all(!is.na(df[,1:6])),         'ERROR DETECTED: There can be no blanks in your dataset. Check your data, and try again.\n'),
      need(all(unique(df[,1]) %in% 1:5),  'ERROR DETECTED: The first column should contain only the numbers 1 through 5, corresponding to the number of the tree individual from which you collected each leaf. Check your data, and try again.\n'),
      need(all(unique(df[,2]) %in% 1:10), 'ERROR DETECTED: The second column should contain only the numbers 1 through 10, corresponding to the number of the leaf collected from each tree. Correct the error, and try again.\n'),
      need(length(unique(paste(df[,1], df[,2]))) == 50, "ERROR DETECTED: Every leaf must be uniquely labelled. In other words, each combination of 'Tree number' and 'Leaf number' can occur only once. Check your data, and try again.\n"),
      need(all(df[,3:6] > 0),             'ERROR DETECTED: All leaf Lengths, Widths, Leaf Areas, and Tree Heights should be greater than 0. Correct the error, and try again.\n'),
      need(mean(decimalplaces(unlist(df[,3:4]))) > 0.25 & max(decimalplaces(unlist(df[,3:4]))) == 1, 'ERROR DETECTED: Lengths and widths should be provided with a single digit after the decimal point. Correct the error, and try again.\n'),
      need(max(decimalplaces(df[,5])) == 2, 'ERROR DETECTED: Leaf areas should be provided with two digits after the decimal point. Correct the error, and try again.\n'),
      #      need(all(round(df[,5]) == round(df[,3] * df[,4])), 'ERROR DETECTED: Leaf area is the product of leaf length and leaf width. Check your calculations, and try again.\n'),  # Is this enough precision?
      need(all(sapply(1:50, function(x){isTRUE(all.equal(target = df[x,5], current = df[x,3] * df[x,4], tolerance = 0.01))})), 'ERROR DETECTED: Leaf area is the product of leaf length and leaf width. Check your calculations, and try again.\n'),  # This flags an error if ANY value deviates from expectation by > 1%.
      need(mean(decimalplaces(df[,6])) > 0.00 & max(decimalplaces(df[,6])) == 1, 'ERROR DETECTED: Tree heights should be provided with one digit after the decimal point. Correct the error, and try again.\n'),
      need(all(tapply(df[,6], df[,1], sd) == 0), 'ERROR DETECTED: There should be no variation in height within a tree. In other words, the height should be repeated for each leaf sampled from the tree. Correct the error, and try again.\n'),
      need(is.numeric(input$student_number), 'ERROR DETECTED: Student number should be numeric. Correct the error, and try again.\n'),
      need(input$last_name != "", 'ERROR DETECTED: Provide your last name. Correct the error, and try again.\n')
    )
    return(df)
  })
  
  output$contents <- renderTable({
    df()
  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    saveData_gs(df(), input$student_number, input$last_name)
    shinyjs::alert("Thank you for uploading your data! The password for assessing the workbook tasks for chapter 2 is: \"Ecology!\" (without the quotation marks)")
  })
}

# Create Shiny app ----
shinyApp(ui, server)
