library(shiny)
library(dplyr)    # For the data wrangling
library(lazyeval) 

ui <- fluidPage(
  headerPanel("Data Wrangler for Wordsmith"),
  
  sidebarPanel(
    "Instructions: Upload your CSV file, then use this tool to format your data for Wordsmith.",
    hr(),
    fileInput(inputId = "datafile", label= "Choose CSV file", 
              accept='.csv'),
    textOutput("filedim"), 
    checkboxInput("missing", "There are missing value codes in my dataset", value=FALSE), br(),
    uiOutput("missingvalues"), br(),

    actionButton("action1", "Apply changes"), br(), br(), br(), 
    
    hr(), 
    "Choose wrangling options below.", br(), br(),
    uiOutput("dplyrOperations"),
    uiOutput("dplyrOperationsSelect"),
    uiOutput("dplyrOperationsGroup"),
    uiOutput("dplyrOperationsMutate"), 
    uiOutput("dplyrOperationsArrange"),
    uiOutput("dplyrOperationsFilter"),
    actionButton("action2", "Wrangle!"), br(), br(), br()
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("data", 
               textOutput("filehead"), br(), br(),
               tableOutput("filetable")),
      tabPanel("wrangled", 
               textOutput("myInstructions"),
              tableOutput("wrangled"))
        )
    )
)  




server <- function(input, output){
  require(shiny)
  require(psych)
  require(dplyr)
  require(lazyeval)
  
  
  #Load in the selected file
  filedata = reactive({ 
    infile = input$datafile
    if (is.null(infile)){
      return(NULL)
    }
    read.csv(infile$datapath, na.strings=input$missingvalues, stringsAsFactors=FALSE)
  })
  
  output$missingvalues = renderUI({
    req(input$missing)
    textInput(inputId = "missingvalues", 
              label="Enter any missing value codes that are present in your data (ex. -999, ., NA).
              If multiple missing value codes are present, separate them with commas.",
              value="")
  })
  
  #The file you uploaded has X rows and Y columns 
  output$filedim = renderText({    
                                   df = filedata()
                                   if (is.null(df)) return()
                                   paste("The file you uploaded has", dim(df)[1], "rows and", dim(df)[2], "columns.")
                                 })
  
  
  output$filehead <- renderText({ 
    if (is.null(filedata())){
      return(NULL)
    }
    br()
    paste("Below is a preview of your dataset.")
  })
  
  #Preview the CSV data file
  output$filetable <- renderTable({ 
    filedata()
  })
  
  
  # Data wrangling options all stored as output$dplyrXXX
  output$dplyrOperations = renderUI({
    req(input$action1)
    items = c("select", "group", "mutate", "filter", "arrange")
    selectInput("dplyrOperations", "Select all of the data wrangling techniques you would like performed.", items, multiple=TRUE)
  })
  
  output$dplyrOperationsSelect = renderUI({
    req(input$action1)
    df = filedata()
    items = names(df)
    if ("select" %in% input$dplyrOperations)
      selectInput("dplyrOperationsSelect", "Select the variables you would like in your wrangled dataset.", items, multiple=TRUE)
  })
  
  output$dplyrOperationsGroup = renderUI({
    req(input$action1)
    df = filedata()
    items = names(df)
    if ("group" %in% input$dplyrOperations)
      selectInput("dplyrOperationsGroup", "Select the variable you would like to group by.", items, multiple=FALSE)
  })
  
  output$dplyrOperationsMutate = renderUI({
    req(input$action1)
    if ("mutate" %in% input$dplyrOperations)
      textInput("dplyrOperationsMutate", "Enter the expression(s) you would like to compute.", placeholder="Example: mean(age)")
  })
  
  output$dplyrOperationsArrange = renderUI({
    req(input$action1)
    df = filedata()
    items = names(df)
    if ("arrange" %in% input$dplyrOperations)
      selectInput("dplyrOperationsArrange", "Select the variable you would like to arrange by.", items, multiple=FALSE)
  })
  
  output$dplyrOperationsFilter = renderUI({
    req(input$action1)
    if ("filter" %in% input$dplyrOperations)
      textInput("dplyrOperationsFilter", "Enter the expression you would like to filter by.", placeholder="Example: age > 25")
  })
  
  
  # The main entry point to this script
  # Define the transformation that should be performed
  myList = list()
  instructions = function(selectVar=NULL, groupVar=NULL, arrangeVar=NULL, filterVar=NULL, mutateVar=NULL){  
    
    selectVarList = as.list(selectVar)
    groupVarList = as.list(groupVar)
    arrangeVarList = as.list(arrangeVar)
    filterVarList = as.list(filterVar)
    mutateVarList = as.list(mutateVar)
    
    myList = list(
      # limit to some variables
      list(transform='select', dots=selectVarList), #input$dplyrOperationsSelect
      # group by "vore"
      list(transform='group', dots=groupVarList),
      list(transform='mutate', dots=mutateVarList),
      list(transform='arrange', dots=arrangeVarList),
      list(transform='filter', dots=filterVarList))
    
    return(myList)
  }
  
  
  perform_transformation = function(instructions, given_df) {
    # Perform the specified transformation
    transformed_df = given_df
    # Loop over each entry in the instruction set
    for (entry in instructions) {
      
      # Pick the operation
      transform_fn = switch(entry$transform, 
                            arrange=arrange_,
                            filter=filter_,
                            select=select_,
                            group=group_by_,
                            summarise=summarise_,
                            mutate=mutate_,
                            distinct=distinct_)
      
      # Perform the operation
      transformed_df = transform_fn(transformed_df, .dots=setNames(entry$dots, entry$names))
    }
    return(transformed_df)
  }
  
  
  
  output$wrangled = renderTable({  
    req(input$action2)
    df = filedata()
    # Create instruction list based on user input
    myInstructions = instructions(selectVar = input$dplyrOperationsSelect,
                                  groupVar = input$dplyrOperationsGroup,
                                  arrangeVar = input$dplyrOperationsArrange,
                                  filterVar = input$dplyrOperationsFilter,
                                  mutateVar = input$dplyrOperationsMutate)
    # Wrangle the data
    perform_transformation(myInstructions, df)  }) 
  
  
}


shinyApp(ui, server)