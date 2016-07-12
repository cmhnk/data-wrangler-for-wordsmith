library(shiny)
library(dplyr)    # For the data wrangling
library(lazyeval) 
library(shinyURL) # to save state

ui <- fluidPage(
  headerPanel("Data Wrangler for Wordsmith"),
  
  sidebarPanel(
    "Instructions: Upload your CSV file, then use this tool to format your data for Wordsmith.",
    hr(),
    fileInput(inputId = "datafile", label= "Choose CSV file", 
              accept='.csv'),
    uiOutput("missing"), br(),
    uiOutput("missingvalues"), br(),

    br(), br(), br(), 
    
    hr(), 
    "Choose wrangling options below.", br(), br(),
    uiOutput("dplyrOperations"),
    uiOutput("dplyrOperationsSelect"),
    uiOutput("dplyrOperationsGroup"),
    textOutput("dplyrOperationsMutateLabel"),
    uiOutput("dplyrOperationsMutate"), 
    uiOutput("dplyrOperationsArrange"),
    uiOutput("dplyrOperationsFilter"),
    br(), br(), br(),
    shinyURL.ui(),
    actionButton("save", "Save Wrangled Data")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("data", 
      		   textOutput("filedim"), 
               textOutput("filehead"), br(), br(),
               tableOutput("filetable")),
      tabPanel("wrangled", 
              textOutput("myInstructions"),
              tableOutput("wrangled"))
        )
    )
)  




server <- function(input, output, server){
  require(shiny)
  require(shinyURL)
  require(psych)
  require(dplyr)
  require(lazyeval)
  
  
  #Load in the selected file
  filedata = reactive({ 
  	req(input$datafile)
    infile = input$datafile
    read.csv(infile$datapath, na.strings=input$missingvalues, stringsAsFactors=FALSE)
    })
  
  output$missing = renderUI({
  	radioButtons("missing", "Are there missing value codes present in your dataset?", 
  				 choices=c("Yes", "No"), selected="No") 
  	})
  
  output$missingvalues = renderUI({
    if (req(input$missing)=="Yes"){
    textInput(inputId = "missingvalues", 
              label="Enter any missing value codes that are present in your data (ex. -999, ., NA).
              If multiple missing value codes are present, separate them with commas.",
              value="")
    }
  })
  
  #The file you uploaded has X rows and Y columns 
  output$filedim = renderText({    
      df = filedata()
      paste("The file you uploaded has", dim(df)[1], "rows and", dim(df)[2], "columns.")
  })
  
  
  output$filehead <- renderText({ 
    paste("Below is a preview of your dataset.")
  })
  
  #Preview the CSV data file
  output$filetable <- renderTable({ 
    filedata()
  })
  
  
  # Data wrangling options all stored as output$dplyrXXX
  output$dplyrOperations = renderUI({
    items = c("select", "group", "mutate", "filter", "arrange")
    selectInput("dplyrOperations", "Select all of the data wrangling techniques you would like performed.", items, multiple=TRUE)
  })
  
  output$dplyrOperationsSelect = renderUI({
    df = filedata()
    items = names(df)
    if ("select" %in% input$dplyrOperations)
      selectInput("dplyrOperationsSelect", "Select the variables you would like in your wrangled dataset.", items, multiple=TRUE)
  })
  
  output$dplyrOperationsGroup = renderUI({
    df = filedata()
    items = names(df)
    if ("group" %in% input$dplyrOperations)
      selectInput("dplyrOperationsGroup", "Select the variable you would like to group by.", items, multiple=FALSE)
  })
  
  output$dplyrOperationsMutateLabel = renderText({
    if ("mutate" %in% input$dplyrOperations)
    paste("Enter the expression(s) you would like to have appended to your dataset as additional columns. If multiple expressions are entered, they must be separated by an exclamation point (!).")
  })
  
  output$dplyrOperationsMutate = renderUI({
    if ("mutate" %in% input$dplyrOperations)
    tags$textarea(id="dplyrOperationsMutate", rows=5, cols=40, placeholder="Example: rank(age) ! mean(age) ! age - mean(age)")
  })
  
  output$dplyrOperationsArrange = renderUI({
    df = filedata()
    items = names(df)
    if ("arrange" %in% input$dplyrOperations)
      selectInput("dplyrOperationsArrange", "Select the variable you would like to arrange by.", items, multiple=FALSE)
  })
  
  output$dplyrOperationsFilter = renderUI({
    if ("filter" %in% input$dplyrOperations)
      textInput("dplyrOperationsFilter", "Enter the expression you would like to filter by.", placeholder="Example: age > 25")
  })
  
  
  # The main entry point to this script
  # Define the transformation that should be performed
  myList = list()
  instructions = function(selectVar=NULL, groupVar=NULL, arrangeVar=NULL, filterVar=NULL, mutateVar=NULL){  
    
    
    # myListMelt() will take a list with 1 character vector of length n and convert it to 
	# a list of n elements each with 1 character vector of length 1 

    myListMelt = function(thelist){
	total = length(thelist[[1]])
	newlist = list()
	for (i in 1:total){
		newlist[[i]] = thelist[[1]][i]
	}
	return(newlist)
}

	
	# Mutate has different input than other dplyr operations (textInput), so we need to use strsplit 
	myDelimiter = "!"
	mutateVar2 = strsplit(mutateVar, myDelimiter)


    selectVarList = as.list(selectVar)
    groupVarList = as.list(groupVar)
    arrangeVarList = as.list(arrangeVar)
    filterVarList = as.list(filterVar)
    mutateVarList = myListMelt(mutateVar2)
    
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
  
  # Create the reactive expression 
   dplyrReactive = reactive({  
   	# req() 
    df = filedata()
    # Create instruction list based on user input
    myInstructions = instructions(selectVar = input$dplyrOperationsSelect,
                                  groupVar = input$dplyrOperationsGroup,
                                  arrangeVar = input$dplyrOperationsArrange,
                                  filterVar = input$dplyrOperationsFilter,
                                  mutateVar = input$dplyrOperationsMutate)
    # Wrangle the data
    perform_transformation(myInstructions, df)  }) 
  
  # Display results 
  output$wrangled = renderTable({ dplyrReactive() })
  
  shinyURL.server()
  
  observeEvent(input$save,
  	{ write.csv(dplyrReactive(), "wrangled_data.csv", row.names=FALSE)
  	})
  
}


shinyApp(ui, server)
  