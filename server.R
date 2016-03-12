server <- function(input, output) {
  # uploaded file
  dataset <- reactive({
    if(is.null(input$file1)) return(NULL)
    upfile <- read.csv2(input$file1$datapath,
                        sep = input$sep,
                        dec = input$dec,
                        quote = input$quote,
                        header = input$header)
    upfile
  })
  
  # dynamic UI based on dataset
  # Choose experimental condition ####
  output$choose_condition <- renderUI({    
    if(is.null(dataset())) return(NULL)
    
    selectInput('cond_col',
                'Experimental conditions',
                choices = c('None',names(dataset())),
                selected = 'None',
                multiple = F
    )
  })
  
  # Choose questions ####
  output$choose_questions <- renderUI({
    if(is.null(dataset())) return(NULL)
    selectInput('quest_cols', 'Select questions',
                selected = names(dataset())[-c(1,length(names(dataset())))],
                multiple=TRUE,
                choices = names(dataset()),
                selectize = FALSE)
  })
  
  
  
  
  
  
  
  
  # generate UI parts that depend on data or computations
  # UI Fileupload: choose condition ####
  # output$choose_condition <- renderUI({    
  #   if(is.null(dataset()$nms)) return(NULL)
  #   selectInput('cond_col',
  #               'Choose the column that holds the experimental conditions',
  #               choices = c('None',dataset()$nms),
  #               selected = 'None',
  #               multiple = F
  #   )
  # })
  
  set.seed(122)
  histdata <- rnorm(500)

  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    if(is.null(input$file1)) return(NULL)
    else{
      dataset()
    }
  }
  ))
}
