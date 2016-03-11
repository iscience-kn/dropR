server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    if(is.null(input$file1)) return(NULL)
    else{
      upfile <- read.csv2(input$file1$datapath,
                          sep = input$sep,
                          dec = input$dec,
                          quote = input$quote,
                          header = input$header)
      
      upfile  
    }
  }
  ))
}
