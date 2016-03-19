source("R/extract_drop_out_from_df.R")
source("R/computeRemaining.R")
source("R/find_drop_out.R")
source("R/utils.R")
library(ggplot2)

server <- function(input, output) {
  # uploaded file
  dataset <- reactive({
    if(is.null(input$file1)) return(NULL)
    upfile <- read.csv2(input$file1$datapath,
                        sep = input$sep,
                        dec = input$dec,
                        quote = input$quote,
                        header = input$header,
                        na.strings = input$nas)
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
  
# Choose questions #########
  output$choose_questions <- renderUI({
    if(is.null(dataset())) return(NULL)
    selectInput('quest_cols', 'Select questions',
                selected = names(dataset())[-c(1,length(names(dataset())))],
                multiple=TRUE,
                choices = names(dataset()),
                selectize = FALSE)
  })
  
# show conditions ############
  output$show_conditions <- renderUI({
    # w <- ""
    # for(i in 1:length(levels(data_procd()$condition))) {
    #   w <- paste(w, textInput(paste("a", i, sep = ""),
    #                           paste("a", i, sep = ""),
    #                           value = levels(data_procd()$condition)[i]))
    # }
    # HTML(w)
    
    checkboxGroupInput('sel_cond', 'Show conditions',
                       levels(data_procd()$condition),
                       selected = levels(data_procd()$condition))
  })
  
  
# data transformation and computation ####
  data_procd <- reactive({
    input$goButton
    isolate({
      if(is.null(input$file1) | is.null(input$cond_col) |
         is.null(input$quest_cols)){
        #data.frame(id = 0, pct_remain = 0, condition = "total")
        NULL
      } else {
        # compute dropout
        data_in <- dataset()
        data_in$drop_out <- extract_drop_out_from_df(data_in,input$quest_cols)
        # number of questions columns
        n_q <- length(input$quest_cols)
        # total share
        total_dropout <- computeRemaining(data_in,n_q)
        tdo_df <- data.frame(condition = "total",
                             pct_remain = total_dropout$remain_pct,
                             id = total_dropout$id)
        # if no condition is column is specified we can only show total
        if(input$cond_col != "None"){
          # reformat the total dropout data.frame to rbind it with the grouped df
          dropout_by_grp <- computeRemaining(data_in,n_q,input$cond_col)
          dropout_by_grp_full <- rbind(tdo_df,dropout_by_grp)  
          dropout_by_grp_full
        } else {
          tdo_df
        }
      }      
    })

  })

  # output$debug_txt <- renderText({
  #   #class(data_4_plot())
  #   
  # })
  # 
# reactive plot element ####
  output$do_curve_plot <- renderPlot({
    validate(
      need(data_procd(),"Please upload a dataset.
Make sure to hit 'update data!' in the upload tab.")
    )
    
    d <- data_procd()
    d <- d[d$condition %in% input$sel_cond,]
    do_curve <- ggplot(d)
    do_curve <- do_curve + geom_line(aes(x=id,y=pct_remain,
                                         col=condition),
                                     size = input$stroke_width) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank())
   
    # optional plot parameters 
    if(input$show_points){
      do_curve <- do_curve + geom_point(aes(x=id,y=pct_remain,
                                            col=condition),
                                        size = input$stroke_width*1.5)
    }
    
    if(input$color_palette == "color_blind"){
      do_curve <- do_curve + scale_color_manual(values=c("#000000", "#E69F00",
                                                         "#56B4E9", "#009E73",
                                                         "#F0E442", "#0072B2",
                                                         "#D55E00", "#CC79A7"))
    }
    
    if(input$color_palette == "gray"){
      do_curve <- do_curve + 
        scale_color_manual(values = gray(seq(from=0,1,
                                             by=1/8)[c(1,8,3,7,4,5,2,6)]
                                         )
        )
    }
    
    do_curve
  })
  

# Preview data Table ####
  output$table <- DT::renderDataTable(DT::datatable({
    if(is.null(input$file1)) return(NULL)
    else{
      dataset()
    }
  }
  ))
}
