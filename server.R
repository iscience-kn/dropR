source("R/extract_drop_out_from_df.R")
source("R/computeRemaining.R")
source("R/find_drop_out.R")
source("R/utils.R")

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
    checkboxGroupInput('sel_cond', 'conditions to show',
                       levels(data_procd()$condition),
                       selected = levels(data_procd()$condition))
  })
  
  
# data transformation and computation ####
  data_procd <- reactive({
    input$goButton
    isolate({
      if(is.null(input$file1) | is.null(input$cond_col) |
         is.null(input$quest_cols)){
        data.frame(id = 0, pct_remain = 0, condition = "total")
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
  #   class(input$cond_col)
  # })
  
# reactive plot element ####
input_strokeW <- reactive({input$strokeW})

gv <- reactive({
  d <- data_procd()
  d <- d[d$condition %in% input$sel_cond,]
  d %>% ggvis() %>%
    group_by(condition) %>%
    layer_paths(~id,~pct_remain,stroke=~condition,
                strokeWidth := input_strokeW) %>%
    add_axis("x", title = "question")
})

gv %>% bind_shiny("dropout_curve","plot_ui")

# Preview data Table ####
  output$table <- DT::renderDataTable(DT::datatable({
    if(is.null(input$file1)) return(NULL)
    else{
      dataset()
    }
  }
  ))
}
