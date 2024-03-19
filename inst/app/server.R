library(dropR)
# more NA
# -1, -9, 999
server <- function(input, output) {
  
  # REACTIVES ##########################
  # uploaded file
  dataset <- reactive({
    if(input$demo_ds){
      data("dropRdemo")
      upfile <- dropRdemo
    } else {
      if(is.null(input$file1)) return(NULL)
      upfile <- read.csv2(input$file1$datapath,
                          sep = input$sep,
                          dec = input$dec,
                          quote = input$quote,
                          header = input$header,
                          na.strings = input$nas)  
    }
    upfile
  })
  
  # condition column reactive
  react_cond_col <- reactive({
    if(is.null(input$cond_col)){
      "total"
    } else {
      if(input$cond_col == "None"){
        "total"
      } else {
        input$sel_cond  
      }
    }
    
  })
  
  
  stats <- reactive({
    input$goButton
    isolate({
      if((is.null(input$file1) & !input$demo_ds )
         | is.null(input$cond_col) |
         is.null(input$quest_cols)){
        #data.frame(id = 0, pct_remain = 0, condition = "total")
        NULL
      } else {
        dta <- dataset()
        dta <- add_dropout_idx(dta, input$quest_cols)
        
        
        # compute stats
        stats <- compute_stats(dta,
                               by_cond = input$cond_col,
                               no_of_vars = length(input$quest_cols))
        stats
      }
    })
  })
  

  kaplan_meier <- reactive({
    dta <- dataset()
    do_kpm(d = dta,
           qs = input$quest_cols,
           condition_col = input$cond_col,
           model_fit = input$kaplan_fit)
  })

  
# DYNAMIC UI Parts #######
# Choose experimental condition ####
  output$choose_condition <- renderUI({    
    if(is.null(dataset())) return(NULL)
    
    selectInput('cond_col',
                'Experimental conditions',
                choices = c('None',names(dataset())),
                selected = 'experimental_condition',
                multiple = F
    )
  })
  
    ## Choose questions #########
  output$choose_questions <- renderUI({
    if(is.null(dataset())) return(NULL)
    selectInput('quest_cols', 'Select questions',
                selected = names(dataset())[-c(1,length(names(dataset())))],
                multiple=TRUE,
                choices = names(dataset()),
                selectize = FALSE,
                size = 15)
  })
  
  ## show conditions ############
  output$show_conditions <- renderUI({
    # w <- ""
    # for(i in 1:length(levels(data_procd()$condition))) {
    #   w <- paste(w, textInput(paste("a", i, sep = ""),
    #                           paste("a", i, sep = ""),
    #                           value = levels(data_procd()$condition)[i]))
    # }
    # HTML(w)
    if(input$cond_col == "None"){
      NULL
    } else{
      checkboxGroupInput('sel_cond', 'Show conditions',
                         levels(stats()[,get("condition")]),
                         selected = levels(stats()[,get("condition")]))  
    }
  })
  
  
  output$kpm_conditions <- renderUI({
    if(input$kaplan_fit == "total"){
      NULL
    } else{
      
      # remove total from the list cause that's a seperate thing
      # from the dropdown box... 
      cs <- levels(stats()[,get("condition")])
      cs <- cs[-match("total",cs)]
      
      checkboxGroupInput('sel_cond_kpm', 'Show conditions',
                         cs,
                         selected = levels(stats()[,get("condition")]))  
    }
  })
  
  output$chisq_conditions <- renderUI({
      # remove total from the list cause that's a seperate thing
      # from the dropdown box... 
      cs <- levels(stats()[,get("condition")])
      cs <- cs[-match("total",cs)]
      
      checkboxGroupInput('sel_cond_chisq', 'Compute test for selected conditions',
                         cs,
                         selected = levels(stats()[,get("condition")]))  
    
  })
  
  
  

  ## Chisq slider ############  
  output$xsq_slider <- renderUI({
    sliderInput('chisq_question',"Select question",
                1,length(input$quest_cols)-1,1,1)
  })
  
  ## Log RANK ###############
  output$rho_tests <- renderUI({
    if(input$kaplan_fit == "conditions"){
      selectInput("test_type","",list("rho = 0 (log-rank)" = 0 ,
                                      "rho = 1 (Peto & Peto Wilcox)" = 1))
    } else {
      p("Only available for two or more survival curves.")
    }
  })
  
# PLOTTING ####################
  
  # reactive plot element ####
  do_curve_plot <- reactive({
    validate(
      need(stats(),"Please upload a dataset.
           Make sure to hit 'update data!' in the upload tab.")
      )
    
   

    d <- as.data.frame(stats())
    if(input$cutoff){
      last_q <- length(input$quest_cols)
      d <- subset(d,do_idx != last_q)
    }
    
    d$condition <- factor(d$condition)
    d <- d[d$condition %in% react_cond_col(),]
    d$condition <- droplevels(d$condition)
    
    if(input$rename_conditions != ""){
      str_by_comma <- unlist(strsplit(input$rename_conditions,","))
      if(length(str_by_comma) == length(levels(d$condition))){
        levels(d$condition) <- str_by_comma
      }
    }
    
    plot_do_curve(d, linetypes = input$linetypes,
                  stroke_width = input$stroke_width,
                  show_points = input$show_points,
                  color_palette = input$color_palette)

  })


  output$do_curve_plot <- renderPlot({
    
    dc <- do_curve_plot()
    
    ggsave(paste0("curve_plot.",input$export_format),
           plot = dc, device = input$export_format,
           dpi = input$dpi,
           width = input$w,
           height = input$h)
    
    dc
    
    })
  
  output$do_curve_plot_2 <- renderPlot({do_curve_plot()})
  
  
  
  output$kpm_plot <- renderPlot({
    validate(
      need(dataset(),"Please upload a dataset.
           Make sure to hit 'update data!' in the upload tab.")
    )
    
    k <- do_kpm_plot(kds = kaplan_meier(),
                sel_cond_kpm = input$sel_cond_kpm,
                kpm_ci = input$kpm_ci,
                color_palette_kp = input$color_palette_kp,
                full_scale_kpm = input$full_scale_kpm)
    
    
    ggsave(paste0("kpm_plot.",input$kpm_export_format),
           plot = k, device = input$kpm_export_format,
           dpi = input$kpm_dpi,
           width = input$kpm_w,
           height = input$kpm_h)
    k
  })

  
# download handler plot ############  
  output$downloadCurvePlot <- downloadHandler(
    filename = function(){
      paste(input$plot_fname,input$export_format,sep=".")
    },
    content = function(file) {
      file.copy(paste("curve_plot",input$export_format,sep="."),
                file, overwrite=TRUE)
    }
  )
  
  
  output$downloadKpmPlot <- downloadHandler(
    filename = function(){
      paste(input$kpm_plot_fname,input$kpm_export_format,sep=".")
    },
    content = function(file) {
      file.copy(paste("kpm_plot",input$kpm_export_format,sep="."),
                file, overwrite=TRUE)
    }
  )
  
  

####################################################
# TEXT AND TABLES ##################################  
####################################################  
  
  # Preview data Table ####
  output$table <- DT::renderDataTable(DT::datatable({
    if(is.null(input$file1) & !input$demo_ds) return(NULL)
    else{
      dataset()
    }
  }
  ))
  
  output$cond_table <- renderTable({
       d <- as.data.frame(stats())
      d$condition <- factor(d$condition)
      
      d <- subset(d,condition != "total")
      test_input <- subset(d,do_idx == input$chisq_question)
      # #chisq.test(as.table(as.matrix(test_input)))
      # test_input
      test_input
 
  })

  output$chisq_tests <- renderPrint({
    d <- as.data.frame(stats())
    
    do_chisq(d,
             chisq_question = input$chisq_question,
             sel_cond_chisq = input$sel_cond_chisq,
             fisher = input$fisher)
  })
  
  output$odds_ratio <- renderTable({
    d <- as.data.frame(stats())
    d <- subset(d,condition %in% input$sel_cond_chisq)
    d$condition <- factor(d$condition)
    # d <- subset(d,condition != "total")
    
    test_input <- subset(d, do_idx == input$chisq_question)
    
    OR_matrix <- outer(test_input$pct_remain,
                       test_input$pct_remain,
                       FUN = get_odds_ratio)
    colnames(OR_matrix) <- test_input$condition
    row.names(OR_matrix) <- test_input$condition
    OR_matrix
  })
  
  
  output$surv_tests <- renderPrint({
    if(input$kaplan_fit == "conditions"){
      kp_ds <- kaplan_meier()$d
      
      get_survdiff(d = kp_ds, cond = input$cond_col,
                   test_type =  input$test_type)
      
    } else {
      "Only available for two or more survival curves."
    }
    
    
  })
  output$test_table <- renderTable({
    kaplan_meier()$steps
  })
  
  output$test_text <- renderText({input$color_palette_kp})  






# TEST ##########
output$debug_txt <- renderText({
  input$cond_col
})


output$ctable <- renderTable({
  stats()
})


}
