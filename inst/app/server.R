library(dropR)
library(DT)
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
                          na.strings = c(input$nas, input$nas_custom1, input$nas_custom2, input$nas_custom3))
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
  
  observeEvent(input$goButton, {
    showNotification("Updated!", duration = 2)
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
    do_kpm(d = add_dropout_idx(dta, input$quest_cols),
           # q_pos = input$quest_cols,
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
    # selectInput('quest_cols', label = NULL, #'Hold down Shift key to select multiple.',
    #             selected = names(dataset())[c(3:length(names(dataset())))], # previously: -c(1,length(names(dataset())))
    #             multiple=TRUE,
    #             choices = names(dataset()),
    #             selectize = FALSE,
    #             size = 13)
    
    
    # checkboxGroupInput('quest_cols', label = NULL,
    #                    choices = names(dataset()),
    #                    selected = names(dataset())[c(3:length(names(dataset())))]
    #                    )
    
    pickerInput(
      inputId = "quest_cols",
      label = "Select items",
      choices = names(dataset()),
      selected = names(dataset())[c(3:length(names(dataset())))],
      multiple = T,
      options = list(`actions-box` = TRUE)
    )
    
    
    
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
      checkboxGroupInput('sel_cond', 'Selected conditions',
                         levels(stats()[,get("condition")]),
                         selected = levels(stats()[,get("condition")]))  
    }
  })
  
  
  output$kpm_conditions <- renderUI({
    if(input$kaplan_fit == "total"){
      NULL
    } else{
      
      # remove total from the list cause that's a separate thing
      # from the dropdown box... 
      cs <- levels(stats()[,get("condition")])
      cs <- cs[-match("total",cs)]
      
      checkboxGroupInput('sel_cond_kpm', 'Show conditions',
                         cs,
                         selected = levels(stats()[,get("condition")]))  
    }
  })
  
  output$chisq_conditions <- renderUI({
      # remove total from the list cause that's a separate thing
      # from the dropdown box... 
      cs <- levels(stats()[,get("condition")])
      cs <- cs[-match("total",cs)]
      
      checkboxGroupInput('sel_cond_chisq', 'Compute test for selected conditions',
                         cs,
                         selected = input$sel_cond)# levels(stats()[,get("condition")]))  
    
  })
  
  
  

  ## Chisq slider ############  
  output$xsq_slider <- renderUI({
    sliderInput('chisq_question', "Select item",
                1, length(input$quest_cols), # previously length - 1
                value = length(input$quest_cols), step = 1)
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
           Make sure to hit 'Update data!' in the upload tab.")
      )
    
   
    d <- as.data.frame(stats())
    # if(input$cutoff){
    #   last_q <- length(input$quest_cols)
    #   d <- subset(d, q_idx != last_q)
    # }
    
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
                  full_scale = input$full_scale,
                  color_palette = input$color_palette,
                  show_confbands = input$show_confbands)
  })


  output$do_curve_plot <- renderPlot({
    
    do_curve_plot()
    
    })
  
  output$do_curve_plot_2 <- renderPlot({do_curve_plot()})
  
  
  
  output$kpm_plot <- renderPlot({
    validate(
      need(dataset(),"Please upload a dataset.
           Make sure to hit 'update data!' in the upload tab.")
    )
    plot_do_kpm(kds = kaplan_meier(),
                sel_conds = input$sel_cond_kpm,
                kpm_ci = input$kpm_ci,
                color_palette_kp = input$color_palette_kp,
                full_scale_kpm = input$full_scale_kpm)

  })
  
# KS TAB #######
  ## KS slider ############  
  output$ks_slider <- renderUI({
    sliderInput('ks_question', "Select item",
                1, length(input$quest_cols),
                value = length(input$quest_cols),
                step = 1)
  })
  
  # KS plot
  output$ks_plot <- renderPlot({
    validate(
      need(stats(),"Please upload a dataset.
           Make sure to hit 'update data!' in the upload tab.")
    )
    
    if(input$ks_color_palette == "custom"){
      if(input$ks_color_manual1 == "" | input$ks_color_manual2 == ""){validate("Please provide two custom colors.")}
      ks_colorpal <- c(input$ks_color_manual1, input$ks_color_manual2)
    } else {ks_colorpal <- input$ks_color_palette}
      
    
    ksplot <- plot_do_ks(stats(),
               do_ks(stats(), input$ks_question),
               linetypes = input$ks_linetypes,
               show_confbands = input$ks_ci,
               color_palette = ks_colorpal)
    
    if(input$ks_ql){
      ksplot <- ksplot + ggplot2::geom_vline(xintercept= input$ks_question, color = "lightgray")
    }
    
    ksplot
    
  })
  
  output$ks_test <- renderPrint({
    do_ks(stats(), input$ks_question)
  })

  
# download handler plot ############  
  output$downloadCurvePlot <- downloadHandler(
    filename = function(){
      paste(input$plot_fname,input$export_format,sep=".")
    },
    content = function(file) {
      # New Try
      ggplot2::ggsave(file, 
                      plot = do_curve_plot(),
                      device = input$export_format,
                      dpi = input$dpi,
                      width = input$w,
                      height = input$h)
    }
  )
  
  
  output$downloadKpmPlot <- downloadHandler(
    filename = function(){
      paste(input$kpm_plot_fname,input$kpm_export_format,sep=".")
    },
    content = function(file) {
      # New Version
      ggplot2::ggsave(file,
                      plot = plot_do_kpm(kds = kaplan_meier(),
                                         sel_conds = input$sel_cond_kpm,
                                         kpm_ci = input$kpm_ci,
                                         color_palette_kp = input$color_palette_kp,
                                         full_scale_kpm = input$full_scale_kpm), 
                      device = input$kpm_export_format,
                      dpi = input$kpm_dpi,
                      width = input$kpm_w,
                      height = input$kpm_h)
    }
  )
  
  output$downloadKSplot <- downloadHandler(
    filename = function(){
      paste(input$ks_plot_fname,input$ks_export_format,sep=".")
    },
    content = function(file) {
      # New Version
      ggplot2::ggsave(file,
                      plot = plot_do_ks(stats(),
                                        do_ks(stats(), input$ks_question),
                                        linetypes = input$ks_linetypes,
                                        show_confbands = input$ks_ci,
                                        color_palette = input$ks_color_palette), 
                      device = input$ks_export_format,
                      dpi = input$ks_dpi,
                      width = input$ks_w,
                      height = input$ks_h)
    }
  )
  
  

####################################################
# TEXT AND TABLES ##################################  
####################################################  
  
  # Preview data Table ####
  # Should show full data if no item selection, otherwise show data of selected items
  output$table <- renderDataTable(datatable({
    if(is.null(input$file1) & !input$demo_ds) return(NULL)
    else{
      if(is.null(input$quest_cols)){
        dataset()
      } else{
        dataset()[, c(input$cond_col, input$quest_cols)]
      }
    }
  }
  ))
  
  output$cond_table <- renderTable({
       d <- as.data.frame(stats())
      d$condition <- factor(d$condition)
      
      d <- subset(d,condition != "total")
      test_input <- subset(d,q_idx == input$chisq_question)
      # #chisq.test(as.table(as.matrix(test_input)))
      # test_input
      test_input
 
  })

  output$chisq_tests <- renderPrint({
    d <- as.data.frame(stats())
    
    do_chisq(d,
             chisq_question = input$chisq_question,
             sel_cond_chisq = input$sel_cond_chisq,
             p_sim = input$p_sim)
 
  })
  
  output$odds_ratio <- renderTable({
    
    do_or_table(stats(), chisq_question = input$chisq_question, sel_cond_chisq = input$sel_cond_chisq)
  },
  rownames = TRUE)
  
  output$chisq_code <- renderPrint({
    paste0("do_chisq(d, chisq_question = ", paste(input$chisq_question), ", ",
           "sel_cond_chisq = ", paste0("c(", paste(input$sel_cond_chisq, collapse = ", "), ")"), ", ",
           "p_sim = ", paste(input$p_sim), ")"
    )
  })
  
  
  output$surv_tests <- renderPrint({
    if(input$kaplan_fit == "conditions"){
      kp_ds <- kaplan_meier()$d
      
      get_survdiff(kds = kp_ds, cond = input$cond_col,
                   test_type =  input$test_type)
      
    } else {
      "Only available for two or more survival curves."
    }
    
    
  })
  
  output$test_table <- renderTable({
    kaplan_meier()$steps
  })
  
  output$test_text <- renderText({input$color_palette_kp})  



output$debug_txt <- renderText({
  input$cond_col
})


output$ctable <- renderTable({
  stats()
})


}
