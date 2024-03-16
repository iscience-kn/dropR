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
        dta$drop_out_idx <- extract_drop_out_from_df(dta,input$quest_cols)
        
        
        # compute stats
        

        stats <- compute_stats(dta,
                                   by_cond = input$cond_col,
                                   do_indicator = "drop_out_idx",
                                   no_of_vars = length(input$quest_cols))
        stats
      }
    })
  })
  

  kaplan_meier <- reactive({
    ds <- dataset()
    ds$drop_out <- extract_drop_out_from_df(ds,input$quest_cols)
    ds$surv <- with(ds,Surv(drop_out,drop_out != max(ds$drop_out)))
    if(input$kaplan_fit == "total"){
      fit1 <- survfit(surv~1,data = ds)
      steps <- get_steps_by_cond(fit1,"total")
      steps
    } else {
      by_cond <- split(ds,factor(ds[,input$cond_col]))
      by_cond_fit <- lapply(by_cond,
                            function(x) survfit(surv~1,data = x))
      
      by_cond_steps <- lapply(names(by_cond_fit),function(x){
        get_steps_by_cond(by_cond_fit[[x]],x)
      })
      
      steps <- do.call("rbind",by_cond_steps)
    }
    out <- list()
    out$steps <- steps
    out$ds <- ds
    out
    
  })

  
# DYNAMIC UI Parts #######
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
      d <- subset(d,drop_out_idx != last_q)
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
    do_curve <- ggplot(d)
    
    if(input$linetypes){
      do_curve <- do_curve + geom_line(aes(x=drop_out_idx,
                                           y=(pct_remain)*100,
                                           col = factor(condition),
                                           linetype = factor(condition)),
                                       size = as.numeric(input$stroke_width))
    } else {
      do_curve <- do_curve + geom_line(aes(x=drop_out_idx,
                                           y=(pct_remain)*100,
                                           col = factor(condition)),
                                       size = as.numeric(input$stroke_width))  
    }
    do_curve <- do_curve + 
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black")) + 
      xlab("Dropout Index") +
      ylab("Percent Remaining")
      

    # optional plot parameters 
    if(input$full_scale){
      do_curve <- do_curve + 
        scale_y_continuous(limits = c(0,100))
    }
    
    
    if(input$show_points){
      do_curve <- do_curve + geom_point(aes(x=drop_out_idx,
                                            y=(pct_remain)*100,
                                            col=condition),
                                        size = as.numeric(input$stroke_width)*1.5)
    }
    
    if(input$color_palette == "color_blind" & length(levels(d$condition) < 9)){
      do_curve <- do_curve + scale_color_manual(values=c("#000000", "#E69F00",
                                                         "#56B4E9", "#009E73",
                                                         "#F0E442", "#0072B2",
                                                         "#D55E00", "#CC79A7"))
    }
    
    if(input$color_palette == "gray" & length(levels(d$condition) < 9)){
      do_curve <- do_curve + 
        scale_color_manual(values = gray(seq(from=0,1,
                                             by=1/8)[c(1,8,3,7,4,5,2,6)]
        )
        )
    }
   
    do_curve <- do_curve + guides(color = guide_legend(title = NULL), linetype = guide_legend(NULL))
    
    do_curve
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
    
    if(input$kaplan_fit == "conditions"){
      k <- ggplot(subset(kaplan_meier()$steps, condition %in% input$sel_cond_kpm),
                  aes(x,y*100,col=condition,fill = condition))
    } else {
      k <- ggplot(subset(kaplan_meier()$steps, condition %in% "total"),
                  aes(x,y*100,col=condition,fill = condition))
    }
    
    k <- k + 
      geom_line() +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"))
    if(input$kpm_ci){
      k <- k + geom_ribbon(aes(ymin = lwr, ymax = uppr,
                               linetype=NA), alpha=.3)
    }

    if(input$color_palette_kp == "color_blind"){
      k <- k + scale_fill_manual(values=c("#000000", "#E69F00",
                                          "#56B4E9", "#009E73",
                                          "#F0E442", "#0072B2",
                                          "#D55E00", "#CC79A7")) +
        scale_color_manual(values=c("#000000", "#E69F00",
                                    "#56B4E9", "#009E73",
                                    "#F0E442", "#0072B2",
                                    "#D55E00", "#CC79A7"))
    }

    if(input$color_palette_kp == "gray"){
      k <- k +
        scale_color_manual(values = gray(seq(from=0,1,
                                             by=1/8)[c(1,8,3,7,4,5,2,6)]
        )) +
        scale_fill_manual(values = gray(seq(from=0,1,
                                            by=1/8)[c(1,8,3,7,4,5,2,6)])
        )
    }
    
    if(input$full_scale_kpm){
      k <- k + 
        scale_y_continuous(limits = c(0,100))
    }
    
    k <- k + guides(color = guide_legend(title = NULL),
                    fill = guide_legend(title = NULL)) +
      xlab("Dropout Index") + 
      ylab("Percent Remaining")
    
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
      test_input <- subset(d,drop_out_idx == input$chisq_question)
      # #chisq.test(as.table(as.matrix(test_input)))
      # test_input
      test_input
 
  })

  output$chisq_tests <- renderPrint({
    d <- as.data.frame(stats())
    
    d <- subset(d,condition %in% input$sel_cond_chisq)
    d$condition <- factor(d$condition)
    
    # d <- subset(d,condition != "total")
    
    test_input <- subset(d,drop_out_idx == input$chisq_question)
    test_table <- as.table(as.matrix(test_input[,c("cs","remain")]))
    dimnames(test_table) <- list(conditions = test_input$condition,
                                 participants = c("dropout","remaining"))
    # chisq.test(as.table(as.matrix(test_input[,c("condition","cs","remain")])))
    test_result <- chisq.test(test_table,simulate.p.value = input$fisher)
    lname2 <- sprintf("Dropout at question %s",input$chisq_question)
    li <- list("Test result" = test_result,
         lname2 = test_table)
    names(li)[2] <- lname2
    li
  })
  
  output$odds_ratio <- renderTable({
    d <- as.data.frame(stats())
    d <- subset(d,condition %in% input$sel_cond_chisq)
    d$condition <- factor(d$condition)
    # d <- subset(d,condition != "total")
    
    test_input <- subset(d,drop_out_idx == input$chisq_question)
    
    OR_matrix <- outer(test_input$pct_remain,
                       test_input$pct_remain,
                       FUN = get_odds_ratio)
    colnames(OR_matrix) <- test_input$condition
    row.names(OR_matrix) <- test_input$condition
    OR_matrix
  })
  
  
  output$surv_tests <- renderPrint({
    if(input$kaplan_fit == "conditions"){
      kp_ds <- kaplan_meier()$ds
      f <- as.formula(paste("surv",input$cond_col,sep="~"))
      survdiff(f,data = kp_ds,rho = input$test_type)  
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
