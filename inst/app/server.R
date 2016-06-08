library(dropR)
# more NA
# -1, -9, 999
server <- function(input, output) {
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
  
# dynamic UI based on dataset #######
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
                selectize = FALSE,
                size = 15)
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
      if((is.null(input$file1) & !input$demo_ds )
         | is.null(input$cond_col) |
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

  output$xsq_slider <- renderUI({
    sliderInput('chisq_question',"Select question",
                1,length(input$quest_cols),1,1)
  })
  
  
  # output$debug_txt <- renderText({
  #   #class(data_4_plot())
  #   
  # })
  # 
  
  output$ctable <- renderTable({
    dataset()
  })
  
# reactive plot element ####
  output$do_curve_plot <- renderPlot({
    validate(
      need(data_procd(),"Please upload a dataset.
Make sure to hit 'update data!' in the upload tab.")
    )
    
    d <- data_procd()
    d <- d[d$condition %in% input$sel_cond,]
    d$condition <- droplevels(d$condition)
    
    if(input$rename_conditions != ""){
      str_by_comma <- unlist(strsplit(input$rename_conditions,","))
      if(length(str_by_comma) == length(levels(d$condition))){
        levels(d$condition) <- str_by_comma
      }
    } 

        do_curve <- ggplot(d)
    do_curve <- do_curve + geom_line(aes(x=id,y=pct_remain,
                                         col=condition),
                                     size = input$stroke_width) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"))
   
    # optional plot parameters 
    if(input$show_points){
      do_curve <- do_curve + geom_point(aes(x=id,y=pct_remain,
                                            col=condition),
                                        size = input$stroke_width*1.5)
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
    ggsave(paste0("curve_plot.",input$export_format),
           plot = do_curve, device = input$export_format,
           dpi = input$dpi,
           width = input$w,
           height = input$h)
    do_curve
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
  
  
# Preview data Table ####
  output$table <- DT::renderDataTable(DT::datatable({
    if(is.null(input$file1) & !input$demo_ds) return(NULL)
    else{
      dataset()
    }
  }
  ))
  
  kaplan_meier <- reactive({
    ds <- dataset()
    ds$drop_out <- extract_drop_out_from_df(ds,input$quest_cols)
    ds$surv <- with(ds,Surv(drop_out,drop_out != max(ds$drop_out)))
    if(input$kaplan_fit == "total"){
      fit1 <- survfit(surv~1,data = ds)
      steps <- getStepsByCond(fit1,"total")
      steps
    } else {
      by_cond <- split(ds,factor(ds[,input$cond_col]))
      by_cond_fit <- lapply(by_cond,
                            function(x) survfit(surv~1,data = x))
      
      by_cond_steps <- lapply(names(by_cond_fit),function(x){
        getStepsByCond(by_cond_fit[[x]],x)
      })
      
      steps <- do.call("rbind",by_cond_steps)
    }
    out <- list()
    out$steps <- steps
    out$ds <- ds
    out
    
  })
  
  output$rho_tests <- renderUI({
    if(input$kaplan_fit == "conditions"){
      selectInput("test_type","",list("rho = 0 (log-rank)" = 0 ,
                                      "rho = 1 (Peto & Peto Wilcox)" = 1))
    } else {
      p("Only available for two or more survival curves.")
    }
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
  
  output$kpm_plot <- renderPlot({
    validate(
      need(dataset(),"Please upload a dataset.
           Make sure to hit 'update data!' in the upload tab.")
      )
    k <- ggplot(kaplan_meier()$steps,aes(x,y,col=condition,fill = condition)) +
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
    ggsave(paste0("kpm_plot.",input$kpm_export_format),
           plot = k, device = input$kpm_export_format,
           dpi = input$kpm_dpi,
           width = input$kpm_w,
           height = input$kpm_h)
    k
  })
  
  output$test_table <- renderTable({
    kaplan_meier()$steps
  })
  
  output$test_text <- renderText({input$color_palette_kp})  
  
  
}
