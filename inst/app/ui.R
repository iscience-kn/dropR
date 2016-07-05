## app.R ##
library(shiny)
library(shinydashboard)

# Tab items 
# Home ####
tabHome <- tabItem(tabName = "home",
                   div(align="center",
                       img(src="decrease.svg",width=120),
                       h1("dropR"),
                       h2("About dropout analysis using R"),
                       p("Some text about the GUI and the underlying R
                     package.")
                       )
                   # ,textOutput("debug_txt")
                   )

# Visual Analysis ####
tabViz <- tabItem(tabName = "viz",
                  fluidRow(
                    box(width=4,
                        h3("Plot options"),
                        uiOutput("show_conditions"),
                        textInput("rename_conditions","Rename selected conditions"),
                        selectInput("stroke_width","Stroke width",c(1,2,3,4,5),1),
                        checkboxInput("show_points","Show points"),
                        checkboxInput("linetypes","Use line type to distinguish conditions",value = T),
                        checkboxInput("cutoff","cut off last question",value = T),
                        checkboxInput("full_scale","Show full Y-axis from 0 to 100",value = T),
                        radioButtons("color_palette","Color palettes",
                                     c("color blind-friendly" = "color_blind",
                                       "ggplot default" = "default",
                                       "gray scale" = "gray"),
                                     "color_blind"),
                        h3("Hints"),
                        p("- color blind and printer friendly palettes support up to 8 different categories (colors)."),
                        p("- When re-labelling condtions, use comma (,) as a seperator. Make sure to list as many names as conditions selected."),
                        p("- With dropR you can produce graphs for publication in various formats. You may choose from vector formats such as .svg and .pdf and the .png format for rendered pixels. While size is relevant to any format, resolution only applies to .png and will be ignored when vector formats are chosen.")),
                    box(width = 8,
                        h3("Dropout by question"),
                        div(plotOutput("do_curve_plot"),
                            style = 'overflow:auto'),
                        textInput("plot_fname","file name (w/o file extension)",
                                  width=240,
                                  value = paste0("dropR_",round(as.numeric(Sys.time())))
                                  ),
                        selectInput("export_format","export graph as:",
                                    c("pdf" = "pdf",
                                      "svg" = "svg",
                                      "png" = "png"),
                                    "pdf",width=240),
                        sliderInput("dpi","resolution (dpi, .png only)",
                                    min = 75, max = 600,value = 300,
                                    width = 240),
                        sliderInput("h","height (in inches)",
                                    min = 3, max = 50,value = 4,
                                    width = 240),
                        sliderInput("w","width (in inches)",
                                    min = 3, max = 50,value = 10,
                                    width = 240),
                        downloadButton('downloadCurvePlot', 'download plot')
                        )
                    
                    
                    
                    )
           
                  
                  )
# Upload ####
tabUpload <- tabItem(tabName = "upload",
                      h2("Upload your data"),
                     fluidRow(
                       box(width=6,
                         h3("1 Choose"),
                         h4("a .csv file from your disk"),
                         tags$ul(
                           tags$li("Indicate whether the first line of your
                                   data is meant to be a header."),
                           tags$li("Choose the proper column delimiter,
                                   text quote and missing value coding for your file.
                                   Note that, proper missing values (empty fields) are 
                                   accepted in addition by default."),
                           tags$li("Make sure to use reasonable coding for
                                   missing values (i.e. empty cells). Avoid
                                   ambigous coding such as -99, -999 etc."),
                           tags$li("Check the preview window below. Iff your
                                   data is displayed as expected you are good to 
                                   start with your analysis. DON'T forget to 
                                   hit 'update data' in the right box when
                                   you're ready.")
                         ),
                         fileInput('file1', '',
                                     accept=c('text/csv', 
                                              'text/comma-separated-values,text/plain', 
                                              '.csv')),
                         h4("or use a demo dataset instead"),
                         checkboxInput("demo_ds","use demo data",value = F)
                         ),
                       box(width=3,
                         h3("2 Specify"),
                         h4(".csv properties"),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Semicolon=';',
                                        Comma=',',
                                        Tab='\t'),
                                      ';'),
                         radioButtons('quote', 'Text quote',
                                      c(None='',
                                        'Double quote'='"',
                                        'Single quote'="'"),
                                      '"'),
                         checkboxGroupInput('nas','Interpret as missing:',
                                            c('-99','-999','-9999','-1',
                                              '999','9',
                                              '#N/A','NA','.'),c("NA"))
                       ),
                       box(width=3,
                         h3("3 Identify"),
                         h4("questions and conditions"),
                         uiOutput('choose_condition'),
                         uiOutput("choose_questions"),
                         actionButton("goButton", "update data!")
                       )
                     ),
                     fluidRow(
                       box(width = 12,
                         h3("Data Preview"),
                        div(DT::dataTableOutput("table"),style = 'overflow:auto')
                       )
                     )
                     # fluidRow(
                     #   box(width = 12),
                     #   textOutput("debug_txt")
                     # )
                     )

# Tab Chisq ####
tabXsq <- tabItem(tabName = "xsq",
                  fluidRow(
                    box(width=5,
                        HTML("<h3>χ<sup>2</sup>-test options</h3>"),
                        uiOutput("xsq_slider"),
                        checkboxInput("fisher","Simulate p-values",T)
                    ),
                    box(width=7,
                        h3("Test outcomes"),
                        verbatimTextOutput("chisq_tests"),
                        h3("Odds ratio by item"),
                        tableOutput("odds_ratio"),
                        div(plotOutput("do_curve_plot_2"),
                            style = 'overflow:auto')
                        )
                  )
                  )


# Tab Kaplan-Meier ####
tabKaplan <- tabItem(tabName = "kaplan",
                     h2("Kaplan-Meier Estimation"),
                     fluidRow(
                       box(width = 3,
                           h3("Model specifications"),
                           selectInput("kaplan_fit","Choose model fit",
                                       c("total" = "total",
                                         "by condition" = "conditions"),
                                       "total"),
                           h3("Plot options"),
                           checkboxInput("kpm_ci","confidence bands",T),
                           radioButtons("color_palette_kp","Color palettes",
                                        c("ggplot default" = "default",
                                          "color blind-friendly" = "color_blind",
                                          "gray scale" = "gray"),
                                        "default"),
                           h3("Rho family tests"),
                           uiOutput("rho_tests"),
                           h3("Export plot"),
                           textInput("kpm_plot_fname","file name (w/o file extension)",
                                     width=240,
                                     value = paste0("dropR_",round(as.numeric(Sys.time())))
                           ),
                           selectInput("kpm_export_format","export graph as:",
                                       c("pdf" = "pdf",
                                         "svg" = "svg",
                                         "png" = "png"),
                                       "pdf",width=240),
                           sliderInput("kpm_dpi","resolution (dpi, .png only)",
                                       min = 75, max = 600,value = 300,
                                       width = 240),
                           sliderInput("kpm_h","height (in inches)",
                                       min = 3, max = 50,value = 4,
                                       width = 240),
                           sliderInput("kpm_w","width (in inches)",
                                       min = 3, max = 50,value = 4,
                                       width = 240),
                           downloadButton('downloadKpmPlot', 'download plot')
                           # Exports
                           ),
                       box(width = 9,
                           h3("Kaplan-Meier survival curve"),
                           #tableOutput("test_table"),
                           plotOutput("kpm_plot"),
                           h3("Test Survival Curve Differences"),
                           verbatimTextOutput("surv_tests")
                           #textOutput("test_text")
                           
                           )
                       )
                     )

# Main Page structure ####
ui <- dashboardPage(
  dashboardHeader(title = "dropR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Visual inspection", tabName = "viz",
               icon = icon("area-chart",lib="font-awesome")),
      menuItem("Chi-square", tabName = "xsq",
               icon = icon("percent",lib="font-awesome")),
      menuItem("Kaplan-Meier est.", tabName = "kaplan",
               icon = icon("percent",lib="font-awesome")),
      menuItem("About", tabName = "about")
    )
  ),
  # Body of the App #############
  dashboardBody(
    tabItems(tabHome,
             tabViz,
             tabUpload,
             tabXsq,
             tabKaplan)
  )
)

