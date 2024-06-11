## app.R ##
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(svglite)

# Tab items 
# Home ####
tabHome <- tabItem(tabName = "home",
                   tags$style(HTML("
                                .slimtext {
                                  width: 60%;
                                  word-break: break-word;
                                }
                                
                                .extraslimtext {
                                  width: 35%;
                                  word-break: break-word;
                                }
                                
                                .btn {height: auto; width: 85%; vertical-align: center;}
                                ")
                   ),
                   
                   div(align="center",
                       img(src="decrease.svg",width=120),
                       h2("dropR"),
                       p("dropR analyzes data from Internet-based 
                       experiments for", strong("differences in dropout between conditions."), 
                         class="slimtext"),
                       p("Currently, dropR supports visual 
                         inspection of dropout, Odds ratio by item, Chi 
                         Square tests of differences at any particular 
                         item (e.g. test for overall dropout difference), 
                         Kaplan-Maier survival estimation, Rho family 
                         tests.", class="slimtext"),
                       p("dropR follows a simple step-by-step process which starts in the Upload tab.", br(),
                         "1. Upload your data under 'Upload your 
                         data' or choose our demo file.", br(),
                         "2. Specify some datafile characteristics, such as csv separator or NA coding.", br(),
                         "3. Identify the experimental condition variable, i.e. 'experimental_condition' in demo data 
                         or something similar in your data and select all experimental variables for which to analyze dropout.", br(),
                         "Make sure to click 'update data!' to get started on analyses 
                         and visualization.", class="extraslimtext"),
                       p("To read more about dropout as a relevant dependent variable in analysis of internet-based
                         experiments, we recommend",
                         a("this paper by Reips (2002)", href = "https://www.uni-konstanz.de/iscience/reips/pubs/papers/Reips2002.pdf"), 
                         "as a starting point.", class="slimtext")
                       )
                   # ,textOutput("debug_txt")
                   )
# Upload ####
tabUpload <- tabItem(tabName = "upload",
                     # h2("Upload your data"),
                     fluidRow(
                       box(width=5,
                           title = "1. Choose",
                           # h3("1. Choose"),
                           # h5("a .csv file from your disk"),
                           # strong('How To dropR'),
                           tags$ul(
                             tags$li(strong("... a .csv file from your disk or our demo dataset."), 
                                     "(52 variables, 4 experimental conditions)"),
                             tags$li("Indicate whether the first line of your
                                   data is a header."),
                             tags$li("Choose the proper column delimiter,
                                   text quote and missing value coding for your file.
                                   Note that empty cells are recognized as 
                                   missing values by default."),
                             tags$li("Make sure to use reasonable coding for
                                   missing values in your data. Add custom missing values
                                   if necessary."),
                             tags$li("Check the data preview below. If your
                                   data is displayed as expected you are ready to 
                                   start with your analyses.")
                           ),
                           p("DON'T forget to hit 'update data' in the box on the right when 
                             you're ready!"),
                           fileInput('file1', '',
                                     accept=c('text/csv', 
                                              'text/comma-separated-values,text/plain', 
                                              '.csv')),
                           # h4("or use the default demo dataset (52 variables, 4 experimental conditions) instead"),
                           checkboxInput("demo_ds",strong("Use demo data"), value = TRUE)
                       ),
                       box(width=2,
                           title = "2. Specify",
                           # h3("2. Specify"),
                           h4("CSV properties:"),
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
                                               '"')
                          
                       ),
                       box(width=2,
                           title = "2.1 Missings",
                           # h3("2.1 Missings"),
                           checkboxGroupInput('nas','Interpret as missing:',
                                              c('NA', '#N/A','.', '-99','-999',
                                                '-1',''),c("NA", "")),
                           p(strong('Add your own NA coding (automatically applied):')),
                           textInput('nas_custom1', label = NULL,
                                     value = "-66", width = '30%'),
                           textInput('nas_custom2', label = NULL,
                                     value = "-77", width = '30%'),
                           textInput('nas_custom3', label = NULL,
                                     value = "-9999", width = '30%')
                       ),
                       box(width=3,
                           title = "3. Identify",
                           # h3("3. Identify"),
                           # h5("conditions and questions to analyze"),
                           uiOutput('choose_condition'),
                           uiOutput("choose_questions"),
                           actionButton("goButton", "Update data!")
                       )
                     ),
                     fluidRow(
                       box(width = 12,
                           title = "Data Preview",
                           # h3("Data Preview"),
                           div(DT::dataTableOutput("table"),style = 'overflow:auto')
                       )
                     )
                  )

# Visual Analysis ####
tabViz <- tabItem(tabName = "viz",
                  fluidRow(
                    box(width=4,
                        title = "Data Visualization",
                        column(width = 3,
                               uiOutput("show_conditions")
                               ),
                        
                        column(width = 5,
                               p(strong("Options")),
                               checkboxInput("show_points","Show points"),
                               checkboxInput("show_confbands","Show confidence bands"),
                               checkboxInput("linetypes","Use line type to distinguish conditions",
                                             value = T),
                               checkboxInput("cutoff","Cut off last question",value = T),
                               checkboxInput("full_scale","Show full Y-axis (0 to 100)",value = F)
                               ),
                        
                        column(width = 4,
                               radioButtons("color_palette","Color palettes",
                                            c("color blind-friendly" = "color_blind",
                                              "ggplot default" = "default",
                                              "gray scale" = "gray"),
                                            "color_blind")
                               ),
                        
                        textInput("rename_conditions","Rename selected conditions (comma delimited)*"),
                        selectInput("stroke_width","Stroke width",c(1,2,3,4,5),1),
                       
                        
                        h4("Hints"),
                        tags$ul(
                          tags$li("Color-blind friendly palettes support up to 8 different conditions (colors)."),
                          tags$li("* When renaming conditions, use a comma (,) as a seperator. Make sure to list 
                                  as many names as conditions you have selected."),
                          tags$li("With dropR you can produce graphs for publication in various formats. You may 
                                  choose vector formats such as .svg and .pdf or the .png format for rendered pixels.")
                        )),
                    box(width = 8,
                        title = "Dropout by question",
                        div(plotOutput("do_curve_plot"),
                            style = 'overflow:auto'),
                        
                        h4("Easily export your visualization by choosing a name, file format and optionally the size:"),
                        
                        column(width = 5,
                               textInput("plot_fname","Name (without file extension)",
                                         width=240,
                                         value = paste0("dropR_", round(as.numeric(Sys.time())))
                               ),
                               selectInput("export_format","Export plot as:",
                                           c("pdf" = "pdf",
                                             "svg" = "svg",
                                             "png" = "png"),
                                           "pdf",width=240)
                               ),
                        
                        
                        column(width = 5,
                               tags$br(),
                               dropdownButton(
                                 
                                 label ="Export options",
                                 
                                 
                                 sliderInput("dpi","Resolution (dpi, .png only)",
                                             min = 75, max = 600,value = 300,
                                             width = 240, ticks = F),
                                 sliderInput("h","Height (in inches)",
                                             min = 3, max = 50,value = 4,
                                             width = 240, ticks = F),
                                 sliderInput("w","Width (in inches)",
                                             min = 3, max = 50,value = 10,
                                             width = 240, ticks = F),
                                 
                                 circle = F, 
                                 status = "default",
                                 icon = icon("gear"), 
                                 width = "180px",
                                 
                                 tooltip = tooltipOptions(title = "Resolution, height & width can be adjusted",
                                                          placement = "left")
                               ),
                               
                               tags$br(),
                               
                               downloadButton('downloadCurvePlot', 'Download plot')
                               )
                        
                        )
                    )
                  )


# Tab Chisq ####
tabXsq <- tabItem(tabName = "xsq",
                  fluidRow(
                    box(width=5,
                        title = HTML("Chi<sup>2</sup>-test options"),
                        # HTML("<h3>Chi<sup>2</sup>-test options</h3>"),
                        uiOutput("chisq_conditions"),
                        uiOutput("xsq_slider"),
                        checkboxInput("p_sim","Simulate p-values",T),
                        h4("Dropout by question"),
                        plotOutput("do_curve_plot_2"),
                        HTML('<em>This plot shows the same overview that was created in Visual Inspection.</em>')
                    ),
                    
                    box(width=7,
                        title = "Contingency Test Outcomes",
                        # h3("Contingency Test Outcomes"),
                        verbatimTextOutput("chisq_tests"),
                        h4("Odds ratio by item"),
                        tableOutput("odds_ratio"),
                        # div(plotOutput("do_curve_plot_2"),
                        #     style = 'overflow:auto')
                        )
                    )
                  )


# Tab Kaplan-Meier ####
tabSurv <- tabItem(tabName = "survival",
                     fluidRow(
                       box(width = 3,
                           title = "Kaplan-Meier Model Specifications",
                           selectInput("kaplan_fit","Choose model fit",
                                       c("total" = "total",
                                         "by condition" = "conditions"),
                                       "total"),
                           uiOutput("kpm_conditions"),
                           
                           column(width = 6,
                                  p(strong("Plot options")),
                                  checkboxInput("kpm_ci","Show confidence bands",T),
                                  checkboxInput("full_scale_kpm","Show full Y-axis (0 to 100)",value = T)
                                  ),
                           column(width = 4,
                                  radioButtons("color_palette_kp","Color palettes",
                                               c("color blind-friendly" = "color_blind",
                                                 "ggplot default" = "default",
                                                 "gray scale" = "gray"),
                                               "color_blind")
                                  )
                           ),
                       
                       box(width = 7,
                           title = "Kaplan-Meier survival curve",
                           plotOutput("kpm_plot")
                           ),
                       box(width = 2,
                           title = "Export Plot",
                           textInput("kpm_plot_fname","Name (without file extension)",
                                     width=240,
                                     value = paste0("dropR-kpm_",round(as.numeric(Sys.time())))
                           ),
                           selectInput("kpm_export_format","Export plot as:",
                                       c("pdf" = "pdf",
                                         "svg" = "svg",
                                         "png" = "png"),
                                       "pdf",width=240),
                           
                           dropdownButton(
                             
                             label ="Export options",
                             
                            
                             sliderInput("kpm_dpi","Resolution (dpi, .png only)",
                                         min = 75, max = 600, value = 300,
                                         width = 240, ticks = F),
                             sliderInput("kpm_h","Height (in inches)",
                                         min = 3, max = 50, value = 4,
                                         width = 240, ticks = F),
                             sliderInput("kpm_w","Width (in inches)",
                                         min = 3, max = 50, value = 10,
                                         width = 240, ticks = F),
                             
                             circle = F, 
                             status = "default",
                             icon = icon("gear"), 
                             width = "180px",
                             
                             tooltip = tooltipOptions(title = "Resolution, height & width can be adjusted",
                                                      placement = "left")
                           ),
                           tags$br(),
                           
                           downloadButton('downloadKpmPlot', 'Download plot', class = ".btn {width: '300px'}")
                           )
                     ),
                     fluidRow(
                       box(width = 3,
                           title = "Rho-family Survival Difference Tests",
                           uiOutput("rho_tests")
                           ),
                       box(width = 7,
                           title = "Test Survival Curve Differences",
                           verbatimTextOutput("surv_tests")
                       )
                     )
                  ) 
                      

tabAbout <- tabItem(tabName = "about",
                    h2("About"),
                    p("DropR is a joint project by Ulf-Dietrich Reips, Matthias Bannert and Annika Tave Overlander that 
                      followed naturally from the long-standing need in",
                      a("Internet science", href = "https://iscience.uni-konstanz.de/"), 
                      "and online research for methods and tools to address the fact that dropout (aka attrition, mortality, 
                      break-off) occurs much more frequently when research is conducted via the Internet than traditionally
                      in the lab.", class="slimtext"),
                    p("You can find the full documentation of the dropR package on",
                      a("GitHub.", href = "https://iscience-kn.github.io/dropR/index.html"), class="slimtext")
                    )

# Main Page structure ####
ui <- dashboardPage(
  
  dashboardHeader(title = "dropR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Start: Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Visual inspection", tabName = "viz",
               icon = icon("area-chart",lib="font-awesome")),
      menuItem("Contingency Analyses", tabName = "xsq",
               icon = icon("percent",lib="font-awesome")),
      menuItem("Survival Analyses", 
               menuSubItem("Kaplan-Meier", tabName = "survival"),
               icon = icon("percent",lib="font-awesome")
               ),
      menuItem("About", tabName = "about",
               icon = icon("circle-info",lib="font-awesome"))
    )
  ),
  # Body of the App #############
  dashboardBody(
    chooseSliderSkin("Shiny", color = "gray"),
    tags$head(tags$link(rel = "shortcut icon", 
                href = "decrease.svg")),

    tabItems(tabHome,
             tabViz,
             tabUpload,
             tabXsq,
             tabSurv,
             tabAbout)
  )
)

