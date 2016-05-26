## app.R ##
library(shiny)
library(shinydashboard)

# Tab items 
# Home ####
tabHome <- tabItem(tabName = "home",
                   h1("dropR"),
                   h2("About dropout analysis using R"),
                   p("Some text about the GUI and the underlying R
                     package."),
                   textOutput("debug_txt")
                   )

# Visual Analysis ####
tabViz <- tabItem(tabName = "viz",
                  fluidRow(
                    box(width=3,
                        h3("Plot options"),
                        uiOutput("show_conditions"),
                        sliderInput("stroke_width","Stroke width",1,5,2),
                        checkboxInput("show_points","Show points"),
                        radioButtons("color_palette","Color palettes",
                                     c("ggplot default" = "default",
                                       "color blind-friendly" = "color_blind",
                                       "gray scale" = "gray"),
                                     "default"),
                        textInput("rename_conditions","Rename selected conditions"),
                        selectInput("export_format","Export graph as:",
                                    c("pdf" = "pdf",
                                      "svg" = "svg"),
                                    "pdf")
                        ),
                    box(width = 2,
                        h3("Hints"),
                        p("- color blind and printer friendly palettes support up to 8 different categories (colors)."),
                        p("- When re-labelling condtions, use ',' as a seperator. Make sure to list as many names as conditions selected.")
                    ),
                    
                    box(width = 7,
                        h3("Dropout by question"),
                        div(plotOutput("do_curve_plot"),
                            style = 'overflow:auto')
                        )
                    
                    
                    
                    ),
                  fluidRow(
                    box(width=5,
                        HTML("<h3>χ<sup>2</sup>-test options</h3>"),
                        sliderInput('chisq_question',"Select question",
                                    1,52,1)
                                                ),
                    box(width=7,
                        h3("Test outcomes"))
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
                                              '.csv'))
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
                                            c('-99','-999','-9999',
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
                           uiOutput("rho_tests")
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
      menuItem("Visual inspection, Chisq.", tabName = "viz",
               icon = icon("area-chart",lib="font-awesome")),
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
             tabKaplan)
  )
)

