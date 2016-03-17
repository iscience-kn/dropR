## app.R ##
library(shiny)
library(shinydashboard)
library(ggvis)


# Tab items 
# Home ####
tabHome <- tabItem(tabName = "home",
                   h1("dropR"),
                   h2("About dropout analysis using R"),
                   p("Some text about the GUI and the underlying R
                     package."))

# Visual Analysis ####
tabViz <- tabItem(tabName = "viz",
                  fluidRow(
                    box(width=3,
                        h3("Plot options"),
                        uiOutput("show_conditions"),
                        sliderInput("strokeW","Stroke width",1,5,2),
                        checkboxInput("show_points","Show points and hover tooltips")
                        ),
                    box(width = 9,
                        h3("Dropout by question"),
                        div(ggvisOutput("dropout_curve"),
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
                                              '#N/A','NA','.'))
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

# Tab Analysis ####
tabKaplan <- tabItem(tabName = "kaplan",
                       h2("Tests and stats"))

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

