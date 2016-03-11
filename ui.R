## app.R ##
library(shiny)
library(shinydashboard)
library(ggvis)


# Tab items 
# Home ####
tabHome <- tabItem(tabName = "home",
                   h1("dropR"),
                   h2("Drop-out analysis using R"))

# Visual Analysis ####
tabViz <- tabItem(tabName = "viz",
                  fluidRow(
                    box(plotOutput("plot1", height = 250)),
                    box(title = "Controls",
                        sliderInput("slider", "Number of observations:",
                                    1, 100, 50)
                        )
                    )
                  
                  )
# Upload ####
tabUpload <- tabItem(tabName = "upload",
                      h2("Upload your data"),
                     fluidRow(
                       box(
                         h3("Choose .csv file"),
                         tags$ul(
                           tags$li("Indicate whether the first line of your
                                   data is meant to be a header."),
                           tags$li("Choose the proper column delimiter and
                                   text quote for your file."),
                           tags$li("Make sure to use reasonable coding for
                                   missing values (i.e. empty cells). Avoid
                                   ambigous coding such as -99, -999 etc."),
                           tags$li("Check the preview window below. Iff your
                                   data is displayed as expected you are good to 
                                   start with your analysis.")
                         ),
                         fileInput('file1', '',
                                     accept=c('text/csv', 
                                              'text/comma-separated-values,text/plain', 
                                              '.csv'))
                         ),
                       box(
                         h3("Specify .csv properties"),
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
                       )
                     ),
                     fluidRow(
                       box(width = 12,
                         h3("Data Preview"),
                        div(DT::dataTableOutput("table"),style = 'overflow:auto')
                       )
                     )
                     )

tabAnalysis <- tabItem(tabName = "viz",
                       h2("graph"))




# Main Page structure ####
ui <- dashboardPage(
  dashboardHeader(title = "dropR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Visual analysis", tabName = "viz",
               icon = icon("area-chart",lib="font-awesome"))
    )
  ),
  # Body of the App #############
  dashboardBody(
    tabItems(tabHome,
             tabViz,
             tabUpload,
             tabAnalysis)
  )
)

