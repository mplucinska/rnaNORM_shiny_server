# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(shinyBS)

shinyUI(
  fluidPage(
    useShinyjs(),
    navbarPage(theme = shinytheme("yeti"), "rna", position = "fixed-top", collapsible = TRUE,
      fluid = TRUE,
      header=singleton(tags$head(tags$head(tags$script(src="/scripts/fornac.js")),
                                 tags$head(tags$script(src="/js/vis.js")),
                                 tags$head(tags$script(src="/js/sequence-viewer.js")),
                                 tags$head(
                                   tags$link(rel = "stylesheet", type = "text/css", href = "my_theme.css")
                                 ))),
      
      #tab rnaPRE
      tabPanel("probNORM",
               div(class = "container-fluid main-container",
                   conditionalPanel(
                     condition = "input.submit_norm  == '0' && input.start == '0'",
                     style = "background-image: url(g38710.png); background-size: cover;",
                     
                     div(style="height: 200px;"),
                     
                     
                     fluidRow(
                       column(6, align="center", offset =  3,
                              h1(strong("probNORM"), class = "text-muted")
                       )
                     ),
                     br(),
                     br(),
                     fluidRow(
                       column(6, align="center", offset =  3,
                              h3("Welcome in probNORM webserver!")
                       )
                     ),
                     
                     fluidRow(
                       column(6, align="center", offset =  3,
                              h4("Click below to analyze RNA probing data.")
                       )
                     ),
                     
                     br(),
                     br(),
                     fluidRow(
                       column(6, align="center", offset =  3,
                              actionButton(
                                "start",
                                label = "Start analysis",
                                class = "btn-success  btn-lg",
                                style = "width: 200px;"
                              )
                       )
                     ),
                     div(style="height: 400px;")
                   ),
                   # input panel rnaPRE
                   conditionalPanel(condition = "input.submit_norm == '0' && input.load_example == '0' && input.start == '1'",
                                    h3(strong("probNORM"), class = "text-muted"),
                                    hr(),
                                    br(),
                                    splitLayout(
                                      div(style = "margin: 30px; background-color: #ffffff; border-width: 0px; border-color: #ffffff;",
                                        fluidRow(
                                          column(1,
                                                 bsButton("q3", label = "", icon = icon("question"), style = 'primary',
                                                          size = "extra-small"),
                                                 bsPopover(id = "q3", title = "Help",
                                                           content = paste0("Input file format: id    position   count_in_control     count_in_modified"),                                                       
                                                           placement = "right", 
                                                           trigger = "hover", 
                                                           options = list(container = "body")
                                                 )
                                          ),
                                          column(5,
                                                 h5(strong("Upload input file with counts")))
                                        ),
          
                                        fileInput("file2", "",
                                                  multiple = FALSE),
                                        hr(),
                                        fluidRow(
                                          #column(4,""),
                                          column(4,div(class = "text-primary", h6(strong("Choose column number")))),
                                          column(4,"")
                                        ),
                                        fluidRow(
                                          #column(4,""),
                                          column(2, numericInput("ID_col", "transcript ID", value = 1)),
                                          column(2, numericInput("position_col", "position", value = 2)),
                                          column(2, numericInput("control_col", "control", value = 3)),
                                          column(2, numericInput("treated_col", "treated", value = 4)),
                                          column(4,"")
                                        )
                                        #actionButton("submit_file_norm", label = "Submit",  class = "btn-primary"),
                                       ),
                                       div(style = "margin: 30px; background-color: #ffffff; border-width: 0px; border-color: #ffffff;",
                                        fluidRow(
                                          column(1,
                                                 bsButton("q4", label = "", icon = icon("question"), style = 'primary',
                                                          size = "extra-small"),
                                                 bsPopover(id = "q4", title = "Help",
                                                           content = paste0("Input format: position   count_in_control     count_in_modified"),                                                       
                                                           placement = "right", 
                                                           trigger = "hover", 
                                                           options = list(container = "body")
                                                 )
                                          ),
                                          column(3,
                                                 h5(strong("or Paste counts")))
                                        ),
                                       
                                        div(class = "text-primary",
                                             textAreaInput("counts", "",  placeholder = "id    1    215    315", resize = "vertical", height = "200px")
                                        ),
                                        br()
                                      #actionButton("submit_form", label = "Submit",  class = "btn-primary")
                                     )
                                    ),#splitLayout
                                    fluidRow(
                                      column(5,""),
                                      column(2,actionButton("submit_norm", label = "Submit",  class = "btn-primary", style = "width: 200px;")),
                                      column(1,actionButton("load_example", label = "Example",  class = "btn-link")),
                                      column(3,"")
                                    )
                   ),
                   
                   #results panel rnaPRE
                   conditionalPanel(condition = "input.start == '1' && input.submit_norm == '1'|| input.load_example == '1'",
                                    fluidRow(
                                      column(5,h3(strong("probNORM"), class = "text-muted"), span(textOutput("done"),style="color:white")),
                                      column(2, actionButton( "new_analysis", "New analysis", class="btn-warning")),
                                      
                                      column(3,
                                             downloadButton("downloadData", "Download results for transcript", style='font-size:90%')),
                                      column(2,
                                             conditionalPanel(condition = "output.done != 1",
                                                              actionButton("calculate", "Calculate all", style='font-size:90%')),
                                             conditionalPanel(condition = "output.done == 1",
                                                              downloadButton("download", "Download all", style='font-size:90%', class="btn-success"))
                                             ),
                                      column(1, "")
                                    ),
                                    hr(),
                                    div(class="text-primary",
                                    uiOutput("selectID")),
                                    span(textOutput("maxy"),style="color:white"),
                                    conditionalPanel(condition = "output.maxy != 0",
                                      br(),
                                      textOutput("maxy2"),
                                      br(),
                                      fluidRow(
                                        column(4,
                                               h5(strong("Normalized reactivities calculated with probNORM"))),
                                        column(1,
                                               bsButton("q1", label = "", icon = icon("question"), style = 'primary',
                                                        size = "extra-small"),
                                               bsPopover(id = "q1", title = "Help",
                                                         content = paste0("Barplot shows reactivities for each position in transcript"),
                                                         placement = "right", 
                                                         trigger = "hover", 
                                                         options = list(container = "body")
                                               )
                                        )
                                      ),
                                      br(),
                                      br(),
                                      plotlyOutput("plot",  width = '100%', height = 300),
                                      verbatimTextOutput("event"),
                                      br(),
                                      DTOutput('table_results'),
                                      br(),
                                      fluidRow(
                                        column(4,
                                               h5(strong("Stops correlation before and after normalization"))),
                                        column(1,
                                               bsButton("q2", label = "", icon = icon("question"), style = 'primary',
                                                        size = "extra-small"),
                                               bsPopover(id = "q2", title = "Help",
                                                         content = paste0("Each point refers to number of stops in control and treated sample in particular position "),
                                                         placement = "right", 
                                                         trigger = "hover", 
                                                         options = list(container = "body")
                                               )
                                        )
                                      ),
                                      br(),
                                      splitLayout(
                                        div( style = "height: 50vw",
                                             plotlyOutput("plot_scatter1")
                                        ),
                                        div( style = "height: 50vw",
                                             plotlyOutput("plot_scatter2")
                                        )
                                      )
                                    ),# conditional maxy > 0
                                    conditionalPanel(condition = "output.maxy == 0",
                                        fluidRow(
                                          column(3, ""),
                                          column(6, div(class="breadcrumb", h5("Choose another transcript. Not enough positions with stops to calculate reactivity."))),
                                          column(3, "")
                                          )
                                    ) #conditionsl maxy == 0

                   )# results panel rnaPRE end
               )
      ),
      tabPanel("Help",
               div(class = "container-fluid main-container",
               h4(strong("About probNORM"), class = "text-muted"),
               br(),
               br(),
               p("probNORM is a method of signal calculation that eliminate read distribution bias and prevent underestimation of reactivity."),
               p("Check 'How it works?' section for more info. There is also command-line version availible."),
               br(),
               h5(strong("How probNORM works?"), class = "text-primary"),
               hr(),
               img(src='grafika_probNORM_small.png', style="display: block; margin-right: auto; width: 70%; align:left"),
               br(),
               tags$ol(
                 tags$li(h5("Read distribution is identified by analysis of regression shift towards control sample. First stage of normalization process is calculation of log2 fold change of modified counts with respect to control counts.")), 
                 tags$li(h5("Distribution of log2 fold changes is used for density function estimation. It enables to calculate value of fold change characteristic for background, which next is used as normalization factor. In background signal after correction, normalized control counts are approximate value of counts in modified sample. Only positions above standard deviation are used for reactivity calculation.")), 
                 tags$li(h5("Normalized counts are used for reactivity calculation for each position by substraction of normalized control signal from modified. Final profile is scaled with 2/8 normalization (Deigan et al., 2009)."))
               ),
               #hr(),
               #h5("probNORM is method reactivity calculation that eliminate read distribution bias and prevent underestimation of reactivity."),
               br(),
               h5(strong("Input format"), class = "text-primary"),
               hr(),
               h5("Input consist of 4 tab delimited columns (default order):"),
               fluidRow(
                 column(1,"id"),
                 column(2, "position"),
                 column(3, "number_of_stops_in_control"),
                 column(2, "number_of_stops_in_modified")
               ),
               br(),
               h5("User can also specify column order in file."),
               h5("It is possible to upload multiple transcripts."),
               h5("Script counting stops from chemical probing experiments is availible on github. Input file is indexed BAM file with reads aligned to transcriptome."),
               br(),
               h5(strong("Output"), class = "text-primary"),
               hr(),
               h5("Transcript of interest can be chosen from drop-down list."),
               br(),
               tags$ol(
                 tags$li(h5("Normalized reactivity for each position in transcript visualized on bar plot.")),
                 tags$li(h5("Table with input counts, normalized counts, reactivity and information whether reactivity passed filter or not (P - passed, F - FAILED)")),
                 tags$li(h5("Dot plot representing counts correlation before and after normalization. Each dot on the plot referes to number of counts in control and modified sample in particular position in transcript."))
               ),
               br(),
               h5(strong("Downloading results"), class = "text-primary"),
               hr(),
               h5("Button 'Download results for transcript' enables downloading text file with results for selected transcript from the list."),
               h5("In order to download results for all uploaded transcripts click on 'Calculate all'. It starts calculation for all transcripts. It may take a while. After finishing calculation, button changes to green 'Download all' button."),
               br(),
               h5(strong("Download command-line version of probNORM from github"), class = "text-primary"),
               hr(),
               h5("probNORM is availble in command-line version on github."),
               br(),
               actionButton(inputId='ab1', label="Go to github", 
                            icon = icon("send"), 
                            onclick ="window.open('https://github.com/mplucinska/probNORM')")
               )
               )#end tab Help
    )# navbarPage end
  )# fluidPage end
)# UI end