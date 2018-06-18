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
      tabPanel("rnaNORM",
               div(class = "container-fluid main-container",
                   # input panel rnaPRE
                   conditionalPanel(condition = "input.submit_norm == '0' && input.load_example == '0'",
                                    h3(strong("rnaNORM"), class = "text-muted"),
                                    hr(),
                                    br(),
                                    splitLayout(
                                      div(style = "margin: 30px; background-color: #ffffff; border-width: 0px; border-color: #ffffff;",
                                        fluidRow(
                                          column(5,
                                                 h5(strong("Upload input file with counts"))),
                                          column(1,
                                                 bsButton("q3", label = "", icon = icon("question"), style = 'primary',
                                                          size = "extra-small"),
                                                 bsPopover(id = "q3", title = "Help",
                                                           content = paste0("Input file format: id    position   count_in_control     count_in_modified"),                                                       
                                                           placement = "right", 
                                                           trigger = "hover", 
                                                           options = list(container = "body")
                                                 )
                                          )
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
                                          column(2, numericInput("ID_col", "ID", value = 1)),
                                          column(2, numericInput("position_col", "position", value = 2)),
                                          column(2, numericInput("control_col", "control", value = 3)),
                                          column(2, numericInput("treated_col", "treated", value = 4)),
                                          column(4,"")
                                        )
                                        #actionButton("submit_file_norm", label = "Submit",  class = "btn-primary"),
                                       ),
                                       div(style = "margin: 30px; background-color: #ffffff; border-width: 0px; border-color: #ffffff;",
                                        fluidRow(
                                          column(3,
                                                 h5(strong("or Paste counts"))),
                                          column(1,
                                                 bsButton("q4", label = "", icon = icon("question"), style = 'primary',
                                                          size = "extra-small"),
                                                 bsPopover(id = "q4", title = "Help",
                                                           content = paste0("Input format: position   count_in_control     count_in_modified"),                                                       
                                                           placement = "right", 
                                                           trigger = "hover", 
                                                           options = list(container = "body")
                                                 )
                                          )
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
                   conditionalPanel(condition = "input.submit_norm == '1'|| input.load_example == '1'",
                                    fluidRow(
                                      column(5,h3(strong("rnaNORM"), class = "text-muted"), span(textOutput("done"),style="color:white")),
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
                                        column(11,
                                               h5(strong("Normalized reactivities calculated with rnaNORM"))),
                                        column(1,
                                               bsButton("q1", label = "", icon = icon("question"), style = 'primary',
                                                        size = "extra-small"),
                                               bsPopover(id = "q1", title = "Help",
                                                         content = paste0("Barplot shows reactivities for each position in transcript"),
                                                         placement = "left", 
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
                                        column(11,
                                               h5(strong("Stops correlation before and after normalization"))),
                                        column(1,
                                               bsButton("q2", label = "", icon = icon("question"), style = 'primary',
                                                        size = "extra-small"),
                                               bsPopover(id = "q2", title = "Help",
                                                         content = paste0("Each point refers to number of stops in control and treated sample in particular position "),
                                                         placement = "left", 
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
      ), # tab rnaPRE end
      # #tab rnaCARD
      # tabPanel("rnaCARD",
      #   div(class = "container-fluid main-container",
      #     h3(strong("rnaCARD"), class = "text-muted"),
      #     hr(),
      #     # rnaCARD input panel
      #     conditionalPanel(condition = "input.submit == '0'",
      #       h5(strong("Paste sequence & structures")),
      #       #paste input
      #       div(class = "text-primary",
      #         verticalLayout(
      #           textAreaInput("sequence", h6(strong("NUCLEOTIDE SEQUENCE")),  "TAATGCCTTTGTTTGGCCAAGCTATGTGCAAATATCACAAATTAAAAATTTGGTTAAGCAGTTAGGCTGGACCTAATATTTTAGAAAAACCTAATTTTTTTTGTGGACCCATTTTCGATATTTACTCACAAATGGAATTCAAGGGGAACAACTTCGGTCTCAGCACTTTAATTATTCTTCTCGTTCCCACCTAATTTCGCAATTTATTGTCCTTGACTTCTACCACGAGAAAAAAATTAAGAAAATGCAACGCTGCCCGTGCAGGGTTTTCTGAGCGGGATGAAAAAATCAGACAAATATCCAAGTTATGAGTAATTACTTTGTTGGAAGGAGGGAGCAGAGGATAAGGAAATTCTTAAAACTGTTATGTATATAAAGGAAGAACCATTTCTAGTTATTTCACTTTTTGATACTTGTCAACTATCTTAGTAAAAATACAGAACTCTATAAAGAACCACAGAAAAATCGACAGCAATGACAAGCATTGACATTAACAACTTACAAAATACCTTTCAACAAGCTATGAATATGAGCGGCTCCCCAGGCGCTGT", resize = "vertical"),
      #           textAreaInput("str1", h6(strong("STRUCTURE 1")), "(((((.....(((((((((((...((((.......)))).........))))))))))).((((((.....))))))...((((......)))).(((((((((((..((((((..((.......))..)))))).......((((...........))))......((..((((.(((((.((((((.(((......((((....)))).(((((.((((......)))).....))))).....(((....)))((((.((((....)))).))))(((......)))..........((((.....((((....))))..)))).))).)))))))))))))))..)).(((((......(((.(((((.....((((......)))).(((......)))....(((....))).((((...))))....))))).)))......)))))))))))))))).......(((((.....))))).)))))....((((....(((.(((.....))).))).....))))((((.((...)).)))).", resize = "vertical"),
      #           textAreaInput("str2", h6(strong("STRUCTURE 2")), "(((((.....(((((((((((...((((.......)))).........))))))))))).((((((.....))))))..................(((((((((((..((((((..((.......))..)))))).......((((...........))))......((..((((.(((((.((((((.(((......((((....)))).(((((.((((......)))).....))))).....(((....)))((((.((((....)))).))))(((......)))..........((((.....((((....))))..)))).))).)))))))))))))))..)).(((((......(((.(((((..(((((((((((........))).)))).)))).((((....))))...............))))).)))......)))))))))))))))).......(((((.....))))).)))))....((((....(((.(((.....))).))).....))))((((.((...)).)))).", resize = "vertical")
      #         )
      #         ),
      #   
      #       #radioButtons("radio", label = "Options",
      #       #         choices = list("hairpins" = 1, "hairpins & domain closing stems" = 2, "hairpins & stems" = 3), 
      #       #         selected = 1),
      #   
      #       actionButton("submit", label = "Submit",  class = "btn-primary"),
      #       hr(),
      #       
      #       # file input
      #       h5(strong("Or upload input file")),
      #       
      #       fileInput("file1", "",
      #                     multiple = FALSE),
      #       
      #       actionButton("submit_file", label = "Submit",  class = "btn-primary")
      #       
      #     ), #rnaCARD input panel end
      #     
      #     #rnaCARD results panel
      #     conditionalPanel(condition = "input.submit == '1'",
      #                      
      #         h5(strong("RESULTS for ID")),
      #         br(),
      #         
      #         h6(strong("NUCLEOTIDE SEQUENCE"), class = "text-primary"),
      #         div(id="sequence-viewer", class = 'view'),
      #         br(),
      #         h6(strong("STRUCTURE 1"), class = "text-primary"),
      #         div(id="str1-viewer", class = 'view'),
      #         br(),
      #         h6(strong("STRUCTURE 2"), class = "text-primary"),
      #         div(id="str2-viewer", class = 'view'),
      #         br(),
      #         
      #         # bsCollapse(id = "collapseExample", open = "Nucleotide sequence",
      #         #            bsCollapsePanel(h5(strong("Nucleotide sequence", span(class="glyphicon glyphicon-menu-down"))),
      #         #                            div(id="sequence-viewer", class = 'view'), style = "secondary"),
      #         #            bsCollapsePanel(h5(strong("Structure 1", span(class="glyphicon glyphicon-menu-down"))),                          
      #         #                            div(id="str1-viewer", class = 'view'), style = "secondary"),
      #         #            bsCollapsePanel(h5(strong("Structure 2", span(class="glyphicon glyphicon-menu-down"))),                          
      #         #                            div(id="str2-viewer", class = 'view'), style = "secondary")
      #         # ),
      #         br(),
      #         #Forna view
      #           div(class = "breadcrumb",id = "rna1", style = "max-height: 700px; border-color: #888; border-radius: 25px; background-color: white; border-width: 5px;"),
      #           #fluidRow(
      # 
      #           #  column(6, align="center",
      #           #      div(class = "breadcrumb", id = "rna2", style = "max-height: 700px; max-width: 700px; border-color: green; background-color: white;")
      #           #  )
      #           
      #          # Forna view end
      #     
      #       #R4RNA view 
      #       plotOutput("plot_structure", click = "plot_click", width = "100%", height = "700px")
      #     
      #     )
      #       ) # results panel view end
      # ), #tab rnaCARD end
      tabPanel("Help",
               div(class = "container-fluid main-container",
               h4(strong("About rnaNORM"), class = "text-muted"),
               #hr(),
               #h5("rnaNORM is method reactivity calculation that eliminate read distribution bias and prevent underestimation of reactivity."),
               br(),
               h5(strong("Input format"), class = "text-primary"),
               hr(),
               h5("Input consist of 4 tab delimited columns (default order):"),
               br(),
               fluidRow(
                 column(1,"id"),
                 column(1, "position"),
                 column(2, "number_of_stops_in_control"),
                 column(2, "number_of_stops_in_modified")
               ),                 
               br(),
               h5("User can also specify column order in file."),
               br(),
               h5("example"),
               fluidRow(column(1,"tL(UAA)B1"), column(1,"28"), column(1,"35"), column(1, "13")),
               fluidRow(column(1,"tL(UAA)B1"), column(1,"29"), column(1,"14"), column(1, "7")),
               fluidRow(column(1,"tL(UAA)B1"), column(1,"30"), column(1,"15"), column(1, "7")),
               fluidRow(column(1,"tL(UAA)B1"), column(1,"31"), column(1,"96"), column(1, "50")),
               fluidRow(column(1,"tL(UAA)B1"), column(1,"32"), column(1,"99"), column(1, "71")),
               br(),
               h5(strong("Output"), class ="text-primary"),
               hr(),
               h6(),
               h5(strong("Download from github"), class = "text-primary"),
               hr()
               )
               ) #end tab Help
    )# navbarPage end
  )# fluidPage end
)# UI end