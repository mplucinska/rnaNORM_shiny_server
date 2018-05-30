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
                   conditionalPanel(condition = "input.submit_file_norm == '0' && input.submit_form == '0' && input.load_example == '0'",
                                    h3(strong("rnaNORM"), class = "text-muted"),
                                    hr(),
                                    div(class = "text-primary", h6(strong("Choose column number"))),
                                    fluidRow(
                                      column(1, numericInput("ID_col", "ID", value = 1)),
                                      column(1, numericInput("position_col", "position", value = 2)),
                                      column(1, numericInput("control_col", "control", value = 3)),
                                      column(1, numericInput("treated_col", "treated", value = 4)),
                                      column(4,""),
                                      column(4,"")
                                    ),
                                    br(),
                                    fluidRow(
                                      column(11,
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
                                    
                                    actionButton("submit_file_norm", label = "Submit",  class = "btn-primary"),
                                    actionButton("load_example", label = "Example",  class = "btn-link"),
                                    
                                    hr(),
                                    fluidRow(
                                      column(11,
                                             h5(strong("Paste counts"))),
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
                                         textAreaInput("counts", "",  "id    1    215    315", resize = "vertical")
                                    ),
                                    br(),
                                  actionButton("submit_form", label = "Submit",  class = "btn-primary")
                   ),
                   
                   #results panel rnaPRE
                   conditionalPanel(condition = "input.submit_file_norm == '1' || input.submit_form == '1' || input.load_example == '1'",
                                    fluidRow(
                                      column(8,h3(strong("rnaNORM"), class = "text-muted")),
                                      column(1, downloadButton("downloadData", "Download results")), 
                                      column(1, ""),
                                      column(1, actionButton( "new_analysis", "New analysis", class="btn-warning")),
                                      column(1, "")
                                    ),
                                    
                                    hr(),  
                                    div(class="text-primary",
                                    uiOutput("selectID")),
                                    br(),
                                    textOutput("maxy"),
                                    br(),
                                    conditionalPanel(condition = "output.maxy != 0",
                                                     
                                      fluidRow(
                                        column(11,
                                               h5(strong("Normalized reactivities calculated with rnaPRE"))),
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
                                        #fluidRow(
                                        #  column( 3 ,bsAlert("alert"))
                                        #)
                                        div(class="breadcrumb", h5("Choose another transcript. Not enough positions with stops to calculate reactivity."))
                                        
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
               hr(),
               h5(strong("Input format"), class = "text-primary"),
               hr(),
               h5(strong("Output"), class ="text-primary"),
               hr(),
               h5(strong("Download from github"), class = "text-primary"),
               hr()
               )
               ) #end tab Help
    )# navbarPage end
  )# fluidPage end
)# UI end