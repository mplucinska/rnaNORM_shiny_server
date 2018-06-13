library(ggplot2)
library(ggthemes)  
library(shiny)
library(plotly)
#library(R4RNA)
library(DT)

shinyServer(function(input, output, session) {
  
  
  observeEvent(input$submit, {
    output$sequence <- renderText({input$sequence})
    output$str1 <- renderText({input$str1})
    output$str2 <- renderText({input$str2})
    # #R4RNA view
    write(">str1", file = "str1.br")
    write(input$str1, file = "str1.br", append = TRUE)
    file <- system.file("extdata", "str1.br", package = "R4RNA")
    str1 <- readVienna("str1.br")
    
    write(">str2", file = "str2.br")
    write(input$str2, file = "str2.br", append = TRUE)
    str2 <- readVienna("str2.br")
     
    output$plot_structure <- renderPlot({
      plotDoubleHelix(colourByUnknottedGroups(str1), colourByUnknottedGroups(str2), line = TRUE, arrow = FALSE)
     })
     
     #FORNA view
     session$sendCustomMessage("mymessage", list( sequence = input$sequence, str1 = input$str1, str2 = input$str2, size = input$char_num))
    })
  
  #icons to remove from plotly modebar
  list_buttons <- list("autoScale2d" , 'Collaborate', 'toggleSpikelines','lasso2d', 'pan2d', 'sendDataToCloud', 'select2d', 'zoomIn2d', 'zoomOut2d', 'hoverClosestCartesian', 'hoverCompareCartesian')
  
  observeEvent(input$submit_file_norm, {
    df <- read.csv(input$file2$datapath,sep = "\t", header = F)
    state = reactiveValues(choice = unique(df$V1))
    output$selectID <- renderUI({
      selectInput("selected", strong(h5("Select transcript ID:")), state$choice  ,selected = 1)
    })
    
    observeEvent(input$selected,{
      state$val <- input$selected
      data_selected_1 <- subset(df, df$V1 == state$val)
      #print(as.numeric(input$ID_col))
      data_selected <- cbind(data_selected_1[input$ID_col], data_selected_1[input$position_col], data_selected_1[input$control_col], data_selected_1[input$treated_col])
      #if (sum(data_selected$V3) > 0){
        res <- rnaPRE_results(data_selected)
        normalized_all_data <- res[1]
        table_res <- res[2]
        draw_plots(normalized_all_data, table_res)
      #} 
    })
  })
  
  observeEvent(input$submit_form, {
    dfp <- as.data.frame(matrix(unlist(strsplit(input$counts, "\n")[[1]])))
    df <- data.frame(do.call('rbind', strsplit(as.character(dfp$V1),'\t',fixed=TRUE)))
    state = reactiveValues(choice = unique(df$X1))
    
    output$selectID <- renderUI({
      selectInput("selected", strong(h5("Select transcript ID:")), state$choice  ,selected = 1)
    })
    observeEvent(input$selected,{
      state$val <- input$selected
      data_selected_1 <- subset(df, df$X1 == state$val)
      #print(as.numeric(input$ID_col))
      data_selected <- cbind(data_selected_1[input$ID_col], data_selected_1[input$position_col], data_selected_1[input$control_col], data_selected_1[input$treated_col])
      #if (sum(data_selected$V3) > 0){
      res <- rnaPRE_results(data_selected)
      normalized_all_data <- res[1]
      table_res <- res[2]
      draw_plots(normalized_all_data, table_res)
      #} 
    })
})
  

  download_all <- function(){
    output$download <- downloadHandler(
      filename <- function() {
        paste("aaaa", "csv", sep=".")
      },
      content = function(file){        
        file.copy("www/working_dir/output_all_rnaPRE.txt", file)
      }
    )
  }
  
  observeEvent(input$load_example, {
    output$done <-renderText(0)
    df <- read.csv("test_multiple" ,sep = "\t", header = F)[1:30000,]
      
    #promise for download all data
    d_all <- future({
      write.table(df, "www/working_dir/input_all_rnaPRE", row.names = FALSE, col.names = FALSE, quote = FALSE)
      system('python new_normalization.py -i www/working_dir/input_all_rnaPRE -o www/working_dir/output_all_rnaPRE.txt ')
      normalized_all_data <- read_delim("www/working_dir/output_all_rnaPRE.txt",
                                        "\t", escape_double = FALSE, col_names = FALSE,  trim_ws = TRUE)
    }) %plan% multiprocess
      
    observeEvent(input$calculate,{
      disable("calculate")
      while(!resolved(d_all)){
        Sys.sleep(1)
      }
      download_all()
      output$done <- renderText(1)
      enable("calculate")
     })

    state = reactiveValues(choice = unique(df$V1))
    output$selectID <- renderUI({
      selectInput("selected", h6(strong("Select transcript ID:")), state$choice, selected = 1)
    })
    
    observeEvent(input$selected,{
      state$val <- input$selected
      data_selected_1 <- subset(df, df$V1 == state$val)
      #print(as.numeric(input$ID_col))
      data_selected <- cbind(data_selected_1[input$ID_col], data_selected_1[input$position_col], data_selected_1[input$control_col], data_selected_1[input$treated_col])
      #if (sum(data_selected$V3) > 0){
      res <- rnaPRE_results(data_selected)
      normalized_all_data <- res[1]
      table_res <- res[2]
      draw_plots(normalized_all_data, table_res)
      #}
      
    })
    

  })

  
  observeEvent(input$new_analysis, {
    session$reload()
  })
  
  
  rnaPRE_results <- function(df){
    write.table(df, "www/working_dir/input_file_rnaPRE", row.names = FALSE, col.names = FALSE, quote = FALSE)
    system('python new_normalization.py -i www/working_dir/input_file_rnaPRE -o www/working_dir/output_file_rnaPRE.txt ')
    normalized_all_data <- read_delim("www/working_dir/output_file_rnaPRE.txt",
                                        "\t", escape_double = FALSE, col_names = FALSE,  trim_ws = TRUE)
    system('rm www/working_dir/output_file_rnaPRE.txt')
    maxy <- read_delim("www/working_dir/maxy_output_file_rnaPRE.txt","\t", escape_double = FALSE, col_names = FALSE,  trim_ws = TRUE)
    output$maxy2 <- renderText(as.numeric(maxy[1,1]))
    system('rm www/working_dir/maxy_output_file_rnaPRE.txt')
    output$maxy <- renderText(as.numeric(maxy[1,1]))
    if(maxy == '0') {
      createAlert(session, "alert", "exampleAlert", title = "Oops",
                  content = "Choose another transcript. Not enough positions with stops to calculate reactivity.", append = FALSE, style = 'danger')
    } 
    print(as.numeric(maxy[1,1]))
    normalized_all_data$X7[normalized_all_data$X8 == "F"] <- 0
    normalized_all_data$colour[normalized_all_data$X7 < 1.2] <- "0.9 - 1.2"
    normalized_all_data$colour[normalized_all_data$X7 < 0.9] <- "0.6 - 0.9"
    normalized_all_data$colour[normalized_all_data$X7 < 0.6] <- "< 0.6"
    normalized_all_data$colour[normalized_all_data$X7 > 1.2] <-  "> 1.2"
    write.csv(normalized_all_data[,c(1,2,7)], "www/working_dir/output_rnaPRE.csv", row.names = FALSE)
    table_res <- as.data.frame(cbind(df, normalized_all_data$X6, normalized_all_data$X8))
    colnames(table_res) <- c("ID", "position", "counts in control", "counts in modified", "reactivity", "passed filter")
    return(list(normalized_all_data, table_res))
  }
  
  draw_plots <- function(ndata, table_res){
    ndata <- as.data.frame(ndata)
    #download selscted transcript
    print(unique(ndata$X1))
    output$downloadData <- downloadHandler(
      filename <- function() {
        paste(unique(ndata$X1), "csv", sep=".")
      },
      content <- function(file) {
        file.copy("www/working_dir/output_rnaPRE.csv", file)
      }
    )
    
    output$plot_scatter1 <- renderPlotly({
      p <- plot_ly(ndata, x = ~X3, y = ~X4)
      ggplotly(p) %>% config(displayModeBar = T,   modeBarButtonsToRemove = list_buttons) %>% layout(margin = m, yaxis = list(title = "stops control"),  xaxis = list(title = "stops treated", range = c(0, max(ndata$X3,ndata$X4))),                                                                                                  xaxis = list(title = "")) 
    })
    
    output$plot_scatter2 <- renderPlotly({
      p <- plot_ly(ndata, x = ~X3, y = ~X5, color = ~X8)
      ggplotly(p) %>% config(displayModeBar = T,   modeBarButtonsToRemove = list_buttons) %>% layout(showlegend = FALSE, margin = m, yaxis = list(title = "normalized stops control"),  xaxis = list(title = "stops treated", range = c(0, max(ndata$X3,ndata$X5))))
    })
    
    output$plot_histogram <- renderPlotly({
      p <- plot_ly(ndata, x = ~log)
      ggplotly(p) %>% config(displayModeBar = T,   modeBarButtonsToRemove = list_buttons) %>% layout(showlegend = FALSE, margin = m,  xaxis = list(title = "log2(FC)"))
    })
    
    output$plot_histogram2 <- renderPlotly({
      p <- plot_ly(ndata, x = ~X7)
      ggplotly(p) %>% config(displayModeBar = T,   modeBarButtonsToRemove = list_buttons) %>% layout(showlegend = FALSE, margin = m,  xaxis = list(title = "log2(FC)"))
    })
    
    output$table_results <- renderDT(as.data.frame(table_res)[-1], filter = list(position = 'bottom', clear = FALSE), options = list(pageLength = 15), rownames = FALSE) #%>% formatStyle(1:5, 'text-align' = 'left')
    
    output$plot <- renderPlotly({
      colour_map = c(`0.9 - 1.2` = "#AD4C41", `0.6 - 0.9` = "#F2C05C", `< 0.6 `= "#C1BAB6", `> 1.2` = "#D0864E")
      #p <- ggplot(normalized_all_data, aes(X2, as.character(X7), fill = colour)) + geom_bar()
      p <- plot_ly(ndata, x = ~X2, y = ~X7, color = ~colour,colors = c("#C1BAB6", "#AD4C41","#D0864E", "#F2C05C"), type = 'bar') #+ scale_fill_manual(breaks = c("0.9 - 1.2", "0.6 - 0.9", "< 0.6", "> 1.2"), values=c("#AD4C41", "#F2C05C", "#C1BAB6", "#D0864E"))
      ggplotly(p) %>% config(displayModeBar = T,   modeBarButtonsToRemove = list_buttons) %>% layout(yaxis = list(fixedrange = T, title = "reactivity"), 
                                                                                                     legend = list(orientation = 'h', y = -0.2), xaxis = list(title = "")) 
    })
    
    m <- list(
      l = 50,
      r = 50,
      b = 50,
      t = 50
    )
  }
})
