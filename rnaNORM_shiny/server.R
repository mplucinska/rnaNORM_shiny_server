library(ggplot2)
library(ggthemes)
library(shiny)
library(plotly)
#library(R4RNA)
library(DT)

shinyServer(function(input, output, session) {
  #icons to remove from plotly modebar
  list_buttons <-
    list(
      "autoScale2d" ,
      'Collaborate',
      'toggleSpikelines',
      'lasso2d',
      'pan2d',
      'sendDataToCloud',
      'select2d',
      'zoomIn2d',
      'zoomOut2d',
      'hoverClosestCartesian',
      'hoverCompareCartesian'
    )
  
  observeEvent(input$submit_norm, {
    sessions_id <- stringi::stri_rand_strings(1, 5)
    if (!is.null(input$file2)) {
      df <- read.csv(input$file2$datapath,
                     sep = "\t",
                     header = F)
      if (ncol(df) == 1) {
        df <- read.csv(input$file2$datapath,
                       sep = " ",
                       header = F)
      }
      download_all(df, sessions_id)
      state = reactiveValues(choice = unique(df$V1))
      output$selectID <- renderUI({
        selectInput("selected", strong(h5("Select transcript ID:")), state$choice  , selected = 1)
      })
    } else{
      dfp <-
        as.data.frame(matrix(unlist(strsplit(
          input$counts, "\n"
        )[[1]])))
      
      df <-
        data.frame(do.call('rbind', strsplit(as.character(dfp$V1), '\t', fixed =
                                               TRUE)))
      if (ncol(dfp) == 1) {
        df <-
          data.frame(do.call('rbind', strsplit(as.character(dfp$V1), ' ', fixed =
                                                 TRUE)))
      }
      colnames(df) <- c("V1", 'V2', 'V3', 'V4')
      state = reactiveValues(choice = unique(df$V1))
      download_all(df, sessions_id)
      output$selectID <- renderUI({
        selectInput("selected", strong(h5("Select transcript ID:")), state$choice  , selected = 1)
      })
    }
    
    observeEvent(input$selected, {
      state$val <- input$selected
      print(state$val)
      data_selected_1 <- subset(df, df$V1 == state$val)
      #print(as.numeric(input$ID_col))
      data_selected <-
        cbind(data_selected_1[input$ID_col],
              data_selected_1[input$position_col],
              data_selected_1[input$control_col],
              data_selected_1[input$treated_col])
      #if (sum(data_selected$V3) > 0){
      res <- rnaPRE_results(data_selected, session_id = sessions_id)
      normalized_all_data <- res[1]
      table_res <- res[2]
      draw_plots(normalized_all_data, table_res, session_id = sessions_id)
      #}
    })
  })
  
  download_all_button <- function(session_id, output_file_name) {
    output$download <- downloadHandler(
      filename <- function() {
        paste("rnaNORM_results_all", session_id, "csv", sep = ".")
      },
      content = function(file) {
        file.copy(output_file_name, file)
      }
    )
  }
  
  download_all <- function(df, session_id) {
    #promise for download all data
    input_file_name <-
      paste("www/working_dir/input_all_rnaPRE", session_id, sep = "_")
    
    output_file_name <-
      paste("www/working_dir/output_all_rnaPRE", session_id, sep = "_")
    
    d_all <- future({
      write.table(
        df,
        input_file_name,
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE
      )
      
      system(
        paste(
          './probnorm counts -i',
          input_file_name,
          '-o ',
          output_file_name
        )
      )
      normalized_all_data <- read_delim(
        output_file_name,
        "\t",
        escape_double = FALSE,
        col_names = TRUE,
        trim_ws = TRUE
      )
      
      write.table(
        normalized_all_data,
        output_file_name,
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE,
        sep = "\t"
      )
      
    }) %plan% multicore
    
    observeEvent(input$calculate, {
      disable("calculate")
      while (!resolved(d_all)) {
        Sys.sleep(1)
      }
      download_all_button(session_id, output_file_name)
      output$done <- renderText(1)
    })
  }
  
  observeEvent(input$load_example, {
    sessions_id <- "example"
    output$done <- renderText(0)
    df <- read.csv("test_multiple" , sep = "\t", header = F)
    df <- df[which(df$V1 %in% c('RDN18-1', 'RDN25-1')), ]
    
    download_all(df, sessions_id)
    
    state = reactiveValues(choice = unique(df$V1))
    output$selectID <- renderUI({
      selectInput("selected", h6(strong("Select transcript ID:")), state$choice, selected = 1)
    })
    
    observeEvent(input$selected, {
      #state$val <- input$selected
      #data_selected_1 <- subset(df, df$V1 == state$val)
      #View(data_selected_1)
      res <- rnaPRE_results(df, sessions_id)
      normalized_all_data <- res[1]
      table_res <- res[2]
      draw_plots(normalized_all_data, table_res, session_id = sessions_id)
      #}
      
    })
  })
  
  observeEvent(input$new_analysis, {
    session$reload()
  })
  
  rnaPRE_results <- function(df, session_id) {
    input_file_name <-
      paste("www/working_dir/input_file_rnaPRE", session_id, sep = "_")
    
    output_file_name <-
      paste("www/working_dir/output_file_rnaPRE", session_id, sep = "_")
    
    write.table(
      df,
      input_file_name,
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      sep = "\t"
    )
    
    system(
      paste(
        './probnorm counts -i',
        input_file_name,
        '-o ',
        output_file_name
      )
    )
    
    normalized_data <-
      read_delim(
        output_file_name,
        "\t",
        escape_double = FALSE,
        col_names = TRUE,
        trim_ws = TRUE
      )
    system(paste('rm ', output_file_name))
  
    normalized_data_save <- normalized_data
    colnames(normalized_data_save) <-
      c(
        "transcript_id",
        "position",
        "stops_treated",
        "stops_control",
        "stops_norm_control",
        "reactivity",
        "fold_change",
        "p_value",
        "passed_quality_filter"
      )
    
    write.csv(normalized_data_save,
              output_file_name,
              row.names = FALSE)
    
    normalized_data$X7[normalized_data$passed_quality_filter == "N"] <- 0
    normalized_data$colour[normalized_data$reactivity < 1.2] <- "0.9 - 1.2"
    normalized_data$colour[normalized_data$reactivity < 0.9] <- "0.6 - 0.9"
    normalized_data$colour[normalized_data$reactivity < 0.6] <- "< 0.6"
    normalized_data$colour[normalized_data$reactivity > 1.2] <- "> 1.2"
    
    table_res <-
      as.data.frame(
        cbind(
          normalized_data$transcript_id,
          normalized_data$position,
          normalized_data$stops_treated,
          normalized_data$stops_control,
          normalized_data$stops_norm_control ,
          normalized_data$reactivity ,
          normalized_data$fold_change,
          normalized_data$p_value,
          normalized_data$passed_quality_filter
        )
      )
    
    colnames(table_res) <-
      c(
        "transcript_id",
        "position",
        "stops_treated",
        "stops_control",
        "stops_norm_control",
        "reactivity",
        "fold_change",
        "p_value",
        "passed_quality_filter"
      )
    
    return(list(normalized_data, table_res))
  }
  
  draw_plots <- function(ndata, table_res, session_id) {
    ndata <- as.data.frame(ndata)
    #download selscted transcript
    print(unique(ndata$transcript_id))
    output$downloadData <- downloadHandler(filename <- function() {
      paste(unique(ndata$X1), "csv", sep = ".")
    },
    content <- function(file) {
      output_file_name <-
        paste("www/working_dir/output_file_rnaPRE", session_id, sep = "_")
      file.copy(output_file_name, file)
    })
    
    output$plot_scatter1 <- renderPlotly({
      p <- plot_ly(ndata, x = ~ stops_treated, y = ~ stops_control)
      ggplotly(p) %>% config(displayModeBar = T,
                             modeBarButtonsToRemove = list_buttons) %>% layout(
                               margin = m,
                               yaxis = list(title = "stops control"),
                               xaxis = list(title = "stops treated", range = c(0, max(
                                 ndata$stops_control, ndata$stops_treated
                               ))),
                               xaxis = list(title = "")
                             )
    })
    
    output$plot_scatter2 <- renderPlotly({
      p <- plot_ly(ndata,
                   x = ~ stops_treated,
                   y = ~ stops_norm_control,
                   color = ~ passed_quality_filter)
      ggplotly(p) %>% config(displayModeBar = T,
                             modeBarButtonsToRemove = list_buttons) %>% layout(
                               showlegend = FALSE,
                               margin = m,
                               yaxis = list(title = "normalized stops control"),
                               xaxis = list(title = "stops treated", range = c(0, max(
                                 ndata$stops_norm_control, ndata$stops_treated
                               )))
                             )
    })
    
    output$plot_histogram <- renderPlotly({
      p <- plot_ly(ndata, x = ~ log)
      ggplotly(p) %>% config(displayModeBar = T,
                             modeBarButtonsToRemove = list_buttons) %>% layout(
                               showlegend = FALSE,
                               margin = m,
                               xaxis = list(title = "log2(FC)")
                             )
    })
    
    output$plot_histogram2 <- renderPlotly({
      p <- plot_ly(ndata, x = ~ reactivities)
      ggplotly(p) %>% config(displayModeBar = T,
                             modeBarButtonsToRemove = list_buttons) %>% layout(
                               showlegend = FALSE,
                               margin = m,
                               xaxis = list(title = "log2(FC)")
                             )
    })
    
    output$table_results <-
      renderDT(
        as.data.frame(table_res)[-1],
        filter = list(position = 'bottom', clear = FALSE),
        options = list(pageLength = 15),
        rownames = FALSE
      ) #%>% formatStyle(1:5, 'text-align' = 'left')
    
    output$plot <- renderPlotly({
      colour_map = c(
        `0.9 - 1.2` = "#AD4C41",
        `0.6 - 0.9` = "#F2C05C",
        `< 0.6 ` = "#C1BAB6",
        `> 1.2` = "#D0864E"
      )
      #p <- ggplot(normalized_all_data, aes(X2, as.character(X7), fill = colour)) + geom_bar()
      p <-
        plot_ly(
          ndata,
          x = ~ position,
          y = ~ reactivity,
          color = ~ colour,
          colors = c("#C1BAB6", "#AD4C41", "#D0864E", "#F2C05C"),
          type = 'bar'
        ) #+ scale_fill_manual(breaks = c("0.9 - 1.2", "0.6 - 0.9", "< 0.6", "> 1.2"), values=c("#AD4C41", "#F2C05C", "#C1BAB6", "#D0864E"))
      ggplotly(p) %>% config(displayModeBar = T,
                             modeBarButtonsToRemove = list_buttons) %>% layout(
                               yaxis = list(fixedrange = T, title = "reactivity"),
                               legend = list(orientation = 'h', y = -0.2),
                               xaxis = list(title = "")
                             )
    })
    
    m <- list(l = 50,
              r = 50,
              b = 50,
              t = 50)
  }
})
