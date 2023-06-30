
library(shiny)
library(shiny.i18n)
library(rwisp)

i18n <- Translator$new(translation_json_path='translations/translation.json')
i18n$set_translation_language('en')

ui <- fluidPage(
  
  usei18n(i18n),
  tags$div(
    style='float: right;',
    selectInput(
      inputId = "selected_language",
      label = "",
      choices = setNames(
        i18n$get_languages(),
        c("English", "Português")
      ),
      selected = i18n$get_key_translation()
    )
  ),  
  titlePanel(i18n$t('WISP Calculator'), windowTitle=NULL),
  tags$div(class="clearfix"),
  
  p(
    i18n$t("Select the spreadsheet to be processed. The file MUST be in CSV format, and you should remember not to use a thousand separator."),
    br(),
    i18n$t("If you have any doubts, "),
    downloadLink("downloadData", i18n$t("download the example file")),
    "."
  ),
  
  fileInput(
    "file",
    i18n$t("Select spreadsheet"),
    buttonLabel = i18n$t("Browse..."),
    #   @BUG: https://github.com/Appsilon/shiny.i18n/issues/122
    #    placeholder = i18n$t("No file selected"),  
    accept = c("text/csv",
               "text/comma-separated-values,text/plain",
               ".csv")
  ),
  
  actionButton("do", i18n$t("Resolve")),
  
  p(verbatimTextOutput("errors")),
  
  conditionalPanel(
    condition = "output.calculated==1",
    p(h3(i18n$t("Ranking Result")), tableOutput("ui")),
    p(h3(i18n$t("Normalized Data")), tableOutput("normalizedData")),
    p(h3(i18n$t("Utility Matrix")), tableOutput("utilities"))
  ),
  
  helpText(
    i18n$t("This implementation follows the article DOI 10.1109/TEM.2021.3075783 however, the set of equations used in step 3 is that of the article DOI 10.3390/axioms10040347, following the recommendations of Professor Dragisa Stanujkić."),
    br(),
    i18n$t("The code is open at"),
    a(
      href = "https://github.com/dioubernardo/rwisp/",
      "https://github.com/dioubernardo/rwisp/",
      download = NA,
      target = "_blank"
    ),
    i18n$t("and"),
    a(
      href = "https://github.com/dioubernardo/rwispweb/",
      "https://github.com/dioubernardo/rwispweb/",
      download = NA,
      target = "_blank"
    )
  )
)

server <- function(input, output, session) {
  output$calculated <- reactive(0)
  outputOptions(output, "calculated", suspendWhenHidden = FALSE)
  
  observeEvent(input$do, {
    tryCatch({
      output$errors <- NULL
      output$calculated <- reactive(0)
      
      if (is.null(input$file))
        stop("Select a file")
      
      result <- rwispfromcsv(input$file$datapath)
      
      colnames(result$ui) <- c(i18n$t('Position'), 'ui')
      
      output$ui <- renderTable({
        result$ui[,2] <- formatC(result$ui[,2], digits = 3)
        result$ui[,1] <- formatC(result$ui[,1], digits = 0)
        result$ui
      }, rownames = TRUE, align = 'lrr')
      
      output$normalizedData <-
        renderTable(result$normalizedData,
                    rownames = TRUE,
                    digits = 3)
      
      colnames(result$utilities) <-
        c('uiwsd',
          'uiwpd',
          'uiwsr',
          'uiwpr',
          'ūiwsd',
          'ūiwpd',
          'ūiwsr',
          'ūiwpr')
      output$utilities <-
        renderTable({
          result$utilities[,] <- formatC(result$utilities[,], digits = 4)
          result$utilities
        }, rownames = TRUE, align = 'lrrrrrrrr')
      
      output$calculated <- reactive(1)
    },
    error = function(err) {
      output$errors <- renderText(i18n$t(err[["message"]]))
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = "example.csv",
    content = function(con) {
      file.copy("example.csv", con)
    }
  )
  
  observeEvent(input$selected_language, {
    update_lang(input$selected_language)
  })
  
}

shinyApp(ui, server)
