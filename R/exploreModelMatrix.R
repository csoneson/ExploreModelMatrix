#' Explore model matrix
#'
#' Given a sample table and a design formula, explore the resulting design
#' matrix graphically in a shiny app.
#'
#' @param sampleData A \code{data.frame} with sample information.
#' @param designFormula A \code{formula}. All terms must be present as columns
#'   in \code{sampleData}.
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @return A Shiny app object
#'
#' @examples
#' \dontrun{
#' exploreModelMatrix(
#'   sampleData = data.frame(genotype = rep(c("A", "B"), each = 4),
#'                           treatment = rep(c("treated", "untreated"), 4),
#'                           stringsAsFactors = FALSE),
#'   designFormula = ~genotype + treatment
#' )
#' }
#'
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#'   dashboardBody menuItem box
#' @importFrom shiny uiOutput numericInput fluidRow column reactiveValues
#'   reactive renderUI fileInput observeEvent isolate textInput plotOutput
#'   shinyApp icon renderPlot tagList selectInput
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom utils read.delim
#' @importFrom cowplot plot_grid
#' @importFrom methods is
#'
exploreModelMatrix <- function(sampleData = NULL, designFormula = NULL) {
  ## ----------------------------------------------------------------------- ##
  ## Check input arguments
  ## ----------------------------------------------------------------------- ##
  if (!is.null(sampleData) && !methods::is(sampleData, "data.frame")) {
    stop("'sampleData' must be a data.frame")
  }

  if (!is.null(designFormula) && !methods::is(designFormula, "formula")) {
    stop("'designFormula' must be a formula")
  }

  ## ----------------------------------------------------------------------- ##
  ## Define layout
  ## ----------------------------------------------------------------------- ##
  p_layout <-
    shinydashboard::dashboardPage(
      skin = "purple",

      ## ------------------------------------------------------------------- ##
      ## Header
      ## ------------------------------------------------------------------- ##
      shinydashboard::dashboardHeader(
        title = "Design matrix visualization",
        titleWidth = 350
      ),

      ## ------------------------------------------------------------------- ##
      ## Inputs
      ## ------------------------------------------------------------------- ##
      shinydashboard::dashboardSidebar(
        width = 250,

        shiny::uiOutput("choose_sampledata_file"),
        shiny::uiOutput("choose_design_formula"),

        shinydashboard::menuItem(
          "Choose reference levels", icon = shiny::icon("bookmark"),
          shiny::uiOutput("reflevels")
        ),

        shinydashboard::menuItem(
          "Settings", icon = shiny::icon("paint-brush"),
          shiny::numericInput(inputId = "plot_height",
                              label = "Plot height (numeric, in pixels)",
                              value = 400, min = 200, max = 3000, step = 10),
          shiny::checkboxInput(inputId = "flipcoord",
                               label = "Flip coordinates",
                               value = FALSE),
          shiny::numericInput(inputId = "textsize",
                              label = "Text size, matrix entries",
                              value = 5, min = 1, max = 25, step = 1),
          shiny::numericInput(inputId = "textsizelabs",
                              label = "Text size, axis labels",
                              value = 12, min = 1, max = 25, step = 1),
          shiny::numericInput(inputId = "linewidth",
                              label = "Maximal row length",
                              value = 25, min = 1, max = 100, step = 1)
        )
      ),

      ## ------------------------------------------------------------------- ##
      ## Outputs
      ## ------------------------------------------------------------------- ##
      shinydashboard::dashboardBody(
        shiny::fluidRow(
          shiny::column(8, shinydashboard::box(
            width = NULL, status = "info",
            title = "Predicted values",
            shiny::uiOutput("plot_design"))
          ),
          shiny::column(4, shinydashboard::box(
            width = NULL, status = "warning",
            title = "Predicted values",
            DT::dataTableOutput("table_sampledata")))
        ),

        shiny::fluidRow(
          shiny::column(7, shinydashboard::box(
            width = NULL, title = "Full sample table",
            collapsible = TRUE, collapsed = TRUE,
            DT::dataTableOutput("table_full"))),
          shiny::column(5, shinydashboard::box(
            width = NULL, title = "Sample table summary",
            collapsible = TRUE, collapsed = TRUE,
            shiny::verbatimTextOutput("table_summary")))
        )
      )
    )

  options(shiny.maxRequestSize = 15*1024^2)

  ## ----------------------------------------------------------------------- ##
  ## Define server function
  ## ----------------------------------------------------------------------- ##
  server_function <- function(input, output, session) {

    ## --------------------------------------------------------------------- ##
    ## Initialize data storage
    ## --------------------------------------------------------------------- ##
    values <- shiny::reactiveValues()
    values$sampledata <- NULL

    ## --------------------------------------------------------------------- ##
    ## Define sample data file if sampleData is not provided
    ## --------------------------------------------------------------------- ##
    if (is.null(sampleData)) {
      output$choose_sampledata_file <- shiny::renderUI({
        shiny::fileInput(inputId = "sampledatasel",
                         label = "Load sample data file",
                         accept = c("text/tab-separated-values", "text/plain",
                                    ".tsv", ".tab", ".txt"),
                         multiple = FALSE)
      })
    } else {
      values$sampledata <- sampleData
    }

    ## --------------------------------------------------------------------- ##
    ## Load sample data file
    ## --------------------------------------------------------------------- ##
    shiny::observeEvent(input$sampledatasel, {
      cdt <- utils::read.delim(input$sampledatasel$datapath, header = TRUE,
                               as.is = TRUE, sep = "\t", quote = "",
                               check.names = FALSE)
      values$sampledata <- cdt
    })

    ## TODO: Set the levels of factors in values$sampledata rather than in visualizeDesign()

    ## --------------------------------------------------------------------- ##
    ## Define inputs to choose reference levels
    ## --------------------------------------------------------------------- ##
    output$reflevels <- renderUI({
      if (is.null(values$sampledata)) {
        NULL
      } else {
        do.call(shiny::tagList,
                lapply(colnames(values$sampledata), function(i) {
                  vct <- values$sampledata[, i]
                  if (is.character(vct) || is.factor(vct)) {
                    shiny::selectInput(inputId = paste0(i, "_ref"),
                                       label = i,
                                       choices = unique(vct),
                                       selected = levels(factor(vct))[1],
                                       multiple = FALSE,
                                       selectize = TRUE)
                  } else {
                    NULL
                  }
                }))
      }
    })

    ## --------------------------------------------------------------------- ##
    ## Define input to specify design formula
    ## --------------------------------------------------------------------- ##
    output$choose_design_formula <- renderUI({
      if (!is.null(designFormula)) {
        shiny::textInput("designformula", "Design formula",
                         paste(as.character(designFormula), collapse = ""))
      } else {
        shiny::textInput("designformula", "Design formula", "")
      }
    })

    ## --------------------------------------------------------------------- ##
    ## Generate output
    ## --------------------------------------------------------------------- ##
    generated_output <- shiny::reactive({
      if (is.null(values$sampledata) || input$designformula == "") {
        return(list(sampledata = NULL, designformula = NULL))
      } else {
        ## TODO: Can we send only the relevant parts of input to refLevels? Or,
        ## rather, we should not send refLevels to visualizeDesign at all, they
        ## should be set in the app.
        return(visualizeDesign(sampleData = values$sampledata,
                               designFormula = input$designformula,
                               flipCoord = input$flipcoord,
                               textSize = input$textsize,
                               textSizeLabs = input$textsizelabs,
                               lineWidth = input$linewidth,
                               refLevels = input))
      }
    })

    ## --------------------------------------------------------------------- ##
    ## Generate sample data table
    ## --------------------------------------------------------------------- ##
    output$table_sampledata <- DT::renderDataTable({
      if (is.null(generated_output()$sampledata)) {
        NULL
      } else {
        DT::datatable(generated_output()$sampledata,
                      options = list(scrollX = TRUE),
                      rownames = FALSE)
      }
    })

    ## --------------------------------------------------------------------- ##
    ## Generate sample data table summary
    ## --------------------------------------------------------------------- ##
    output$table_summary <- shiny::renderPrint({
      if (is.null(values$sampledata)) {
        NULL
      } else {
        summary(values$sampledata)
      }
    })

    ## --------------------------------------------------------------------- ##
    ## Generate full sample data table
    ## --------------------------------------------------------------------- ##
    output$table_full <- DT::renderDataTable({
      if (is.null(values$sampledata)) {
        NULL
      } else {
        DT::datatable(values$sampledata,
                      options = list(scrollX = TRUE),
                      rownames = FALSE)
      }
    })

    ## --------------------------------------------------------------------- ##
    ## Generate design matrix plot
    ## --------------------------------------------------------------------- ##
    output$plot_design_plot <- shiny::renderPlot({
      if (is.null(generated_output()$plotlist)) {
        NULL
      } else {
        cowplot::plot_grid(plotlist = generated_output()$plotlist,
                           ncol = 1)
      }
    })

    output$plot_design <- shiny::renderUI({
      shiny::plotOutput("plot_design_plot",
                        width = "100%",
                        height = paste0(input$plot_height, "px"))
    })
  }

  ## ----------------------------------------------------------------------- ##
  ## Generate app
  ## ----------------------------------------------------------------------- ##
  shiny::shinyApp(ui = p_layout, server = server_function)
}
