#' Explore model matrix
#'
#' Given a sample data table and a design formula, explore the resulting design
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
#'   dashboardBody menuItem
#' @importFrom shiny uiOutput numericInput fluidRow column reactiveValues
#'   reactive renderUI fileInput observeEvent isolate textInput plotOutput
#'   shinyApp icon renderPlot
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom utils read.delim
#' @importFrom cowplot plot_grid
#'
exploreModelMatrix <- function(sampleData = NULL, designFormula = NULL) {
  p_layout <-
    shinydashboard::dashboardPage(
      skin = "purple",

      shinydashboard::dashboardHeader(
        title = "Design matrix visualization",
        titleWidth = 350
      ),

      shinydashboard::dashboardSidebar(
        width = 300,
        shiny::uiOutput("choose_sampledata_file"),
        shiny::uiOutput("choose_design_formula"),
        shinydashboard::menuItem(
          "Settings", icon = shiny::icon("paint-brush"),
          shiny::numericInput(inputId = "plot_height",
                              label = "Plot height (numeric, in pixels)",
                              value = 400, min = 200, max = 3000, step = 10),
          shiny::checkboxInput(inputId = "flipcoord",
                               label = "Flip coordinates",
                               value = FALSE),
          shiny::numericInput(inputId = "textsize",
                              label = "Text size",
                              value = 5, min = 1, max = 25, step = 1),
          shiny::numericInput(inputId = "linewidth",
                              label = "Maximal line width",
                              value = 25, min = 1, max = 100, step = 1)
        )
      ),

      shinydashboard::dashboardBody(
        shiny::fluidRow(
          shiny::column(8, shiny::uiOutput("plot_design")),
          shiny::column(4, DT::dataTableOutput("table_sampledata"))
        )
      )
    )

  options(shiny.maxRequestSize = 15*1024^2)

  server_function <- function(input, output, session) {
    values <- shiny::reactiveValues()
    values$sampledata <- NULL

    if (is.null(sampleData)) {
      output$choose_sampledata_file <- shiny::renderUI({
        shiny::fileInput(inputId = "sampledatasel", label = "Load sample data file",
                  accept = c("text/tab-separated-values", "text/plain",
                             ".tsv", ".tab", ".txt"),
                  multiple = FALSE)
      })
    } else {
      values$sampledata <- sampleData
    }

    shiny::observeEvent(input$sampledatasel, {
      cdt <- utils::read.delim(input$sampledatasel$datapath, header = TRUE,
                               as.is = TRUE, sep = "\t", quote = "",
                               check.names = FALSE)
      shiny::isolate(values$sampledata <- cdt)
    })

    output$choose_design_formula <- renderUI({
      if (!is.null(designFormula)) {
        shiny::textInput("designformula", "Design formula",
                         paste(as.character(designFormula), collapse = ""))
      } else {
        shiny::textInput("designformula", "Design formula", "")
      }
    })

    generateOutput <- shiny::reactive({
      if (is.null(values$sampledata) || input$designformula == "") {
        return(list(sampledata = NULL, designformula = NULL))
      } else {
        return(visualizeDesign(sampleData = values$sampledata,
                               designFormula = input$designformula,
                               flipCoord = input$flipcoord,
                               textSize = input$textsize,
                               lineWidth = input$linewidth))
      }
    })

    output$table_sampledata <- DT::renderDataTable({
      if (is.null(generateOutput()$sampledata)) {
        NULL
      } else {
        DT::datatable(generateOutput()$sampledata,
                      options = list(scrollX = TRUE),
                      rownames = FALSE)
      }
    })

    output$plot_design_plot <- shiny::renderPlot({
      if (is.null(generateOutput()$plotlist)) {
        NULL
      } else {
        cowplot::plot_grid(plotlist = generateOutput()$plotlist,
                           ncol = 1)
      }
    })
    output$plot_design <- shiny::renderUI({
      shiny::plotOutput("plot_design_plot",
                        width = "100%",
                        height = paste0(input$plot_height, "px"))
    })
  }

  shiny::shinyApp(ui = p_layout, server = server_function)
}
