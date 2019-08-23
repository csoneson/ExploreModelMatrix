#' Explore model matrix
#'
#' Given a sample table and a design formula, explore the resulting design
#' matrix graphically in a shiny app.
#'
#' @param sampleData A \code{data.frame} with sample information.
#' @param designFormula A \code{formula}. All components of the terms must be
#'   present as columns in \code{sampleData}.
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @return A Shiny app object
#'
#' @examples
#' app <- ExploreModelMatrix(
#'   sampleData = data.frame(genotype = rep(c("A", "B"), each = 4),
#'                           treatment = rep(c("treated", "untreated"), 4)),
#'   designFormula = ~genotype + treatment
#' )
#' if (interactive()) shiny::runApp(app)
#'
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#'   dashboardBody menuItem box valueBox dropdownMenu notificationItem
#' @importFrom shiny uiOutput numericInput fluidRow column reactiveValues
#'   reactive renderUI fileInput observeEvent isolate textInput plotOutput
#'   shinyApp icon renderPlot tagList selectInput checkboxInput
#'   verbatimTextOutput textOutput observe renderPrint actionButton div
#'   need validate
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom utils read.delim
#' @importFrom cowplot plot_grid
#' @importFrom methods is
#' @importFrom stats model.matrix as.formula relevel
#' @importFrom dplyr mutate_if
#' @importFrom rintrojs introjs introjsUI
#' @importFrom scales hue_pal
#' @importFrom pheatmap pheatmap
#'
ExploreModelMatrix <- function(sampleData = NULL, designFormula = NULL) {
  ## ----------------------------------------------------------------------- ##
  ## Check input arguments
  ## ----------------------------------------------------------------------- ##
  if (!is.null(sampleData) && !methods::is(sampleData, "data.frame")) {
    stop("'sampleData' must be a data.frame")
  }

  if (!is.null(designFormula) && !methods::is(designFormula, "formula")) {
    stop("'designFormula' must be a formula")
  }

  if (any(is.na(sampleData))) {
    stop("'sampleData' can not contain NA values")
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
        titleWidth = 350,
        shinydashboard::dropdownMenu(
          type = "tasks",
          icon = shiny::icon("question-circle fa-1g"),
          badgeStatus = NULL,
          headerText = "Documentation",
          shinydashboard::notificationItem(
            text = shiny::actionButton(
              "interface_overview", "Overview of the interface",
              shiny::icon("hand-o-right")
            ),
            icon = shiny::icon(""), # tricking it to not have additional icon
            status = "primary"
          )
        )
      ),

      ## ------------------------------------------------------------------- ##
      ## Inputs
      ## ------------------------------------------------------------------- ##
      shinydashboard::dashboardSidebar(
        width = 250,

        shiny::uiOutput("choose_sampledata_file"),
        shiny::uiOutput("choose_design_formula"),

        shinydashboard::menuItem(
          "Choose reference levels", icon = shiny::icon("anchor"),
          startExpanded = TRUE,
          shiny::uiOutput("reflevels")
        ),

        shinydashboard::menuItem(
          "Drop columns", icon = shiny::icon("trash"),
          startExpanded = TRUE,
          shiny::uiOutput("dropcols")
        ),

        shinydashboard::menuItem(
          "Settings", icon = shiny::icon("sliders-h"),
          startExpanded = TRUE,
          shiny::numericInput(inputId = "plotheight",
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
                              value = 25, min = 1, max = 100, step = 1),
          shiny::checkboxInput(inputId = "colorterms",
                               label = "Color terms",
                               value = TRUE)
        )
      ),

      ## ------------------------------------------------------------------- ##
      ## Outputs
      ## ------------------------------------------------------------------- ##
      shinydashboard::dashboardBody(
        rintrojs::introjsUI(),
        
        ## Define output size and style of error messages
        shiny::tags$head(
          shiny::tags$style(
            shiny::HTML(".shiny-output-error-validation {
                 font-size: 15px;
                 color: forestgreen;
                 text-align: center;
                 }
                 ")
          )
        ),

        shiny::fluidRow(
          shiny::column(
            8, shiny::div(
              id = "fitted_values_plot_box",
              shinydashboard::box(
                width = NULL, status = "primary",
                collapsible = TRUE, collapsed = FALSE,
                title = "Fitted values",
                shiny::uiOutput("fitted_values_plot")
              )
            )
          ),
          shiny::column(
            4, shiny::div(
              id = "fitted_values_table_box",
              shinydashboard::box(
                width = NULL, status = "warning",
                collapsible = TRUE, collapsed = FALSE,
                title = "Fitted values",
                DT::dataTableOutput("fitted_values_table")
              )
            )
          )
        ),

        shiny::fluidRow(
          shiny::column(
            7, shiny::div(
              id = "sample_table_box",
              shinydashboard::box(
                width = NULL, title = "Full sample table",
                collapsible = TRUE, collapsed = TRUE,
                DT::dataTableOutput("sample_table")
              )
            )
          ),
          shiny::column(
            5, shiny::div(
              id = "sample_table_summary_box",
              shinydashboard::box(
                width = NULL, title = "Sample table summary",
                collapsible = TRUE, collapsed = TRUE,
                shiny::verbatimTextOutput("sample_table_summary")
              )
            )
          )
        ),

        shiny::fluidRow(
          shiny::column(
            7, shiny::div(
              id = "design_matrix_box",
              shinydashboard::box(
                width = NULL, title = "Design matrix",
                collapsible = TRUE, collapsed = FALSE,
                shiny::verbatimTextOutput("design_matrix")
              )
            )
          ),
          shiny::column(
            5, shiny::div(
              id = "design_matrix_rank_box",
              shinydashboard::box(
                width = NULL, title = "Rank",
                collapsible = TRUE, collapsed = FALSE,
                "Rank of design matrix: ",
                shiny::textOutput("design_matrix_rank"),
                "Number of columns in design matrix: ",
                shiny::textOutput("design_matrix_ncol"),
                shiny::uiOutput("rank_warning")
              )
            )
          )
        ),

        shiny::fluidRow(
          shiny::column(
            12, shiny::div(
              id = "pinv_design_matrix_box",
              shinydashboard::box(
                width = NULL, title = "Pseudoinverse of design matrix",
                collapsible = TRUE, collapsed = FALSE,
                shiny::plotOutput("pinv_design_matrix")
              )
            )
          )
        )

      )
    )

  options(shiny.maxRequestSize = 15*1024^2)

  ## ----------------------------------------------------------------------- ##
  ## Define server function
  ## ----------------------------------------------------------------------- ##
  #nocov start
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
      values$sampledata <- sampleData %>%
        dplyr::mutate_if(is.character, factor)
    }

    ## --------------------------------------------------------------------- ##
    ## Load sample data file
    ## --------------------------------------------------------------------- ##
    shiny::observeEvent(input$sampledatasel, {
      cdt <- utils::read.delim(input$sampledatasel$datapath, header = TRUE,
                               as.is = FALSE, sep = "\t", quote = "",
                               check.names = FALSE)
      values$sampledata <- cdt
    })

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
    ## Set reference levels of factors
    ## --------------------------------------------------------------------- ##
    shiny::observe({
      for (nm in colnames(values$sampledata)) {
        if (!is.null(input[[paste0(nm, "_ref")]]) &&
            input[[paste0(nm, "_ref")]] !=
            levels(factor(values$sampledata[, nm]))[1]) {
          values$sampledata[, nm] <- stats::relevel(
            factor(values$sampledata[, nm]), ref = input[[paste0(nm, "_ref")]]
          )
        }
      }
    })

    ## --------------------------------------------------------------------- ##
    ## Define input to drop columns in design matrix
    ## --------------------------------------------------------------------- ##
    output$dropcols <- renderUI({
      if (is.null(values$sampledata) || is.null(input$designformula) ||
          input$designformula == "") {
        NULL
      } else {
        mm <- stats::model.matrix(stats::as.formula(input$designformula),
                                  data = values$sampledata)
        shiny::selectInput(inputId = "dropcols",
                           label = "Columns to drop",
                           choices = colnames(mm),
                           selectize = TRUE, multiple = TRUE)
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
      if (is.null(values$sampledata) || is.null(input$designformula) ||
          input$designformula == "") {
        return(list(sampledata = NULL, designformula = NULL,
                    designmatrix = NULL))
      } else {
        return(VisualizeDesign(sampleData = values$sampledata,
                               designFormula = input$designformula,
                               flipCoord = input$flipcoord,
                               textSize = input$textsize,
                               textSizeLabs = input$textsizelabs,
                               lineWidth = input$linewidth,
                               dropCols = input$dropcols,
                               addColor = input$colorterms,
                               colorPalette = scales::hue_pal()))
      }
    })

    ## --------------------------------------------------------------------- ##
    ## Generate sample data table
    ## --------------------------------------------------------------------- ##
    output$fitted_values_table <- DT::renderDataTable({
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
    output$sample_table_summary <- shiny::renderPrint({
      if (is.null(values$sampledata)) {
        NULL
      } else {
        summary(values$sampledata)
      }
    })

    ## --------------------------------------------------------------------- ##
    ## Generate design matrix
    ## --------------------------------------------------------------------- ##
    output$design_matrix <- shiny::renderPrint({
      generated_output()$designmatrix
    })

    ## --------------------------------------------------------------------- ##
    ## Generate design matrix pseudoinverse
    ## --------------------------------------------------------------------- ##
    output$pinv_design_matrix <- shiny::renderPlot({
      pheatmap::pheatmap(generated_output()$pseudoinverse,
                         cluster_rows = FALSE,
                         cluster_cols = FALSE,
                         display_numbers = TRUE)
    })

    ## --------------------------------------------------------------------- ##
    ## Check rank and number of columns of design matrix
    ## --------------------------------------------------------------------- ##
    output$design_matrix_rank <- shiny::renderPrint({
      if (is.null(generated_output()$designmatrix)) {
        NULL
      } else {
        qr(generated_output()$designmatrix)$rank
      }
    })

    output$design_matrix_ncol <- shiny::renderPrint({
      if (is.null(generated_output()$designmatrix)) {
        NULL
      } else {
        ncol(generated_output()$designmatrix)
      }
    })

    output$rank_warning <- shiny::renderUI({
      if (is.null(generated_output()$designmatrix)) {
        NULL
      } else {
        if (qr(generated_output()$designmatrix)$rank >=
            ncol(generated_output()$designmatrix)) {
          NULL
        } else {
          shinydashboard::valueBox("", "The design matrix is not full rank",
                                   color = "red", icon = NULL, width = 4.5)
        }
      }
    })

    ## --------------------------------------------------------------------- ##
    ## Generate full sample data table
    ## --------------------------------------------------------------------- ##
    output$sample_table <- DT::renderDataTable({
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
    output$fitted_values_plot_plot <- shiny::renderPlot({
      shiny::validate(
        shiny::need(
          is.valid.formula(as.formula(input$designformula), values$sampledata),
          "Please provide a formula where factors are all appearing in the experimental metadata"
        )
      )
      
      if (is.null(generated_output()$plotlist)) {
        NULL
      } else {
        cowplot::plot_grid(plotlist = generated_output()$plotlist,
                           ncol = 1)
      }
    })

    output$fitted_values_plot <- shiny::renderUI({
      shiny::plotOutput("fitted_values_plot_plot",
                        width = "100%",
                        height = paste0(input$plotheight, "px"))
    })

    ## --------------------------------------------------------------------- ##
    ## Tour
    ## --------------------------------------------------------------------- ##
    observeEvent(input$interface_overview, {
      tour <- read.delim(system.file("extdata", "interface_overview.txt",
                                     package = "ExploreModelMatrix"),
                         sep = ";", stringsAsFactors = FALSE,
                         row.names = NULL, quote = "")
      rintrojs::introjs(session, options = list(steps = tour))
    })

  }
  #nocov end

  ## ----------------------------------------------------------------------- ##
  ## Generate app
  ## ----------------------------------------------------------------------- ##
  shiny::shinyApp(ui = p_layout, server = server_function)
}


#' Is the formula valid?
#' 
#' Checks whether the object is indeed a formula, and whether all specified 
#' factors are present in the experimental metadata provided
#'iSEE
#' @param design The specified formula
#' @param expdata The experimental metadata data.frame
#'
#' @return Logical value
#' 
#' @rdname INTERNAL_is.valid.formula
is.valid.formula <- function(design, expdata) {
  isFormula <- inherits(design,"formula")
  
  expVars <- all.vars(design)
  allVarsThere <- all(expVars %in% colnames(expdata))
  
  return(isFormula & allVarsThere)
}

