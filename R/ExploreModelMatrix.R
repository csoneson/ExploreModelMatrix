#' Explore model matrix
#'
#' Given a sample table and a design formula, explore the resulting design
#' matrix graphically in an interactive application.
#'
#' @param sampleData (optional) A \code{data.frame} or \code{DataFrame}
#'   with sample information. If set to \code{NULL}, the user can upload
#'   the sample information from a tab-separated text file inside the app, or
#'   choose among a collection of example designs provided in the app.
#' @param designFormula (optional) A \code{formula}. All components of
#'   the terms must be present as columns in \code{sampleData}. If set to
#'   \code{NULL}, the design formula can be specified after launching the app.
#'
#' @author Charlotte Soneson, Federico Marini, Michael I Love, Florian Geier,
#'   Michael B Stadler
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
#'   need validate span
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom utils read.delim packageVersion
#' @importFrom cowplot plot_grid
#' @importFrom methods is
#' @importFrom stats model.matrix as.formula relevel cov2cor
#' @importFrom dplyr mutate_if mutate
#' @importFrom rintrojs introjs introjsUI
#' @importFrom scales hue_pal
#' @importFrom ggplot2 ggplot aes geom_raster theme_bw theme labs
#'   scale_fill_gradient2 geom_text scale_x_discrete scale_y_discrete geom_bar
#'   coord_flip scale_fill_manual element_blank element_text
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom limma nonEstimable is.fullrank
#' @importFrom MASS fractions
#' @importFrom S4Vectors DataFrame
#'
ExploreModelMatrix <- function(sampleData = NULL, designFormula = NULL) {

  # Check input arguments -----------------------------------------------------
  if (!is.null(sampleData) && !(methods::is(sampleData, "data.frame") ||
                                methods::is(sampleData, "DataFrame"))) {
    stop("'sampleData' must be a data.frame or DataFrame")
  }

  if (methods::is(sampleData, "DataFrame")) {
    sampleData <- methods::as(sampleData, "data.frame")
  }

  if (!is.null(designFormula) && !methods::is(designFormula, "formula")) {
    stop("'designFormula' must be a formula")
  }

  if (any(is.na(sampleData))) {
    stop("'sampleData' can not contain NA values")
  }

  lmText <- shiny::span(
    icon("warning"),
    paste0("Note that the content of this panel is ",
           "particularly useful for interpreting regular ",
           "linear models fit with a least squares approach.")
  )

  # UI definition -------------------------------------------------------------
  p_layout <-
    shinydashboard::dashboardPage(
      skin = "purple",

      # Header definition ---------------------------------------------------
      header = shinydashboard::dashboardHeader(
        title = paste0("Design matrix visualization (ExploreModelMatrix v",
                       utils::packageVersion("ExploreModelMatrix"), ")"),
        titleWidth = 600,
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

      # Sidebar definition --------------------------------------------------
      sidebar = shinydashboard::dashboardSidebar(
        width = 300,

        shiny::uiOutput("choose_sampledata_file"),
        shiny::uiOutput("choose_design_formula"),
        shiny::uiOutput("use_example_design"),

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
          startExpanded = TRUE, id = "settings",
          shinydashboard::menuItem(
            "Fitted values plot", startExpanded = TRUE,
            shiny::numericInput(inputId = "plotheight_fitted",
                                label = "Plot height (numeric, in pixels)",
                                value = 400, min = 200, max = 3000, step = 10),
            shiny::checkboxInput(inputId = "flipcoord_fitted",
                                 label = "Flip coordinates",
                                 value = FALSE),
            shiny::numericInput(inputId = "textsize_fitted",
                                label = "Text size, matrix entries",
                                value = 5, min = 1, max = 25, step = 1),
            shiny::numericInput(inputId = "textsizelabs_fitted",
                                label = "Text size, axis labels",
                                value = 12, min = 1, max = 25, step = 1),
            shiny::checkboxInput(inputId = "colorterms_fitted",
                                 label = "Color terms",
                                 value = TRUE),
            shiny::numericInput(inputId = "linewidth_fitted",
                                label = "Maximal row length",
                                value = 25, min = 1, max = 100, step = 1)
          ),
          shinydashboard::menuItem(
            "Pseudoinverse plot", startExpanded = FALSE,
            shiny::numericInput(inputId = "plotheight_pinv",
                                label = "Plot height (numeric, in pixels)",
                                value = 400, min = 200, max = 3000, step = 10),
            shiny::numericInput(inputId = "textsize_pinv",
                                label = "Text size, matrix entries",
                                value = 5, min = 1, max = 25, step = 1),
            shiny::numericInput(inputId = "textsizelabs_pinv",
                                label = "Text size, axis labels",
                                value = 12, min = 1, max = 25, step = 1),
            shiny::radioButtons(inputId = "shownumbers_pinv",
                                label = "Display numbers",
                                choices = c("Do not show numbers",
                                            "Show numbers as decimal",
                                            "Show numbers as fractions"),
                                selected = "Show numbers as decimal")
          ),
          shinydashboard::menuItem(
            "Co-occurrence plot", startExpanded = FALSE,
            shiny::numericInput(inputId = "plotheight_coocc",
                                label = "Plot height (numeric, in pixels)",
                                value = 400, min = 200, max = 3000, step = 10),
            shiny::checkboxInput(inputId = "flipcoord_coocc",
                                 label = "Flip coordinates",
                                 value = FALSE),
            shiny::numericInput(inputId = "textsize_coocc",
                                label = "Text size, matrix entries",
                                value = 5, min = 1, max = 25, step = 1),
            shiny::numericInput(inputId = "textsizelabs_coocc",
                                label = "Text size, axis labels",
                                value = 12, min = 1, max = 25, step = 1)
          ),
          shinydashboard::menuItem(
            "Correlation plot", startExpanded = FALSE,
            shiny::numericInput(inputId = "plotheight_corr",
                                label = "Plot height (numeric, in pixels)",
                                value = 400, min = 200, max = 3000, step = 10),
            shiny::numericInput(inputId = "textsize_corr",
                                label = "Text size, matrix entries",
                                value = 5, min = 1, max = 25, step = 1),
            shiny::numericInput(inputId = "textsizelabs_corr",
                                label = "Text size, axis labels",
                                value = 12, min = 1, max = 25, step = 1),
            shiny::radioButtons(inputId = "shownumbers_corr",
                                label = "Display numbers",
                                choices = c("Do not show numbers",
                                            "Show numbers as decimal"),
                                selected = "Show numbers as decimal")
          )
        )
      ),

      # Body definition -----------------------------------------------------
      body = shinydashboard::dashboardBody(
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
            12, shiny::uiOutput("rank_warning")
          )
        ),

        shiny::fluidRow(
          shiny::column(
            12, shiny::uiOutput("resdf_warning")
          )
        ),

        shiny::fluidRow(
          shinydashboard::tabBox(
            id = "fitted_values_box",
            title = "Fitted values",
            side = "right",
            width = 12, height = NULL,
            shiny::tabPanel("Plot", shiny::uiOutput("fitted_values_plot")),
            shiny::tabPanel("Table", DT::dataTableOutput("fitted_values_table"))
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
                shiny::radioButtons(
                  inputId = "design_matrix_type",
                  label = "Display as:",
                  choices = c("data table", "R output"),
                  selected = "R output", inline = TRUE
                ),
                shiny::uiOutput("design_matrix")
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
                paste0("Residual degrees of freedom (number of ",
                       "observations - rank of design matrix): "),
                shiny::textOutput("design_matrix_resdf")
              )
            )
          )
        ),

        shiny::fluidRow(
          shiny::column(
            8, shiny::div(
              id = "pinv_design_matrix_box",
              shinydashboard::box(
                width = NULL, title = "Pseudoinverse of design matrix",
                collapsible = TRUE, collapsed = TRUE,
                footer = lmText,
                shiny::uiOutput("pinv_design_matrix")
              )
            )
          ),
          shiny::column(
            4, shiny::div(
              id = "vifs_box",
              shinydashboard::box(
                width = NULL, title = "Variance inflation factors",
                collapsible = TRUE, collapsed = TRUE,
                footer = lmText,
                shiny::plotOutput("vifs"),
                shiny::uiOutput("rank_warning_2")
              )
            )
          )
        ),

        shiny::fluidRow(
          shiny::column(
            6, shiny::div(
              id = "cooccurrence_matrix_box",
              shinydashboard::box(
                width = NULL, title = "Co-occurrence plot",
                collapsible = TRUE, collapsed = TRUE,
                shiny::uiOutput("cooccurrence_matrix")
              )
            )
          ),
          shiny::column(
            6, shiny::div(
              id = "correlation_matrix_box",
              shinydashboard::box(
                width = NULL, title = "Correlation plot",
                collapsible = TRUE, collapsed = TRUE,
                footer = lmText,
                shiny::uiOutput("correlation_matrix")
              )
            )
          )
        )

      )
    )

  options(shiny.maxRequestSize = 15*1024^2)

  # Server definition -------------------------------------------------------
  #nocov start
  server_function <- function(input, output, session) {

    # Initialize data storage -----------------------------------------------
    values <- shiny::reactiveValues()
    values$sampledata <- NULL
    values$sampledata_ext <- NULL

    # Define sample data file if sampleData is not provided -----------------
    if (is.null(sampleData)) {
      output$choose_sampledata_file <- shiny::renderUI({
        shiny::fileInput(inputId = "sampledatasel",
                         label = "Load sample data file (tab-separated)",
                         accept = c("text/tab-separated-values", "text/plain",
                                    ".tsv", ".tab", ".txt"),
                         multiple = FALSE)
      })
    } else {
      values$sampledata <- sampleData %>%
        dplyr::mutate_if(is.character, factor)
      values$sampledata_ext <- sampleData %>%
        dplyr::mutate_if(is.character, factor)
    }

    # Load sample data file -------------------------------------------------
    shiny::observeEvent(input$sampledatasel, {
      cdt <- utils::read.delim(input$sampledatasel$datapath, header = TRUE,
                               as.is = FALSE, sep = "\t", quote = "",
                               check.names = FALSE)
      values$sampledata <- cdt
      values$sampledata_ext <- cdt
    })

    # Define input to specify design formula --------------------------------
    output$choose_design_formula <- renderUI({
      if (!is.null(designFormula)) {
        shiny::textInput("designformula", "Design formula",
                         paste(as.character(designFormula), collapse = ""))
      } else {
        shiny::textInput("designformula", "Design formula", "")
      }
    })

    # Define input to choose example design ---------------------------------
    output$use_example_design <- renderUI({
      shiny::selectInput(
        inputId = "exampledesign",
        label = "Use example design",
        choices = c("---", "One factor, unpaired samples",
                    "One factor, paired samples",
                    "Two crossed factors",
                    "Two crossed, one blocking factor",
                    "Two crossed, one nested factor",
                    "Two crossed, one nested factor, dummy coded",
                    "Two crossed, one nested factor (manuscript example)"),
        selectize = TRUE, multiple = FALSE
      )
    })

    # Populate variables if example design is used --------------------------
    observeEvent(input$exampledesign, {
      if (input$exampledesign == "---") {
        values$sampledata <- values$sampledata_ext
      } else {
        ed <- .ExampleDesigns(exampleID = input$exampledesign)
        values$sampledata <- ed$sampledata
        shiny::updateTextInput(session, "designformula", value = ed$design)
      }
    })

    # Define inputs to choose reference levels ------------------------------
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

    # Set reference levels of factors ---------------------------------------
    shiny::observe({
      for (nm in colnames(values$sampledata)) {
        if (!is.null(input[[paste0(nm, "_ref")]]) &&
            input[[paste0(nm, "_ref")]] %in%
            levels(factor(values$sampledata[, nm])) &&
            input[[paste0(nm, "_ref")]] !=
            levels(factor(values$sampledata[, nm]))[1]) {
          values$sampledata[, nm] <- stats::relevel(
            factor(values$sampledata[, nm]), ref = input[[paste0(nm, "_ref")]]
          )
        }
      }
    })

    # Define input to drop columns in design matrix -------------------------
    output$dropcols <- renderUI({
      if (is.null(values$sampledata) || is.null(input$designformula) ||
          input$designformula == "" ||
          !(.IsValidFormula(as.formula(input$designformula),
                            values$sampledata))) {
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

    # Generate output -------------------------------------------------------
    generated_output <- shiny::reactive({
      if (is.null(values$sampledata) || is.null(input$designformula) ||
          input$designformula == "") {
        return(list())
      } else {
        vd <- VisualizeDesign(sampleData = values$sampledata,
                              designFormula = input$designformula,
                              flipCoordFitted = input$flipcoord_fitted,
                              flipCoordCoocc = input$flipcoord_coocc,
                              textSizeFitted = input$textsize_fitted,
                              textSizeCoocc = input$textsize_coocc,
                              textSizeLabsFitted = input$textsizelabs_fitted,
                              textSizeLabsCoocc = input$textsizelabs_coocc,
                              lineWidthFitted = input$linewidth_fitted,
                              addColorFitted = input$colorterms_fitted,
                              dropCols = input$dropcols,
                              colorPaletteFitted = scales::hue_pal(),
                              designMatrix = NULL)
        shiny::updateNumericInput(session, "plotheight_fitted",
                                  value = vd$totnbrrows * 120)
        return(vd)
      }
    })

    # Generate sample data table --------------------------------------------
    output$fitted_values_table <- DT::renderDataTable({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          paste0("Please provide a formula where all terms appear in ",
                 "the sample data")
        )
      )
      if (is.null(generated_output()$sampledata)) {
        NULL
      } else {
        DT::datatable(generated_output()$sampledata,
                      options = list(scrollX = TRUE),
                      rownames = FALSE)
      }
    })

    # Generate sample data table summary ------------------------------------
    output$sample_table_summary <- shiny::renderPrint({
      if (is.null(values$sampledata)) {
        NULL
      } else {
        summary(values$sampledata)
      }
    })

    # Generate design matrix ------------------------------------------------
    output$design_matrix_R <- shiny::renderPrint({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          paste0("Please provide a formula where all terms appear in ",
                 "the sample data")
        )
      )
      generated_output()$designmatrix
    })

    output$design_matrix_DT <- DT::renderDataTable({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          paste0("Please provide a formula where all terms appear in ",
                 "the sample data")
        )
      )
      DT::datatable(data.frame(generated_output()$designmatrix,
                               check.names = FALSE),
                    options = list(scrollX = TRUE),
                    rownames = TRUE)
    })

    output$design_matrix <- shiny::renderUI({
      if (input$design_matrix_type == "R output") {
        shiny::verbatimTextOutput("design_matrix_R")
      } else if (input$design_matrix_type == "data table") {
        DT::dataTableOutput("design_matrix_DT")
      } else {
        stop("Unknown design matrix display type")
      }
    })

    # Plot design matrix pseudoinverse --------------------------------------
    output$pinv_design_matrix_plot <- shiny::renderPlot({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          paste0("Please provide a formula where all terms appear in ",
                 "the sample data")
        )
      )
      if (is.null(generated_output()$pseudoinverse)) {
        NULL
      } else {
        tmp <- as.data.frame(generated_output()$pseudoinverse)
        tmp$coefficient <- rownames(tmp)
        tmp <- tmp %>%
          tidyr::gather(key = "Sample", value = "value", -coefficient) %>%
          dplyr::mutate(Sample = factor(Sample, levels = colnames(
            generated_output()$pseudoinverse)),
            coefficient = factor(coefficient, levels = rev(rownames(
              generated_output()$pseudoinverse)))
          )
        if (input$shownumbers_pinv == "Show numbers as fractions") {
          tmp$value <- MASS::fractions(tmp$value)
        } else {
          tmp$value <- round(tmp$value, digits = 4)
        }
        gg <- ggplot2::ggplot(tmp,
                              ggplot2::aes(x = Sample,
                                           y = coefficient,
                                           fill = value,
                                           label = value)) +
          ggplot2::geom_tile(color = "black") +
          ggplot2::theme_bw() +
          ggplot2::theme(rect = ggplot2::element_blank(),
                         axis.text = ggplot2::element_text(
                           size = input$textsizelabs_pinv),
                         axis.title = ggplot2::element_text(
                           size = input$textsizelabs_pinv)) +
          ggplot2::scale_fill_gradient2(low = "red", high = "blue",
                                        mid = "white", midpoint = 0,
                                        name = "") +
          ggplot2::scale_x_discrete(expand = c(0, 0)) +
          ggplot2::scale_y_discrete(expand = c(0, 0)) +
          ggplot2::labs(y = "Model coefficient", x = "Sample")
        if (input$shownumbers_pinv != "Do not show numbers") {
          gg <- gg + ggplot2::geom_text(size = input$textsize_pinv)
        }
        gg
      }
    })

    output$pinv_design_matrix <- shiny::renderUI({
      shiny::plotOutput("pinv_design_matrix_plot",
                        width = "100%",
                        height = paste0(input$plotheight_pinv, "px"))
    })

    # Plot correlation among coefficients -----------------------------------
    output$correlation_matrix_plot <- shiny::renderPlot({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          paste0("Please provide a formula where all terms appear in ",
                 "the sample data")
        )
      )
      if (is.null(generated_output()$designmatrix)) {
        NULL
      } else {
        tmp <- generated_output()$designmatrix
        coeffs_to_keep <- colnames(tmp)
        tmp <- t(tmp) %*% tmp
        if (!limma::is.fullrank(tmp)) {
          NULL
        } else {
          cormat <- as.data.frame(stats::cov2cor(solve(tmp)))
          cormat$rows <- rownames(cormat)
          gg <- cormat %>%
            tidyr::gather(key = "cols", value = "correlation", -rows) %>%
            dplyr::mutate(cols = factor(cols, levels = coeffs_to_keep),
                          rows = factor(rows, levels = rev(coeffs_to_keep))) %>%
            dplyr::mutate(correlation = round(correlation, digits = 3)) %>%
            ggplot2::ggplot(ggplot2::aes(x = cols, y = rows, fill = correlation,
                                         label = correlation)) +
            ggplot2::geom_tile(color = "black") +
            ggplot2::theme_bw() +
            ggplot2::theme(
              rect = ggplot2::element_blank(),
              axis.text.x = ggplot2::element_text(
                size = input$textsizelabs_corr,
                angle = 90,
                hjust = 1, vjust = 0.5
              ),
              axis.text.y = ggplot2::element_text(
                size = input$textsizelabs_corr
              ),
              axis.title = ggplot2::element_text(
                size = input$textsizelabs_corr
              )
            ) +
            ggplot2::scale_fill_gradient2(low = "red", high = "blue",
                                          mid = "white", midpoint = 0,
                                          name = "", limits = c(-1, 1)) +
            ggplot2::scale_x_discrete(expand = c(0, 0)) +
            ggplot2::scale_y_discrete(expand = c(0, 0)) +
            ggplot2::labs(y = "Model coefficient", x = "Model coefficient")
          if (input$shownumbers_corr == "Show numbers as decimal") {
            gg <- gg + ggplot2::geom_text(size = input$textsize_corr)
          }
          gg
        }
      }
    })

    output$correlation_matrix <- shiny::renderUI({
      shiny::plotOutput("correlation_matrix_plot",
                        width = "100%",
                        height = paste0(input$plotheight_corr, "px"))
    })

    # Plot variance inflation factors ---------------------------------------
    output$vifs <- shiny::renderPlot({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          paste0("Please provide a formula where all terms appear in ",
                 "the sample data")
        )
      )
      if (is.null(generated_output()$vifs)) {
        NULL
      } else {
        ggplot2::ggplot(generated_output()$vifs,
                        ggplot2::aes(x = coefficient,
                                     y = vif,
                                     fill = coefficient)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::labs(x = "Model coefficient", y = "VIF") +
          ggplot2::theme_bw() +
          ggplot2::coord_flip() +
          ggplot2::scale_y_continuous(expand = c(0, 0, 0.05, 0)) +
          ggplot2::scale_fill_manual(values = generated_output()$colors) +
          ggplot2::theme(legend.position = "none")
      }
    })

    output$rank_warning_2 <- shiny::renderUI({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          paste0("Please provide a formula where all terms appear in ",
                 "the sample data")
        )
      )
      if (is.null(generated_output()$vifs)) {
        shinydashboard::valueBox("", "VIFs could not be calculated",
                                 color = "blue", icon = NULL, width = 4.5)
      } else {
        NULL
      }
    })

    # Check rank and number of columns of design matrix ---------------------
    output$design_matrix_rank <- shiny::renderText({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          paste0("Please provide a formula where all terms appear in ",
                 "the sample data")
        )
      )
      if (is.null(generated_output()$designmatrix)) {
        NULL
      } else {
        qr(generated_output()$designmatrix)$rank
      }
    })

    output$design_matrix_ncol <- shiny::renderText({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          paste0("Please provide a formula where all terms appear in ",
                 "the sample data")
        )
      )
      if (is.null(generated_output()$designmatrix)) {
        NULL
      } else {
        ncol(generated_output()$designmatrix)
      }
    })

    output$design_matrix_resdf <- shiny::renderText({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          paste0("Please provide a formula where all terms appear in ",
                 "the sample data")
        )
      )
      if (is.null(generated_output()$designmatrix)) {
        NULL
      } else {
        nrow(generated_output()$designmatrix) -
          qr(generated_output()$designmatrix)$rank
      }
    })

    output$rank_warning <- shiny::renderUI({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          ""
        )
      )
      if (is.null(generated_output()$designmatrix)) {
        NULL
      } else {
        if (qr(generated_output()$designmatrix)$rank >=
            ncol(generated_output()$designmatrix)) {
          NULL
        } else {
          nonestim <- limma::nonEstimable(generated_output()$designmatrix)
          msg <- paste0("The design matrix is not full rank. ",
                        "Non-estimable parameters: ",
                        paste(nonestim, collapse = ", "))
          shinydashboard::valueBox("", msg,
                                   color = "red", icon = NULL, width = 4.5)
        }
      }
    })

    output$resdf_warning <- shiny::renderUI({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          ""
        )
      )
      if (is.null(generated_output()$designmatrix)) {
        NULL
      } else {
        if (qr(generated_output()$designmatrix)$rank <
            nrow(generated_output()$designmatrix)) {
          NULL
        } else {
          msg <- paste0("The residual degrees of freedom is 0. ",
                        "Values such as variances or dispersions can not be ",
                        "estimated from data with this design.")
          shinydashboard::valueBox("", msg,
                                   color = "red", icon = NULL, width = 4.5)
        }
      }
    })

    # Generate full sample data table ---------------------------------------
    output$sample_table <- DT::renderDataTable({
      if (is.null(values$sampledata)) {
        NULL
      } else {
        DT::datatable(values$sampledata,
                      options = list(scrollX = TRUE),
                      rownames = TRUE)
      }
    })

    # Generate design matrix plot -------------------------------------------
    output$fitted_values_plot_plot <- shiny::renderPlot({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          paste0("Please provide a formula where all terms appear in ",
                 "the sample data")
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
                        height = paste0(input$plotheight_fitted, "px"))
    })

    # Plot cooccurrence matrix ----------------------------------------------
    output$cooccurrence_matrix_plot <- shiny::renderPlot({
      shiny::validate(
        shiny::need(
          input$designformula != "" &&
            .IsValidFormula(as.formula(input$designformula), values$sampledata),
          paste0("Please provide a formula where all terms appear in ",
                 "the sample data")
        )
      )
      if (is.null(generated_output()$cooccurrenceplots)) {
        NULL
      } else {
        cowplot::plot_grid(plotlist = generated_output()$cooccurrenceplots,
                           ncol = 1)
      }
    })

    output$cooccurrence_matrix <- shiny::renderUI({
      shiny::plotOutput("cooccurrence_matrix_plot",
                        width = "100%",
                        height = paste0(input$plotheight_coocc, "px"))
    })

    # Tour ------------------------------------------------------------------
    observeEvent(input$interface_overview, {
      tour <- read.delim(system.file("extdata", "interface_overview.txt",
                                     package = "ExploreModelMatrix"),
                         sep = ";", stringsAsFactors = FALSE,
                         row.names = NULL, quote = "")
      rintrojs::introjs(session, options = list(steps = tour))
    })

  }
  #nocov end

  # Generate app ------------------------------------------------------------
  shiny::shinyApp(ui = p_layout, server = server_function)
}


#' Check whether a design formula is valid
#'
#' Checks whether the object is indeed a formula, and whether all specified
#' factors are present in the experimental metadata provided
#'
#' @param design The specified formula
#' @param expdata The experimental metadata data.frame
#'
#' @return Logical value
#'
#' @keywords internal
#'
#' @rdname INTERNAL_.IsValidFormula
#'
.IsValidFormula <- function(design, expdata) {
  isFormula <- inherits(design, "formula")

  expVars <- all.vars(design)
  allVarsThere <- all(expVars %in% colnames(expdata))

  return(isFormula & allVarsThere)
}

