#' Acute_Cold UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Acute_Cold_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      title = "Gene expression at Acute Cold",
      bslib::layout_column_wrap(width = 1/4,
                                selectizeInput(
                                  inputId = ns("gene_1"),
                                  label = "",
                                  choices = NULL,
                                  selected = NULL,
                                  multiple = T
                                )
      ),

      bslib::layout_columns(
        shiny::column(width = 12,
                      plotly::plotlyOutput(outputId = ns("cpm_thermo"))
        )
      )

    ))
}

#' Acute_Cold Server Functions
#'
#' @noRd
mod_Acute_Cold_server <- function(id,data,parentsession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #####create choices for select####
    choices <- NULL
    observe(if (is.null(choices)){
      choices <- data$choices
      shiny::updateSelectizeInput(session,
                                  "gene_1",
                                  choices = choices,
                                  selected = "Tbc1d4",
                                  server = TRUE)
    })


    #####Render CPM plot####
    output$cpm_thermo <- plotly::renderPlotly({
      req(!is.null(input$gene_1[1]))
      display_data <- data$cpmcold|>
        duckplyr::filter(magrittr::is_in(SYMBOL, input$gene_1) & (Temperature == "TN"|Temperature == "Cold_Acute"))



      #sometimes genes are only present in a subset of tissues. Identify those
      #and fill with NA


      metadata_tn <- data$metadata |>
        duckplyr::filter(Temperature == "TN"|Temperature == "Cold_Acute")

      display_data <- tissue_fill(display_data,
                                  metadata_tn,
                                  data$tissueorder)

      color_palette <- viridisLite::turbo(length(unique(display_data$SYMBOL)))


      plotly::plot_ly(data = display_data,
                      x = ~Tissue,
                      y = ~CPM,
                      color = ~Temperature,
                      colors = color_palette,
                      type = "box",
                      boxpoints = "all") |>
        suppressWarnings(plotly::layout(title = "CPM expression pr. tissue",
                                        yaxis = list(title = "CPM"),
                                        xaxis = list(title = "Tissue"),
                                        boxmode = "group"))
    })
  })
}

## To be copied in the UI
# mod_Acute_Cold_ui("Acute_Cold_1")

## To be copied in the server
# mod_Acute_Cold_server("Acute_Cold_1",dataobject,parentsession)
