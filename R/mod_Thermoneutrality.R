#' Thermoneutrality UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Thermoneutrality_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      title = "Gene expression at Thermoneutrality",
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

#' Thermoneutrality Server Functions
#'
#' @noRd
mod_Thermoneutrality_server <- function(id,data,parentsession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #####create choices for select####
    choices <- NULL
    observe(if (is.null(choices)){
      choices <- data$choices
      shiny::updateSelectizeInput(session,
                                  "gene_1",
                                  choices = choices,
                                  selected = "Ppargc1a",
                                  server = TRUE)
    })


    #####Render CPM plot####
    output$cpm_thermo <- plotly::renderPlotly({
      req(!is.null(input$gene_1[1]))
      display_data <- data$cpmcold|>
        duckplyr::filter(magrittr::is_in(SYMBOL, input$gene_1) & Temperature == "TN")



      #sometimes genes are only present in a subset of tissues. Identify those
      #and fill with NA


        metadata_tn <- data$metadata |>
          duckplyr::filter(Temperature == "TN")

        display_data <- tissue_fill(display_data,
                                    metadata_tn,
                                    data$tissueorder)

        color_palette <- viridisLite::turbo(length(unique(display_data$SYMBOL)))


      plotly::plot_ly(data = display_data,
                      x = ~Tissue,
                      y = ~CPM,
                      color = ~SYMBOL,
                      colors = color_palette,
                      type = "box",
                      boxpoints = "all") |>
        plotly::layout(title = "CPM expression pr. tissue",
                       yaxis = list(title = "CPM"),
                       xaxis = list(title = "Tissue"),
                       boxmode = "group")
    })




  })
}

## To be copied in the UI
# mod_Thermoneutrality_ui("Thermoneutrality_1")

## To be copied in the server
# mod_Thermoneutrality_server("Thermoneutrality_1",dataobject,parentsession)
