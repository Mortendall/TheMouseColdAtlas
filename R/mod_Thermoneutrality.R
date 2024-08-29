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
      bslib::layout_columns(
        shiny::column(width = 12,
                        selectizeInput(
                          inputId = ns("gene_1"),
                          label = "",
                          choices = NULL,
                          selected = NULL
                      )),
        shiny::column(width = 12,
                      selectizeInput(
                        inputId = ns("gene_2"),
                        label = "",
                        choices = NULL,
                        selected = NULL
                      )),
        shiny::column(width = 12,
                      selectizeInput(
                        inputId = ns("gene_3"),
                        label = "",
                        choices = NULL,
                        selected = NULL
                      )),
        shiny::column(width = 12,
                      selectizeInput(
                        inputId = ns("gene_4"),
                        label = "",
                        choices = NULL,
                        selected = NULL
                      )),
        shiny::column(width = 12,
                      selectizeInput(
                        inputId = ns("gene_5"),
                        label = "",
                        choices = NULL,
                        selected = NULL
                      )),
      ),

      bslib::layout_columns(
      shiny::column(width = 12,
                    plotly::plotlyOutput(outputId = ns("cpm_thermo"))
                    )
                    )

      ),
      bslib::layout_columns(
        column(3,
               bslib::card(bslib::card_header("Set number of genes to display"),
                           bslib::card_body(shinyWidgets::chooseSliderSkin(skin = "Modern",
                                                                    color = "black"),
                                     sliderInput(
                                       inputId = ns("gene_number"),
                                       label = "",
                                       value = 1,
                                       min = 1,
                                       max = 5,
                                       step = 1
                                     )))
                       )
      ))



}

#' Thermoneutrality Server Functions
#'
#' @noRd
mod_Thermoneutrality_server <- function(id,data,parentsession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #lcreate choices for select
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
      display_data <- data$cpmcold|>
        dplyr::filter(SYMBOL == input$gene_1 & Temperature == "TN")

      #sometimes genes are only present in a subset of tissues. Identify those
      #and fill with NA


        metadata_tn <- data$metadata |>
          duckplyr::filter(Temperature == "TN")

        display_data <- tissue_fill(display_data,
                                    metadata_tn,
                                    data$tissueorder)


      plotly::plot_ly(data = display_data,
                      x = ~Tissue,
                      y = ~CPM,
                      color = ~Temperature,
                      colors = c("#DDD596"),
                      type = "box",
                      boxpoints = "all") |>
        plotly::layout(title = "CPM expression pr. tissue",
                       yaxis = list(title = "CPM"),
                       xaxis = list(title = "Tissue"))
    })


  })
}

## To be copied in the UI
# mod_Thermoneutrality_ui("Thermoneutrality_1")

## To be copied in the server
# mod_Thermoneutrality_server("Thermoneutrality_1",dataobject,parentsession)
