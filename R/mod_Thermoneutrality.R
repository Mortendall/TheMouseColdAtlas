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
      bslib::layout_column_wrap(width = 1 / 4,
                                selectizeInput(
                                  inputId = ns("gene_1"),
                                  label = "Select your genes of interest",
                                  choices = NULL,
                                  selected = NULL,
                                  multiple = T
                                )
      ),

      bslib::layout_columns(
      shiny::column(width = 12,
                    bslib::card(plotly::plotlyOutput(outputId = ns("cpm_thermo")))
                    )
                    ),
      bslib::layout_column_wrap(width = 1 / 2,
                                tagList(
                                  bslib::card(
                                    shiny::uiOutput(outputId = ns("ensembllink")),
                                    shiny::h5("Secretion"),
                                    # bslib::tooltip(bsicons::bs_icon("info-circle"),
                                    #                "Data obtained from",
                                    #                tags$a("COMPARTMENTS.",
                                    #                       href = "https://compartments.jensenlab.org/Downloads"),
                                    #                "Scores indicate certainty of location."),
                                    bslib::card_body(uiOutput(outputId = ns("localizationui")))

                                  )
                                ))

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
        dplyr::filter(magrittr::is_in(SYMBOL, input$gene_1) & Temperature == "TN")



      #sometimes genes are only present in a subset of tissues. Identify those
      #and fill with NA


        metadata_tn <- data$metadata |>
          dplyr::filter(Temperature == "TN")

        display_data <- tissue_fill(display_data,
                                    metadata_tn,
                                    data$tissueorder)

        color_palette <- viridisLite::turbo(length(unique(display_data$SYMBOL)))
        color_palette[1]<- "#DDD596"


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
    output$ensembllink <- shiny::renderUI({
      req(input$gene_1)
      ncbi_url_generator(target_gene = input$gene_1[1],
                         gene_database = data$ensembl,
                         ensembl_converter = data$ensembl_converter)

    })

    output$localizationui <- shiny::renderUI({
      req(input$gene_1)
      if(magrittr::is_in(input$gene_1[1], data$localization$Gene)==F){
        shiny::h6(paste0(input$gene_1[1], " is not found in the localization dataset"))
      }
      else{
        shiny::tagList(shiny::h6("Predicted secretion status, based on human protein atlas"),
                       shiny::tableOutput(outputId = ns("localizationtable")))
      }
      # if(magrittr::is_in(input$gene_1[1], data$localization$SYMBOL)==F){
      #   shiny::h6(paste0(input$gene_1[1], " is not found in the localization dataset"))
      # }
      # else{
      #   DT::dataTableOutput(outputId = ns("localizationtable"))
      # }
    })

    output$localizationtable <- shiny::renderTable({
      req(input$gene_1)
      location_table <-secretome_preparer(input$gene_1,
                                          data$localization,
                                          data$ensembl_converter)
      location_table
    })
    #   DT::renderDataTable({
    #   req(input$gene_1)
    #   localization_table <- data$localization |>
    #     dplyr::filter(SYMBOL == input$gene_1[1]) |>
    #     dplyr::distinct(Term, .keep_all = T) |>
    #     DT::datatable()
    # })

  })
}

## To be copied in the UI
# mod_Thermoneutrality_ui("Thermoneutrality_1")

## To be copied in the server
# mod_Thermoneutrality_server("Thermoneutrality_1",dataobject,parentsession)
