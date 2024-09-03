#' Home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Home_ui <- function(id){
  ns <- NS(id)



  tagList(
    tags$head(
      tags$style(HTML("
      .card {
        background-color: rgba(255, 255, 255, 0.5);
        border: none;
      }
      .bttn {
        background-color: rgba(255, 255, 255, 0.5);
        border: none;
        color: black;
      }
    "))),
    shinyWidgets::setBackgroundImage(src = "www/Mouse_tracks_in_snow.jpg"),
    bslib::layout_columns(row_heights = c(1,2,1),
                          col_widths = c(3,6,3),
                    column(12),
                    tagList(column(12),
                            bslib::card(
                                tagList(h3("Welcome to the Mouse Cold Atlas"),
                                        h5("Explore the multi-organ regulation
                                           of your favorite gene, in response to
                                           temperature challenges:"),
                                        shinyWidgets::actionBttn(ns("thermo"),
                                                                 label = "Thermoneutrality"),
                                        shinyWidgets::actionBttn(ns("acute"),
                                                                 label = "Acute Cold"),
                                        shinyWidgets::actionBttn(ns("chronic"),
                                                                 label = "Chronic Cold"),
                                        shinyWidgets::actionBttn(ns("memory"),
                                                                 label = "Cold Memory"),
                                        shinyWidgets::actionBttn(ns("infotab"),
                                                                 label = "Dataset information")),

                                align = "center"),
                            column(12)),
                    column(12))

  )
}

#' Home Server Functions
#'
#' @noRd
mod_Home_server <- function(id, data, parentsession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #Load in data when the app launches
    observe(if (is.null(data$cpmcold)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Loading data...", value  = 0)
      data$choices <- readRDS(here::here("data/choicescombined.rds"))
      progress$inc(1 / 3, detail = "Loading CPM summary data...")
      data$cpmcold <-
        arrow::read_parquet(here::here("data/cpmcoldcombined.parquet"))
      data$cpmcold$Temperature <- factor(data$cpmcold$Temperature,
                                         c("TN", "Cold_Acute", "Cold_Long", "Cold_Memory"))

      progress$inc(2 / 3, detail = "Loading DE data...!")
      data$decold <-
        arrow::read_parquet(here::here("data/combined_dedata.parquet"))
      data$tissueorder <-
        readRDS(here::here("data/tissueorder2.rds"))
      data$tissueorder <- data$tissueorder |>
        duckplyr::mutate(
          Tissue = dplyr::case_when(
            Tissue == "Small.intestine" ~ "SmallIntestine",
            Tissue == "Large.intestine" ~ "LargeIntestine",
            .default = as.character(Tissue)
          )
        )
      data$metadata <- read_parquet(here::here("data/metadata.parquet"))
      progress$inc(3 / 3, detail = "Loading Done!")
    })


    #####Update tabset to analysis of interest####

    observeEvent(input$thermo,{
      updateTabsetPanel(session = parentsession,
                        inputId = "inTabset",
                        selected = "thermo"
      )
    })

    observeEvent(input$acute,{
      updateTabsetPanel(session = parentsession,
                        inputId = "inTabset",
                        selected = "acute")
    })

    observeEvent(input$chronic,{
      updateTabsetPanel(session = parentsession,
                        inputId = "inTabset",
                        selected = "chronic")
    })

    observeEvent(input$memory,{
      updateTabsetPanel(session = parentsession,
                        inputId = "inTabset",
                        selected = "memory")
    })

    observeEvent(input$infotab,{
      updateTabsetPanel(session = parentsession,
                        inputId = "inTabset",
                        selected = "infotab")
    })

  })
}

## To be copied in the UI
# mod_Home_ui("Home_1")

## To be copied in the server
# mod_Home_server("Home_1")
