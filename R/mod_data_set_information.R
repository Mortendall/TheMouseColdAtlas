#' data_set_information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_set_information_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' data_set_information Server Functions
#'
#' @noRd
mod_data_set_information_server <- function(id,data,parentsession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_data_set_information_ui("data_set_information_1")

## To be copied in the server
# mod_data_set_information_server("data_set_information_1",dataobject,parentsession)
