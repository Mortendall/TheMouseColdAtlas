#' Chronic_Cold UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Chronic_Cold_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' Chronic_Cold Server Functions
#'
#' @noRd
mod_Chronic_Cold_server <- function(id,data,parentsession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Chronic_Cold_ui("Chronic_Cold_1")

## To be copied in the server
# mod_Chronic_Cold_server("Chronic_Cold_1",dataobject,parentsession)
