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

  )
}

#' Acute_Cold Server Functions
#'
#' @noRd
mod_Acute_Cold_server <- function(id,data,parentsession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Acute_Cold_ui("Acute_Cold_1")

## To be copied in the server
# mod_Acute_Cold_server("Acute_Cold_1",dataobject,parentsession)
