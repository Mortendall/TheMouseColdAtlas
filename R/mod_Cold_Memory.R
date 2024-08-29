#' Cold_Memory UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Cold_Memory_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' Cold_Memory Server Functions
#'
#' @noRd
mod_Cold_Memory_server <- function(id,data,parentsession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Cold_Memory_ui("Cold_Memory_1")

## To be copied in the server
# mod_Cold_Memory_server("Cold_Memory_1",dataobject,parentsession)
