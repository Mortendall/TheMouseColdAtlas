#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  dataobject <- shiny::reactiveValues()
  parentsession <- session
  mod_Home_server("Home_1", dataobject, parentsession)
  mod_data_set_information_server("data_set_information_1",dataobject,parentsession)
  mod_Cold_Memory_server("Cold_Memory_1",dataobject,parentsession)
  mod_Chronic_Cold_server("Chronic_Cold_1",dataobject,parentsession)
  mod_Acute_Cold_server("Acute_Cold_1",dataobject,parentsession)
  mod_Thermoneutrality_server("Thermoneutrality_1",dataobject,parentsession)
  mod_heatmapper_server("heatmapper_1", dataobject, parentsession)
}
