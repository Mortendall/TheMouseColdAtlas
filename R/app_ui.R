#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bslib::page_navbar(id = "inTabset",
                       theme = bslib::bs_theme(version= 5),
      bslib::nav_panel(title = "Home",
                       mod_Home_ui("Home_1"),
                       value = "Home"
      ),
      bslib::nav_panel(title = "Thermoneutrality",
                       mod_Thermoneutrality_ui("Thermoneutrality_1"),
                       value = "thermo"
                       ),
      bslib::nav_panel(title = "Acute Cold",
                       mod_Acute_Cold_ui("Acute_Cold_1"),
                       value = "acute"
      ),
      bslib::nav_panel(title = "Chronic Cold",
                       mod_Chronic_Cold_ui("Chronic_Cold_1"),
                       value = "chronic"

    ),
    bslib::nav_panel(title = "Cold Memory",
                     mod_Cold_Memory_ui("Cold_Memory_1"),
                     value = "memory"),
    bslib::nav_panel(title= "Heat Map Creator",
                     mod_heatmapper_ui("heatmapper_1"),
                     value = "heatmap"),
    bslib::nav_panel(title = "Dataset information",
                    mod_data_set_information_ui("data_set_information_1"),
                    value = "infotab")
  ))
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "TheMouseColdAtlas"
    ),
    tags$head(
      tags$style(HTML("
    .dataTable thead th {
      background-color: #000;
      color: #fff;
    }
  "))
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )

}
