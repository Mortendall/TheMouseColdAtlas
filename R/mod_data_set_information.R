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
                      bslib::page_fluid(
                      shiny::h2("Information about the app, the setup and the data"),
                        bslib::accordion(id = "cohortinfo",
                                       multiple = F,
                                       open = F,
                                        bslib::accordion_panel(title = "How to cite", shiny::HTML("[Insert reference to paper].<br>
                                                               If you use data from the app please include the following statement in the acknowledgement section:
                                                               \"We acknowledge The Single-Cell Omics platform and the Rodent Metabolic Phenotyping Platform
                                                               at the Novo Nordisk Foundation Center for Basic Metabolic Research (CBMR) for their contribution to the Mouse Cold Atlas.\"
                                                               "),
                                                               icon = bsicons::bs_icon("pencil-square")),
                                        bslib::accordion_panel(title = "Cohort Info",
                              "[Lucile fills text in here about cohorts, and how they were ]",
                              icon = bsicons::bs_icon("clipboard")),
                              bslib::accordion_panel(title = "Bioinformatics Analysis",
                                                     "Bioinformatics analysis was performed by Christian Grønbæk,
                                                     Single Cell Omics Platform, at CBMR. Scripts available at [github-url]",
                                                     icon = bsicons::bs_icon("file-code")),
                              bslib::accordion_panel(title = "App design",
                                                     tagList("App UI and server designed by Morten Dall, Platform Manager at Rodent
                                                     Metabolic Phenotyping Platform at CBMR. Scripts available at", tags$a("GitHub", href = "https://github.com/Mortendall/TheMouseColdAtlas")),
                                                     icon = bsicons::bs_icon("file-bar-graph")),
                              bslib::accordion_panel(title = "About CBMR",
                                                     "The Novo Nordisk Foundation Center for Basic Metabolic Research is an independent research center at the
                                                     University of Copenhagen, partially funded by an unrestricted donation from the Novo Nordisk Foundation (Grant ID	Extended Funding of CBMR 2024–2028)",
                                                     icon = bsicons::bs_icon("house")),
                              bslib::accordion_panel(title = "version info",
                                                     shiny::tagList(shiny::h5("current version: 0.1.1"),
                                                                    shiny::tagList(
                                                                      "1st of November 2024. ",
                                                                      tags$br(),
                                                                      "Added features: ",
                                                                      tags$br(),
                                                                      "- UI update (design)",
                                                                      tags$br(),
                                                                      "- Changed link generated to opentarget instead of ncbi",
                                                                      tags$br(),
                                                                      "-changed location to secretome data from human protein atlas",
                                                                      "9th of Oct 2024.",
                                                                                   tags$br(),
                                                                                   "Added features:",
                                                                                   tags$br(),
                                                                                   "- Heatmap generator", tags$br(),
                                                                                   "- Compartment feature",
                                                                    tags$br(),
                                                                    "- ENSEMBL link feature")),
                                                     icon = bsicons::bs_icon("patch-question"))
    )

    )
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
