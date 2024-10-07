#' heatmapper UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_heatmapper_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        bslib::card(
          bslib::tooltip(bsicons::bs_icon("info-circle"),
                         "Select a number of genes to display and press the button
                       below to generate a heatmap."),
          selectizeInput(
            inputId = ns("gene_list"),
            label = "",
            choices = NULL,
            selected = NULL,
            multiple = T
          ),
          shiny::numericInput(inputId = ns("height"),
                              label = "Set row height on heat map",
                              value = 20,
                              min = 0,
                              max = 10),
          shiny::textInput(inputId = ns("plottitle"),
                           label = "Plot title",
                           value = "My fabulous heatmap"),
          shinyWidgets::actionBttn(ns("plot_heatmap"),
                                   label = "Plot heatmap",
                                   style = "jelly"),
          shinyWidgets::downloadBttn(outputId = ns("downloadheatmap"),
                                     label = "Download heatmap")
        )
      ),
      bslib::card(
        plotOutput(outputId = ns("heatmap"))
      )

    )

  )
}

#' heatmapper Server Functions
#'
#' @noRd
mod_heatmapper_server <- function(id, data, parentsession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    choices <- NULL
    observe(if (is.null(choices)){
      choices <- data$choices
      shiny::updateSelectizeInput(session,
                                  "gene_list",
                                  choices = choices,
                                  selected = c("Ucp1","Tbc1d4", "Ppargc1a","Nampt"),
                                  server = TRUE)
    })

    shiny::observeEvent(input$plot_heatmap,{
      if(length(input$gene_list)<=1){
        shinyWidgets::sendSweetAlert(
          title = "Too few genes detected",
          text = "Please select at least two genes for heatmap plotting",
          type = "error"
        )
      }
      else{
      data$heatmap <- heatmapplotter(data_object = data$cpmcold,
                                     input_genes = input$gene_list,
                                     tissueorder = data$tissueorder,
                                     cellheights = input$height,
                                     plottitle = input$plottitle)
      }
      })

    #output for heatmap plot
    output$heatmap <- shiny::renderPlot({
      req(data$heatmap)
      data$heatmap
    })

    #download heatmap
    output$downloadheatmap <- shiny::downloadHandler(

      filename = function(){

        paste0(Sys.Date(),"_heatmap.png")


    },
    content = function(file){
        req(data$heatmap)
      target_plot <- ggplotify::as.ggplot(data$heatmap,scale = 1)

        grDevices::png(file,width = 50, height = 40, units = "cm",res = 300)
        plot(target_plot)
        grDevices::dev.off()
    })


  })
}

## To be copied in the UI
# mod_heatmapper_ui("heatmapper_1")

## To be copied in the server
# mod_heatmapper_server("heatmapper_1")
