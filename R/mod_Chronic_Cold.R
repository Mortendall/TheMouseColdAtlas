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
    bslib::page_fluid(
      title = "Gene expression at Chronic Cold",
      bslib::layout_column_wrap(width = 1/4,
                                selectizeInput(
                                  inputId = ns("gene_1"),
                                  label = "",
                                  choices = NULL,
                                  selected = NULL,
                                  multiple = T
                                )
      ),

      bslib::layout_columns(
        shiny::column(width = 12,
                      uiOutput(outputId = ns("plotlyrender"))
        )
      ),
      bslib::layout_column_wrap(width = 1/2,
                                bslib::card(shinyWidgets::downloadBttn(outputId = ns("downloadCPM"),
                                                                       label = "Download data for first selected gene"),
                                            DT::DTOutput(outputId = ns("statstable"))),
                                bslib::card(shiny::uiOutput(outputId = ns("tissuevolcano")))
      )

    ))
}

#' Chronic_Cold Server Functions
#'
#' @noRd
mod_Chronic_Cold_server <- function(id,data,parentsession){
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
      req(input$gene_1)
      display_data <- data$cpmcold|>
        duckplyr::filter(magrittr::is_in(SYMBOL, input$gene_1) & (Temperature == "TN"|Temperature == "Cold_Long"))


      metadata_tn <- data$metadata |>
        duckplyr::filter(Temperature == "TN"|Temperature == "Cold_Long")

      display_data <- tissue_fill(display_data,
                                  metadata_tn,
                                  data$tissueorder)
      #We check if there is more than one gene selected
      #if one gene is selected, we just plot a plotly output
      if(length(input$gene_1)==1){
        plotly::plot_ly(data = display_data,
                        x = ~Tissue,
                        y = ~CPM,
                        color = ~Temperature,
                        colors =  c("#DDD596", "#313878"),
                        type = "box",
                        boxpoints = "all") |>
          plotly::layout(title = "CPM expression pr. tissue",
                         yaxis = list(title = "CPM"),
                         xaxis = list(title = "Tissue"),
                         boxmode = "group")
      }

      else{
        #if more than one gene is selected, we make a number of plots based on
        #selected genes
        plot_colors <- c("#DDD596", "#313878")
        plotly_list <-plotly_list_generator(display_data,
                                            input$gene_1,
                                            plot_colors)
        plotly::subplot(plotly_list,
                        nrows = length(plotly_list),
                        shareX = T)

      }
    })
    #calculate height of plotly area based on number of genes selected
    output$plotlyrender <- renderUI({
      plotly::plotlyOutput(outputId = ns("cpm_thermo"),
                           height = height_calculator(input$gene_1))
    })

    #####UI for volcano graph####
    output$tissuevolcano <- shiny::renderUI({
      shiny::tagList(
        shiny::selectInput(inputId = ns("tissueselect"),
                           label = "Pick a tissue to highlight",
                           choices = data$tissueorder$Tissue,
                           selected = "BAT"),
        shinyWidgets::radioGroupButtons(inputId = ns("plottype"),
                                        label = "Select plot type",
                                        choices = c("Volcano", "MA-plot")),
        #MA-plot is an added feature - so most IDs still refer to volcano
        shiny::actionButton(inputId = ns("volcanotrigger"),
                            label = "Plot volcano for tissue"),
        plotly::plotlyOutput(outputId = ns("plotlyvolcano")),
        shiny::uiOutput(ns("volcanodownloads"))
      )
    })

    #UI for download buttons for volcano plot
    output$volcanodownloads <- shiny::renderUI({
      shiny::tagList(
        shiny::downloadButton(outputId = ns("downloadDE"),
                              label = "Download DE data for target tissue"),
        shiny::downloadButton(outputId = ns("pdfdownload"),
                              label = "Download plot as PDF")
      )
    })

    #####Output for plotly volcano####
    shiny::observeEvent(input$volcanotrigger,{
      data$gene <- input$gene_1
      data$tissue <- input$tissueselect
    })

    #Plot
    output$plotlyvolcano <- plotly::renderPlotly({
      req(data$gene)

      if(input$plottype=="Volcano"){
        volcano_plotter(data$decold,
                        data$gene,
                        data$tissue,
                        "cold_vs_TN")
      }

      else if(input$plottype == "MA-plot"){
        ma_plotter(data$decold,
                   data$gene,
                   data$tissue,
                   "cold_vs_TN")
      }


    })
    #####Download DE data as XLSX####
    output$downloadDE <-  shiny::downloadHandler(

      filename = function(){
        paste0(input$tissueselect, "_DE_Long.xlsx")
      },
      content = function(file){
        filedata <- subset(data$decold,
                           Tissue == input$tissueselect & comparison == "cold_vs_TN") |>
          dplyr::mutate("-log10FDR"=-log10(FDR))
        writexl::write_xlsx(x = filedata, path = file)
      }



    )
    #####Download volcano plot as PDF####
    output$pdfdownload <-  shiny::downloadHandler(



      filename = function(){
        if(input$plottype == "Volcano"){
          paste0(input$tissueselect,"_",data$gene, "_volcano_long_cold_vs_TN.pdf")
        }
        else if (input$plottype == "MA-plot"){
          paste0(input$tissueselect,"_",data$gene, "_MA_plot_long_cold_vs_TN.pdf")
        }

      },
      content = function(file){
        if(input$plottype == "Volcano"){
          target_plot <- pdf_creater(data$decold, data$gene, input$tissueselect, "cold_vs_TN")

          grDevices::pdf(file, width = 16, height = 8)
          plot(target_plot)
          grDevices::dev.off()
        }

        else if (input$plottype == "MA-plot"){
          target_plot <- pdf_creater_MA(data$decold, data$gene, input$tissueselect, "cold_vs_TN")

          grDevices::pdf(file, width = 16, height = 8)
          plot(target_plot)
          grDevices::dev.off()
        }

      }
    )

    #####Code for table with stats####
    output$statstable <- DT::renderDT({
      req(data$decold)
      req(input$tissueselect)

      DEdata <- DTdataprep(data$decold,
                           input$gene_1,
                           input$tissueselect,
                           data$tissueorder,
                           "cold_vs_TN")
      DEdata
    })

    #####download CPM data####

    output$downloadCPM <-  shiny::downloadHandler(

      filename = function(){
        paste0(input$gene_1[1], "_chronic.xlsx")
      },
      content = function(file){

        prismtransformed <- prism_transformer(data$cpmcold,
                                              input$gene_1,
                                              "Cold_Long")
        rawdata <- data$cpmcold |>
          duckplyr::filter(SYMBOL == input$gene_1[1] &
                             (Temperature== "TN"|Temperature == "Cold_Long"))

        filedata <- list(Rawdata = rawdata,
                         PrismMode = prismtransformed)

        writexl::write_xlsx(x = filedata, path = file)

      })
  })
}

## To be copied in the UI
# mod_Chronic_Cold_ui("Chronic_Cold_1")

## To be copied in the server
# mod_Chronic_Cold_server("Chronic_Cold_1",dataobject,parentsession)
