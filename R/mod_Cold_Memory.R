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
    bslib::page_fluid(
      title = "Gene expression at Cold Memory",
      bslib::layout_column_wrap(width = 1/4,
                                selectizeInput(
                                  inputId = ns("gene_1"),
                                  label = "Select your genes of interest",
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
                                                                       label = "Download data for first selected gene")
                                            ,
                                            uiOutput(outputId = ns("ensembllink"))
                                            ,DT::DTOutput(outputId = ns("statstable"))),
                                bslib::card(shiny::uiOutput(outputId = ns("tissuevolcano")))
      ),
      bslib::layout_column_wrap(width = 1 / 2,
                                tagList(
                                  bslib::card(
                                    shiny::h5("Secretion:"),
                                    # bslib::tooltip(
                                    #   bsicons::bs_icon("info-circle"),
                                    #   "Data obtained from",
                                    #   tags$a("COMPARTMENTS.",
                                    #          href = "https://compartments.jensenlab.org/Downloads"),
                                    #   "Scores indicate certainty of location."
                                    # ),
                                    bslib::card_body(uiOutput(outputId = ns("localizationui")))

                                  )
                                ))

    ))
}

#' Cold_Memory Server Functions
#'
#' @noRd
mod_Cold_Memory_server <- function(id,data,parentsession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
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
        duckplyr::filter(magrittr::is_in(SYMBOL, input$gene_1) & (Temperature == "TN"|Temperature == "Cold_Memory"))


      metadata_tn <- data$metadata |>
        duckplyr::filter(Temperature == "TN"|Temperature == "Cold_Memory")

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
                        colors =  c("#DDD596", "#7A2C25"),
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
        plot_colors <- c("#DDD596", "#7A2C25")
        plotly_list <-plotly_list_generator(display_data,
                                            input$gene_1,
                                            plot_colors)
        plotly::subplot(plotly_list,
                        nrows = length(plotly_list),
                        shareX = F)

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
                        "cold_TN_vs_TN")
      }

      else if(input$plottype == "MA-plot"){
        ma_plotter(data$decold,
                   data$gene,
                   data$tissue,
                   "cold_TN_vs_TN")
      }


    })
    #####Download DE data as XLSX####
    output$downloadDE <-  shiny::downloadHandler(

      filename = function(){
        paste0(input$tissueselect, "_DE_acute.xlsx")
      },
      content = function(file){
        filedata <- subset(data$decold,
                           Tissue == input$tissueselect & comparison == "cold_TN_vs_TN") |>
          dplyr::mutate("-log10FDR"=-log10(FDR))
        writexl::write_xlsx(x = filedata, path = file)
      }



    )
    #####Download volcano plot as PDF####
    output$pdfdownload <-  shiny::downloadHandler(



      filename = function(){
        if(input$plottype == "Volcano"){
          paste0(input$tissueselect,"_",data$gene, "_volcano_cold_memory_vs_TN.pdf")
        }
        else if (input$plottype == "MA-plot"){
          paste0(input$tissueselect,"_",data$gene, "_MA_plot_cold_memory_vs_TN.pdf")
        }

      },
      content = function(file){
        if(input$plottype == "Volcano"){
          target_plot <- pdf_creater(data$decold, data$gene, input$tissueselect, "cold_TN_vs_TN")

          grDevices::pdf(file, width = 16, height = 8)
          plot(target_plot)
          grDevices::dev.off()
        }

        else if (input$plottype == "MA-plot"){
          target_plot <- pdf_creater_MA(data$decold, data$gene, input$tissueselect, "cold_TN_vs_TN")

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
                           "cold_TN_vs_TN")
      DEdata
    })

    #####download CPM data####
    output$downloadCPM <-  shiny::downloadHandler(

      filename = function(){
        paste0(input$gene_1[1], "_memory.xlsx")
      },
      content = function(file){

        prismtransformed <- prism_transformer(data$cpmcold,
                                              input$gene_1,
                                              "Cold_Memory")

        rawdata <- data$cpmcold |>
          duckplyr::filter(SYMBOL == input$gene_1[1] &
                             (Temperature== "TN"|Temperature == "Cold_Memory"))

        filedata <- list(Rawdata = rawdata,
                         PrismMode = prismtransformed)

        writexl::write_xlsx(x = filedata, path = file)

      })

    output$ensembllink <- shiny::renderUI({
      req(input$gene_1)
      shiny::tagList(
        shiny::img(src = "www/opentargets.JPG",height = "10%", width = "8%"),
        ncbi_url_generator(target_gene = input$gene_1[1],
                           gene_database = data$ensembl,
                           ensembl_converter = data$ensembl_converter)
      )

    })

    output$localizationui <- shiny::renderUI({
      req(input$gene_1)
      if(magrittr::is_in(input$gene_1[1], data$localization$Gene)==F){
        shiny::h6(paste0(input$gene_1[1], " is not found in the localization dataset"))
      }
      else{
        shiny::tagList(shiny::h6("Predicted secretion status, based on human protein atlas"),
                       shiny::tableOutput(outputId = ns("localizationtable")))
      }
      # if(magrittr::is_in(input$gene_1[1], data$localization$SYMBOL)==F){
      #   shiny::h6(paste0(input$gene_1[1], " is not found in the localization dataset"))
      # }
      # else{
      #   DT::dataTableOutput(outputId = ns("localizationtable"))
      # }
    })

    output$localizationtable <- shiny::renderTable({
      req(input$gene_1)
      location_table <-secretome_preparer(input$gene_1,
                                          data$localization,
                                          data$ensembl_converter)
      location_table
    })
  })
}

## To be copied in the UI
# mod_Cold_Memory_ui("Cold_Memory_1")

## To be copied in the server
# mod_Cold_Memory_server("Cold_Memory_1",dataobject,parentsession)
