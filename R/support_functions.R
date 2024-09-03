#' Fill missing values in CPM table
#'
#' @param data_input a data frame with 6 columns "ENSEMBL", "SYMBOL", "Sample",
#'  "CPM" ,"Tissue" and"Temperature"
#' @param metadata a data frame with 3 columns, "Sample", "Tissue" and"Temperature"
#' @param tissueorder a vector
#'
#' @return a data frame with filled missing values

tissue_fill <- function(data_input, metadata, tissueorder){
  #identify ensembl and symbol
  if(length(data_input$ENSEMBL)==0){
    return(data_input)
  }
  else{

      ENSEMBL <-
        duckplyr::distinct(data_input, ENSEMBL) |> duckplyr::pull()
      Gene_name <-
        duckplyr::distinct(data_input, SYMBOL) |> duckplyr::pull()
      if (length(ENSEMBL) == 1) {
      #identify missing tissues from dataset
      missing_samples<- metadata |>
        duckplyr::filter(!magrittr::is_in(Sample, data_input$Sample)) |>
        #create columns with NA
        duckplyr::mutate(ENSEMBL = ENSEMBL,
                         SYMBOL = Gene_name,
                         CPM = NA) |>
        #select columns in correct order
        duckplyr::select(colnames(data_input))

      #create data_input with added rows
      output_data <- duckplyr::rows_append(data_input,
                                           missing_samples)
      output_data$Tissue <- factor(output_data$Tissue,
                                   levels = c(tissueorder$Tissue))

      #sometimes all values are NA and then plotly removes them. Identify.

      summary_stats <- output_data |>
        dplyr::group_by(Tissue) |>
        duckplyr::summarise(sum = sum(CPM, na.rm = T)) |>
        duckplyr::filter(sum == 0) |>
        duckplyr::pull(Tissue)

      if (length(summary_stats) != 0) {
        output_data <- output_data |>
          duckplyr::mutate(CPM =
                             duckplyr::case_when(magrittr::is_in(Tissue, summary_stats) ~
                                                   0,
                                                 .default = CPM))
      }


  return(output_data)}
      else{
        #create combinations of ENSEMBL and Gene_name
        key <- data.frame(ENSEMBL = ENSEMBL,
                          SYMBOL = Gene_name)

        #split the dataset in subdata
        subset_by_gene <-
          purrr::map(key$ENSEMBL, ~duckplyr::filter(data_input, ENSEMBL==.x))

        #identify missing samples pr. gene
        missing_samples <- purrr::map(subset_by_gene,~duckplyr::filter(metadata, !magrittr::is_in(Sample,
                                                                                 .x$Sample)))

        missing_samples <- map2(missing_samples,
                                subset_by_gene,
                                ~duckplyr::mutate(.x, ENSEMBL = .y$ENSEMBL[1],
                                                    SYMBOL = .y$SYMBOL[1],
                                                    CPM = NA))

        #select data in right order
        missing_samples <- map(missing_samples,
                               ~duckplyr::select(.x, colnames(data_input)))

        #append each dataset

        subset_by_gene <- map2(subset_by_gene,
                               missing_samples,
                              ~duckplyr::rows_append(.x,
                                                        .y))


        output_data <- duckplyr::bind_rows(subset_by_gene)



        output_data$Tissue <- factor(output_data$Tissue,
                                     levels = c(tissueorder$Tissue))

        #sometimes all values are NA and then plotly removes them. Identify.

        summary_stats <- output_data |>
          dplyr::group_by(Tissue) |>
          duckplyr::summarise(sum = sum(CPM, na.rm = T)) |>
          duckplyr::filter(sum == 0) |>
          duckplyr::pull(Tissue)

        if (length(summary_stats) != 0) {
          output_data <- output_data |>
            duckplyr::mutate(CPM =
                               duckplyr::case_when(magrittr::is_in(Tissue, summary_stats) ~
                                                     0,
                                                   .default = CPM))
        }
        return(output_data)
      }
  }
}


#'  Plotly list generator
#'
#' @param data_input a data frame with CPM values
#' @param gene_input a vector of input genes
#'
#' @return a list containing plotly graphs

plotly_list_generator <- function(data_input, gene_input){
  #detect how many genes we have in gene_input
  plotly_list <- vector(mode = "list",
                      length = length(gene_input))


  names(plotly_list)<- gene_input

for (gene in gene_input){
    subset_data <- data_input |> duckplyr::filter(SYMBOL == gene)
    if(isTRUE(gene_input[gene]==gene_input[1])){
      p <- plotly::plot_ly(
        data = subset_data,
        x = ~ Tissue,
        y = ~ CPM,
        color = ~ Temperature,
        colors =  c("#DDD596", "#0F99B2"),
        type = "box",
        boxpoints = "all"
      ) |>
        plotly::layout(
          #title = gene,
          yaxis = list(title = "CPM"),
          xaxis = list(title = "Tissue"),
          boxmode = "group"
        ) |>
        add_annotations(
          text = gene,
          x = 0.5,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "middle",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15)
        )
      plotly_list[[gene]]<-p
    }
    else{
      p <- plotly::plot_ly(
        data = subset_data,
        x = ~ Tissue,
        y = ~ CPM,
        color = ~ Temperature,
        colors =  c("#DDD596", "#0F99B2"),
        type = "box",
        boxpoints = "all"
      ) |>
        plotly::layout(
          #title = gene,
          yaxis = list(title = "CPM"),
          xaxis = list(title = "Tissue"),
          boxmode = "group"
        ) |>
        add_annotations(
          text = gene,
          x = 0.5,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "middle",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15)
        )
      plotly_list[[gene]]<-p
    }
}

  return(plotly_list)

}

#' Heigth generator that expands plot area with number of graphs selected
#'
#' @param gene_input a vector of gene names
#'
#' @return a vector of the form "XXXpx" based on length of input vector

height_calculator <- function(gene_input){
  height <- 400*length(gene_input)
  text <- paste(height, "px",sep = "")
  return(text)
}

#' Volcano Plotter - plot volcano plot based on input parameters
#'
#' @param input_data a data frame with dedata
#' @param input_genes vector of genes to highlight
#' @param tissue character of tissue from pre-defined list
#' @param comparison what statistical comparison to display
#'
#' @return
#' @export
#'
#' @examples
volcano_plotter <- function(input_data, input_genes, tissue, target_comparison){
  m <- input_data |>
    duckplyr::filter(magrittr::is_in(SYMBOL, input_genes)&
                       Tissue == tissue &
                       comparison == target_comparison)


  plotdata <- input_data |>
    duckplyr::filter(Tissue == tissue &
                       comparison == target_comparison)  |>
    dplyr::mutate(is_in = dplyr::case_when(magrittr::is_in(SYMBOL, input_genes) ~ "Y",
                                           TRUE ~ "N"))

  if(any(!magrittr::is_in(input_genes, plotdata$SYMBOL))){
    shinyWidgets::sendSweetAlert(title = "Data not found in DE list",
                                 text = "One of your target genes is not found
                                         in the DE analysis result file. It was
                                         likely filtered out due to low
                                         expression",
                                 type = "warning")
  }
  else{
    p <- plotly::plot_ly(data = plotdata,
                    x = ~logFC,
                    y = ~-log10(FDR),
                    color = ~is_in,
                    type = "scatter",
                    mode = "markers",
                    text = ~SYMBOL,
                    hovertemplate = paste('Gene: <b>%{text}</b>',
                                          '<br>-log10 FDR: %{y}<br>',
                                          'logFC: %{x}',
                                          '<extra></extra>')) |>
      plotly::add_annotations(x = m$logFC,
                              y = -log10(m$FDR),
                              text = m$SYMBOL,
                              xref = "x",
                              yref = "y",
                              bgcolor = 'lightgrey') |>
      plotly::layout(title = tissue,
                     xaxis = list(title = "logFC"),
                     yaxis = list(title = "-log10(FDR)"),
                     showlegend = F
      )
    return(p)
  }
}

#' PDF creater - creates a PDF volcano plot
#'
#' @param input_data a data frame with dedata
#' @param selected_tissue character of tissue from pre-defined list
#' @param selected_gene vector of genes to highlight
#' @param selected_comparison what statistical comparison to display
#'
#' @return a plot

pdf_creater <- function(input_data, selected_gene, selected_tissue, selected_comparison){

    m <- input_data |>
      duckplyr::filter(magrittr::is_in(SYMBOL, selected_gene)&
                            Tissue == selected_tissue &
                            comparison == selected_comparison)
    plotdata <- input_data |>
      duckplyr::filter(Tissue == selected_tissue &
                          comparison== selected_comparison) |>
      dplyr::mutate(is_in = dplyr::case_when(
        magrittr::is_in(SYMBOL,selected_gene)~ "Y",
        TRUE ~ "N"
      ))

    target_plot <- ggplot2::ggplot(plotdata,
                                   ggplot2::aes(x = logFC,
                                                y = -log10(FDR)))+

      ggplot2::theme_bw()+
      ggplot2::geom_point(data = subset(plotdata, is_in=="N"), color = "#7dc4a4")+
      ggrepel::geom_label_repel(data = subset(plotdata, is_in == "Y"),
                                ggplot2::aes(x = logFC,
                                             y = -log10(FDR),
                                             label = SYMBOL,
                                             size = 10),
                                force_pull = 0.5,
                                arrow = ggplot2::arrow(length = ggplot2::unit(0.015, "npc"))

      )+
      ggplot2::geom_point(data = subset(plotdata, is_in=="Y"), color = "black")+
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 16),
                     axis.title.y = ggplot2::element_text(size = 16),
                     plot.title =  ggplot2::element_text(size = 24,
                                                         hjust = 0.5),
                     panel.border = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(size = 14),
                     axis.text.y = ggplot2::element_text(size = 14),
                     legend.position = "none")+
      ggplot2::ggtitle(selected_tissue)+
      ggplot2::xlab("logFC")+
      ggplot2::ylab("-log10(FDR)")

    return(target_plot)




}
