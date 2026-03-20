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
        dplyr::distinct(data_input, ENSEMBL) |> dplyr::pull()
      Gene_name <-
        dplyr::distinct(data_input, SYMBOL) |> dplyr::pull()
      if (length(ENSEMBL) == 1) {
      #identify missing tissues from dataset
      missing_samples<- metadata |>
        dplyr::filter(!magrittr::is_in(Sample, data_input$Sample)) |>
        #create columns with NA
        dplyr::mutate(ENSEMBL = ENSEMBL,
                         SYMBOL = Gene_name,
                         CPM = NA) |>
        #select columns in correct order
        dplyr::select(colnames(data_input))

      #create data_input with added rows
      output_data <- dplyr::rows_append(data_input,
                                           missing_samples)
      output_data$Tissue <- factor(output_data$Tissue,
                                   levels = c(tissueorder$Tissue))

      #sometimes all values are NA and then plotly removes them. Identify.

      summary_stats <- output_data |>
        dplyr::group_by(Tissue) |>
        dplyr::summarise(sum = sum(CPM, na.rm = T)) |>
        dplyr::filter(sum == 0) |>
        dplyr::pull(Tissue)

      if (length(summary_stats) != 0) {
        output_data <- output_data |>
          dplyr::mutate(CPM =
                             dplyr::case_when(magrittr::is_in(Tissue, summary_stats) ~
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
          purrr::map(key$ENSEMBL, ~dplyr::filter(data_input, ENSEMBL==.x))

        #identify missing samples pr. gene
        missing_samples <- purrr::map(subset_by_gene,~dplyr::filter(metadata, !magrittr::is_in(Sample,
                                                                                 .x$Sample)))

        missing_samples <- purrr::map2(missing_samples,
                                subset_by_gene,
                                ~dplyr::mutate(.x, ENSEMBL = .y$ENSEMBL[1],
                                                    SYMBOL = .y$SYMBOL[1],
                                                    CPM = NA))

        #select data in right order
        missing_samples <- purrr::map(missing_samples,
                               ~dplyr::select(.x, colnames(data_input)))

        #append each dataset

        subset_by_gene <- purrr::map2(subset_by_gene,
                               missing_samples,
                              ~dplyr::rows_append(.x,
                                                        .y))


        output_data <- dplyr::bind_rows(subset_by_gene)



        output_data$Tissue <- factor(output_data$Tissue,
                                     levels = c(tissueorder$Tissue))

        #sometimes all values are NA and then plotly removes them. Identify.

        summary_stats <- output_data |>
          dplyr::group_by(Tissue) |>
          dplyr::summarise(sum = sum(CPM, na.rm = T)) |>
          dplyr::filter(sum == 0) |>
          dplyr::pull(Tissue)

        if (length(summary_stats) != 0) {
          output_data <- output_data |>
            dplyr::mutate(CPM =
                               dplyr::case_when(magrittr::is_in(Tissue, summary_stats) ~
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

plotly_list_generator <- function(data_input, gene_input, color_vector, tissueorder){
  #detect how many genes we have in gene_input
  plotly_list <- vector(mode = "list",
                      length = length(gene_input))


  names(plotly_list)<- gene_input

for (gene in gene_input){
    subset_data <- data_input |> dplyr::filter(SYMBOL == gene)
    if(isTRUE(gene==gene_input[1])){
      p <- plotly::plot_ly(
        data = subset_data,
        x = ~ Tissue,
        y = ~ CPM,
        color = ~ Temperature,
        colors =  color_vector,
        type = "box",
        boxpoints = "all"
      ) |>
        plotly::layout(
          #title = gene,
          yaxis = list(title = "CPM"),
          xaxis = list(title = "Tissue",
                       autorange ="min",
                       range = c(-1,19)),
          boxmode = "group",
          legendgroup = ~Temperature
        ) |>
        plotly::add_annotations(
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
        colors =  color_vector,
        type = "box",
        boxpoints = "all",
        alignmentgroup = "group1"
      ) |>
        plotly::layout(
          #title = gene,
          yaxis = list(title = "CPM"),
          xaxis = list(title = "Tissue",
                       autorange ="min",
                       range = c(-1,19)),
          boxmode = "group",
          legendgroup = ~Temperature,
          showlegend = FALSE
        ) |>
        plotly::add_annotations(
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

volcano_plotter <- function(input_data, input_genes, tissue, target_comparison){
  m <- input_data |>
    dplyr::filter(magrittr::is_in(SYMBOL, input_genes)&
                       Tissue == tissue &
                       comparison == target_comparison)


  plotdata <- input_data |>
    dplyr::filter(Tissue == tissue &
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
      dplyr::filter(magrittr::is_in(SYMBOL, selected_gene)&
                            Tissue == selected_tissue &
                            comparison == selected_comparison)
    plotdata <- input_data |>
      dplyr::filter(Tissue == selected_tissue &
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

#' Generate a data.table object for rendering
#'
#' @param input_data a data frame with dedata
#' @param input_gene a vector of gene names
#' @param input_tissue a character input of tissue from tissueorder list
#' @param input_tissueorder a vector of tissues from dataset
#' @param input_comparison a character input of statistical comparison
#'
#' @return a data.table object for rendering

DTdataprep <-
  function(input_data,
                       input_gene,
                       input_tissue,
                       input_tissueorder,
                       input_comparison){
  req(!is.null(input_gene[1]))
  DEdata <- input_data |>
    dplyr::filter(SYMBOL == input_gene[1] & comparison == input_comparison)
  tissueorder <- input_tissueorder|>
    dplyr::filter(magrittr::is_in(input_tissue, DEdata$Tissue)==T)
  DEdata <- DEdata |>
    dplyr::arrange(factor(Tissue, tissueorder$Tissue)) |>
    dplyr::select(-comparison) |>
    DT::datatable(class = 'cell-border-stripes') |>
    DT::formatSignif(columns = c("logFC","PValue","FDR")) |>
    DT::formatRound(columns = c("avg_log2", "logFC"),digits = 2)

  return(DEdata)

}


#' MA plotter function
#'
#' @param input_data a data frame with dedata
#' @param input_genes a vector of gene names
#' @param tissue a character input of tissue from tissueorder list
#' @param target_comparison a character input of statistical comparison
#'
#' @return a plotly object

ma_plotter <- function(input_data, input_genes, tissue, target_comparison){
  m <- input_data |>
    dplyr::filter(magrittr::is_in(SYMBOL, input_genes)&
                       Tissue == tissue &
                       comparison == target_comparison)


  plotdata <- input_data |>
    dplyr::filter(Tissue == tissue &
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
                         x = ~avg_log2,
                         y = ~logFC,
                         color = ~is_in,
                         type = "scatter",
                         mode = "markers",
                         text = ~SYMBOL,
                         hovertemplate = paste('Gene: <b>%{text}</b>',
                                               '<br>log2_avr %{y}<br>',
                                               'logFC: %{x}',
                                               '<extra></extra>')) |>
      plotly::add_annotations(x = m$avg_log2,
                              y = m$logFC,
                              text = m$SYMBOL,
                              xref = "x",
                              yref = "y",
                              bgcolor = 'lightgrey') |>
      plotly::layout(title = tissue,
                     xaxis = list(title = "avg_log2"),
                     yaxis = list(title = "logFC"),
                     showlegend = F
      )
    return(p)
  }
}

#write PDF writer for MA_plot
#' Title
#'
#' @param input_data a data frame with dedata
#' @param selected_gene a vector of gene names
#' @param selected_tissue a character input of tissue from tissueorder list
#' @param selected_comparison a character input of statistical comparison
#'
#' @return a ggplot object

pdf_creater_MA <- function(input_data, selected_gene, selected_tissue, selected_comparison){

  m <- input_data |>
    dplyr::filter(magrittr::is_in(SYMBOL, selected_gene)&
                       Tissue == selected_tissue &
                       comparison == selected_comparison)
  plotdata <- input_data |>
    dplyr::filter(Tissue == selected_tissue &
                       comparison== selected_comparison) |>
    dplyr::mutate(is_in = dplyr::case_when(
      magrittr::is_in(SYMBOL,selected_gene)~ "Y",
      TRUE ~ "N"
    ))

  target_plot <- ggplot2::ggplot(plotdata,
                                 ggplot2::aes(x = avg_log2,
                                              y = logFC))+

    ggplot2::theme_bw()+
    ggplot2::geom_point(data = subset(plotdata, is_in=="N"), color = "#7dc4a4")+
    ggrepel::geom_label_repel(data = subset(plotdata, is_in == "Y"),
                              ggplot2::aes(x = avg_log2,
                                           y = logFC,
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
    ggplot2::xlab("avg_log2")+
    ggplot2::ylab("logFC")

  return(target_plot)
}

#' Prism Transformer
#'
#' @param cpm_data a CPM data frame in long format
#' @param input_gene a vector of genes (only first will be used)
#' @param comparison the group to be used
#'
#' @return a data frame for copy-paste into prism

prism_transformer <- function(cpm_data, input_gene, comparison){
  prismtransformed <-  cpm_data|>
    dplyr::filter(magrittr::is_in(SYMBOL, input_gene[1]) & (Temperature == "TN"|Temperature == comparison))  |>
    #make unique identifiers for groups
    dplyr::group_by(Tissue, Temperature) |>
    dplyr::mutate(Group = paste(Temperature,
                                dplyr::row_number(),
                                sep = "_")) |>
    dplyr::arrange(Group) |>
    dplyr::ungroup() |>
    dplyr::select(CPM, Tissue, Group) |>
    tidyr::pivot_wider(names_from = "Group",
                       values_from = "CPM")
  return(prismtransformed)
}

#' heatmapplotter
#'
#' @param GOobject
#' @param targetrow
#' @param DGElist
#' @param setup
#' @param cellheights
#'
#' @return
#' @export
#'
#' @examples
heatmapplotter <- function(data_object, input_genes,input_tissues, tissueorder, cellheights,
                           cellwidths, plottitle, rowfont, fontsize ){
  #subset genes from input list
  if(is.null(input_genes)==T){
    shinyWidgets::sendSweetAlert(type = "error",
                                 title = "No genes selected",
                                 text = "Please select input genes from menu")
  }
  else{
    #subset data to include data from selection menu
    subset_data <- data_object |>
      dplyr::filter(SYMBOL %in% input_genes)
    #factor setup
    subset_data <- subset_data |>
      dplyr::arrange(SYMBOL = SYMBOL,
                     Tissue = base::factor(Tissue, tissueorder$Tissue),
                      Temperature = base::factor(Temperature, c("TN",
                                                               "Cold_Acute",
                                                               "Cold_Long",
                                                               "Cold_Memory"))) |>
    #prepare unique name for key
    dplyr::mutate(unique_sample = paste(Sample, Tissue, sep = "_"))|>
      dplyr::filter(magrittr::is_in(Tissue, input_tissues))
    #prepare key
    key_data <- as.data.frame(subset_data )|>
      dplyr::distinct(unique_sample, .keep_all = T)
    key_names <- key_data$unique_sample
    key <- key_data |>
      dplyr::select(Tissue, Temperature) |>
      dplyr::arrange(Tissue = Tissue,
                     Temperature = base::factor(Temperature, c("TN",
                                                               "Cold_Acute",
                                                               "Cold_Long",
                                                               "Cold_Memory"))) |>
      dplyr::filter(magrittr::is_in(Tissue, input_tissues))
    rownames(key)<-key_names


    #prepare matrix

    subset_data <- subset_data |>
      dplyr::select(unique_sample, CPM, SYMBOL) |>
    #log2 transform data
      dplyr::mutate(CPM = log2(CPM))

    data_matrix <- tidyr::pivot_wider(subset_data,
                                      names_from = unique_sample,
                                      values_from = CPM
                                      )


    matrix_rows <- data_matrix$SYMBOL

    data_matrix <- data_matrix|>
      dplyr::select(rownames(key))


    rownames(data_matrix)<- matrix_rows
    data_matrix <- as.matrix(data_matrix)

    AnnoColor <- list(Temperature = c(TN = "#DDD596",
                                      Cold_Acute = "#0F99B2",
                                      Cold_Long = "#313878",
                                      Cold_Memory = "#7A2C25"))

    Heatmap <- pheatmap::pheatmap(data_matrix,
                                  treeheight_col = 0,
                                  treeheight_row = 0,
                                  scale = "row",
                                  legend = T,
                                  na_col = "white",
                                  Colv = NA,
                                  na.rm = T,
                                  cluster_cols = F,
                                  fontsize_row = rowfont,
                                  cellwidth = cellwidths,
                                  cellheight = cellheights,
                                  annotation_col = key,
                                  show_colnames = F,
                                  show_rownames = T,
                                  cluster_rows = T,
                                  main = plottitle,
                                  fontsize = fontsize,
                                  annotation_colors = AnnoColor,
                                  width = 20,
                                  height = 40
    )
    return(Heatmap)

  }
}

#' ncbi url generator
#'
#' @param target_gene gene on the SYMBOL form
#' @param gene_database a data frame containing the columns SYMBOL and ENSEMBL
#'
#' @return
#' @export
#'
#' @examples
ncbi_url_generator <- function(target_gene, gene_database, ensembl_converter){

  ensembl <- gene_database |>
    dplyr::filter(SYMBOL == target_gene) |>
    dplyr::pull(ENSEMBL)

  if(length(ensembl)>1){
    ensembl <- ensembl[1]
  }

  if(length(ensembl)==0){
    return(
      shiny::tagList(
        shiny::h6(
          paste0(
            target_gene,
            " does not have a ensemblID in the database")
          )
        )
      )
  }

  else{
    human_ensembl<- ensembl_converter |>
      dplyr::filter(ENSEMBL_mouse == ensembl) |>
      dplyr::pull(ENSEMBL_human)
    if(length(human_ensembl)>1){
      human_ensembl <- human_ensembl[1]
    }

    if(is.na(human_ensembl)){
      return(
        shiny::tagList(
          shiny::h6(
            paste0(
              target_gene,
              " does not have a ensemblID in the database")
          )
        )
      )
    }
    else{
      #was link to ncbi, user requested change to opentargets
      # site_info <- paste0("https://www.ncbi.nlm.nih.gov/gene/?term=",
      #                     ensembl)
      site_info <- paste0("https://genetics.opentargets.org/gene/",
                          human_ensembl)
      url <- shiny::a(paste0("opentarget link to ",target_gene),
                      href = site_info, target = "_blank")
      return(
        tagList(url)
      )
    }

  }
}

secretome_preparer <- function(target_gene, secretome_database, ensembl_converter){
  ensembl <- ensembl_converter |>
    dplyr::filter(SYMBOL == target_gene) |>
    dplyr::pull(ENSEMBL_human)

  if(length(ensembl)>1){
    ensembl <- ensembl[1]
  }

  if(is.na(ensembl)){
    return(
      shiny::tagList(
        shiny::h6(
          paste0(
            target_gene,
            " does not have a ensemblID in the database")
        )
      )
    )
  }

  else{
    secretome_subset <- secretome_database |>
      dplyr::filter(Ensembl == ensembl)
    return(secretome_subset)
  }


}

