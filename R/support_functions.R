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
