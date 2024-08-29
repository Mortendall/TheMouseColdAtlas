tissue_fill <- function(data_input, metadata, tissueorder){
  #identify ensembl and symbol
  if(length(data_input$ENSEMBL)==0){
    return(data_input)
  }
  else{
  ENSEMBL <- duckplyr::distinct(data_input, ENSEMBL) |> duckplyr::pull()
  Gene_name <- duckplyr::distinct(data_input, SYMBOL) |> duckplyr::pull()

  #identify missing tissues from dataset
  missing_tissues <- metadata |>
    duckplyr::filter(!magrittr::is_in(Sample, data_input$Sample)) |>
    #create columns with NA
    duckplyr::mutate(
      ENSEMBL = ENSEMBL,
      SYMBOL = Gene_name,
      CPM = NA
    ) |>
    #select columns in correct order
    duckplyr::select(colnames(data_input))

  #create data_input with added rows
  output_data <- duckplyr::rows_append(data_input,
                                       missing_tissues)
  output_data$Tissue <- factor(output_data$Tissue,
                               levels = c(tissueorder$Tissue))

  #sometimes all values are NA and then plotly removes them. Identify.

  summary_stats <- output_data |>
    dplyr::group_by(Tissue) |>
    duckplyr::summarise(sum = sum(CPM, na.rm = T)) |>
    duckplyr::filter(sum==0) |>
    duckplyr::pull(Tissue)

   if(length(summary_stats)!=0){
     output_data <- output_data |>
       duckplyr::mutate(CPM =
                          duckplyr::case_when(
         magrittr::is_in(Tissue, summary_stats)~0,
         .default = CPM
       ))
   }


  return(output_data)

  }

}
