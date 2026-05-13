dedatacold <- arrow::read_parquet(here::here("Data/combined_dedata.parquet"))
tissueorder <- readRDS(here::here("data/tissueorder2.rds")) |>
  duckplyr::mutate(
    Tissue = dplyr::case_when(
      Tissue == "Small.intestine" ~ "SmallIntestine",
      Tissue == "Large.intestine" ~ "LargeIntestine",
      .default = as.character(Tissue)
    )
  ) |>
  dplyr::filter(Tissue != "Spleen")
#
# dedatacold <- dedatacold |>
#   dplyr::filter(comparison == "cold_vs_TN")

dedatacold$Tissue <- factor(dedatacold$Tissue,
                            levels = tissueorder$Tissue)

#add manual color scale
graph_scale <- c("#74180b",
                 "#648cac",
                 "#dfdfdf")

dedataplot <- dedatacold |>
  dplyr::mutate(direction = dplyr::case_when(FDR<0.05 & logFC > 0 ~ "Upregulated",
                                             FDR<0.05 & logFC < 0 ~ "Downregulated",
                                             TRUE ~ "Unchanged")) |>
  dplyr::group_by(Tissue, direction, comparison) |>
  dplyr::summarise(Count = dplyr::n()) |>
  dplyr::mutate(direction = factor(direction,levels = c("Upregulated",
                                                        "Downregulated",
                                                        "Unchanged")))


dedatapercent <- dedataplot |>
  dplyr::group_by(Tissue, comparison) |>
  dplyr::summarise(Total = sum(Count))
dedatapercent <- dplyr::left_join(dedataplot, dedatapercent) |>
  dplyr::mutate(PercentageChanged = (Count/Total)*100,
                direction = factor(direction,levels = c("Upregulated",
                                                        "Downregulated",
                                                        "Unchanged")))

dedatapercent$Tissue <- factor(dedatapercent$Tissue,
                               levels = tissueorder$Tissue)

ggplot2::ggplot(dedatapercent, ggplot2::aes(x = Tissue,
                                            y = PercentageChanged,
                                            fill = direction))+
  ggplot2::geom_col()+
  ggplot2::theme_bw()+
  ggplot2::scale_fill_discrete(name = "Count")+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.9, hjust = 0.8)
  )+
  ggplot2::ylab("Percentage Change")+
  ggplot2::scale_fill_manual(values = graph_scale)+
  ggplot2::facet_wrap(~comparison, nrow = 3)

names_keys <- dedatapercent |>
  dplyr::group_by(comparison) |>
  dplyr::group_keys()
formatted_dedatapercent <- dedatapercent |>
  dplyr::group_by(comparison) |>
  dplyr::group_split()
formatted_dedatapercent<-  set_names(formatted_dedatapercent, names_keys$comparison)

writexl::write_xlsx(x = formatted_dedatapercent,
                    here::here("doc/tissue_percentage.xlsx"))
