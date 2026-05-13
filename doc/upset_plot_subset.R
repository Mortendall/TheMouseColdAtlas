#####Upset plots####

# Load data
dedatacold <- arrow::read_parquet(here::here("data/combined_dedata.parquet"))
tissueorder <- readRDS("data/tissueorder2.rds")
tissueorder <- tissueorder |>
  dplyr::mutate(Tissue = dplyr::case_when(
    Tissue == "Small.intestine"~"SmallIntestine",
    Tissue == "Large.intestine"~"LargeIntestine",
    .default = as.character(Tissue)
  ))
#remove spleen as this was omitted
tissueorder <- tissueorder |>
  dplyr::filter(Tissue !="Spleen")


#' Title
#'
#' @param dedata data frame with DE data
#' @param tissueorder vector to define tissueorder
#' @param direction a string, "increased", "decreased" or "all"
#' @param comparison a string "acute", "chronic" or "memory"
#'
#' @return a beautiful upsetplot

create_upset <- function(dedata, tissueorder, comparison, direction){
  dedata$Tissue <- factor(dedata$Tissue,
                              levels = tissueorder$Tissue)

  # make same upsetplot with ComplexUpset
  #no significant for hippocampus and cerebellum so these one is removed
  order_generator <- subset(dedata, Tissue != "Hippocampus"&Tissue != "Cerebellum")
  tissueordermod <- subset(tissueorder, Tissue != "Hippocampus"&Tissue != "Cerebellum")

  #memory lacks more tissues
  if(comparison == "memory"){
    order_generator <- subset(dedata, Tissue != "Hippocampus"&
                                Tissue != "Cerebellum"&Tissue != "EDL"&
                                Tissue != "Gastrocnemius"&Tissue != "Stomach"&
                              Tissue != "SmallIntestine"&Tissue != "LargeIntestine"&
                                Tissue != "Cortex"& Tissue != "Pituitary"&Tissue != "Testes")
    tissueordermod <- subset(tissueordermod, Tissue != "Hippocampus"&
                               Tissue != "Cerebellum"&Tissue != "EDL"&
                               Tissue != "Gastrocnemius"&Tissue != "Stomach"&
                               Tissue != "SmallIntestine"&Tissue != "LargeIntestine"&
                               Tissue != "Cortex"& Tissue != "Pituitary"&Tissue != "Testes")
  }

  order_generator$Tissue <- factor(order_generator$Tissue,
                                   levels = tissueordermod$Tissue)
  order_upset <-  levels(order_generator$Tissue)

  if(comparison == "memory"){
    dedata <- dedata |>
      dplyr::filter(comparison == "cold_TN_vs_TN")
  }

  if(comparison == "chronic"){
    dedata <- dedata |>
      dplyr::filter(comparison == "cold_vs_TN")
  }

  if(comparison == "acute"){
    dedata <- dedata |>
      dplyr::filter(comparison == "acute_vs_TN")
  }

  #filter up- or downregulated when applicaple
  if(direction == "increased"){
    dedata <- dedata |>
      dplyr::filter(logFC > 0)
  }

  if(direction == "decreased"){
    dedata <- dedata |>
      dplyr::filter(logFC < 0)
  }

  dedatasig <- dedata |>
    dplyr::filter(FDR<0.05) |>
    dplyr::mutate("TRUE"=TRUE) |>
    dplyr::select(-FDR, -logFC, -PValue, -comparison) |>
    dplyr::filter(!is.na(SYMBOL)) |>
    dplyr::mutate(ID = paste(Tissue, SYMBOL, sep = "_")) |>
    dplyr::distinct(ID, .keep_all = T) |>
    dplyr::select(-ID)


  upset_wide <- tidyr::pivot_wider(dedatasig,
                                   id_cols = SYMBOL,
                                   names_from = Tissue,
                                   values_from = "TRUE",
                                   values_fill = FALSE) |>
    dplyr::select(order_upset)

  upsetRNA <- ComplexUpset::upset(upset_wide,
                                  order_upset,
                                  name = "",
                                  sort_sets = F,
                                  themes = ComplexUpset::upset_modify_themes(list(
                                    "intersections_matrix" = ggplot2::theme(text = ggplot2::element_text(size = 16))
                                  )),min_size = 150)



  # ggplotify to use the object in patchwork
  upsetRNA <- ggplotify::as.ggplot(upsetRNA)
  upsetRNA <- upsetRNA +
    ggplot2::ggtitle(paste(comparison, direction, "genes",sep = " ")) +
    ggplot2::theme(plot.title = ggplot2::element_text(
      size = 24,
      hjust = 0.5,
      vjust = 0.95
    ))
  upsetRNA <- ggplotify::as.ggplot(upsetRNA)

  upsetRNA
}

acute_all <- create_upset(dedatacold, tissueorder, "acute","all")
acute_incrased <- create_upset(dedatacold, tissueorder, "acute","increased")
acute_decreased <- create_upset(dedatacold, tissueorder, "acute","decreased")

chronic_all <- create_upset(dedatacold, tissueorder, "chronic","all")
chronic_incrased <- create_upset(dedatacold, tissueorder, "chronic","increased")
chronic_decreased <- create_upset(dedatacold, tissueorder, "chronic","decreased")

# memory_all <- create_upset(dedatacold, tissueorder, "memory","all")
# memory_incrased <- create_upset(dedatacold, tissueorder, "memory","increased")
# memory_decreased <- create_upset(dedatacold, tissueorder, "memory","decreased")

library(patchwork)

patchwork_acute <- acute_all/acute_incrased/acute_decreased
patchwork_chronic <- chronic_all/chronic_incrased/chronic_decreased

pdf(here::here("doc/acute_upsets.pdf"), width = 20, height = 30)
patchwork_acute
dev.off()

pdf(here::here("doc/chronic_upsets.pdf"), width = 20, height = 30)
patchwork_chronic
dev.off()
