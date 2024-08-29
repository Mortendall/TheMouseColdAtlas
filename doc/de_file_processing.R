#####Load in Excel files and concatenate them into one####

cold_vs_tn_files <- fs::dir_ls(here::here("Data-raw/DE_analysis/"),
                               regexp = "_DE_Cold_vs_TN.xlsx")

#create result sheet
DEdata <- vector(mode = "list",length = 19)

#read in list of names
tissuenames <- readxl::read_xlsx(here::here("Data-raw/Tissueorder2.xlsx"))
file_names <- unlist(stringr::str_extract_all(cold_vs_tn_files, "(?<=/)[:alpha:]+_DE_Cold_vs_TN.xlsx$"))
tissues <- unlist(stringr::str_extract_all(file_names, "^[:alpha:]+(?=_)"))

tissues <- tissues |>
  as.data.frame() |>
  dplyr::rename(Tissue = tissues) |>
  dplyr::mutate(Tissue = dplyr::case_when(
  Tissue == "Cere"~"Cerebellum",
  Tissue == "Gast"~"Gastrocnemius",
  Tissue == "Hipp"~"Hippocampus",
  Tissue == "Hypo"~ "Hypothalamus",
  Tissue == "Kid"~"Kidney",
  Tissue =="lInt"~"Large.intestine",
  Tissue == "Pit"~"Pituitary",
  Tissue == "sInt"~"Small.intestine",
  Tissue == "Spl"~"Spleen",
  Tissue =="Stom"~"Stomach",
  Tissue == "Testis"~"Testes",
  .default = as.character(Tissue)
))



DEdata <- purrr::map(cold_vs_tn_files,
                     ~readxl::read_xlsx(.))

names(DEdata)<- tissues$Tissue

DEdata_long_cold_vs_TN <- dplyr::bind_rows(DEdata, .id = "Tissue")

#writexl::write_xlsx(DEdata_long_cold_vs_TN, here::here("Data-raw/DEdata_cold_TN_round2.xlsx"))


#Repeat for Cold_TN vs TN

cold_tn_vs_tn_files <- fs::dir_ls(here::here("Data-raw/DE_analysis/"),
                               regexp = "_DE_Cold_TN_vs_TN.xlsx")

#create result sheet
DEdata <- vector(mode = "list",length = 19)

#read in list of names

file_names <- unlist(stringr::str_extract_all(cold_tn_vs_tn_files, "(?<=/)[:alpha:]+_DE_Cold_TN_vs_TN.xlsx$"))
tissues <- unlist(stringr::str_extract_all(file_names, "^[:alpha:]+(?=_)"))

tissues <- tissues |>
  as.data.frame() |>
  dplyr::rename(Tissue = tissues) |>
  dplyr::mutate(Tissue = dplyr::case_when(
    Tissue == "Cere"~"Cerebellum",
    Tissue == "Gast"~"Gastrocnemius",
    Tissue == "Hipp"~"Hippocampus",
    Tissue == "Hypo"~ "Hypothalamus",
    Tissue == "Kid"~"Kidney",
    Tissue =="lInt"~"Large.intestine",
    Tissue == "Pit"~"Pituitary",
    Tissue == "sInt"~"Small.intestine",
    Tissue == "Spl"~"Spleen",
    Tissue =="Stom"~"Stomach",
    Tissue == "Testis"~"Testes",
    .default = as.character(Tissue)
  ))



DEdata <- purrr::map(cold_tn_vs_tn_files,
                     ~readxl::read_xlsx(.))

names(DEdata)<- tissues$Tissue

DEdata_long_cold_TN_vs_TN <- dplyr::bind_rows(DEdata, .id = "Tissue")

#writexl::write_xlsx(DEdata_long_cold_TN_vs_TN, here::here("Data-raw/DEdata_cold_TN_vs_TN_round2.xlsx"))

#####Make one united object and save it as RDS####
cold_TN_vs_TN <- DEdata_long_cold_TN_vs_TN
cold_vs_TN <- DEdata_long_cold_vs_TN

DEdataCombined <- dplyr::bind_rows(cold_TN_vs_TN,cold_vs_TN, .id = "comparison") |>
  dplyr::mutate(comparison = dplyr::case_when(
    comparison == 1 ~ "cold_TN_vs_TN",
    comparison == 2 ~"cold_vs_TN"
  )) |>
  dplyr::select(Name, Tissue, logFC, PValue, FDR, comparison)
saveRDS(DEdataCombined, here::here("Data/DEdataRound2.rds"))

#####Save as Parquet####
DEdataRound2 <- readRDS(here::here("Data/DEdataRound2.rds"))
arrow::write_parquet(DEdataRound2, here::here("Data/DEdataRound2.parquet"))


#####Repeat on combined dataset####
cold_vs_tn_files <- fs::dir_ls(here::here("Data-raw/DEG analysis/"),
                               regexp = "*gene_expression.xlsx")

#create result sheet
DEdata <- vector(mode = "list",length = 18)

#read in list of names
tissuenames <- readxl::read_xlsx(here::here("Data-raw/Tissueorder2.xlsx"))
file_names <- unlist(stringr::str_extract_all(cold_vs_tn_files, "(?<=/)[:alpha:]+_differential_gene_expression.xlsx$"))
tissues <- unlist(stringr::str_extract_all(file_names, "^[:alpha:]+(?=_)"))

tissues <- tissues |>
  as.data.frame() |>
  dplyr::rename(Tissue = tissues) |>
  dplyr::mutate(Tissue = dplyr::case_when(
    Tissue == "Cere"~"Cerebellum",
    Tissue == "Gast"~"Gastrocnemius",
    Tissue == "Hipp"~"Hippocampus",
    Tissue == "Hypo"~ "Hypothalamus",
    Tissue == "Kid"~"Kidney",
    Tissue =="lInt"~"LargeIntestine",
    Tissue == "Pit"~"Pituitary",
    Tissue == "sInt"~"SmallIntestine",
    Tissue == "Spl"~"Spleen",
    Tissue =="Stom"~"Stomach",
    Tissue == "Sol"~"Soleus",
    Tissue == "Testis"~"Testes",
    .default = as.character(Tissue)
  ))

DEdata_cold_vs_TN <- purrr::map(cold_vs_tn_files,
                     ~readxl::read_xlsx(.,sheet = 1))

names(DEdata_cold_vs_TN)<- tissues$Tissue

DEdata_acute_cold_vs_TN <- purrr::map(cold_vs_tn_files,
                                ~readxl::read_xlsx(.,sheet = 2))
names(DEdata_acute_cold_vs_TN)<- tissues$Tissue

DEdata_memory_cold_vs_TN <- purrr::map(cold_vs_tn_files,
                                      ~readxl::read_xlsx(.,sheet = 3))

names(DEdata_memory_cold_vs_TN)<- tissues$Tissue

#create long files
DEdata_acute_cold_vs_TN_long <- dplyr::bind_rows(DEdata_acute_cold_vs_TN,
                                                 .id = "Tissue")
DEdata_cold_vs_TN_long <- dplyr::bind_rows(DEdata_cold_vs_TN,
                                                 .id = "Tissue")

DEdata_memory_cold_vs_TN_long <- dplyr::bind_rows(DEdata_memory_cold_vs_TN,
                                                 .id = "Tissue")

#save round 1 as parquet
arrow::write_parquet(DEdata_acute_cold_vs_TN_long,
                     here::here("data/DEdatacoldCombined.parquet"))
#####Make one united object for round 2 and save it as RDS####
cold_TN_vs_TN <- DEdata_memory_cold_vs_TN_long
cold_vs_TN <- DEdata_cold_vs_TN_long

DEdataCombined <- dplyr::bind_rows(cold_TN_vs_TN,cold_vs_TN, .id = "comparison") |>
  dplyr::mutate(comparison = dplyr::case_when(
    comparison == 1 ~ "cold_TN_vs_TN",
    comparison == 2 ~"cold_vs_TN"
  )) |>
  dplyr::mutate(SYMBOL = dplyr::case_when(
    is.na(SYMBOL)==TRUE~ENSEMBL,
    .default = as.character(SYMBOL)
  ))|>
  dplyr::select(SYMBOL, Tissue, logFC, PValue, FDR, comparison)



arrow::write_parquet(DEdataCombined, here::here("data/DEdataRound2Combined.parquet"))

#####Combine the two datasets

acute_set <- read_parquet(here("data/DEdatacoldCombined.parquet")) |>
  select(-ENSEMBL, -GENENAME, -avg_log2_CPM_project_wide) |>
  mutate(comparison = "acute_vs_TN")
long_set <- read_parquet(here("data/DEdataRound2Combined.parquet")) |>
  select(colnames(acute_set))

combined_set <-   rows_append(acute_set,
                              long_set)
write_parquet(combined_set, here("data/combined_dedata.parquet"))


