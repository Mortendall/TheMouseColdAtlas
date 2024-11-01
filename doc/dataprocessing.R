CPMdata <- openxlsx::read.xlsx(here::here("Data-raw/CPM_values_for_all_samples.xlsx"))
DEdata <- openxlsx::read.xlsx(here::here("Data-raw/DE_Cold_for_each_tissue_formatted.xlsx"))

#headers have the same name. Create setup sheet and unique identifier

setup <- data.frame(sampleid = colnames(CPMdata)[4:206],
                    Group = colnames(CPMdata)[4:206],
                    Tissue = NA,
                    Temperature = NA)
setup <- setup |>   dplyr::mutate(sampleid = make.unique(setup$sampleid, sep = "_"),
                Tissue = stringr::str_extract(sampleid,
                                               pattern = "(?<=_)[:alpha:]+(?=_?)"),
                Temperature = stringr::str_extract(sampleid,
                                                   pattern = "^[:alpha:]+(?=_)"))

CPMdatasubset <- CPMdata
colnames(CPMdatasubset)[4:206]<- setup$sampleid
CPMdatasubset <- CPMdatasubset |>
  dplyr::select(-description)

#dataset contains duplicates. Sort by one arbitrary samples and remove rest?

CPMdatasubset <- CPMdatasubset |>
  dplyr::arrange(dplyr::desc(TN_EDL)) |>
  dplyr::distinct(mgi_symbol, .keep_all = T)

#create long dataset for plotting
longcpm <- tidyr::pivot_longer(CPMdatasubset,
                               cols = -c(mgi_symbol, ENSEMBL_ID),
                               names_to =  "Sample",
                               values_to = "CPM")
longcpm <- dplyr::left_join(longcpm,
                            setup,
                            by = c("Sample"="sampleid"))

longcpm$Temperature <- factor(longcpm$Temperature,levels = c("TN", "Cold"))

DEdatalong <- DEdata |>
  dplyr::select(-description)
DEdatalong <- tidyr::pivot_longer(DEdatalong,
                                  cols = -c(ENSEMBL_ID, mgi_symbol),
                                  values_to = "values",
                                  names_to = "category") |>
  dplyr::mutate(Tissue = stringr::str_extract(category,
                                              "^[:graph:]+(?=_)"),
                Variable = stringr::str_extract(category,
                                                "(?<=_)[:alpha:]+$"))
#keep only the DE values present in the CPM set
DEdatawrangled <- DEdatalong |>
  dplyr::filter(magrittr::is_in(ENSEMBL_ID,
                                CPMdatasubset$ENSEMBL_ID)) |>
  dplyr::select(-ENSEMBL_ID, -category) |>
  dplyr::filter(!is.na(values)&!is.na(mgi_symbol))

DEdatawrangled <-  tidyr::pivot_wider(DEdatawrangled,
                                      values_from = values,
                                      names_from = Variable)
saveRDS(DEdatawrangled, here::here("Data/DEdatacold.rds"))
saveRDS(longcpm, here::here("Data/cpmcold.rds"))


#####create summary for cpm####
CPMsummary <- longcpm |>
  dplyr::group_by(Tissue, Temperature, mgi_symbol) |>
  dplyr::summarize(mean = mean(CPM))
#saveRDS(CPMsummary, here::here("Data/cpmsummary.rds"))

#####Read in data from raw####
rawdata <- openxlsx::read.xlsx(here::here("Data-raw/iwatewatraw.xlsx"),sheet = 1)
setup <- openxlsx::read.xlsx(here::here("Data-raw/iwatewatraw.xlsx"),sheet = 2)

ewatiwat <- tidyr::pivot_longer(rawdata,
                                cols = -Gene,
                                values_to = "expression",
                                names_to = "Sample")

ewatiwat <- dplyr::left_join(ewatiwat, setup, by = c("Sample"="Sample"))
ewatiwat <- ewatiwat |>
  dplyr::mutate(Group = paste(Cell,
                              Diet,
                               sep = "_"))
#saveRDS(ewatiwat, here::here("Data/ewatiwatraw.rds"))

#####Load in cpm data and factor tissues and fix broken names####
longcpm <- readRDS(here::here("Data/cpmcold.rds"))
longcpm <- longcpm |>
  dplyr::mutate(Tissue = dplyr::case_when(
    Tissue == "Small"~"Small.intestine",
    Tissue == "Large"~"Large.intestine",
    Tissue == "Gastochnemius"~"Gastrocnemius",
    TRUE ~ Tissue
  ))
tissueorder <- openxlsx::read.xlsx(here::here("Data-raw/Tissueorder.xlsx"),)
longcpm$Tissue <- factor(longcpm$Tissue,
                         levels = c(tissueorder$Tissue))
#saveRDS(longcpm, here::here("Data/cpmcold.rds"))
#CPM summary
CPMsummary <- readRDS(here::here("Data/cpmsummary.rds"))
CPMsummary <- CPMsummary |>
  dplyr::mutate(Tissue = dplyr::case_when(
    Tissue == "Small"~"Small.intestine",
    Tissue == "Large"~"Large.intestine",
    Tissue == "Gastochnemius"~"Gastrocnemius",
    TRUE ~ Tissue
  )
  )
CPMsummary$Tissue <- factor(CPMsummary$Tissue,
                            levels = c(tissueorder$Tissue))
#saveRDS(CPMsummary, here::here("Data/cpmsummary.rds"))

#DEdata
DEdatawrangled <- readRDS(here::here("Data/DEdatacold.rds"))
DEdatawrangled <- DEdatawrangled |>
  dplyr::mutate(Tissue = dplyr::case_when(
    Tissue == "Gastochnemius"~"Gastrocnemius",
    TRUE ~ Tissue
  ))
DEdatawrangled$Tissue <-factor(DEdatawrangled$Tissue,
                               levels = c(tissueorder$Tissue))
#saveRDS(DEdatawrangled, here::here("Data/DEdatacold.rds"))
#saveRDS(tissueorder, here::here("Data/tissueorder.rds"))

#####Order DT in right manner####
testset <- DEdatawrangled
testset <- testset |>
  dplyr::filter(mgi_symbol == "Il6")
testset <- data.table::as.data.table(testset)

testset[order(match(Tissue, tissueorder))]


#####CPM data for round 2####
CPMdata2 <- openxlsx::read.xlsx(here::here("Data-raw/CPM_values_round2.xlsx"))

metadata <- openxlsx::read.xlsx(here::here("Data-raw/metadata_round2.xlsx"))

#change format of metadata to match first round
metadata_processed <- metadata |>
  dplyr::mutate(Condition1 = dplyr::case_when(
    Condition1 == "A"~"TN",
    Condition1 == "B"~"Cold",
    Condition1 == "C"~"Cold_Memory"
  )
  ) |>
  dplyr::mutate(sampleid = `Sample ID.(Library.ID)`,
                Group = paste(Condition1,`Cell type`, sep = "_"),
                Tissue = `Cell type`,
                Temperature = Condition1) |>
  dplyr::mutate(Tissue = dplyr::case_when(
    Tissue == "Cere"~"Cerebellum",
    Tissue == "Gast"~"Gastrocnemius",
    Tissue == "Hipp"~"Hippocampus",
    Tissue == "Hypo"~ "Hypothalamus",
    Tissue == "Kid"~"Kidney",
    Tissue =="l.int"~"Large.intestine",
    Tissue == "Pit"~"Pituitary",
    Tissue == "s.int"~"Small.intestine",
    Tissue == "Spl"~"Spleen",
    Tissue =="Stom"~"Stomach",
    Tissue == "Testis"~"Testes",
    .default = as.character(Tissue)
  )) |>
  dplyr::select(sampleid, Group, Tissue, Temperature)

#remove description from CPM
CPMdata2trim <- CPMdata2 |>
  dplyr::select(-description)

#sort by TN_EDL sample to have most abundant counts
CPMdata2trim  <- CPMdata2trim  |>
  dplyr::arrange(dplyr::desc(	"0243_X1_077")) |>
  dplyr::distinct(Name, .keep_all = T)

#create long dataset for plotting
longcpm2 <- tidyr::pivot_longer(CPMdata2trim,
                               cols = -c(Name, ENSEMBL_ID),
                               names_to =  "Sample",
                               values_to = "CPM")
longcpm2 <- dplyr::left_join(longcpm2,
                            metadata_processed,
                            by = c("Sample"="sampleid"))

longcpm2$Temperature <- factor(longcpm2$Temperature,levels = c("TN", "Cold", "Cold_Memory"))

tissueorder2 <- openxlsx::read.xlsx(here::here("Data-raw/Tissueorder2.xlsx"),)
longcpm2$Tissue <- factor(longcpm2$Tissue,
                         levels = c(tissueorder2$Tissue))
saveRDS(longcpm2, file = here::here("Data/cpmcold2.rds"))


#####Parquet conversion for increased performance####
#Parquet files can be read in much faster. Convert files to parquet for increased performance
longcpm <- readRDS(here::here("Data/cpmcold.rds"))
arrow::write_parquet(longcpm, here::here("Data/cpmcold.parquet"))
longcpm2 <- readRDS(here::here("Data/cpmcold2.rds"))
arrow::write_parquet(longcpm2, here::here("Data/cpmcold2.parquet"))
DEdatacold <- readRDS(here::here("Data/DEdatacold.rds"))
arrow::write_parquet(DEdatacold, here::here("Data/DEdatacold.parquet"))

#####file processing for combined study####
#The two studies have been merged into one, and new files therefore need to be prepared

#####Load combined CPM files####
# cpm_files <- fs::dir_ls(here::here("Data-raw/batch corrected CPM/"),
#                                regexp = "_batch_corrected.xlsx")

#updated 18th of June 2024 to include more genes in list
cpm_files <- fs::dir_ls(here::here("Data-raw/batch corrected CPM more genes/"),
                        regexp = "_batch_corrected.xlsx")

#create result sheet
CPMdata <- vector(mode = "list",length = 18)

#read in list of names
tissuenames <- readxl::read_xlsx(here::here("Data-raw/Tissueorder2.xlsx"))
file_names <- unlist(stringr::str_extract_all(cpm_files, "(?<=/)[:alpha:]+_logCPM_batch_corrected.xlsx$"))
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
    Tissue =="Stom"~"Stomach",
    Tissue == "Testis"~"Testes",
    .default = as.character(Tissue)
  ))


cpm_data <- purrr::map(cpm_files,
                     ~readxl::read_xlsx(.))

names(cpm_data)<- tissues$Tissue

#load combined metadata
metacombined <- readxl::read_xlsx(here::here("data-raw/metadatacombined.xlsx"))
#align names to CPM for setup 2
metacombined$ID <- stringr::str_remove(metacombined$ID,
                                       "_X1")
#add the right IDs for metadata
metacombined <- metacombined |>
  dplyr::mutate(Treatment = dplyr::case_when(
    Treatment == "A"~"TN",
    Treatment == "B"~"Cold_Long",
    Treatment == "C"~"Cold_Memory",
    Treatment == "D"~"Cold_Acute"
  )
  )

saveRDS(metacombined, here::here("data/metacombined.rds"))

#create long format cpm
cpm_data_long <- cpm_data
cpm_data_long <- purrr::map(cpm_data_long,
                            ~dplyr::select(.,
                                           -GENENAME))
#for each tissue, sort by first data column and keep highest count column
cpm_data_long <- purrr::map(cpm_data_long,
                            ~dplyr::arrange(.,dplyr::desc(.[[3]])))
cpm_data_long <- purrr::map(cpm_data_long,
                            ~dplyr::distinct(.,
                                             SYMBOL,
                                             .keep_all = T))
#create long dataset for plotting
cpm_data_long <- purrr::map(cpm_data_long, ~tidyr::pivot_longer(.,
                                                                cols = -c(SYMBOL, ENSEMBL),
                                                                names_to =  "Sample",
                                                                values_to = "CPM"))
cpm_data_long <- purrr::map(cpm_data_long,
                            ~dplyr::left_join(.,
                                       metacombined,
                                       by = c("Sample"="ID")))
#Make the cpm file into one long list
cpm_data_final <-dplyr::bind_rows(cpm_data_long)

#Factor treatment
cpm_data_final$Treatment <- factor(cpm_data_final$Treatment,
                                   levels = c("TN",
                                              "Cold_Acute",
                                              "Cold_Long",
                                              "Cold_Memory"))

tissueorder2 <- openxlsx::read.xlsx(here::here("Data-raw/Tissueorder2.xlsx"))
tissueorder2 <- tissueorder2 |>
  dplyr::mutate(Tissue = dplyr::case_when(
    Tissue == "Small.intestine"~"SmallIntestine",
    Tissue == "Large.intestine"~"LargeIntestine",
    .default = as.character(Tissue)
  ))
#remove spleen as this was omitted
tissueorder2 <- tissueorder2 |>
  dplyr::filter(Tissue !="Spleen")
cpm_data_final$Tissue <- factor(cpm_data_final$Tissue,
                          levels = c(tissueorder2$Tissue))
#data here is logCPM. convert to CPM
cpm_data_final$CPM <-2^(cpm_data_final$CPM)

#Save as parquet
arrow::write_parquet(cpm_data_final, here::here("Data/cpmcoldcombined.parquet"))

#save two subsets to use in app
# cpm_data_final <- cpm_data_final |>
#   dplyr::rename(Temperature = Treatment)
#
# #make ENS the name if name is absent
# cpm_data_final <- cpm_data_final |>
#   dplyr::mutate(SYMBOL = dplyr::case_when(
#     is.na(SYMBOL)==TRUE~ENSEMBL,
#     .default = as.character(SYMBOL)
#   ))
#
# cpmlongcold <- subset(cpm_data_final,Temperature != "Cold_Acute")
# cpmacute <-  subset(cpm_data_final,Temperature == "Cold_Acute"|Temperature == "TN")


# arrow::write_parquet(cpmacute, here::here("data/cpmacute.parquet"))
# arrow::write_parquet(cpmlongcold, here::here("data/cpmlongcold.parquet"))

#write a wide data table for CPM values

#Write metadata file
metadata <- read_parquet(here("data/cpmcoldcombined.parquet")) |>
  duckplyr::distinct(Sample,.keep_all = T) |>
  duckplyr::select(Sample, Tissue, Temperature)

write_parquet(metadata,
             here("data/metadata.parquet"))

#####MakeclusterProfiler key####
gene_names <- read_rds(here("data/choicescombined.rds"))

keys <- clusterProfiler::bitr(gene_names, fromType = "SYMBOL",
                              toType = "ENSEMBL",
                              OrgDb = "org.Mm.eg.db")


arrow::write_parquet(keys, here::here("data/ensembl_key.parquet"))

#####get secretion status####
#download.file("https://download.jensenlab.org/mouse_compartment_knowledge_full.tsv", destfile = here::here("data/mouse_compartment_knowledge_full.tsv"))
database <- read_tsv_arrow("data/mouse_compartment_knowledge_full.tsv",col_names = c("ENSEMBL","SYMBOL","GO","Term","Source","Evidence","Score"))

#many databases included. Make it more simple?
#UniProtKB-SubCell is a large set?

database <- database  |>
  dplyr::select(SYMBOL, Source, Term, Score)
arrow::write_parquet(database, here::here("data/localizationdata.parquet") )

#####Human/mouse ENS converter####
mouse_symbols <- arrow::read_parquet(here::here("data/cpmcoldcombined.parquet")) |>
  duckplyr::select(SYMBOL, ENSEMBL) |>
  duckplyr::distinct(ENSEMBL,.keep_all = T) |>
  duckplyr::mutate(SYMBOL =toupper(SYMBOL))

#join human ENS

human_symbols <- clusterProfiler::bitr(mouse_symbols$SYMBOL,fromType = "SYMBOL", toType = "ENSEMBL",
                                       OrgDb = "org.Hs.eg.db",drop = F) |>
  duckplyr::distinct(ENSEMBL,.keep_all = T)
joined_key <- duckplyr::left_join(mouse_symbols, human_symbols, by = c("SYMBOL"="SYMBOL")) |>
  duckplyr::rename(ENSEMBL_mouse = ENSEMBL.x,
                   ENSEMBL_human = ENSEMBL.y) |>
  duckplyr::mutate(SYMBOL = stringr::str_to_title(SYMBOL))
arrow::write_parquet(joined_key, here::here("data/ensembl_converter.parquet"))

#make human protein atlas plugin
prot_file <- arrow::read_tsv_arrow(here::here("doc/proteinatlas.tsv")) |>
  duckplyr::select(Ensembl, Gene, `Secretome location`, `Secretome function`)
arrow::write_parquet(prot_file, here::here("data/secretome.parquet"))

