library(readxl)
library(writexl)
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(qlcData)

# VERY IMPORTANT NOTES
# everytime finishing exporting the spreadsheet into SFM, always check lexicons that have more than one meaning (polysemous) in the spreadsheet
#   - esp. check "pururu" that appears in fauna and flora
#   - esp. check "are'iar" in fauna
#   - esp. check words for "kepiting" (ẽũk kiėhėr)
#   - esp. check words for "skin" (iuk kane)
# then, re-organise the sfm entry for these polysemous items in the exported .db SFM file.
# the re-organisation needs to follow the format of SFM import whereby the second sense is started with \ps
# do this for flora and fauna
# only after this re-organisation then the sfm .db will be imported into FLEx
# STEPS TO DO THAT:
# 1. Open the spreadsheet for either flora and fauna
# 2. Open one of the files named "flora/fauna-sfm-YYYYMMDD.db"
# 3. Go to the spreadsheet and filter under SENSE_ID column the non-blank cells
# 4. Go go the .db textfile and bring the other sense of the item under the same SFM entry where each sense is marked with \ps

flora_fauna_drive_path <- "https://drive.google.com/drive/u/0/folders/1QF1kvNglqZQC7sjJ1ryHRbzB4XMzgvfH"

# Created Drive file:
#   • flora <id: 1w1G0R6Fs9K5unkVLNEDYk9iyLd7O7RJYaj-XS2ssS1A>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet
# flora_sheet_id <- "1w1G0R6Fs9K5unkVLNEDYk9iyLd7O7RJYaj-XS2ssS1A"

# Created Drive file:
#   • fauna <id: 1E-UhNf7OgxRo2zoNzetwgtnU0f36L2bZXRtwoNRmlpA>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet
# fauna_sheet_id <- "1E-UhNf7OgxRo2zoNzetwgtnU0f36L2bZXRtwoNRmlpA"

# Created Drive file:
#   • flora_with_picture <id: 1h55oN7JQL6lRj7Km0VhSLu5zEvXD-ZKk5HCva02HYMA>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet
# flora_with_pict_sheet_id <- "1h55oN7JQL6lRj7Km0VhSLu5zEvXD-ZKk5HCva02HYMA"

# Created Drive file:
#   • verb <id: 11sV_Iaw8OECur_SP3NsctFPRxSTyGRffZZ2YQ_7bYTE>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet
# verb_sheet_id <- "11sV_Iaw8OECur_SP3NsctFPRxSTyGRffZZ2YQ_7bYTE"

# Store the list of files and directories inside the flora and fauna directory ==========
flora_fauna_drive <- drive_ls(path = as_id("https://drive.google.com/drive/u/0/folders/1QF1kvNglqZQC7sjJ1ryHRbzB4XMzgvfH"))
flora_fauna_drive

# List the files inside the flora and fauna picture directory =========
flora_picts <- drive_ls(path = as_id("https://drive.google.com/drive/u/0/folders/1u31JkgLzssdC24LxXN1Vy2eL281scLDx"))
fauna_picts <- drive_ls(path = as_id("https://drive.google.com/drive/u/0/folders/10ary9mG0jwAPSC1wx_i-MRIhEQtvsxGw"))

# FLORA: Pre-processing the naming of some of the files =========
flora_picts1 <- flora_picts |> 
  
  # Create the Google Drive URL for each file based on the numeric id
  mutate(url = paste("https://drive.google.com/file/d/", id, "/view", sep = "" )) |> 
  
  # Check if the file is named using number or not
  mutate(photo_numeric_name = if_else(str_detect(name, "^[0-9 \\(\\)]+\\.(JPG|jpg|jpeg|png|PNG)$"),
                                      TRUE,
                                      FALSE)) |> 
  mutate(name2 = str_replace(name, "\\.(jpe?g|JPG|png|PNG)$", ""))# |> 
  # mutate(name2 = replace(name2,
  #                        name2 == "ku pi",
  #                        "142"),
  #        name2 = replace(name2,
  #                        name2 == "ku apu",
  #                        "6"),
  #        name2 = replace(name2,
  #                        name2 == "ku ea",
  #                        "125"),
  #        name2 = replace(name2,
  #                        name2 == "ku kiko'op",
  #                        "134"),
  #        name2 = replace(name2,
  #                        str_detect(name2, "^sugar\\-cane"),
  #                        "60"),
  #        name2 = replace(name2,
  #                        str_detect(name2, "^rose\\-"),
  #                        "160")
  #        ) # |> 
  # mutate(name = replace(name,
  #                        url == "https://drive.google.com/file/d/18rKWb_NnAnQpj-g3wIIg1Qh-VrNtQAy6/view",
  #                        "161 (1).JPG"),
  #        name = replace(name,
  #                       url == "https://drive.google.com/file/d/1x86K4faSIUv_ZmmQPAr5lygQDyje7pz5/view",
  #                       "162 (1).JPG"),
  #        name = replace(name,
  #                       url == "https://drive.google.com/file/d/1T0ZHXrux9bPiVTpHtcCiPHWMkn8ZYE5U/view",
  #                       "140 (1).JPG"))

# FAUNA: Pre-processing the naming of some of the files =========
fauna_picts1 <- fauna_picts |> 
  
  # Create the Google Drive URL for each file based on the numeric id
  mutate(url = paste("https://drive.google.com/file/d/", id, "/view", sep = "" )) |> 
  
  # Check if the file is named using number or not
  mutate(photo_numeric_name = if_else(str_detect(name, "^[0-9 \\(\\)]+\\.(JPG|jpg|jpeg|png|PNG)$"),
                                      TRUE,
                                      FALSE)) |> 
  mutate(name2 = str_replace(name, "\\.(jpe?g|JPG|png|PNG)$", ""))

# Copy flora photos to flora-fauna linked files directory ====
# Linked Directory path:
linked_dir <- "C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles"

# IMPORTANT: The following code needs to be re-run whenever there is photo update from Dendi
# If there is any cropping in the FLEx directly, first copy the crop/edited photos in/from "C:\ProgramData\SIL\FieldWorks\Projects\flora-fauna\LinkedFiles\Pictures" to the Google Drive folder and rename the edited photo in the Google Drive with "_...".
# OR IF THERE IS ANY NEW FOTO, COPY DIRECTLY TO "C:\ProgramData\SIL\FieldWorks\Projects\flora-fauna\LinkedFiles"
# fs::dir_delete("C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/flora_photo")
# fs::dir_delete("C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/fauna_photo")
# fs::dir_delete("C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/Pictures")

## The code below copies the photos from flora directory in Google Drive into the FLEx Flora Fauna project directory for the linked picture files
# cp -r flora_photo C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/flora_photo <- FASTER
# fs::dir_copy("flora_photo", "C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/flora_photo", overwrite = TRUE)

## The code below rename files to ensure no ` (...)` marker is present in the file name
flora_filenames <- list.files(path = "C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/flora_photo", full.names = TRUE)
any(str_detect(flora_filenames, "\\("))
sapply(flora_filenames, FUN = function(eachPath) file.rename(from = eachPath, to = str_replace_all(eachPath, "\\s+\\(\\d+\\)", "")))
flora_filenames_new <- list.files(path = "C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/flora_photo", full.names = TRUE)
all(str_detect(flora_filenames_new, "\\("))

## The code below copies the photos from fauna directory in Google Drive into the FLEx Flora Fauna project directory for the linked picture files
# cp -r fauna_photo C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/fauna_photo
# fs::dir_copy("fauna_photo", "C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/fauna_photo", overwrite = TRUE)
# IMPORTANT: Delete one of the Bougenville entries/photos
fauna_filenames <- list.files(path = "C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/fauna_photo", full.names = TRUE)
any(str_detect(fauna_filenames, "\\(")) # check if "(...)" is in the file name
sapply(fauna_filenames, FUN = function(eachPath) file.rename(from = eachPath, to = str_replace_all(eachPath, "\\s+\\(\\d+\\)", "")))
fauna_filenames_new <- list.files(path = "C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/fauna_photo", full.names = TRUE)
all(str_detect(fauna_filenames_new, "\\(")) # check if "(...)" is in the file name after renaming


# Read the G Sheet containing the lexicon of the flora and fauna =====
## FLORA spreadsheet =====
flora_df_orig <- read_sheet(flora_fauna_drive[flora_fauna_drive$name == "flora_with_picture", ][["id"]])

### prepare data for raw data archiving in: https://github.com/engganolang/flora-fauna-lexicon
flora_df_orig_to_archive <- flora_df_orig |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> ## remove NAs
  mutate(across(matches("INCLUDE"), ~replace_na(., TRUE))) |>  ## fill the empty cells with TRUE
  mutate(across(matches("PHONEME"), ~str_replace_all(., "[/]", ""))) |> 
  mutate(across(matches("PHONEME$"), ~str_replace_all(., "\\:", "ː"))) |> 
  mutate(across(matches("PHONEME$"), ~str_replace_all(., "y", "j"))) # replace 'y' with 'j' sounds

### processing the flora data =====
flora_df <- flora_df_orig |> 
  mutate(NO = as.character(NO)) |> 
  filter(is.na(INCLUDE)) |> 
  filter(!ENGLISH %in% c("jellyfish", 
                         #"handle", 
                         # "arranged coral", 
                         "fish bone"),
         NO != "138",
         NO != "113",
         NO != "100",
         NO != "60",
         NO != "95", # a verb, already in FLEx
         NO != "73") # |> # duplicate for 74
  # separate_longer_delim(SUBENTRY, "\n")
  # mutate(SUBENTRY = str_replace_all(SUBENTRY, "\\\n", "\n__\\\\se "))

flora_df1 <- flora_df |> 
  left_join(flora_picts1 |> 
              rename(NO = name2) |> 
              select(NO, url, name)) |> 
  mutate(name = replace_na(name, ""),
         category = "flora") |> 
  mutate(pc = if_else(name != "", paste0(linked_dir, "/flora_photo/", name, sep = ""), ""))

### Check the number of absent photos =====
flora_df1 |> filter(is.na(url))
flora_df1 |> filter(!is.na(url))
write_rds(flora_df1, "output/flora_df1.rds")
write_tsv(flora_df1, "output/flora_df1.tsv")
write_xlsx(flora_df1, "output/flora_df1.xlsx")
write_csv(flora_df1, "output/flora_df1.csv")

## Get the working directory of "flora_df1.rds"
# str_c(getwd(), "/", dir(pattern="flora_df1.rds"), sep = "")
# "G:/.shortcut-targets-by-id/1MO3Q9KZIODxlfPRyjLTtDUZuVkecFBp6/Enggano/enggano-dictionary/flora-fauna/flora_df1.rds"
str_c(getwd(), "/output/flora_df1.rds", sep = "")
# "G:/.shortcut-targets-by-id/1MO3Q9KZIODxlfPRyjLTtDUZuVkecFBp6/Enggano/enggano-dictionary/flora-fauna/output/flora_df1.rds"

## FAUNA spreadsheet =====
fauna_df_orig <- read_sheet(flora_fauna_drive[flora_fauna_drive$name == "fauna", ][["id"]])

### prepare data for raw data archiving in: https://github.com/engganolang/flora-fauna-lexicon
fauna_df_orig_to_archive <- fauna_df_orig |> 
  select(-SENSE_ID) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> ## remove NAs
  mutate(across(matches("INCLUDE"), ~replace_na(., TRUE))) |> ## fill the empty cells with TRUE
  mutate(across(matches("(^IPA$|PHONEME$)"), ~str_replace_all(., "\\/", ""))) |> 
  mutate(across(matches("(^IPA$|PHONEME$)"), ~str_replace_all(., "\\:", "ː"))) |> 
  mutate(across(matches("(^IPA$|PHONEME$)"), ~str_replace_all(., "y", "j"))) # replace 'y' with 'j' sound

### processing the fauna data =====
fauna_df <- fauna_df_orig |> 
  mutate(NO = as.character(NO)) |> 
  mutate(NOTES_EN = replace_na(NOTES_EN, ""),
         NOTES_ID = replace_na(NOTES_ID, "")) |> 
  filter(NOTES_EN != "NOT FAUNA",
         NOTES_EN != "DUPLICATE",
         NO != "1",
         NO != "52", # already handle in FLORA data
         NO != "46" # not sure what hėkė' 'lokan' is actually
         ) |> 
  filter(is.na(INCLUDE))
fauna_df

fauna_df1 <- fauna_df |> 
  left_join(fauna_picts1 |> 
              rename(NO = name2) |> 
              select(NO, url, name)) |> 
  mutate(name = replace_na(name, ""),
         category = "fauna") |> 
  mutate(pc = if_else(name != "", paste0(linked_dir, "/fauna_photo/", name, sep = ""), ""))
fauna_df1

### Check the number of absent photos =========
fauna_df1 |> filter(is.na(url))
fauna_df1 |> filter(is.na(IMAGE))
fauna_df1 |> filter(!is.na(url))
fauna_df1 |> filter(!is.na(IMAGE))
write_rds(fauna_df1, "output/fauna_df1.rds")
write_tsv(fauna_df1, "output/fauna_df1.tsv")
write_xlsx(fauna_df1, "output/fauna_df1.xlsx")
write_csv(fauna_df1, "output/fauna_df1.csv")

## Get the working directory of "fauna_df1.rds"
# str_c(getwd(), "/", dir(pattern="fauna_df1.rds"), sep = "")
# "G:/.shortcut-targets-by-id/1MO3Q9KZIODxlfPRyjLTtDUZuVkecFBp6/Enggano/enggano-dictionary/flora-fauna/fauna_df1.rds"
str_c(getwd(), "/output/fauna_df1.rds", sep = "")
# "G:/.shortcut-targets-by-id/1MO3Q9KZIODxlfPRyjLTtDUZuVkecFBp6/Enggano/enggano-dictionary/flora-fauna/output/fauna_df1.rds"

# create orthography profile from the PHONEME =====
# qlcData::write.profile(c(flora_df_orig_to_archive$PHONEME,
#                          flora_df_orig_to_archive$VARIANT_PHONEME,
#                          fauna_df_orig_to_archive$IPA,
#                          fauna_df_orig_to_archive$VARIANT_PHONEME),
#                        editing = TRUE,
#                        file.out = "ortho/flora-fauna-skeleton.tsv")
## segmentation =====
### flora ====
flora_df_orig_to_archive1 <- flora_df_orig_to_archive |> 
  mutate(PHONEME_SEGMENT = qlcData::tokenize(PHONEME, 
                                             profile = "ortho/flora-fauna-skeleton.tsv", 
                                             method = "global", 
                                             sep.replace = "#", 
                                             normalize = "NFC")$strings[["tokenized"]]) |> 
  mutate(VARIANT_PHONEME_SEGMENT = qlcData::tokenize(VARIANT_PHONEME, 
                                                     profile = "ortho/flora-fauna-skeleton.tsv", 
                                                     method = "global", 
                                                     sep.replace = "#", 
                                                     normalize = "NFC")$strings[["tokenized"]]) |> 
  relocate(PHONEME_SEGMENT, .after = PHONEME) |> 
  relocate(VARIANT_PHONEME_SEGMENT, .after = VARIANT_PHONEME)

### fauna ====
fauna_df_orig_to_archive1 <- fauna_df_orig_to_archive |> 
  mutate(PHONEME_SEGMENT = qlcData::tokenize(IPA, 
                                             profile = "ortho/flora-fauna-skeleton.tsv", 
                                             method = "global", 
                                             sep.replace = "#", 
                                             normalize = "NFC")$strings[["tokenized"]]) |> 
  mutate(VARIANT_PHONEME_SEGMENT = qlcData::tokenize(VARIANT_PHONEME, 
                                                     profile = "ortho/flora-fauna-skeleton.tsv", 
                                                     method = "global", 
                                                     sep.replace = "#", 
                                                     normalize = "NFC")$strings[["tokenized"]]) |> 
  relocate(PHONEME_SEGMENT, .after = IPA) |> 
  relocate(VARIANT_PHONEME_SEGMENT, .after = VARIANT_PHONEME) |> 
  rename(PHONEME = IPA)

# Copy the input-raw data to another directory for archiving ======

write_tsv(flora_df_orig_to_archive1, "input-raw/flora_df_orig.tsv")
write_xlsx(flora_df_orig_to_archive1, "input-raw/flora_df_orig.xlsx")
write_csv(flora_df_orig_to_archive1, "input-raw/flora_df_orig.csv")
write_rds(flora_df_orig_to_archive1, "input-raw/flora_df_orig.rds")

write_tsv(fauna_df_orig_to_archive1, "input-raw/fauna_df_orig.tsv")
write_xlsx(fauna_df_orig_to_archive1, "input-raw/fauna_df_orig.xlsx")
write_csv(fauna_df_orig_to_archive1, "input-raw/fauna_df_orig.csv")
write_rds(fauna_df_orig_to_archive1, "input-raw/fauna_df_orig.rds")

fs::dir_copy("ortho", "C:/Users/GRajeg/OneDrive\ -\ Nexus365/Documents/flora-fauna-raw-data/ortho", overwrite = TRUE)
fs::dir_copy("input-raw", "C:/Users/GRajeg/OneDrive\ -\ Nexus365/Documents/flora-fauna-raw-data/data-raw", overwrite = TRUE)


# Turn into SFM file (NEW with sub-entries coded) ====
## FLORA ====
### Combine the subentries element into long SFM
se_flora <- flora_df1 |> 
  filter(!is.na(MAIN_ENTRY)) |> # retrieve rows that contain info on the root in the MAIN_ENTRY column
  select(-SENSE_ID, -VARIANT_PHONEME, -GAMBAR, -URL) |> 
  mutate(across(matches("VARIANT|CROSSREF|NOTES_EN|NOTES_ID"), ~replace_na(., " "))) |> 
  group_by(MAIN_ENTRY, MAIN_ENTRY_EN, MAIN_ENTRY_IDN) |> 
  mutate(SUBENTRY = paste0("\\se ", ENGGANO, 
                           "\n__\\cf ", CROSSREF, 
                           "\n__\\va ", VARIANT, 
                           "\n__\\ps ", POS, 
                           "\n__\\gn ", INDONESIAN, 
                           "\n__\\ge ", ENGLISH, 
                           "\n__\\pc ", if_else(name != "", paste0(linked_dir, "/flora_photo/", name, sep = ""), ""),
                           "\n__\\nt_en ", NOTES_EN,
                           "\n__\\nt_id ", NOTES_ID,
                           sep = "")) |> 
  select(ENGGANO = MAIN_ENTRY,
         INDONESIAN = MAIN_ENTRY_IDN,
         ENGLISH = MAIN_ENTRY_EN,
         SUBENTRY) |> 
  mutate(SUBENTRY2 = paste0(SUBENTRY, collapse = "\n__")) |> 
  select(-SUBENTRY) |> 
  rename(SUBENTRY = SUBENTRY2) |> 
  distinct()

### Combine the long SFM subentries with the other lexemes and their roots
lx_flora <- flora_df1 |> 
  filter(is.na(MAIN_ENTRY)) |> # retrieve rows that contain info on the root in the MAIN_ENTRY column
  select(-VARIANT_PHONEME, -GAMBAR, -URL, -matches("^MAIN_EN")) |> 
  mutate(across(matches("VARIANT|CROSSREF|NOTES_EN|NOTES_ID"), ~replace_na(., " "))) |> 
  mutate(SENSE_ID = if_else(is.na(SENSE_ID), "0", str_replace_all(SENSE_ID, "^[^_]+_+", ""))) |> 
  mutate(SENSE_ID = replace(SENSE_ID, ENGGANO == "kė'ėh", "0")) |> 
  mutate(MULTISENSE = if_else(SENSE_ID != "0", TRUE, FALSE)) |> 
  left_join(se_flora) # joining the SUBENTRY subset of the flora lexicon processed above
lx_flora_single_sense <- lx_flora |> 
  filter(!MULTISENSE) |> 
  mutate(LX = paste0("\\lx ", ENGGANO, 
                     "\n__\\cf ", CROSSREF, 
                     "\n__\\va ", VARIANT, 
                     "\n__\\ps ", POS, 
                     "\n__\\gn ", INDONESIAN, 
                     "\n__\\ge ", ENGLISH, 
                     "\n__\\pc ", if_else(name != "", paste0(linked_dir, "/flora_photo/", name, sep = ""), ""),
                     "\n__\\nt_en ", NOTES_EN,
                     "\n__\\nt_id ", NOTES_ID,
                     sep = "")) |> 
  mutate(SFM = if_else(is.na(SUBENTRY),
                       paste(LX, "\n", sep = ""),
                       paste(LX, "\n__", SUBENTRY, "\n", sep = ""))) |> 
  select(SFM)
lx_flora_multisense <- lx_flora |> # Picture for multiple sense lexeme needs to be added manually to not mess with the code
  filter(MULTISENSE) |> 
  pivot_longer(cols = c(INDONESIAN, ENGLISH), 
               names_to = "LANG", 
               values_to = "GLOSS") |> 
  pivot_wider(id_cols = -NO, 
              names_from = c(LANG, SENSE_ID), 
              names_glue = "{LANG}_{SENSE_ID}", 
              values_from = c(GLOSS)) |> 
  mutate(LX = paste0("\\lx ", ENGGANO, 
                     "\n__\\cf ", CROSSREF, 
                     "\n__\\va ", VARIANT, 
                     "\n__\\ps ", POS, 
                     "\n__\\gn ", INDONESIAN_1, 
                     "\n__\\ge ", ENGLISH_1, 
                     "\n__\\ps ", POS, 
                     "\n__\\gn ", INDONESIAN_2, 
                     "\n__\\ge ", ENGLISH_2,
                     "\n__\\nt_en ", NOTES_EN,
                     "\n__\\nt_id ", NOTES_ID,
                     "\n\n",
                     sep = "")) |> 
  mutate(SFM = if_else(is.na(SUBENTRY),
                       paste(LX, "\n", sep = ""),
                       paste(LX, "\n__", SUBENTRY, "\n", sep = ""))) |> 
  select(SFM)
lx_flora_sfm <- bind_rows(lx_flora_single_sense, lx_flora_multisense) |> 
  mutate(SFM = str_replace_all(SFM, "\\\\pc C\\:.+LinkedFiles\\/flora_photo\\/\\n", ""),
         SFM = str_replace_all(SFM, "_{2}", "")) |> 
  pull(SFM)
# lx_flora_sfm |> write_lines("output/flora-sfm-20241110.db")


## FAUNA ====
### Combine the subentries element into long SFM
se_fauna <- fauna_df1 |> 
  filter(!is.na(MAIN_ENTRY)) |> # retrieve rows that contain info on the root in the MAIN_ENTRY column
  select(-SENSE_ID, -VARIANT_PHONEME, -IMAGE, -url) |> 
  mutate(across(matches("VARIANT|CROSSREF|NOTES_EN|NOTES_ID"), ~replace_na(., " "))) |> 
  group_by(MAIN_ENTRY, MAIN_ENTRY_EN, MAIN_ENTRY_IDN) |> 
  mutate(SUBENTRY = paste0("\\se ", ENGGANO, 
                           "\n__\\cf ", CROSSREF, 
                           "\n__\\va ", VARIANT, 
                           "\n__\\ps ", POS, 
                           "\n__\\gn ", INDONESIAN, 
                           "\n__\\ge ", ENGLISH, 
                           "\n__\\pc ", if_else(name != "", paste0(linked_dir, "/fauna_photo/", name, sep = ""), ""),
                           "\n__\\nt_en ", NOTES_EN,
                           "\n__\\nt_id ", NOTES_ID,
                           sep = "")) |> 
  select(ENGGANO = MAIN_ENTRY,
         INDONESIAN = MAIN_ENTRY_IDN,
         ENGLISH = MAIN_ENTRY_EN,
         SUBENTRY) |> 
  mutate(SUBENTRY2 = paste0(SUBENTRY, collapse = "\n__")) |> 
  select(-SUBENTRY) |> 
  rename(SUBENTRY = SUBENTRY2) |> 
  distinct()

### Combine the long SFM subentries with the other lexemes and their roots
lx_fauna <- fauna_df1 |> 
  filter(is.na(MAIN_ENTRY)) |> # retrieve rows that contain info on the root in the MAIN_ENTRY column
  select(-VARIANT_PHONEME, -IMAGE, -url, -matches("^MAIN_EN")) |> 
  mutate(across(matches("VARIANT|CROSSREF|NOTES_EN|NOTES_ID"), ~replace_na(., " "))) |> 
  mutate(SENSE_ID = if_else(is.na(SENSE_ID), "0", str_replace_all(SENSE_ID, "^[^_]+_+", ""))) |> 
  mutate(SENSE_ID = replace(SENSE_ID, ENGGANO == "kė'ėh", "0")) |> 
  mutate(MULTISENSE = if_else(SENSE_ID != "0", TRUE, FALSE)) |> 
  left_join(se_fauna) # joining the SUBENTRY subset of the fauna lexicon processed above
lx_fauna_single_sense <- lx_fauna |> 
  filter(!MULTISENSE) |> 
  mutate(LX = paste0("\\lx ", ENGGANO, 
                     "\n__\\cf ", CROSSREF, 
                     "\n__\\va ", VARIANT, 
                     "\n__\\ps ", POS, 
                     "\n__\\gn ", INDONESIAN, 
                     "\n__\\ge ", ENGLISH, 
                     "\n__\\pc ", if_else(name != "", paste0(linked_dir, "/fauna_photo/", name, sep = ""), ""),
                     "\n__\\nt_en ", NOTES_EN,
                     "\n__\\nt_id ", NOTES_ID,
                     sep = "")) |> 
  mutate(SFM = if_else(is.na(SUBENTRY),
                       paste(LX, "\n", sep = ""),
                       paste(LX, "\n__", SUBENTRY, "\n", sep = ""))) |> 
  select(SFM)
lx_fauna_multisense <- lx_fauna |> # Picture for multiple sense lexeme needs to be added manually to not mess with the code
  filter(MULTISENSE) |> 
  pivot_longer(cols = c(INDONESIAN, ENGLISH), 
               names_to = "LANG", 
               values_to = "GLOSS") |> 
  pivot_wider(id_cols = -NO, 
              names_from = c(LANG, SENSE_ID), 
              names_glue = "{LANG}_{SENSE_ID}", 
              values_from = c(GLOSS)) |> 
  mutate(LX = paste0("\\lx ", ENGGANO, 
                     "\n__\\cf ", CROSSREF, 
                     "\n__\\va ", VARIANT, 
                     "\n__\\ps ", POS, 
                     "\n__\\gn ", INDONESIAN_1, 
                     "\n__\\ge ", ENGLISH_1, 
                     "\n__\\ps ", POS, 
                     "\n__\\gn ", INDONESIAN_2, 
                     "\n__\\ge ", ENGLISH_2,
                     "\n__\\nt_en ", NOTES_EN,
                     "\n__\\nt_id ", NOTES_ID,
                     "\n\n",
                     sep = "")) |> 
  mutate(SFM = if_else(is.na(SUBENTRY),
                       paste(LX, "\n", sep = ""),
                       paste(LX, "\n__", SUBENTRY, "\n", sep = ""))) |> 
  select(SFM)
lx_fauna_sfm <- bind_rows(lx_fauna_single_sense, lx_fauna_multisense) |> 
  mutate(SFM = str_replace_all(SFM, "\\\\pc C\\:.+LinkedFiles\\/fauna_photo\\/\\n", ""),
         SFM = str_replace_all(SFM, "_{2}", "")) |> 
  pull(SFM)
# lx_fauna_sfm |> write_lines("output/fauna-sfm-20241110.db")


# Turn into SFM file (OLD) ====
## FLORA ====
flora_df1 |>
  rowwise() |>
  mutate(lx = list(paste0("\\lx ",
                         ENGGANO,
                         # "\n__\\mn ",
                         # MAIN_ENTRY,
                         "\n__\\cf ",
                         CROSSREF,
                         # "\n__\\ph ",
                         # str_replace_all(str_replace_all(PHONEME, "\\/", ""), "ʔ", "ˀ"),
                         "\n__\\va ",
                         VARIANT,
                         "\n__\\ps ",
                         POS,
                         "\n__\\gn ",
                         INDONESIAN,
                         "\n__\\ge ",
                         ENGLISH,
                         "\n__\\pc ",
                         paste0(linked_dir, "/flora_photo/", name, sep = ""),
                         "\n__\\nt ",
                         NOTES_EN,
                         "\n__\\nt_ID ",
                         NOTES_ID,
                         #"\n__\\se ",
                         #SUBENTRY,
                         "\n",
                         sep = ""))) |>
  pull(lx) |>
  map(~str_split(., "_{2}")) |>
  map(unlist) |>
  map(~str_c(., collapse = "")) |>
  unlist() |>
  str_replace_all("NA", " ") |>
  str_replace_all("\\\\pc C\\:.+LinkedFiles\\/flora_photo\\/\\n", "") |>
  write_lines("output/flora-sfm-20241125-to-test-image.db")

## FAUNAS ====
fauna_df1 |>
  rowwise() |>
  mutate(lx = list(paste0("\\lx ",
                          ENGGANO,
                          "\n__\\cf ",
                          CROSSREF,
                          # "\n__\\ph ",
                          # str_replace_all(str_replace_all(PHONEME, "\\/", ""), "ʔ", "ˀ"),
                          "\n__\\va ",
                          VARIANT,
                          "\n__\\ps ",
                          POS,
                          "\n__\\gn ",
                          INDONESIAN,
                          "\n__\\ge ",
                          ENGLISH,
                          "\n__\\pc ",
                          paste0(linked_dir, "/fauna_photo/", name, sep = ""),
                          "\n__\\nt ",
                          NOTES_EN,
                          "\n__\\nt_ID ",
                          NOTES_ID,
                          "\n",
                          sep = ""))) |>
  pull(lx) |>
  map(~str_split(., "_{2}")) |>
  map(unlist) |>
  map(~str_c(., collapse = "")) |>
  unlist() |>
  str_replace_all("NA", " ") |>
  str_replace_all("\\\\pc C\\:.+LinkedFiles\\/fauna_photo\\/\\n", "") |>
  write_lines("output/fauna-sfm-20241125-to-test-image.db")
# 
# # Combined the data for Flora and Fauna for Pak Cok's team ==========
# flora_df1 |> 
#   select(category, NO, ENGGANO, POS, PHONEME, SENSE_ID, CROSSREF, INDONESIAN, ENGLISH, NOTES_EN, NOTES_ID, url, name) |> 
#   bind_rows(fauna_df1 |> 
#               select(category, NO, ENGGANO, POS, PHONEME, CROSSREF, INDONESIAN, ENGLISH, NOTES_EN, NOTES_ID, url, name))
# 
# 
# read_lines("flora-fauna-export-from-FLEx-20240928.db") |> 
#   str_extract_all("^\\\\[^ ]+?(?= )") |> 
#   unlist() |> 
#   unique()
