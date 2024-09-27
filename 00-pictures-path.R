library(readxl)
library(tidyverse)
library(googledrive)
library(googlesheets4)

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

# Store the list of files and directories inside the flora and fauna directory
flora_fauna_drive <- drive_ls(path = as_id("https://drive.google.com/drive/u/0/folders/1QF1kvNglqZQC7sjJ1ryHRbzB4XMzgvfH"))

# List the files inside the flora picture directory
flora_picts <- drive_ls(path = as_id("https://drive.google.com/drive/u/0/folders/1u31JkgLzssdC24LxXN1Vy2eL281scLDx"))

# Pre-processing the naming of some of the files
flora_picts1 <- flora_picts |> 
  
  # Create the Google Drive URL for each file based on the numeric id
  mutate(url = paste("https://drive.google.com/file/d/", id, "/view", sep = "" )) |> 
  
  # Check if the file is named using number or not
  mutate(photo_numeric_name = if_else(str_detect(name, "^[0-9 \\(\\)]+\\.(JPG|jpg|jpeg|png|PNG)$"),
                                      TRUE,
                                      FALSE)) |> 
  mutate(name2 = str_replace(name, "\\.(jpe?g|JPG|png|PNG)$", "")) |> 
  mutate(name2 = replace(name2,
                         name2 == "ku pi",
                         "142"),
         name2 = replace(name2,
                         name2 == "ku apu",
                         "6"),
         name2 = replace(name2,
                         name2 == "ku ea",
                         "125"),
         name2 = replace(name2,
                         name2 == "ku kiko'op",
                         "134"),
         name2 = replace(name2,
                         str_detect(name2, "^sugar\\-cane"),
                         "60"),
         name2 = replace(name2,
                         str_detect(name2, "^rose\\-"),
                         "160")
         ) |> 
  mutate(name = replace(name,
                         url == "https://drive.google.com/file/d/18rKWb_NnAnQpj-g3wIIg1Qh-VrNtQAy6/view",
                         "161 (1).JPG"),
         name = replace(name,
                        url == "https://drive.google.com/file/d/1x86K4faSIUv_ZmmQPAr5lygQDyje7pz5/view",
                        "162 (1).JPG"),
         name = replace(name,
                        url == "https://drive.google.com/file/d/1T0ZHXrux9bPiVTpHtcCiPHWMkn8ZYE5U/view",
                        "140 (1).JPG"))

# Copy flora photos to flora-fauna linked files directory ====
# Linked Directory path:
linked_dir <- "C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles"

# IMPORTANT: The following code needs to be re-run whenever there is photo update from Dendi
# If there is any cropping in the FLEx directly, first copy the crop/edited photos in "C:\ProgramData\SIL\FieldWorks\Projects\flora-fauna\LinkedFiles\Pictures" to the Google Drive folder and rename the edited photo in the Google Drive with "_...".
# fs::dir_delete("C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/flora_photo")
# fs::dir_delete("C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/Pictures")
# fs::dir_copy("flora_photo", "C:/ProgramData/SIL/FieldWorks/Projects/flora-fauna/LinkedFiles/flora_photo", overwrite = TRUE)
# IMPORTANT: Delete one of the Bougenville entries/photos

# Read the G Sheet containing the lexicon of the flora
flora_df <- read_sheet(flora_fauna_drive[flora_fauna_drive$name == "flora_with_picture", ][["id"]]) |> 
  mutate(NO = as.character(NO)) |> 
  filter(!ENGLISH %in% c("jellyfish", "handle", "arranged coral", "fish bone"),
         NO != "138")

flora_df1 <- flora_df |> 
  left_join(flora_picts1 |> 
              rename(NO = name2) |> 
              select(NO, url, name)) |> 
  mutate(name = replace_na(name, ""))

## Check the number of absent photos
flora_df1 |> filter(is.na(url))


# Turn into SFM file ====
flora_df1 |> 
  rowwise() |> 
  mutate(lx = list(paste0("\\lx ", 
                         ENGGANO, 
                         # "\n__\\ph ", 
                         # str_replace_all(str_replace_all(PHONEME, "\\/", ""), "ʔ", "ˀ"), 
                         "\n__\\ps ", 
                         POS, 
                         "\n__\\gn ", 
                         INDONESIAN, 
                         "\n__\\ge ", 
                         ENGLISH,
                         "\n__\\pc ",
                         paste0(linked_dir, "/flora_photo/", name, sep = ""),
                         "\n__\\nt ",
                         NOTES,
                         "\n",
                         sep = ""))) |> 
  pull(lx) |> 
  map(~str_split(., "_{2}")) |> 
  map(unlist) |> 
  map(~str_c(., collapse = "")) |> 
  unlist() |> 
  str_replace_all("NA", " ") |> 
  str_replace_all("\\\\pc C\\:.+LinkedFiles\\/flora_photo\\/\\n", "") |> 
  write_lines("flora-sfm-20240928.db")
