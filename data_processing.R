library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)
library(readxl)
library(openxlsx)

# Reading data
aduanas_df <- read_excel("data/2016-2017.xlsx", sheet = 1, col_types = "text")

competition_data <- map_dfr(excel_sheets("data/Envio Barbi competidores final.xlsx"), ~read_excel("data/Envio Barbi competidores final.xlsx", sheet = ., range = cell_cols("B:H"), col_types = "text"))  # Data given from: Wella, Revlon-AV & Schwarzkoft

