library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)
library(readxl)
library(openxlsx)
library(scales)

# Reading data
aduanas_df <- read_excel("data/2016-2017.xlsx", sheet = 1, col_types = "text")

competition_data <- map_dfr(excel_sheets("data/Envio Barbi competidores final.xlsx"), ~read_excel("data/Envio Barbi competidores final.xlsx", sheet = ., range = cell_cols("B:H"), col_types = "text"))  # Data given from: Wella, Revlon-AV & Schwarzkoft

names(competition_data) <- names(competition_data) %>%
  make.names() %>%
  iconv(to="ASCII//TRANSLIT") %>%
  str_replace_all("[^[:alnum:]]", "")

names(aduanas_df) <- names(aduanas_df) %>%
  make.names() %>%
  iconv(to = "ASCII//TRANSLIT") %>%
  str_replace_all("[^[:alnum:]]", "")

# Filter aduanas data to keep Wella, Revlon-AV & Schwarzkoft
aduanas_wrs<- aduanas_df %>%
  filter(RAZONSOCIAL %in% c("COLOMER ANDINA S.A.", "DISTRIBUIDORA LAS PONCIANAS S A",
                            "CRISOL COMERCIAL PERU S.A.C.", "HENKEL PERUANA S.A.",
                            "QUIMICA SUIZA S A"))  # Revlon, Schwarzkoft, Wella (Quimica Suiza)

# In Aduanas data, create a column with the concatenation of the variables:
# - DESCRIPCION COMERCIAL
# - DESCRIPCION DE PRESENTACION
# - DESCRIPCION DE COMPOSICION
# - DESCRIPCION ADICIONAL

aduanas_wrs <- aduanas_wrs %>%
  mutate(descripcion_consolidada = str_c(ifelse(is.na(DESCRIPCIONCOMERCIAL), "", DESCRIPCIONCOMERCIAL),
                                         ifelse(is.na(DESCRIPCIONDEPRESENTACION), "", DESCRIPCIONDEPRESENTACION),
                                         ifelse(is.na(DESCRIPCIONDECOMPOSICION), "", DESCRIPCIONDECOMPOSICION),
                                         ifelse(is.na(DESCRIPCIONADICIONAL), "", DESCRIPCIONADICIONAL),
                                         sep = " "
                                         ))

aduanas_wrs$descripcion_consolidada <- str_replace_all(aduanas_wrs$descripcion_consolidada, "(\\d\\d+)([[:alpha:]]+)", "\\1 \\2")

# In competition_data, combine "Marcaletras" with "Nombrecompletodelskuletras"
competition_data <- mutate(competition_data,
                           marca_nombre = str_c(str_to_upper(Marcaletras), str_to_upper(Nombrecompletodelskuletras), sep = " "))

# In aduanas_wrs, add the brands to "descripcion_consolidada"
rs_marca <- tibble(RAZONSOCIAL = c("QUIMICA SUIZA S A", "HENKEL PERUANA S.A.", "DISTRIBUIDORA LAS PONCIANAS S A", 
                                   "CRISOL COMERCIAL PERU S.A.C.", "COLOMER ANDINA S.A."),
                   MARCA = c("WELLA - SEBASTIAN", "SCHWARZKOFT", "REVLON REVLONISSIMO STYLE MASTERS OROFLUIDO BLONDERFUL A AMERICAN CREW",
                             "SCHWARZKOFT", "REVLON REVLONISSIMO STYLE MASTERS OROFLUIDO BLONDERFUL A AMERICAN CREW"))  # Correct name: 'Schwarzkopf', not 'Schwarzkoft'

aduanas_wrs <- aduanas_wrs %>%
  left_join(rs_marca, by = "RAZONSOCIAL") %>%
  mutate(descripcion_consolidada = str_c(MARCA, descripcion_consolidada, sep = " "))

# To improve the ratio of success, add the category to 'descripcion_consolidada' & 'marca_nombre'.
# Consider only part of the category: Care, Color or Styling (SkinCare?)
competition_data <- competition_data %>%
  mutate(marca_nombre = str_c(ifelse(Categoriadeproductoletras == "Hair Care", "HCARE",
                                     ifelse(str_detect(Categoriadeproductoletras, "Color"), "COLOR",
                                            ifelse(str_detect(Categoriadeproductoletras, "Styling"), "STYLING", "SCARE"))),
                              marca_nombre, sep = " "))

aduanas_wrs <- aduanas_wrs %>%
  mutate(descripcion_consolidada = str_c(ifelse(str_detect(descripcion_consolidada,
                                                           "(SHAMP)|(ACOND)|(CONDIT)|(COND)|(CLEA)|(TRATAMIENT)|(TREATMEN)"),
                                                "HCARE", #TRUE
                                                elseif(str_detect(descripcion_consolidada,
                                                                  "(BLONDOR)|(TINT)"),
                                                       "COLOR",  #TRUE
                                                       elseif()
                                                       )
                                                ),
                                         descripcion_consolidada))



# Get matches in aduanas_wrs$descripcion_consolidada ----
list_index_max <- aduanas_wrs$descripcion_consolidada %>% str_split(boundary("word")) %>%
  map(function(mw){
    n_matches <- map_dbl(str_split(competition_data$marca_nombre, boundary("word")), ~sum(unique(mw) %in% .))
    results <- c(index_match = which.max(n_matches), max_matches = max(n_matches),
                 n_similar_matches = sum(n_matches ==  max(n_matches)))  # the index for competition_data
    results
  })  # Initially, I used competition_data$Nombrecompletodelskuletras. But a better approach was to use the concatenation of "Marcaletras" with "Nombrecompletodelskuletras", i.e., "marca_nombre"

# Code to check the ratio of success ----
list_index_max %>%
  map_dbl(3) %>%
  table %>%
  sort(decreasing = TRUE) %>%
  as_tibble() %>%
  mutate(perc_descrip_consol = percent(n / sum(n))) %>%
  rename(n_similar_matches = ".", n_descrip_consol = n)


tst <- aduanas_wrs$DESCRIPCIONDEPRESENTACION %>% head %>% str_split(boundary("word")) %>%
  map(function(mw){
    n_matches <- map_dbl(str_split(head(aduanas_wrs$DESCRIPCIONCOMERCIAL), boundary("word")), ~sum(mw %in% .))
    results <- c(index_match = which.max(n_matches), max_matches = max(n_matches))
    results
  })

head(cars) %>% mutate(mylistcol = tst) %>% mutate(index_match = map_dbl(tst, 1), max_matches = map_dbl(tst, 2))