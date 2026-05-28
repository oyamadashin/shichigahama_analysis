# パッケージ読み込み----
library(tidyverse)
library(readxl)
library(purrr)

# データセットの準備----
## データ読み込み----
df_id_name <- read_excel("data/data_id_name.xlsx")
dw1 <- read_excel("data/data_shichigawahama_w1.xlsx")
dw2 <- read_excel("data/data_shichigawahama_w2.xlsx")
dw3 <- read_excel("data/data_shichigawahama_w3.xlsx")
dw4 <- read_excel("data/data_shichigawahama_w4.xlsx")

## idをキーにしてデータ結合----
df <- list(df_id_name, dw1, dw2, dw3, dw4) |> 
  reduce(full_join, by = "id")