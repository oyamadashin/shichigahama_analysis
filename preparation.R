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


# 変数の再構成----

## 営農意向w1----

# ファクター化して順序をつける
order_v1_farming_intention_w1 <- c("する", "しない", "わからない")

df <- df |> mutate(
  v1_farming_intention_w1 = factor(v1_farming_intention_w1, 
                                   levels = order_v1_farming_intention_w1)) 

## 営農意向w2----  

# ファクター化して順序をつける
df <- df |> 
  mutate(
    v9_farming_intention_w2 = factor(
      v9_farming_intention_w2,
      levels = 1:5,
      labels = c(
        "規模拡大", "現状と同じ", "規模縮小", "やめたい", "迷っている"
      )
    )
  )
  

## 営農意向w3----  

# ファクター化して順序をつける
df <- df |> 
  mutate(
    v19_farming_intention_w3 = factor(
      v19_farming_intention_w3,
      levels = 1:6,
      labels = c(
        "規模拡大", "現状維持", "規模縮小", "現状維持（貸してる）", "やめたい", "迷っている"
      )
    )
  ) 
