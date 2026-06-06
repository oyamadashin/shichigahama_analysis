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

## 組合構成員member----

member_id <- c(
  279, 45, 35, 52, 94,
  123, 196, 12, 2, 225,
  223, 209, 189, 177,
  90, 143, 157, 146
)

df <- df |> 
  mutate(
    member = if_else(id %in% member_id, 1, 0)
  )


## 地区w2----
df <- df |> 
  mutate(
    district_w2 = recode(
      district_w2,
      "1" = "町外",
      "2" = "湊浜",
      "3" = "松ヶ浜",
      "4" = "菖蒲田浜",
      "5" = "花渕浜",
      "6" = "吉田浜",
      "7" = "代ヶ崎浜",
      "8" = "東宮浜",
      "9" = "要害",
      .default = NA_character_
    )
  ) |> 
  mutate(
    district_w2 = 
      factor(district_w2,
             levels = 
               c("町外",
                 "湊浜",
                 "松ヶ浜",
                 "菖蒲田浜",
                 "花渕浜",
                 "吉田浜",
                 "代ヶ崎浜",
                 "東宮浜",
                 "要害"))
  )
  

  

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


## 営農意向w4----
# ダミー変数を1つの変数に変換（単一回答の質問なので1つ変数で扱う方が適切）
# ファクター化してレベルもつける
df <- df %>%
  mutate(
    v35_farming_intention_w4 = case_when(
      v35_farming_intention_expansion_w4 == 1 ~ "経営拡大する",
      v35_farming_intention_maintain_status_quo_w4 == 1 ~ "現状維持",
      v35_farming_intention_reduction_w4 == 1 ~ "経営縮小する",
      v35_farming_intention_quit_w4 == 1 ~ "農業をやめたい",
      v35_farming_intention_lend_w4 == 1 ~ "農地を貸したい",
      v35_farming_intention_sell_w4 == 1 ~ "農地を売却したい",
      TRUE ~ NA_character_
    )
  ) |> 
  mutate(
    v35_farming_intention_w4 = factor(
      v35_farming_intention_w4,
      levels = c(
        "経営拡大する",
        "現状維持",
        "経営縮小する",
        "農業をやめたい",
        "農地を貸したい",
        "農地を売却したい"
      )
    )
  )


## 営農形態意向w3----
# 変数名がわかりにくいので修正。ついでにrecodeもしておく。
df <- df |> 
  mutate(
    v20_farming_type_w3 = 
      recode(v20_farming_continuation_management_type,
             "1" = "家族経営",
             "2" = "農業生産法人",
             "3" = "集落営農組織",
             "4" = "その他"
      )
  )

## 営農形態意向w4----
# ダミー変数ではなく単一の変数にする。
df <- df |> 
  mutate(
    v36_farming_type_w4 = case_when(
      v36_farming_type_corporate_w4 == 1 ~ "町内営農組織",
      v36_farming_type_individual_farm_w4 == 1 ~ "個別経営",
      TRUE ~ NA_character_
    ) 
  )


## 機械被害状況w1----

df <- df |> 
  mutate(
    across(
      c(v3_rice_transplanter,
        v3_tractor,
        v3_combine_harvester, 
        v3_crop_harvester, 
        v3_grain_dryer),
      ~ case_when(
        . == "使用可" ~ 0,
        . == "使用不能" ~ 1,
        TRUE ~ NA_real_
      )
    )
  ) 

machine_vars <- c(
  "v3_rice_transplanter",
  "v3_tractor",
  "v3_combine_harvester", 
  "v3_crop_harvester", 
  "v3_grain_dryer"
)

df <- df |>
  mutate(
    v3_machine_loss_number_w1 = if_else(
      if_all(all_of(machine_vars), is.na),
      NA_real_,
      rowSums(pick(all_of(machine_vars)), na.rm = TRUE)
    )
  )


## 機械被害状況w2----

df <- df |> 
  mutate(
    v7_machinery_damage_status = recode(
      v7_machinery_damage_status,
      "1" = "被災なし",
      "2" = "被災あり",
      "9" = NA_character_,
      .default = NA_character_
    )
  )
