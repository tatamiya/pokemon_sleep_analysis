library(dplyr)
library(googlesheets4)
library(readr)

sheet_url <- Sys.getenv("POKEMON_SLEEP_SHEET_URL")

if (!nzchar(sheet_url)) {
  stop(
    paste(
      "環境変数 POKEMON_SLEEP_SHEET_URL に",
      "Google SheetsのURLを設定してください。"
    )
  )
}

analysis_start_date <- as.Date("2025-11-16")
analysis_end_date <- as.Date("2026-09-05")
output_path <- file.path(
  "data",
  "ingredient_patterns_2025-11-16_2026-09-05.csv"
)

source_data <- read_sheet(
  sheet_url,
  sheet = "記録一覧"
)

required_columns <- c(
  "日付",
  "種類",
  "食材パターン",
  "2枠種フラグ",
  "2枠目",
  "3枠目",
  "収集開始前分"
)

missing_columns <- setdiff(
  required_columns,
  names(source_data)
)

if (length(missing_columns) > 0) {
  stop(
    "必要な列がありません: ",
    paste(missing_columns, collapse = ", ")
  )
}

public_data <- source_data %>%
  filter(
    is.na(`収集開始前分`),
    as.Date(日付) >= analysis_start_date,
    as.Date(日付) <= analysis_end_date
  ) %>%
  transmute(
    日付 = as.Date(日付),
    ポケモン名 = as.character(種類),
    食材種数 = if_else(
      is.na(`2枠種フラグ`),
      3L,
      2L
    ),
    食材パターン = as.character(食材パターン),
    `2枠目` = as.character(`2枠目`),
    `3枠目` = as.character(`3枠目`)
  ) %>%
  arrange(日付)

stopifnot(
  nrow(public_data) > 0,
  all(!is.na(public_data$ポケモン名)),
  all(nzchar(trimws(public_data$ポケモン名))),
  all(public_data$食材種数 %in% c(2L, 3L)),
  all(public_data$食材パターン %in% c(
    "AAA", "AAB", "AAC",
    "ABA", "ABB", "ABC"
  )),
  all(public_data$`2枠目` %in% c("A", "B")),
  all(public_data$`3枠目` %in% c("A", "B", "C"))
)

write_csv(
  public_data,
  output_path,
  na = ""
)

message(
  "公開用データを書き出しました: ",
  output_path,
  " (", nrow(public_data), "行)"
)
