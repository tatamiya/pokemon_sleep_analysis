configure_plot_font <- function() {
  candidate_families <- c(
    "Noto Sans CJK JP",
    "Noto Sans JP",
    "Hiragino Sans",
    "Yu Gothic",
    "Arial Unicode MS"
  )

  installed_families <- unique(
    systemfonts::system_fonts()$family
  )

  selected_family <- candidate_families[
    candidate_families %in% installed_families
  ][1]

  if (is.na(selected_family)) {
    message(
      "日本語フォントが見つからないため、",
      "既定のsansフォントを使用します。"
    )
    selected_family <- "sans"
  }

  font_path <- systemfonts::match_font(
    selected_family
  )$path

  sysfonts::font_add(
    family = "noto",
    regular = font_path
  )
  showtext::showtext_auto()

  "noto"
}
