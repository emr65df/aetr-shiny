#'This makes a table for each row of the data so that the tooltip can work
make_table <- function(df, use_year, use_acep_energy_region, use_sector) {
  df %>%
    dplyr::filter(year == use_year & acep_energy_region == use_acep_energy_region & sector == use_sector) %>%
    select(year, weighted_price) %>%
    kableExtra::kbl(digits = 3, row.names = FALSE) %>%
    # change the font family and increase font size
    kableExtra::kable_styling(bootstrap_options = "striped", font_size = 12, html_font = "Courier New") %>%
    # increase the width of the columns, make the text blue and bold, apply white background
    kableExtra::column_spec(1:2, width = "1cm", bold = T, background = "white")
}
