#'This makes a table for each row of the data so that the tooltip can work
make_capacity_table <- function(df, use_year, use_prime_mover) {
  df %>%
    dplyr::filter(year == use_year & prime_mover == use_prime_mover) %>%
    select(year, prime_mover, capacity) %>%
    kableExtra::kbl(digits = 3, row.names = FALSE) %>%
    # change the font family and increase font size
    kableExtra::kable_styling(bootstrap_options = "striped", font_size = 12, html_font = "Courier New") %>%
    # increase the width of the columns, make the text blue and bold, apply white background
    kableExtra::column_spec(1:2, width = "1cm", bold = T, background = "black")
}

make_price_table <- function(df, use_year, use_acep_energy_region, use_sector) {
  df %>%
    dplyr::filter(year == use_year & acep_energy_region == use_acep_energy_region & sector == use_sector) %>%
    select(year, weighted_price) %>%
    kableExtra::kbl(digits = 3, row.names = FALSE) %>%
    # change the font family and increase font size
    kableExtra::kable_styling(bootstrap_options = "striped", font_size = 12, html_font = "Courier New") %>%
    # increase the width of the columns, make the text blue and bold, apply white background
    kableExtra::column_spec(1:2, width = "1cm", bold = T, background = "black")
}

