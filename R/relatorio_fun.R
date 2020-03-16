text.suspect <- function(test_name, test_type, summary_link, product3_link) {
  summary <- rio::import(summary_link);
  product3 <- rio::import(product3_link);
  if (test_type == "hourly") {test_unit <- "hora"};
  if (test_type == "daily") {test_unit <- "dia"};
  test_letter <-
    product3_link %>%
    strsplit("/") %>% unlist() %>% tail(1) %>%
    strsplit("-") %>% unlist() %>% head(1) %>%
    strsplit("") %>% unlist() %>% tail(1);
  n_emas <- nrow(product3);
  product3_suspect <- product3 %>% dplyr::filter(tot > 0);
  n_emas_suspect <- product3_suspect  %>% nrow();
  if (n_emas_suspect == 0) {
    text <- paste0(
      "A aplicação do teste de ", test_name, " '",  test_letter, "' nas ", n_emas,
      " EMAs selecionadas não gerou nenhuma EMA que apresentou dados considerados suspeitos.")
  }
  if (n_emas_suspect == 1) {
    tot_mean_suspect <- product3_suspect$tot %>% mean() %>% round(digits = 0);
    perc_mean_suspect <- product3_suspect$perc %>% mean() %>% round(digits = 4);
    most_suspect_ema <- product3_suspect %>% dplyr::arrange(desc(tot)) %>% dplyr::filter(tot == max(tot));
    mse_site <- most_suspect_ema$site;
    mse_tot <- most_suspect_ema$tot;
    mse_perc <- most_suspect_ema$perc;
    most_suspect_ema_summary <- summary %>% filter(site %in% mse_site);
    mses_state <- most_suspect_ema_summary$state;
    mses_name <- most_suspect_ema_summary$name;
    text <- paste0(
      "A aplicação do teste de ", test_name, " '",  test_letter, "' nas ", n_emas,
      " EMAs selecionadas gerou ", n_emas_suspect,
      " EMA que apresentou dados considerados suspeitos. A média da porcentagem de dados considerados suspeitos nesse teste foi ",
      perc_mean_suspect, "% (≈ ", tot_mean_suspect,
      " ", test_unit, "), e a EMA que apresentou a maior porcentagem de dados considerados suspeitos foi a ",
      mse_site, " – ", mses_name, " (", mses_state, "), com ", mse_perc, "% (", mse_tot, " ", test_unit ,").")
  }
  if (n_emas_suspect > 1) {
    tot_mean_suspect <- product3_suspect$tot %>% mean() %>% round(digits = 0);
    perc_mean_suspect <- product3_suspect$perc %>% mean() %>% round(digits = 4);
    most_suspect_ema <- product3_suspect %>% dplyr::arrange(desc(tot)) %>% dplyr::filter(tot == max(tot));
    mse_site <- most_suspect_ema$site;
    mse_tot <- most_suspect_ema$tot;
    mse_perc <- most_suspect_ema$perc;
    most_suspect_ema_summary <- summary %>% filter(site %in% mse_site);
    mses_state <- most_suspect_ema_summary$state;
    mses_name <- most_suspect_ema_summary$name;
    text <- paste0(
      "A aplicação do teste de ", test_name, " '",  test_letter, "' nas ", n_emas,
      " EMAs selecionadas gerou ", n_emas_suspect,
      " EMAs que apresentaram dados considerados suspeitos. A média da porcentagem de dados considerados suspeitos nesse teste foi ",
      perc_mean_suspect, "% (≈ ", tot_mean_suspect,
      " ", test_unit, "s), e a EMA que apresentou a maior porcentagem de dados considerados suspeitos foi a ",
      mse_site, " – ", mses_name, " (", mses_state, "), com ", mse_perc, "% (", mse_tot, " ", test_unit, "s).")
  }
  return(text);
}

plot.suspect.by.ema <- function(DIR_PRODUCT3) {
  product3 <- rio::import(DIR_PRODUCT3);
  qc <- unique(product3$qc);
  qc_product <- product3 %>% dplyr::filter(!is.na(tot));
  #qc_product <- product3 %>% dplyr::mutate(tot = ifelse(is.na(tot), 0, tot));
  if (unique(qc_product$n_obs) == 78912) {time_unit <- "horas"};
  if (unique(qc_product$n_obs) == 3288) {time_unit <- "dias"};
  psbe <-
    ggplot2::qplot(
      x = as.numeric(names(table(qc_product$tot))),
      y = as.numeric(table(qc_product$tot))) +
    ggplot2::labs(
      x = paste0("Número de casos considerados suspeitos em cada EMA (em ", time_unit, ")"),
      y = "Quantidade de EMAs com casos considerados suspeitos") +
    ggplot2::labs(title = paste("Quantidade de EMAs com casos considerados suspeitos no teste", qc)) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10))
    #    ggplot2::scale_x_continuous(
    #      breaks = scales::pretty_breaks(n = as.numeric(names(table(qc_product$tot))) %>% max())) +
    #    ggplot2::scale_y_continuous(
    #      breaks = scales::pretty_breaks(n = as.numeric(table(qc_product$tot)) %>% max())) +
    #    ggplot2::geom_line()
    return(psbe)
}

#DIR_PRODUCT3 = "../jbc-ic/output/qc5a-summary-tavg-data-inmet-2008-2016-4yrs-south.rds"
















