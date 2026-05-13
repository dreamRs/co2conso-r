library(dplyr)
library(apexcharter)
library(lubridate)
library(data.table)

mois_actuel <- function(donnees) {
  donnees %>%
    filter(date_heure_utc <= lubridate::now(tzone = "UTC")) %>%
    summarise(mois = lubridate::month(max(date_cet), label = TRUE, abbr = FALSE, locale = "fr_FR")) %>%
    pull(mois)
}

box_mois <- function(donnees) {
  mois_ac <- mois_actuel(donnees)
  data_plot <- donnees %>%
    mutate(
      nom_mois = lubridate::month(date_cet, label = TRUE, abbr = FALSE, locale = "fr_FR"),
      annee = year(date_cet)
    ) %>%
    filter(nom_mois == mois_ac)
  
  apex(data = data_plot, type = "boxplot", mapping = aes(x = annee, y = intensite_emissions_conso)) %>%
    ax_plotOptions(boxPlot = boxplot_opts(color.upper = "#1B5E20", color.lower = "#1B5E20")) %>%
    ax_xaxis(title = list(text = "Année")) %>%
    ax_yaxis(title = list(text = "Intensité carbone (gCO2/kWh)"))
}

graphique_periodes <- function(donnees, periode = "24h") {
  now_cet <- max(donnees$date_heure_utc, na.rm = TRUE) %>%
    lubridate::with_tz("Europe/Paris")
  
  data_plot <- donnees %>%
    mutate(datetime_cet = lubridate::with_tz(date_heure_utc, "Europe/Paris")) %>%
    filter(datetime_cet >= now_cet - switch(periode,
                                            "24h" = lubridate::hours(24),
                                            "30j" = lubridate::days(30),
                                            "1an" = lubridate::days(365)
    ))
  
  y_min <- floor(min(data_plot$intensite_emissions_conso, na.rm = TRUE) * 0.9)
  
  apex(data_plot, aes(datetime_cet, intensite_emissions_conso), type = "line", serie_name = "Intensité consommation") %>%
    ax_chart(defaultLocale = "fr", zoom = list(enabled = TRUE), toolbar = list(show = TRUE)) %>%
    ax_colors("#1B5E20") %>%
    ax_stroke(width = 1) %>%
    ax_xaxis(title = list(text = "Date") , type = "datetime", labels = list(datetimeUTC = FALSE)) %>%
    ax_yaxis(title = list(text = "gCO₂éq/kWh"), decimalsInFloat = 2, min = y_min) %>%
    ax_tooltip(
      x = list(format = "dd/MM/yyyy à HH:mm"),
      y = list(formatter = format_num(",", suffix = " gCO₂éq/kWh", locale = "fr-FR"))
    )
}