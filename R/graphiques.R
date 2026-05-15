

mois_actuel <- function(donnees) {
  donnees %>%
    filter(date_heure_utc <= lubridate::now(tzone = "UTC")) %>%
    summarise(mois = lubridate::month(max(date_cet), label = TRUE, abbr = FALSE)) %>%
    pull(mois)
}

box_mois <- function(donnees) {
  mois_ac <- mois_actuel(donnees)
  data_plot <- donnees %>%
    mutate(
      nom_mois = lubridate::month(date_cet, label = TRUE, abbr = FALSE),
      annee = year(date_cet)
    ) %>%
    filter(nom_mois == mois_ac)
  
  apex(
    data = data_plot,
    type = "boxplot", 
    mapping = aes(x = annee, y = intensite_emissions_conso)
  ) %>%
    ax_plotOptions(
      boxPlot = boxplot_opts(color.upper = "#1B5E20", color.lower = "#1B5E20")
    ) %>%
    ax_xaxis(
      title = list(text = "Année")
    ) %>%
    ax_yaxis(
      title = list(text = "Intensité carbone (gCO₂éq/kWh)")
    )
}

graphique_periodes <- function(donnees, periode = c("24h", "7j", "30j", "1an")) {
  
  periode <- match.arg(periode)
  
  now_cet <- max(donnees$date_heure_utc, na.rm = TRUE) %>%
    lubridate::with_tz("Europe/Paris")
  
  data_plot <- donnees %>%
    mutate(datetime_cet = lubridate::with_tz(date_heure_utc, "Europe/Paris")) %>%
    filter(
      datetime_cet >= now_cet - switch(
        periode,
        "24h" = lubridate::hours(24),
        "7j" = lubridate::days(7),
        "30j" = lubridate::days(30),
        "1an" = lubridate::years(1)
      )
    )
  
  y_min <- floor(min(data_plot$intensite_emissions_conso, na.rm = TRUE) * 0.9)
  
  apex(
    data = data_plot,
    mapping = aes(datetime_cet, intensite_emissions_conso), 
    type = "line",
    serie_name = "Intensité consommation"
  ) %>%
    ax_chart(
      defaultLocale = "fr",
      zoom = list(enabled = TRUE),
      toolbar = list(show = TRUE)
    ) %>%
    ax_colors("#1B5E20") %>%
    ax_stroke(width = 1) %>%
    ax_xaxis(
      title = list(text = "Date") ,
      type = "datetime",
      labels = list(datetimeUTC = FALSE)
    ) %>%
    ax_yaxis(
      title = list(text = "gCO₂éq/kWh"), 
      decimalsInFloat = 2, 
      min = 0
    ) %>%
    ax_tooltip(
      x = list(format = "dd/MM/yyyy à HH:mm"),
      y = list(formatter = format_num(",", suffix = " gCO₂éq/kWh", locale = "fr-FR"))
    )
}

heatmap_annee <- function(donnees){
  donnees <- donnees %>%
    mutate(mois= lubridate ::month(date_heure_cet, label = TRUE, abbr= TRUE),
           annee = year(date_heure_cet))%>%
    group_by(mois,annee)%>%
    summarise(moyenne= mean(intensite_emissions_conso, na.rm = TRUE), .groups = "drop")
  
  apex(data = donnees, type = "heatmap", mapping = aes(x= annee , y= mois, fill = moyenne))%>%
    ax_colors("#1B5E20")%>%
    ax_tooltip(
      y = list(formatter = htmlwidgets::JS("function(val) { return val.toFixed(1) + ' gCO₂/kWh' }"))
    ) %>%
    ax_title(text = "Intensité carbone par mois et année") %>%
    ax_xaxis(title = list(text = "Année")) %>%
    ax_yaxis(title = list(text = "Mois")) %>%
    ax_dataLabels(enabled = FALSE)
}

heatmap_jour <- function(donnees){
  annee_ac <- year(max(donnees$date_heure_utc, na.rm = TRUE))
  
  donnees <- donnees %>%
    mutate(mois= lubridate ::month(date_heure_cet, label = TRUE, abbr= TRUE),
           jour = day(date_heure_cet),
           annee = year(date_heure_cet))%>%
    filter(annee == annee_ac)%>%
    group_by(mois,jour)%>%
    summarise(moyenne = mean(intensite_emissions_conso, na.rm = TRUE), .groups = "drop")
  
  apex(data = donnees, type = "heatmap", mapping = aes(x= mois , y= jour, fill = moyenne))%>%
    ax_colors("#1B5E20")%>%
    ax_tooltip(
      y = list(formatter = htmlwidgets::JS("function(val) { return val.toFixed(1) + ' gCO₂/kWh' }"))
    ) %>%
    ax_title(text = paste("Intensité carbone -", format(Sys.Date(), "%Y"))) %>%
    ax_xaxis(title = list(text = "Mois")) %>%
    ax_yaxis(title = list(text = "Jour")) %>%
    ax_dataLabels(enabled = FALSE) }
    
    
histogramme <- function(donnees){
  ggplot(donnees, aes(intensite_emissions_conso)) +
  geom_histogram(fill = "#81C784", color = "white")+
    labs(
      title= "Distribution de l'intensité carbone",
      x = "Intensité carbone (gCO2éq/kWh)",
      y = "Nombre d'heures"
    )+
    theme_minimal() 
  }
  
  
histo_derniere <- function(donnees){
  
  now_cet <- max(donnees$date_heure_utc, na.rm = TRUE) %>%
    lubridate::with_tz("Europe/Paris")
  
  data_plot <- donnees %>%
    filter(date_heure_cet >= now_cet - lubridate::years(1))
  
  ggplot(data_plot, aes(intensite_emissions_conso)) +
    geom_histogram(fill = "#81C784", color = "white")+
    labs(
      title = paste("Distribution", format(now_cet - lubridate::years(1), "%d %B %Y"), "-", format(now_cet, "%d %B %Y")),
      x = "Intensité carbone (gCO2éq/kWh)",
      y = "Nombre d'heures"
    )+
    theme_minimal()
}   

evolution_annuelle <- function(donnees){
  data_plot <- donnees %>%
    mutate(annee = year(date_heure_cet)) %>%
    group_by(annee) %>%
    summarise(moyenne = round(mean(intensite_emissions_conso, na.rm = TRUE), 2), .groups = "drop") %>%
    mutate(annee = as.character(annee))
  
  apex(data = data_plot, type = "bar", mapping = aes(x = annee, y = moyenne)) %>%
    ax_colors("#1B5E20") %>%
    ax_title(text = "Évolution de l'intensité carbone moyenne par année") %>%
    ax_xaxis(title = list(text = "Année")) %>%
    ax_yaxis(title = list(text = "gCO₂éq/kWh"), decimalsInFloat = 2) %>%
    ax_tooltip(
      y = list(formatter = htmlwidgets::JS("function(val) { return val.toFixed(2) + ' gCO₂éq/kWh' }"))
    ) %>%
    ax_dataLabels(enabled = FALSE,
                  formatter = htmlwidgets::JS("function(val) { return val.toFixed(2) }"))
}