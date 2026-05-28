

nom_mois <- function(donnees) {
  mois_fr <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
               "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
  numero_mois <- donnees %>%
    filter(date_heure_utc <= lubridate::now(tzone = "UTC")) %>%
    summarise(mois = lubridate::month(max(date_cet))) %>%
    pull(mois)
  
    mois_fr[numero_mois]
}

num_vers_mois <- function(numero) {
  mois_fr <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
               "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
  mois_fr[numero]
}

box_mois <- function(donnees, mode = c("mensuel", "annuel")){
  mode <- match.arg(mode)
  
  mois_ac <- lubridate::month(max(donnees$date_cet))
  
  data_plot <- donnees %>%
    mutate(
      num_mois = lubridate::month(date_cet),
      annee = year(date_cet))
  
  data_plot <- switch (mode, 
                     "mensuel" = data_plot %>% filter(num_mois == mois_ac), 
                     "annuel" = data_plot)
  
  apex(
    data = data_plot,
    type = "boxplot", 
    mapping = aes(x = annee, y = intensite_emissions_conso)
  ) %>%
    ax_chart(defaultLocale = "fr") %>%
    ax_plotOptions(
      boxPlot = boxplot_opts(color.upper = "#1B5E20", color.lower = "#1B5E20")
    ) %>%
    ax_xaxis(
      title = list(text = "Année")
    ) %>%
    ax_yaxis(
      title = list(text = "Intensité des émissions (en gCO₂éq/kWh)")
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
    add_hline(value = mean(data_plot$intensite_emissions_conso,na.rm = TRUE), dash = 5, 
              label = label(text = paste("Moyenne :",round(mean(data_plot$intensite_emissions_conso,na.rm = TRUE),1),"gCO₂éq/kWh"),
              position = "left",
              offsetX = 120))%>%
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
      title = list(text = "Intensité des émissions (en gCO₂éq/kWh)"), 
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
    mutate(mois = factor(num_vers_mois(lubridate::month(date_heure_cet)), levels = num_vers_mois(1:12)),
           annee = year(date_heure_cet))%>%
    group_by(mois,annee)%>%
    summarise(moyenne= mean(intensite_emissions_conso, na.rm = TRUE), .groups = "drop")
  
  apex(data = donnees, type = "heatmap", mapping = aes(x= annee , y= mois, fill = moyenne))%>%
    ax_colors("#1B5E20")%>%
    ax_tooltip(
      y = list(formatter = htmlwidgets::JS("function(val) { return val.toFixed(1) + ' gCO₂/kWh' }"))
    ) %>%
    ax_chart(defaultLocale = "fr")%>%
    #ax_title(text = "Intensité carbone par mois et année") %>%
    ax_xaxis(title = list(text = "Année")) %>%
    ax_yaxis(title = list(text = "Mois")) %>%
    ax_dataLabels(enabled = FALSE)
}

heatmap_jour <- function(donnees){
  annee_ac <- year(max(donnees$date_heure_cet, na.rm = TRUE))
  
  donnees <- donnees %>%
    mutate(mois = factor(num_vers_mois(lubridate::month(date_heure_cet)), levels = num_vers_mois(1:12)),
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
    ax_chart(defaultLocale = "fr")%>%
    #ax_title(text = paste("Intensité carbone -", format(Sys.Date(), "%Y"))) %>%
    ax_xaxis(title = list(text = "Mois")) %>%
    ax_yaxis(title = list(text = "Jour")) %>%
    ax_dataLabels(enabled = FALSE) }
    
    
histogramme1 <- function(donnees){
  ggplot(donnees, aes(intensite_emissions_conso)) +
  geom_histogram(fill = "#81C784", color = "white")+
    labs(
      x = "Intensité carbone (gCO2éq/kWh)",
      y = "Nombre d'heures"
    )+
    theme_minimal() 
  }
  
histogramme <- function(donnees){
  h <- hist(donnees$intensite_emissions_conso, plot = FALSE, breaks = 30)
  df <- data.frame(
    x = h$mids,
    y = h$counts
  )
  
  apex(
    data = df,
    aes(x = x, y = y),
    type = "column"
  ) %>%
    ax_chart(defaultLocale = "fr")%>%
    ax_xaxis(title = list(text = "Intensité carbone (gCO₂éq/kWh)")) %>%
    ax_yaxis(title = list(text = "Nombre d'heures")) %>%
    ax_colors("#1B5E20") %>%
    ax_plotOptions(bar = bar_opts(columnWidth = "100%")) %>%
    ax_dataLabels(enabled = FALSE) %>%
    ax_tooltip(y = list(title = list(formatter = JS("function() { return 'Nombre d\\'heures :' }"))))
}

format_date_fr <- function(date) {
  paste(format(date, "%d"), num_vers_mois(lubridate::month(date)), format(date, "%Y"))
}


histo_derniere <- function(donnees){
  
  now_cet <- max(donnees$date_heure_cet, na.rm = TRUE)
  
  data_plot <- donnees %>%
    filter(date_heure_cet >= now_cet - lubridate::years(1))
  
  ggplot(data_plot, aes(intensite_emissions_conso)) +
    geom_histogram(fill = "#81C784", color = "white")+
    labs(
      title = paste("Distribution", format_date_fr(now_cet - lubridate::years(1)),"-", format_date_fr(now_cet)),
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
    ax_chart(defaultLocale = "fr")%>%
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
