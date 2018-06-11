library(ggplot2)
library(shiny)
#library(markdown)

# Chargement des donnees presse
performances_model_presse <- read.csv("performances_model_presse.csv")
performances_model_presse <- performances_model_presse[-1]
final_results_presse <- read.csv("final_results_presse.csv")
final_results_presse <- final_results_presse[-1]

# Creation des graphes de performances presse
# graphe de performance RMSE presse
graph_RMSE_presse <- ggplot(performances_model_presse) + 
  geom_bar(aes(x = model, y = RMSE, fill = RMSE), stat="identity", width = 0.85 ) +
  labs(title = "Performances des différents modèles sur note presse", subtitle = "RMSE sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.7) + 
  scale_fill_gradient(low="green", high="red") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "light grey",colour = "black",size = 1),
        axis.text.x = element_text(size = 8, face = "bold", angle = 315, lineheight = 1),
        axis.text.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = RMSE, label = round(RMSE, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)
# graphe de performance Rsquared presse
graph_Rsquared_presse <- ggplot(performances_model_presse) + 
  geom_bar(aes(x = model, y = Rsquared, fill = Rsquared), stat="identity", width = 0.85 ) +
  labs(title = "Performances des différents modèles sur note presse", subtitle = "Rsquared sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.7) + 
  scale_fill_gradient(low="green", high="red") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "light grey",colour = "black",size = 1),
        axis.text.x = element_text(size = 8, face = "bold", angle = 315, lineheight = 1),
        axis.text.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = Rsquared, label = round(Rsquared, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)
# graphe de performance MAE presse
graph_MAE_presse <- ggplot(performances_model_presse) + 
  geom_bar(aes(x = model, y = MAE, fill = MAE), stat="identity", width = 0.85 ) +
  labs(title = "Performances des différents modèles sur note presse", subtitle = "MAE sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.7) + 
  scale_fill_gradient(low="green", high="red") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "light grey",colour = "black",size = 1),
        axis.text.x = element_text(size = 8, face = "bold", angle = 315, lineheight = 1),
        axis.text.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = MAE, label = round(MAE, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)

# Creation du graphe des distributions presse
data <- reshape2::melt(final_results_presse) 
graph_distrib_presse <- ggplot(data) + geom_histogram(aes(x=value, fill = variable), bins = 30) + 
  facet_wrap(~variable, ncol = 2) +
  labs(title ="Distribution des notes presse selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")

# Creation du graphe des nuages presse
data2 <- cbind(data, y_reel = rep(final_results_presse["valeur_reelle"], 8))
data2 <- data2[data2$variable != "valeur_reelle",]
data2 <- as.data.frame(data2)
graph_nuage_presse <- ggplot(data2) + geom_abline(aes(intercept=0, slope=1), color = "light grey") + geom_jitter(aes(x=value, y = y_reel.valeur_reelle,color = variable), shape = 1) + 
  facet_wrap(~variable, ncol = 3) +
  labs(title ="Distribution des notes presse selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")

# Chargement des donnees spectateurs
performances_model_spectateurs <- read.csv("performances_model_spectateurs.csv")
performances_model_spectateurs <- performances_model_spectateurs[-1]
final_results_spectateurs <- read.csv("final_results_spectateurs.csv")
final_results_spectateurs <- final_results_spectateurs[-1]

# Creation des graphes de performances spectateurs
# graphe de performance RMSE spectateurs
graph_RMSE_spectateurs <- ggplot(performances_model_spectateurs) + 
  geom_bar(aes(x = model, y = RMSE, fill = RMSE), stat="identity", width = 0.85 ) +
  labs(title = "Performances des différents modèles sur note spectateurs", subtitle = "RMSE sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.7) + 
  scale_fill_gradient(low="green", high="red") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "light grey",colour = "black",size = 1),
        axis.text.x = element_text(size = 8, face = "bold", angle = 315, lineheight = 1),
        axis.text.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = RMSE, label = round(RMSE, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)
# graphe de performance Rsquared spectateurs
graph_Rsquared_spectateurs <- ggplot(performances_model_spectateurs) + 
  geom_bar(aes(x = model, y = Rsquared, fill = Rsquared), stat="identity", width = 0.85 ) +
  labs(title = "Performances des différents modèles sur note spectateurs", subtitle = "Rsquared sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.7) + 
  scale_fill_gradient(low="green", high="red") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "light grey",colour = "black",size = 1),
        axis.text.x = element_text(size = 8, face = "bold", angle = 315, lineheight = 1),
        axis.text.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = Rsquared, label = round(Rsquared, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)
# graphe de performance MAE spectateurs
graph_MAE_spectateurs <- ggplot(performances_model_spectateurs) + 
  geom_bar(aes(x = model, y = MAE, fill = MAE), stat="identity", width = 0.85 ) +
  labs(title = "Performances des différents modèles sur note spectateurs", subtitle = "MAE sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.7) + 
  scale_fill_gradient(low="green", high="red") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "light grey",colour = "black",size = 1),
        axis.text.x = element_text(size = 8, face = "bold", angle = 315, lineheight = 1),
        axis.text.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = MAE, label = round(MAE, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)

# Creation du graphe des distributions spectateurs
data <- reshape2::melt(final_results_spectateurs) 
graph_distrib_spectateurs <- ggplot(data) + geom_histogram(aes(x=value, fill = variable), bins = 30) + 
  facet_wrap(~variable, ncol = 2) +
  labs(title ="Distribution des notes spectateurs selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")

# Creation du graphe des nuages spectateurs
data2 <- cbind(data, y_reel = rep(final_results_spectateurs["valeur_reelle"], 8))
data2 <- data2[data2$variable != "valeur_reelle",]
data2 <- as.data.frame(data2)
graph_nuage_spectateurs <- ggplot(data2) + geom_abline(aes(intercept=0, slope=1), color = "light grey") + geom_jitter(aes(x=value, y = y_reel.valeur_reelle,color = variable), shape = 1) + 
  facet_wrap(~variable, ncol = 3) +
  labs(title ="Distribution des notes spectateurs selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")

#textes <- read.csv(paste0(data_dir,"textes.csv"))

RMSE_def <- "Root-mean-square error (RMSE) ou root-mean-square deviation (RMSD). C'est la racine carrée de l’erreur quadratique moyenne d’un estimateur. Elle est plus souvent appelée « erreur quadratique »,(« moyenne » étant sous-entendu) ; elle est parfois appelée aussi « risque quadratique »." #, a(href="https://en.wikipedia.org/wiki/Root-mean-square_deviation","lien wiki"))
#RMSE_def <- 'a(href="https://en.wikipedia.org/wiki/Root-mean-square_deviation","lien wiki")'
Rsquared_def <- "La somme des carrés des résidus (SCR ou Sum of Squared Errors). Comme on mesure des carrés, on majore l’importance des grosses erreurs. https://en.wikipedia.org/wiki/Coefficient_of_determination" #a(href="", "link"),
MAE_def <- "L’erreur absolue moyenne (MAE pour Mean Absolute Error) : moyenne arithmétique des valeurs absolues des écarts. https://en.wikipedia.org/wiki/Mean_absolute_error" #a(href="", "link"),
Resultats_txt <- "Présentation des résultats des modèles appliquées sur les données pour définir la note moyenne presse ou spectateurs."



ui <- fluidPage(
  
  navbarPage("Projet Allociné",
             tabPanel("Les données",
                      verbatimTextOutput("donnees")
             ),
             tabPanel("Les résultats",
                      fluidPage(
                        fluidRow(column(width = 3),
                                 textOutput("text_resultat"),
                                 column(width = 12),
                                 radioButtons("choix_press_spect", "Notes : ",
                                     c("presse","spectateur"), "presse", inline = TRUE)),
                        fluidRow(column(width = 12),
                                 tabsetPanel(tabPanel("Mesures",
                                                      selectInput("select_mesure", "mesure choisie :", c("RMSE","Rsquared","MAE"),
                                                                  "RMSE", multiple = FALSE),
                                                      textOutput("description"),
                                                      plotOutput("mesure")),
                                             tabPanel("Distributions", plotOutput("distribution")),
                                             tabPanel("Nuages", plotOutput("nuage"))
                                 ))
                      )
                      
             ),
             tabPanel("Textmining",
                      textOutput("textmining")
             ),
             navbarMenu("Plus",
                        tabPanel("Presentation",
                                 verbatimTextOutput("table")
                        ),
                        tabPanel("About",
                                 fluidRow(
                                   column(6,
                                          #includeMarkdown("about.md")
                                          img(class="img-polaroid",
                                              src=paste0("http://upload.wikimedia.org/",
                                                         "wikipedia/commons/9/92/",
                                                         "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                          tags$small(
                                            "Source: Photographed at the Bay State Antique ",
                                            "Automobile Club's July 10, 2005 show at the ",
                                            "Endicott Estate in Dedham, MA by ",
                                            a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                              "User:Sfoskett")
                                   )),
                                   column(3,
                                          img(class="img-polaroid",
                                              src=paste0("http://upload.wikimedia.org/",
                                                         "wikipedia/commons/9/92/",
                                                         "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                          tags$small(
                                            "Source: Photographed at the Bay State Antique ",
                                            "Automobile Club's July 10, 2005 show at the ",
                                            "Endicott Estate in Dedham, MA by ",
                                            a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                              "User:Sfoskett")
                                          )
                                   )
                                 )
                        )
             )
  )
)

server <- function(input, output) {
  
  output$text_resultat <- renderText({ Resultats_txt })
  output$description <- renderText({
      switch(input$select_mesure,
             "RMSE" = RMSE_def,
             "Rsquared" = Rsquared_def,
             "MAE" = MAE_def,
             "NA")
  })
  output$mesure <- renderPlot({
    if(input$choix_press_spect == "presse") {
         switch(input$select_mesure,
                "RMSE" = graph_RMSE_presse,
                "Rsquared" = graph_Rsquared_presse,
                "MAE" = graph_MAE_presse,
                "NA")
       } else {
         switch(input$select_mesure,
                "RMSE" = graph_RMSE_spectateurs,
                "Rsquared" = graph_Rsquared_spectateurs,
                "MAE" = graph_MAE_spectateurs,
                "NA")
       }
    
  })
  output$distribution <- renderPlot({
    if(input$choix_press_spect == "presse") { graph_distrib_presse } else { graph_distrib_spectateurs}
  })
  
  output$nuage <- renderPlot({
    if(input$choix_press_spect == "presse") { graph_nuage_presse } else { graph_nuage_spectateurs}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
#runApp(display.mode="showcase")
