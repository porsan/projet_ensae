library(ggplot2)
library(shiny)
#library(markdown)

# Repertoire de sauvergarde des donnees
#data_dir <- "/Volumes/Data2/R programing/Projets/projet_ensae/data/"
data_dir <- "./data/"
# Chargement des donnees
performances_model_presse <- read.csv(paste0(data_dir,"performances_model_presse.csv"))
performances_model_presse <- performances_model_presse[-1]
final_results_presse <- read.csv(paste0(data_dir,"final_results_presse.csv"))
final_results_presse <- final_results_presse[-1]

performances_model_spectateurs <- read.csv(paste0(data_dir,"performances_model_spectateurs.csv"))
performances_model_spectateurs <- performances_model_spectateurs[-1]
final_results_spectateurs <- read.csv(paste0(data_dir,"final_results_spectateurs.csv"))
final_results_spectateurs <- final_results_spectateurs[-1]

textes <- read.csv(paste0(data_dir,"textes.csv"))

RMSE_def <- "root-mean-square error (RMSE) ou root-mean-square deviation (RMSD). C'est la racine carrée l’erreur quadratique moyenne d’un estimateur {\displaystyle {\hat {\theta }}} {\hat  \theta } d’un paramètre {\displaystyle \theta } \theta  de dimension 1 (mean squared error ( {\displaystyle \operatorname {MSE} } {\displaystyle \operatorname {MSE} }), en anglais) est une mesure caractérisant la « précision » de cet estimateur. Elle est plus souvent appelée « erreur quadratique », « moyenne » étant sous-entendu) ; elle est parfois appelée aussi « risque quadratique ». https://en.wikipedia.org/wiki/Root-mean-square_deviation" #a(href="", "link"),
RSquared_def <- "La somme des carrés des résidus (SCR ou Sum of Squared Errors). Comme on mesure des carrés, on majore l’importance des grosses erreurs. https://en.wikipedia.org/wiki/Coefficient_of_determination" #a(href="", "link"),
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
  
  output$text_resultat <- renderText({ "Ceci est un texte" })
  output$description <- renderText({ "Ceci est un texte" })
  output$mesure <- renderPlot({
    
    # ggplot(performances_model_presse) + 
    #   geom_bar(aes(x = model, y = Rsquared, fill = Rsquared), stat="identity", width = 0.75 ) +
    #   labs(title = "Performances des différents modèles sur note presse", subtitle = "R2 sur échantillon de validation") + 
    #   theme_minimal()  +  ylim(0, 0.8) + 
    #   scale_fill_gradient(low="dark red", high="light green") +
    #   theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
    #         plot.subtitle = element_text(size = 15),
    #         plot.caption = element_text(face = "italic"), 
    #         axis.text.x = element_text(size = 12, face = "bold"),
    #         axis.text.y = element_blank(),
    #         legend.position = "null") + 
    #   geom_text(aes(x = model, y = Rsquared, label = round(Rsquared, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)
    
    ggplot(performances_model_presse) + 
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
    
  })
  output$distribution <- renderPlot({

    qplot(final_results_presse["valeur_reelle"], final_results_presse["Gradient"])
    sapply(final_results_presse, max)
    
    # Visualisation des distributions obtenues
    data <- reshape2::melt(final_results_presse) 
    ggplot(data) + geom_histogram(aes(x=value, fill = variable), bins = 30) + 
      facet_wrap(~variable, ncol = 2) +
      labs(title ="Distribution des notes presse selon le modèle choisi", subtitle = "Echantillon de validation") +
      theme_minimal() + theme(legend.position = "null")
    
  })
  
  output$nuage <- renderPlot({
    
    # Visualisation des nuages de points Y réel / Y estimé
    data2 <- cbind(data, y_reel = rep(final_results_presse["valeur_reelle"], 8))
    data2 <- data2[data2$variable != "valeur_reelle",]
    #data2 <- cbind(data, y_reel = rep(final_results_presse["valeur_reelle"], 8))[data2$variable != "valeur_reelle",]
    ggplot(data2) + geom_abline(aes(intercept=0, slope=1), color = "light grey") + geom_jitter(aes(x=value, y = y_reel.valeur_reelle,color = variable), shape = 1) + 
      facet_wrap(~variable, ncol = 3) +
      labs(title ="Distribution des notes presse selon le modèle choisi", subtitle = "Echantillon de validation") +
      theme_minimal() + theme(legend.position = "null")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
#runApp(display.mode="showcase")
