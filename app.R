library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(bs4Dash)
library(vroom)
library(cluster)
library(pROC)
library(corrplot)
library(caret)  # Pour la fonction findCorrelation
library(DT)
library(ROSE)

# Chargement des données
bank <- vroom("~/Bank/data/bank-additional-full.csv", delim = ";", show_col_types = FALSE)

# Convertir les variables catégorielles en facteurs
bank$job <- as.factor(bank$job)
bank$marital <- as.factor(bank$marital)
bank$education <- as.factor(bank$education)
bank$default <- as.factor(bank$default)
bank$housing <- as.factor(bank$housing)
bank$loan <- as.factor(bank$loan)
bank$contact <- as.factor(bank$contact)
bank$month <- as.factor(bank$month)
bank$day_of_week <- as.factor(bank$day_of_week)
bank$poutcome <- as.factor(bank$poutcome)
bank$y <- as.factor(bank$y)

# Chargement des données
#bank <- vroom("~/Bank/data/bank-additional-full.csv", delim = ";", show_col_types = FALSE)


# Exemple de préparation des données et K-means (à adapter selon vos données)
data <- bank %>% 
  select(age, duration, campaign, emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed)

# Normalisation des données
data_scaled <- scale(data)

# Appliquer K-means avec 4 clusters (vous pouvez ajuster ce nombre en fonction de la méthode du coude)
set.seed(123)
kmeans_result <- kmeans(data_scaled, centers = 4, nstart = 25)
bank$cluster <- as.factor(kmeans_result$cluster)

# Résumé des segments
summary_segments <- bank %>%
  group_by(cluster) %>%
  summarise(
    mean_age = mean(age),
    mean_duration = mean(duration),
    mean_campaign = mean(campaign),
    mean_emp_var_rate = mean(emp.var.rate),
    mean_cons_price_idx = mean(cons.price.idx),
    mean_cons_conf_idx = mean(cons.conf.idx),
    mean_euribor3m = mean(euribor3m),
    mean_nr_employed = mean(nr.employed),
    .groups = 'drop'
  )

# Calcul des taux de réponse
response_rates <- bank %>%
  group_by(cluster) %>%
  summarise(
    total_clients = n(),
    responded_yes = sum(y == "yes"),
    response_rate = mean(y == "yes")
  )


Profils_des_segments <- data.frame(
  cluster = c(1, 2, 3, 4),
  Profil_démographique = c("Adultes matures, âge moyen 40,56 ans." , "Plus âgés, âge moyen 46,42 ans.", "Jeunes adultes, âge moyen 37,85 ans.", "Adultes matures, âge moyen 40,17 ans."),
  Profil_comportemental = c("Modérément sollicités (12 campagnes), engagement moyen (176 sec).", "Faible saturation (1,82 campagnes), engagement élevé (283 sec).", "Faible saturation (2,15 campagnes), engagement élevé (267 sec).", "Faible saturation (2,20 campagnes), engagement élevé (256 sec):Adapter les campagnes pour cibler les retraités et les diplômés universitaires, qui montrent une plus forte propension à répondre."),
  Profil_économique = c("Contexte favorable : emploi en hausse (+1,28%), confiance modérée (-39,92), Euribor élevé (4,89%).", "Contexte difficile : chômage élevé (-2,82%), faible confiance (-31,74%), Euribor bas (0,78%).", "Contexte fragile : chômage modéré (-1,83%), confiance très basse (-45,57%), Euribor bas (1,27%).", "Contexte favorable : emploi en hausse (+1,10%), confiance modérée (-39,43), Euribor élevé (4,81%).")
)

Analyse_discriminante <- data.frame(
  Coefficients = c(0.27, 0.36, 0.0046, -0.044, -0.61, 0.63, -0.0019),
  Variables = c(
    "educationuniversity.degree (p=0.003) : Les personnes ayant un diplôme universitaire sont également plus susceptibles d'une réponse positive.",
    "jobretired (p=5.1e-4) : Les retraités sont plus susceptibles d'une certaine réponse (par ex., souscrire un produit).",
    "duration (p=0.000) : Une augmentation de la durée de l'appel est fortement associée à une probabilité accrue de l'événement étudié (coefficient très significatif).",
    "campaign (p=0.00012) : Un plus grand nombre de contacts dans la campagne actuelle réduit significativement la probabilité de réponse. Éviter de sur-solliciter les clients, car une augmentation des contacts (variable campaign) réduit l'efficacité.",
    "emp.var.rate (p=1.3e-19) : Un taux de variation de l'emploi plus élevé réduit significativement la probabilité de réponse.",
    "cons.price.idx (p=3.1e-10) : Une augmentation de l'indice des prix à la consommation augmente la probabilité de réponse.",
    "pdays (p=1.03e-105) : Les jours écoulés depuis la dernière campagne sont significatifs et négatifs, indiquant que plus de temps écoulé réduit la probabilité de réponse."
  ),
  stringsAsFactors = FALSE
)

# Recommandations
recommandations <- data.frame(
  Cluster = c(1, 2, 3, 4),
  Positionnement = c(
    "Offres d’investissement et de placements à taux fixes pour tirer parti de leur stabilité économique.",
    "Produits sécurisés (épargne, assurance) adaptés aux contextes de faible emploi.",
    "Offres à court terme, à faible risque, et ajustées à leur capacité d’investissement limitée.",
    "Offres diversifiées : placements financiers, crédits immobiliers, ou produits de retraite."
  ),
  strategie = c(
    "Renforcer leur engagement par des propositions long terme et personnalisées.",
    "Éviter la surcharge d’informations, privilégier la clarté et la simplicité dans les messages.",
    "Proposer des promotions et des solutions accessibles pour gagner leur confiance.",
    "Maximiser leur engagement avec des campagnes ciblées qui mettent en avant la stabilité économique."
  )
)

# Créer un tableau explicatif des variables
variable_info <- data.frame(
  Variable = c("age", "job", "marital", "education", "default", "housing", "loan", 
               "contact", "month", "day_of_week", "duration", "campaign", "pdays", 
               "previous", "poutcome", "emp.var.rate", "cons.price.idx", "cons.conf.idx", 
               "euribor3m", "nr.employed", "y"),
  Description = c(
    "Âge du client", 
    "Profession du client (ex : housemaid, services, admin.)", 
    "Statut matrimonial du client (marié, célibataire, etc.)", 
    "Niveau d'éducation du client (ex : basic.4y, high.school, etc.)", 
    "Indicateur du défaut de paiement (oui/non)",
    "Indicateur si le client a un prêt immobilier (oui/non)", 
    "Indicateur si le client a un prêt personnel (oui/non)",
    "Mode de contact utilisé pour la campagne (ex : téléphone)",
    "Mois du contact (ex : mai, juin, etc.)", 
    "Jour de la semaine du contact (ex : lundi, mardi)",
    "Durée de l'appel ou du contact", 
    "Nombre de contacts au cours de la campagne actuelle", 
    "Nombre de jours depuis le dernier contact",
    "Nombre de contacts précédents", 
    "Résultat de la campagne précédente (ex : nonexistent, failure)",
    "Taux de variation de l'emploi", 
    "Indice des prix à la consommation",
    "Indice de confiance des consommateurs", 
    "Taux d'intérêt EURIBOR à 3 mois", 
    "Nombre d'employés", 
    "Indicateur si le client a souscrit à l'offre bancaire (oui/non)"
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Analyse Marketing Bancaire"),
  dashboardSidebar(
    skin = "dark",
    status = "info",
    collapsed = FALSE,
    sidebarMenu(
      userDescription(
        title = "Emmanuel Paguiel",
        subtitle = "Data Analyst",
        #email = emmanuelpaguiel@gmail.com,
        image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRb18GDTC-t6Hoq9Kw-BidEm2Weym_3pZdAfA7QlZb8e4S7JuzDVge49FI&s"
       ),
      
      menuItem("Vue d'ensemble", tabName = "overview", icon = icon("dashboard")),
      menuItem("Analyse exploratoire", tabName = "temporal_analysis", icon = icon("calendar")),
      menuItem("Analyse de la performance de la campagne", tabName = "campaigns", icon = icon("chart-line")),
      menuItem("Segmentation des clients", tabName = "segmentation", icon = icon("chart-pie")),
      menuItem("Prédiction du succès de la campagne", tabName = "prediction", icon = icon("lightbulb")),
      menuItem("Analyse Discriminante", tabName = "lda_analysis", icon = icon("chart-line")),
      menuItem("Synthèse", tabName = "synthèse", icon = icon("lightbulb")),
      menuItem("Tableau Explicatif des Variables", tabName = "variables", icon = icon("table")),
      menuItem("Exploration des Données", tabName = "data_exploration", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".box { overflow-x: auto; }"))), # CSS to handle overflow
    tabItems(
      # Tableau explicatif des variables
      tabItem(tabName = "variables",
              fluidRow(
                box(title = "Tableau Explicatif des Variables", status = "primary", solidHeader = TRUE, 
                    width = 12, DTOutput("variablesTable"))
              )
      ),
    
      tabItem(tabName = "lda_analysis",
              h2("Analyse Factorielle Discriminante (AFD): Pour éviter que le modèle soit biaisé en faveur de la classe majoritaire no (88.7) contre yes (11.27) nous avons choisit de rééquilibrer les classes."),
              fluidRow(
                box(title = "Résumé de l'Analyse Discriminante", status = "info", solidHeader = TRUE, width = 6, 
                    verbatimTextOutput("lda_summary")
                ),
                box(title = "Axes Discriminants", status = "primary", solidHeader = TRUE, width = 6, 
                    plotlyOutput("lda_plot")
                ),
                box(title = "Commentaires", status = "success", solidHeader = TRUE, width = 12, 
                    verbatimTextOutput("comment")
                )
                
              )),
      tabItem(
        tabName = "overview",  # Onglet "Overview" dans l'interface
        
        # Première ligne : Sélecteur et boîtes de valeur pour les prêts immobiliers/personnels
        fluidRow(
          column(6,
                 valueBoxOutput("housing_loan_admin"),  # Prêts immobiliers (admin)
                 valueBoxOutput("personal_loan_admin")  # Prêts personnels (admin)
          ),
          column(6,
                 selectInput(
                   "job_selector",  # ID du sélecteur
                   "Sélectionnez une profession :",  # Label
                   choices = unique(bank$job),  # Liste des professions disponibles
                   selected = "admin."  # Valeur par défaut
                 ),
                 valueBoxOutput("housing_loan_dynamic"),  # Prêts immobiliers (profession sélectionnée)
                 valueBoxOutput("personal_loan_dynamic")  # Prêts personnels (profession sélectionnée)
          )
        ),
        
        # Deuxième ligne : Statistiques générales sur les clients et réponses
        fluidRow(
          valueBoxOutput("total_clients", width = 2),  # Nombre total de clients
          valueBoxOutput("yes_count", width = 3),  # Nombre de réponses positives
          valueBoxOutput("no_count", width = 2),  # Nombre de réponses négatives
          valueBoxOutput("response_rate_telephone"),  # Taux de réponse par téléphone
          valueBoxOutput("email_response_rate", width = 3),  # Taux de réponse par email
          valueBoxOutput("response_rate_month", width = 3)  # Taux de réponse par mois
        ),
        
        # Quatrième ligne : Informations sur les prêts et professions
        fluidRow(
          valueBoxOutput("loan_clients", width = 2),  # Nombre de clients avec prêt
          valueBoxOutput("loan_yes_ratio", width = 3),  # Ratio de réponses positives pour les clients avec prêt
          valueBoxOutput("most_common_job", width = 3),  # Profession la plus courante
          valueBoxOutput("top_job_yes", width = 3)  # Profession avec le plus de réponses positives
        ),
        
        # Cinquième ligne : Informations sur les groupes d'âge
        fluidRow(
          valueBoxOutput("avg_call_duration", width = 2),  # Durée moyenne des appels
          valueBoxOutput("calls_per_month", width = 3),  # Nombre d'appels par mois
          valueBoxOutput("age_group_max", width = 3)  # Groupe d'âge avec le plus de réponses
        )
      ),
      
      
      tabItem(tabName = "campaigns",
              h2("Analyse de la performance de la campagne"),
              p("Objectif : Identifier les caractéristiques des campagnes réussies."),
              fluidRow(
                box(title = "Taux de Conversion Global", width = 4, status = "primary",
                    plotlyOutput("global_conversion")),
                box(title = "Prêts Immobiliers et Personnels", width = 4, status = "primary",
                    plotlyOutput("y_distribution")),
                box(title = "Taux par Canal de Communication", width = 4, status = "primary",
                    plotlyOutput("contact_conversion"))
                
              ),
              
              fluidRow(
                box(title = "Impact de la durée de l'appel", 
                    width = 4,
                    status = "primary", 
                    solidHeader = TRUE,
                    div(style = "display: flex; justify-content: space-between;",
                        column(6,selectInput("campaignsMonthInput", "Mois", choices = unique(bank$month), selected = unique(bank$month)[1])),
                        column(6,selectInput("campaignsDayInput", "Jour de la semaine", choices = unique(bank$day_of_week), selected = unique(bank$day_of_week)[1]))
                    ),
                    plotlyOutput("durationPlot")),
                box(title = "Taux de réponse", width = 4, status = "primary", solidHeader = TRUE, 
                    plotlyOutput("responseRatePlot")),
                box(title = "Performance par Mois", width = 4, status = "primary",
                    plotlyOutput("month_conversion"))
              ),
              
      h3("Taux de réponse global"),
      fluidRow(
        box(
          title = "Taux de réponse global",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          verbatimTextOutput("global_response_summary"),
          p(
            "Interprétation : Le total représente le nombre de clients. ",
            "Yes_count est le nombre de réponses positives (souscriptions). ",
            "No_count est le nombre de réponses négatives. ",
            "Le taux (rate) indique le pourcentage de clients ayant souscrit à l'offre."
          )
        )
      ),
      tabsetPanel(
        tabPanel("Par profession", plotlyOutput("plot_job")),
        tabPanel("Par état matrimonial", plotlyOutput("plot_marital")),
        tabPanel("Par niveau d'éducation", plotlyOutput("plot_education"))
      ),
      h4("Analyse de la performance de la campagne"),
      fluidRow(
        box(
          title = "Taux de réponse global",
          verbatimTextOutput("global_response_rate"),
          width = 2
        ),
        box(
          title = "Durée des appels par réponse",
          plotOutput("duration_plot"),
          width = 4
      ),
        box(
          title = "Effet des contacts précédents",
          tableOutput("contacts_summary"),
          width = 6
        ))
      ),
      # Onglet Segmentation
      tabItem(tabName = "segmentation",
              h2("Segmentation des clients"),
              p("Objectif : Personnaliser les campagnes selon les segments."),
              fluidRow(
                # Graphiques de segmentation
                box(title = "Visualisation des Clusters", status = "primary", solidHeader = TRUE,
                    plotlyOutput("clusterPlot", height = 400)),
                box(title = "Distribution des Clients", status = "primary", solidHeader = TRUE,
                    plotOutput("agePlot", height = 400)),
                box(
                  title = "Tableau de Contingence : Répartition des Emplois", status = "primary", solidHeader = TRUE,
                  dataTableOutput("contingency_table")
                ),
                box(
                  title = "Visualisation : Répartition des Emplois", status = "primary", solidHeader = TRUE,
                  plotlyOutput("job_distribution_plot")
                  
                )),
                # Tableau des segments
                box(title = "Résumé des Segments", status = "primary", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("summaryTable"), 
                    height = "400px", style = "overflow-y: scroll;"),

      box(title = "Validation des Segments", status = "warning", solidHeader = TRUE, width = 12,
          fluidRow(
            column(6, DTOutput("responseTable")),
            column(6, plotOutput("responsePlot", height = 400)),
            # Commentaires sur les résultats
            box(title = "Commentaires des Résultats", width = NULL, status = "info", solidHeader = TRUE,
                textOutput("comment1"))
          ))), 
      
      # Onglet Analyse Temporelle
      tabItem(
        tabName = "temporal_analysis",
        h2("Analyse Temporelle et Saisonnière"),
        
        p("Objectif : Identifier les périodes optimales pour lancer des campagnes."),
        fluidRow(
          valueBoxOutput("best_month"),
          valueBoxOutput("best_day"),
          valueBoxOutput("economic_factor")
        ),
        fluidRow(
          box(plotlyOutput("monthly_response_rate"), width = 6),
          box(plotlyOutput("weekday_response_rate"), width = 6)
        ),
        h3("Analyse démographique"),
        p("Objectif: Identifier les segments de clients en fonction de caractéristiques démographiques"),
        fluidRow(
          box(title = "État civil", 
              status = "primary", 
              solidHeader = TRUE, 
              div(style = "display: flex; justify-content: space-between;",
                  column(6,selectInput("demographicsMonthInput", "Mois", choices = unique(bank$month), selected = unique(bank$month)[1])),
                  column(6,selectInput("demographicsDayInput", "Jour de la semaine", choices = unique(bank$day_of_week), selected = unique(bank$day_of_week)[1]))
              ),
              plotlyOutput("maritalPlot")),
          box(title = "Niveau d'éducation", status = "primary", solidHeader = TRUE, 
              plotlyOutput("educationPlot"))),
        #h4("Analyse"),
        fluidRow(
          valueBoxOutput("totalClients"),
          valueBoxOutput("positiveResponses"),
          valueBoxOutput("responseRate")
        ),
        fluidRow(
          box(title = "Distribution des âges", status = "primary", solidHeader = TRUE, 
              div(style = "display: flex; gap: 10px; margin-top: 10px;",
                  column(6,selectInput("overviewMonthInput", "Mois", 
                                       choices = unique(bank$month), 
                                       selected = unique(bank$month)[1])),
                  column(6,selectInput("overviewDayInput", "Jour de la semaine", 
                                       choices = unique(bank$day_of_week), 
                                       selected = unique(bank$day_of_week)[1]))
              ),
              plotlyOutput("ageDistPlot")
          ),
          box(title = "Répartition des professions", status = "primary", solidHeader = TRUE, 
              plotlyOutput("jobDistPlot")
          )
        ),
        h5("Corrélation avec les Facteurs Économiques"),
        p("Objectif : Découvrir quels facteurs économiques sont significativement corrélés"),
        fluidRow(
          box(plotOutput("economic_correlation"), width = 12)
        )
      ),
      tabItem(tabName = "synthèse",
              fluidRow(
                h2("Segmentation"),
                box(title = "Régression Logistique", status = "primary", solidHeader = TRUE, width = 12,
                    tableOutput("Analyse_discriminante")),
                h3("Profils des segments"),
                box(title = "Profils des segments", status = "primary", solidHeader = TRUE, width = 12,
                    tableOutput("Profils_des_segments")),
                box(title = "Recommandations stratégiques", status = "primary", solidHeader = TRUE, width = 12,
                    tableOutput("recommandations"))
              )
      ),
      # Onglet : Aperçu des données
      tabItem(
        tabName = "data_exploration",
        h2("Exploration des Données"),
        fluidRow(
          valueBoxOutput("yes1_count", width = 3),
          valueBoxOutput("no1_count", width = 3),
          valueBoxOutput("response1_rate", width = 3)
        ),
        fluidRow(
          valueBoxOutput("total1_clients", width = 3),
          valueBoxOutput("average1_duration", width = 3),
          valueBoxOutput("average1_campaign", width = 3)
        ),
        sidebarLayout(
          sidebarPanel(
            selectInput("month_filter", "Sélectionner un mois :", 
                        choices = c("Tous", levels(bank$month)),
                        selected = "Tous"),
            selectInput("job_filter", "Sélectionner une profession :",
                        choices = c("Tous", levels(bank$job)),
                        selected = "Tous"),
            #actionButton("reset_filters", "Réinitialiser les filtres")
          ),
          mainPanel(
            DTOutput("data_table")
      ))),
      # Onglet Prédiction
      tabItem(
        tabName = "prediction",
        h2("Prédiction du succès de la campagne"),
        p("Objectif : Optimiser les campagnes pour les cibles les plus susceptibles de répondre positivement."),
              fluidRow(
                box(title = "Résumé du Modèle", status = "primary", solidHeader = TRUE, width = 6, 
                    verbatimTextOutput("model_summary")
                ),
                
                box(title = "Courbe ROC", status = "primary", solidHeader = TRUE, width = 6, 
                    plotlyOutput("roc_curve")
                )
              ),
              fluidRow(
                box(title = "Score AUC", status = "primary", solidHeader = TRUE, width = 2, 
                    verbatimTextOutput("auc_score")),
                    box(title = "Commentaires", status = "primary", solidHeader = TRUE, width = 10, 
                        verbatimTextOutput("comments")
                    )
                
              )))
  )
)


server <- function(input, output) {
  
  output$response_rate_telephone <- renderValueBox({
    rate <- round(mean(bank$y[bank$contact == "telephone"] == "yes", na.rm = TRUE) * 100, 2)
    valueBox(
      value = paste0(rate, " %"),
      subtitle = "Taux de réponse pour les Clients Contactés par Téléphone",
      icon = icon("phone"),
      color = "info"
    )
  })
  
  output$response_rate_personal_loan <- renderValueBox({
    rate <- round(mean(bank$y[bank$loan == "yes"] == "yes", na.rm = TRUE) * 100, 2)
    valueBox(
      value = paste0(rate, " %"),
      subtitle = "Taux de réponse (Prêt personnel)",
      icon = icon("bank"),
      color = "success"
    )
  })
  
  output$response_rate_dynamic_marital <- renderValueBox({
    selected_marital <- input$marital_response_selector
    rate <- round(mean(bank$y[bank$marital == selected_marital] == "yes", na.rm = TRUE) * 100, 2)
    valueBox(
      value = paste0(rate, " %"),
      subtitle = paste("Taux de réponse (", selected_marital, ")", sep = ""),
      icon = icon("heart"),
      color = "success"
    )
  })
  
  output$response_rate_dynamic_education <- renderValueBox({
    selected_education <- input$education_response_selector
    rate <- round(mean(bank$y[bank$education == selected_education] == "yes", na.rm = TRUE) * 100, 2)
    valueBox(
      value = paste0(rate, " %"),
      subtitle = paste("Taux de réponse (", selected_education, ")", sep = ""),
      icon = icon("graduation-cap"),
      color = "info"
    )
  })
  
  
  
  output$housing_loan_dynamic <- renderValueBox({
    selected_job <- input$job_selector
    count <- sum(bank$job == selected_job & bank$housing == "yes", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = paste("Clients (", selected_job, ", Prêt immobilier)", sep = ""),
      icon = icon("home"),
      color = "primary"
    )
  })
  
  
  
  output$personal_loan_dynamic <- renderValueBox({
    selected_job <- input$job_selector
    count <- sum(bank$job == selected_job & bank$loan == "yes", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = paste("Clients (", selected_job, ", Prêt personnel)", sep = ""),
      icon = icon("money-bill"),
      color = "success"
    )
  })
  
  
  output$response_rate_dynamic <- renderValueBox({
    selected_job <- input$job_response_selector
    rate <- round(mean(bank$y[bank$job == selected_job] == "yes", na.rm = TRUE) * 100, 2)
    valueBox(
      value = paste0(rate, " %"),
      subtitle = paste("Taux de réponse (", selected_job, ")", sep = ""),
      icon = icon("briefcase"),
      color = "primary"
    )
  })
  
  
  
  output$housing_loan_admin <- renderValueBox({
    count <- sum(bank$job == "admin." & bank$housing == "yes", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Clients avec Prêt Immobilier pour la Profession 'admin'.",
      icon = icon("home"),
      color = "primary"
    )
  })
  
  
  output$personal_loan_admin <- renderValueBox({
    count <- sum(bank$job == "admin." & bank$loan == "yes", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Clients (Admin, Prêt personnel)",
      icon = icon("bank"),
      color = "success"
    )
  })
  
  bank <- bank %>%
    mutate(y_numeric = ifelse(y == "yes", 1, 0)) # Nouvelle colonne `y_numeric`
  bank_data <- reactive({
    bank %>%
      mutate(y_numeric = ifelse(y == "yes", 1, 0))
  })
  
  output$email_response_rate <- renderValueBox({
    data <- bank_data()
    
    # Calcul du taux de réponse
    total_responses <- sum(data$y_numeric, na.rm = TRUE)
    total_emails <- nrow(data)
    
    response_rate <- if (total_emails > 0) {
      (total_responses / total_emails) * 100
    } else {
      0
    }
    
    # Création du valueBox
    valueBox(
      value = paste0(round(response_rate, 2), "%"),
      subtitle = "Taux de réponse (Email)",
      icon = icon("envelope"),
      color = "teal"
    )
  })
  
  
  
  
  loan_response_rate <- bank %>%
    group_by(loan) %>%
    summarise(rate = mean(y == "yes") * 100)
  
  
  profession_yes_rate <- bank %>%
    group_by(job) %>%
    summarise(rate = mean(y == "yes") * 100) %>%
    arrange(desc(rate))
  
  calls_per_month <- bank %>%
    group_by(month) %>%
    summarise(total_calls = n()) %>%
    arrange(desc(total_calls))
  
  
  loan_yes_ratio <- sum(bank$loan == "yes" & bank$y == "yes") / sum(bank$y == "yes") * 100
  
  valueBox(sum(bank$contact == "telephone"), "Clients contactés par téléphone", icon = icon("phone"), color = "danger")
  
  # Données filtrées (réactif)
    filtered_data <- reactive({
      bank
    })
    
    # Calcul des indicateurs
    indicators <- reactive({
      data <- filtered_data()
      total <- nrow(data)
      yes_count <- sum(data$y == "yes")
      no_count <- sum(data$y == "no")
      conversion_rate <- (yes_count / total) * 100
      avg_call_duration <- mean(data$duration, na.rm = TRUE)
      loan_yes_ratio <- sum(data$loan == "yes" & data$y == "yes") / sum(data$y == "yes") * 100
      
      loan_clients <- sum(data$loan == "yes")
      most_common_job <- names(sort(table(data$job), decreasing = TRUE)[1])
      age_group_max <- names(sort(table(cut(data$age, breaks = c(18, 30, 45, 60, 100))), decreasing = TRUE)[1])
      response_rate_month <- data %>%
        group_by(month) %>%
        summarise(rate = mean(y == "yes") * 100, .groups = "drop") %>%
        arrange(desc(rate)) %>%
        head(1) %>%
        pull(month)
      
      list(
        total = total,
        yes_count = yes_count,
        no_count = no_count,
        conversion_rate = round(conversion_rate, 2),
        avg_call_duration = round(avg_call_duration, 2),
        loan_clients = loan_clients,
        loan_yes_ratio = loan_yes_ratio,
        loan_response_rate = loan_response_rate,
        most_common_job = most_common_job,
        age_group_max = age_group_max,
        response_rate_month = response_rate_month
      )
    })
    
    # Value boxes
    output$total_clients <- renderValueBox({
      valueBox(indicators()$total, "Nombre total de clients", icon = icon("users"), color = "aqua")
    })
    
    output$conversion_rate <- renderValueBox({
      valueBox(paste0(indicators()$conversion_rate, " %"), "Taux de conversion", icon = icon("chart-line"), color = "teal")
    })
    
    output$yes_count <- renderValueBox({
      valueBox(indicators()$yes_count, "Réponses positives (yes)", icon = icon("thumbs-up"), color = "olive")
    })
    
    output$no_count <- renderValueBox({
      valueBox(indicators()$no_count, "Réponses négatives (no)", icon = icon("thumbs-down"), color = "purple")
    })
    
    output$avg_call_duration <- renderValueBox({
      valueBox(paste0(indicators()$avg_call_duration, " sec"), "Durée moyenne des appels", icon = icon("phone"), color = "purple")
    })
    
    output$loan_clients <- renderValueBox({
      valueBox(indicators()$loan_clients, "Clients ayant un prêt", icon = icon("credit-card"), color = "maroon")
    })
    
    output$most_common_job <- renderValueBox({
      valueBox(indicators()$most_common_job, "Profession la plus fréquente", icon = icon("briefcase"), color = "orange")
    })
    
    output$age_group_max <- renderValueBox({
      valueBox(indicators()$age_group_max, "Groupe d'âge dominant", icon = icon("user"), color = "teal")
    })
    
    output$response_rate_month <- renderValueBox({
      valueBox(indicators()$response_rate_month, "Mois avec meilleur taux de réponse", icon = icon("calendar-alt"), color = "lime")
    })
    
    output$loan_yes_ratio <- renderValueBox({
      valueBox(
        paste0(round(indicators()$loan_yes_ratio, 2), " %"), 
        "Ratio prêt pour les réponses 'yes'", 
        icon = icon("percentage"), 
        color = "navy"
      )
    })
    
    output$age_yes_no <- renderValueBox({
      valueBox(
        paste0("Yes: ", round(indicators()$age_yes, 1), " ans, No: ", round(indicators()$age_no, 1), " ans"), 
        "Âge moyen des réponses", 
        icon = icon("birthday-cake"), 
        color = "fuchsia"
      )
    })
    
    output$top_job_yes <- renderValueBox({
      valueBox(
        paste0(profession_yes_rate$job[1], " - ", round(profession_yes_rate$rate[1], 2), " %"), 
        "Profession avec le plus de 'yes'", 
        icon = icon("user-tie"), 
        color = "olive"
      )
    })
    
    output$calls_per_month <- renderValueBox({
      valueBox(
        paste0(calls_per_month$month[1], " - ", calls_per_month$total_calls[1], " appels"), 
        "Mois le plus actif", 
        icon = icon("phone"), 
        color = "maroon"
      )
    })
    
    output$loan_response_rate <- renderValueBox({
      valueBox(
        paste0("Oui: ", round(loan_response_rate$rate[loan_response_rate$loan == "yes"], 2), " %, Non: ", 
               round(loan_response_rate$rate[loan_response_rate$loan == "no"], 2), " %"), 
        "Taux de réponse prêt", 
        icon = icon("credit-card"), 
        color = "lime"
      )
    })
    

  # Analyse temporelle
  output$best_month <- renderValueBox({
    best <- bank %>%
      group_by(month) %>%
      summarise(response_rate = mean(y == "yes")* 100, .groups = "drop") %>%
      arrange(desc(response_rate)) %>%
      head(1) %>%
      pull(month)
    valueBox(
      best, "Meilleur mois", icon = icon("calendar-alt"), color = "navy"
    )
  })
  
  output$best_day <- renderValueBox({
    best <- bank %>%
      group_by(day_of_week) %>%
      summarise(response_rate = mean(y == "yes")) %>%
      arrange(desc(response_rate)) %>%
      head(1) %>%
      pull(day_of_week)
    valueBox(
      best, "Meilleur jour", icon = icon("calendar-day"), color = "teal"
    )
  })
  
  output$economic_factor <- renderValueBox({
    # Créez une copie temporaire avec `y` transformé
    bank_transformed <- bank %>%
      mutate(y_numeric = ifelse(y == "yes", 1, 0))
    
    # Sélectionnez les colonnes nécessaires pour la corrélation
    selected_columns <- bank_transformed %>%
      select(emp.var.rate, cons.price.idx, euribor3m, y_numeric)
    
    # Calculer la matrice de corrélation
    cor_matrix <- cor(selected_columns, use = "complete.obs")
    
    # Identifier le facteur économique le plus corrélé avec `y_numeric`
    strongest <- which.max(abs(cor_matrix["y_numeric", -4]))
    factor_name <- colnames(selected_columns)[strongest]
    
    # Créer la valueBox
    valueBox(
      factor_name, "Facteur économique clé", icon = icon("chart-line"), color = "lime"
    )
  })
  
  
  
  filteredData <- reactive({
    bank %>%
      filter(month == input$overviewMonthInput,
             day_of_week == input$overviewDayInput)
  })
  
  demographicsFilteredData <- reactive({
    bank %>%
      filter(month == input$demographicsMonthInput,
             day_of_week == input$demographicsDayInput)
  })
  
  campaignsFilteredData <- reactive({
    bank %>%
      filter(month == input$campaignsMonthInput,
             day_of_week == input$campaignsDayInput)
  })
  
  output$totalClients <- renderValueBox({
    valueBox(
      value = nrow(filteredData()),
      subtitle = "Total des clients",
      icon = icon("users"),
      color = "teal"
    )
  })
  
  output$positiveResponses <- renderValueBox({
    valueBox(
      value = sum(filteredData()$y == "yes"),
      subtitle = "Réponses positives",
      icon = icon("thumbs-up"),
      color = "navy"
    )
  })
  
  output$responseRate <- renderValueBox({
    valueBox(
      value = round(mean(filteredData()$y == "yes") * 100, 2),
      subtitle = "Taux de réponse (%)",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  output$ageDistPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = age)) +
      geom_histogram(binwidth = 5, fill = "blue", color = "white") +
      theme_minimal() +
      labs(title = "Distribution des âges", x = "Âge", y = "Nombre de clients")
    ggplotly(p)
  })
  
  output$jobDistPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = job)) +
      geom_bar(fill = "green", color = "white") +
      theme_minimal() +
      labs(title = "Répartition des professions", x = "Profession", y = "Nombre de clients") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$maritalPlot <- renderPlotly({
    p <- ggplot(demographicsFilteredData(), aes(x = marital)) +
      geom_bar(fill = "purple", color = "white") +
      theme_minimal() +
      labs(title = "État civil", x = "État civil", y = "Nombre de clients")
    ggplotly(p)
  })
  
  output$educationPlot <- renderPlotly({
    p <- ggplot(demographicsFilteredData(), aes(x = education)) +
      geom_bar(fill = "orange", color = "white") +
      theme_minimal() +
      labs(title = "Niveau d'éducation", x = "Niveau d'éducation", y = "Nombre de clients")
    ggplotly(p)
  })
  
  output$durationPlot <- renderPlotly({
    p <- ggplot(campaignsFilteredData(), aes(x = duration)) +
      geom_histogram(binwidth = 50, fill = "red", color = "white") +
      theme_minimal() +
      labs(title = "Durée des appels", x = "Durée (secondes)", y = "Nombre d'appels")
    ggplotly(p)
  })
  
  output$responseRatePlot <- renderPlotly({
    p <- ggplot(campaignsFilteredData(), aes(x = y)) +
      geom_bar(fill = "cyan", color = "white") +
      theme_minimal() +
      labs(title = "Taux de réponse", x = "Réponse", y = "Nombre de clients")
    ggplotly(p)
  })
  # Filtrage des données
  filtered_data <- reactive({
    bank
  })
  
  # Vue Générale
  output$total_clients <- renderValueBox({
    valueBox(nrow(filtered_data()), "Total des clients", icon = icon("users"), color = "navy")
  })
  
  output$positive_responses <- renderValueBox({
    valueBox(
      sum(filtered_data()$y == "yes"), "Réponses positives", icon = icon("thumbs-up"), color = "olive"
    )
  })
  
  output$response_rate <- renderValueBox({
    rate <- round(mean(filtered_data()$y == "yes") * 100, 2)
    valueBox(paste0(rate, "%"), "Taux de réponse", icon = icon("percent"), color = "purple")
  })
  
  output$loan_yes_ratio <- renderValueBox({
    valueBox(
      paste0(round(indicators()$loan_yes_ratio, 2), " %"), 
      "Ratio prêt pour les réponses 'yes'", 
      icon = icon("percentage"), 
      color = "navy"
    )
  })
  
  output$age_distribution <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = age)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
      theme_minimal() +
      labs(title = "Distribution des âges", x = "Âge", y = "Fréquence")
    ggplotly(p)
  })
  
  # Segmentation des Clients
  # Graphique de la répartition des clusters sur l'âge et la durée de l'appel
  output$clusterPlot <- renderPlotly({
    plot_ly(bank, x = ~age, y = ~duration, color = ~as.factor(cluster), colors = "Set1", type = "scatter", mode = "markers", marker = list(opacity = 0.5)) %>%
      layout(
        title = "Répartition des Clients par Segment",
        xaxis = list(title = "Âge"),
        yaxis = list(title = "Durée de l'appel"),
        showlegend = TRUE
      )
  })
  
  # Graphique de distribution de l'âge
  output$agePlot <- renderPlot({
    ggplot(bank, aes(x = age, fill = cluster)) +
      geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
      labs(title = "Distribution de l'Âge par Segment", x = "Âge", y = "Nombre de Clients") +
      theme_minimal()
  })
  
  # Afficher le résumé des segments dans un tableau avec défilement vertical
  output$summaryTable <- DT::renderDataTable({
    DT::datatable(summary_segments, options = list(
      scrollY = "300px",  # Limiter la hauteur du tableau
      scrollCollapse = TRUE,  # Permettre le défilement vertical
      paging = FALSE,  # Désactiver la pagination
      searching = FALSE   # Désactiver la recherche pour simplifier l'interface
    ))
  })
  
  # Ajouter des commentaires en fonction des résultats des segments
  # Ajouter des commentaires en fonction des résultats des segments
  output$comment1 <- renderText({
    segment_comment1 <- c(
      "===== Cluster 1 =====\n",
      "Ce groupe est constitué principalement de clients d'âge mature (moyenne d'âge : 40 ans) avec une durée d'appel relativement courte (moyenne : 176 secondes).\n",
      "- **Contexte économique** : Le taux de chômage est faible, mais l'indice de confiance économique est bas (-39,93), ce qui pourrait expliquer une prudence dans les décisions financières.\n",
      "- **Taux d'intérêt** : L'Euribor moyen est de 4,89 %, ce qui indique un contexte de taux d'intérêt élevé. Cela peut limiter leur propension à souscrire à des produits nécessitant des emprunts, comme les crédits.\n",
      "- **Recommandations** : Ciblez ces clients avec des produits à faible risque et des offres personnalisées pour renforcer leur confiance.\n\n",
      
      "===== Cluster 2 =====\n",
      "Les clients de ce segment sont légèrement plus âgés (moyenne d'âge : 46 ans) avec des durées d'appel plus longues (moyenne : 283 secondes).\n",
      "- **Contexte économique** : Le taux d'emploi est faible, ce qui indique une sensibilité à la situation économique.\n",
      "- **Taux d'intérêt** : L'Euribor moyen est de 0,79 %, ce qui est nettement plus bas que dans le Cluster 1. Cela peut encourager les membres de ce groupe à souscrire à des crédits ou à des produits financiers à taux variables.\n",
      "- **Recommandations** : Proposez des produits à taux variables et des offres attractives pour stimuler leur engagement.\n\n",
      
      "===== Cluster 3 =====\n",
      "Ce groupe est constitué de clients légèrement plus jeunes (moyenne d'âge : 37 ans) avec des durées d'appel proches de 267 secondes.\n",
      "- **Contexte économique** : Le taux d'emploi moyen est de -1,83 %, ce qui indique un environnement économique difficile. Cela pourrait refléter une région où les pertes d'emploi sont encore significatives.\n",
      "- **Recommandations** : Ciblez ces clients avec des offres de sécurité financière et des produits adaptés à leur situation économique.\n\n",
      
      "===== Cluster 4 =====\n",
      "Ce segment est composé principalement d'adultes matures (moyenne d'âge : 40,18 ans) avec une durée moyenne d'appel de 256,90 secondes.\n",
      "- **Contexte économique** : Le taux de chômage est faible, ce qui indique un environnement de création d'emploi. Cela renforce leur sentiment de sécurité et leur capacité à s'engager dans des offres à long terme.\n",
      "- **Recommandations** : Proposez des produits à long terme, comme des plans d'épargne ou des investissements, pour capitaliser sur leur sentiment de sécurité.\n"
    )
    
    # Retourner les commentaires du cluster
    paste(segment_comment1, collapse = "\n")
  })
  
  # Tableau des taux de réponse
  output$responseTable <- renderDT({
    datatable(response_rates, options = list(
      scrollX = TRUE,
      paging = FALSE
    ))
  })
  
  # Graphique des taux de réponse
  output$responsePlot <- renderPlot({
    ggplot(response_rates, aes(x = factor(cluster), y = response_rate, fill = factor(cluster))) +
      geom_bar(stat = "identity", alpha = 0.8) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = "Taux de Réponse par Segment",
        x = "Segment",
        y = "Taux de Réponse (%)",
        fill = "Segment"
      ) +
      theme_minimal()
  })
  
  # Analyse Temporelle
  output$monthly_response_rate <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = month, fill = y)) +
      geom_bar(position = "fill") +
      theme_minimal() +
      labs(title = "Taux de Réponse par Mois", x = "Mois", y = "Proportion")
    ggplotly(p)
  })
  
  output$weekday_response_rate <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = day_of_week, fill = y)) +
      geom_bar(position = "fill") +
      theme_minimal() +
      labs(title = "Taux de Réponse par Jour", x = "Jour", y = "Proportion")
    ggplotly(p)
  })
  
  # Matrice de corrélation
  output$economic_correlation <- renderPlot({
    numeric_vars <- bank %>% select_if(is.numeric)
    corr_matrix <- cor(numeric_vars, use = "complete.obs")
    
    # Amélioration de la corrélation
    corrplot(corr_matrix, method = "color", type = "upper", 
             tl.col = "black", tl.srt = 45, 
             col = colorRampPalette(c("red", "white", "blue"))(200),
             addCoef.col = "black", # Affiche les coefficients de corrélation
             number.cex = 0.7)      # Taille des numéros
  })
  
  
  
  # Modèle prédictif (placeholder)
  # Modèle prédictif : Formation
  library(shiny)
  library(dplyr)
  library(pROC)
  library(plotly)
  library(ROSE)  # Utilisation de ROSE pour le rééquilibrage
  
  # Modèle prédictif (placeholder)
  # Modèle prédictif : Formation
  model <- reactive({
    data <- filtered_data()
    
    # Conversion de y en binaire
    data <- data %>% mutate(y = ifelse(y == "yes", 1, 0))
    
    # Rééquilibrage des classes avec ROSE (si nécessaire)
    if (sum(data$y == 1) / nrow(data) < 0.3) {  # Si la classe minoritaire est < 30%
      data <- ROSE(y ~ ., data = data, seed = 123)$data
    }
    
    # Sélection des variables pertinentes
    train_data <- data %>%
      select(age, job, marital, education, default, housing, loan, duration, campaign, pdays, previous, emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed, y)
    
    # Régression Logistique
    glm(y ~ ., data = train_data, family = binomial)
  })
  
  # Résumé du modèle filtré pour les variables significatives
  output$model_summary <- renderPrint({
    summary_model <- summary(model())
    significant_vars <- summary_model$coefficients[summary_model$coefficients[, "Pr(>|z|)"] < 0.05, ]
    print(significant_vars)
  })
  
  # Commentaires sur les résultats significatifs
  output$comments <- renderPrint({
    summary_model <- summary(model())
    significant_vars <- summary_model$coefficients[summary_model$coefficients[, "Pr(>|z|)"] < 0.05, ]
    
    cat("===== Commentaires sur les résultats significatifs =====\n\n")
    
    # Introduction
    cat("Le modèle de régression logistique identifie les variables suivantes comme ayant un impact statistiquement significatif (p-value < 0.05) sur la probabilité de souscription ('yes').\n\n")
    
    # Variables significatives
    cat("Variables significatives et leur impact :\n")
    significant_names <- rownames(significant_vars)
    for (name in significant_names) {
      effect <- ifelse(significant_vars[name, "Estimate"] > 0, "positif", "négatif")
      p_value <- signif(significant_vars[name, "Pr(>|z|)"], 3)
      
      # Explication de l'effet
      if (effect == "positif") {
        impact <- "augmente"
      } else {
        impact <- "réduit"
      }
      
      cat(paste0("- ", name, " :\n"))
      cat(paste0("\t- Effet : ", effect, " (", impact, " la probabilité de souscription)\n"))
      cat(paste0("\t- p-value : ", p_value, "\n"))
      cat("\n")
    }
    
    # Conclusion et recommandations
    cat("===== Recommandations =====\n\n")
    cat("1. Cibler les clients avec des caractéristiques associées à un effet positif (par exemple, Les étudiants(jobstudent), Les retraités (jobretired), Les clients célibataires (maritalsingle) ont une probabilité plus élevée de souscrire).\n")
    cat("2. Éviter les caractéristiques associées à un effet négatif (par exemple, Les travailleurs manuels (jobblue-collar), Les employés des services (jobservices), Les entrepreneurs(jobentrepreneur) ont une probabilité plus faible de souscrire ).\n")
    cat("3. Considérer des campagnes de marketing personnalisées en fonction des variables significatives afin de maximiser les taux de souscription.\n")
  })
  
  # Prédictions et courbe ROC avec score AUC
  output$roc_curve <- renderPlotly({
    data <- filtered_data()
    
    # Conversion de y en binaire
    data <- data %>% mutate(y = ifelse(y == "yes", 1, 0))
    
    # Prédictions
    prob <- predict(model(), newdata = data, type = "response")
    data$predicted <- ifelse(prob > 0.5, 1, 0)
    
    # Calcul des TPR/FPR pour la courbe ROC
    roc_obj <- roc(data$y, prob)
    auc_score <- auc(roc_obj)
    
    # Création de la courbe ROC avec Plotly
    plot_ly(x = 1 - roc_obj$specificities, y = roc_obj$sensitivities, type = 'scatter', mode = 'lines', name = 'Courbe ROC') %>%
      layout(
        title = paste('Courbe ROC (AUC =', round(auc_score, 2), ')'),
        xaxis = list(title = '1 - Specificity (FPR)'),
        yaxis = list(title = 'Sensitivity (TPR)'),
        shapes = list(
          list(
            type = 'line',
            x0 = 0, x1 = 1,
            y0 = 0, y1 = 1,
            line = list(dash = 'dot', color = 'gray')
          )
        )
      )
  })
  
  # Calcul et affichage du score AUC
  output$auc_score <- renderPrint({
    data <- filtered_data()
    
    # Conversion de y en binaire
    data <- data %>% mutate(y = ifelse(y == "yes", 1, 0))
    
    # Prédictions
    prob <- predict(model(), newdata = data, type = "response")
    
    # Calcul du score AUC
    roc_obj <- roc(data$y, prob)
    auc_score <- auc(roc_obj)
    
    cat("Score AUC : ", round(auc_score, 3), "\n")
  })

  
  
  
  
  
  # Résumé du taux de réponse global
  output$global_response_summary <- renderPrint({
    response_rate <- bank %>%
      summarise(
        total = n(),
        yes_count = sum(y == "yes"),
        no_count = sum(y == "no"),
        rate = mean(y == "yes") * 100
      )
    response_rate
  })
  
  # Analyse par profession
  output$plot_job <- renderPlotly({
    response_by_job <- bank %>%
      group_by(job) %>%
      summarise(
        total = n(),
        yes_count = sum(y == "yes"),
        response_rate = mean(y == "yes") * 100
      ) %>%
      arrange(desc(response_rate))
    
    plot_job <- ggplot(response_by_job, aes(x = reorder(job, response_rate), y = response_rate)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = "Taux de réponse par profession", x = "Profession", y = "Taux de réponse (%)") +
      theme_minimal()
    
    ggplotly(plot_job)
  })
  
  # Analyse par état matrimonial
  output$plot_marital <- renderPlotly({
    response_by_marital <- bank %>%
      group_by(marital) %>%
      summarise(
        total = n(),
        yes_count = sum(y == "yes"),
        response_rate = mean(y == "yes") * 100
      ) %>%
      arrange(desc(response_rate))
    
    plot_marital <- ggplot(response_by_marital, aes(x = reorder(marital, response_rate), y = response_rate)) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +
      labs(title = "Taux de réponse par état matrimonial", x = "État matrimonial", y = "Taux de réponse (%)") +
      theme_minimal()
    
    ggplotly(plot_marital)
  })
  
  # Analyse par niveau d'éducation
  output$plot_education <- renderPlotly({
    response_by_education <- bank %>%
      group_by(education) %>%
      summarise(
        total = n(),
        yes_count = sum(y == "yes"),
        response_rate = mean(y == "yes") * 100
      ) %>%
      arrange(desc(response_rate))
    
    plot_education <- ggplot(response_by_education, aes(x = reorder(education, response_rate), y = response_rate)) +
      geom_bar(stat = "identity", fill = "green") +
      coord_flip() +
      labs(title = "Taux de réponse par niveau d'éducation", x = "Niveau d'éducation", y = "Taux de réponse (%)") +
      theme_minimal()
    
    ggplotly(plot_education)
  })
  
  filtered_data <- reactive({
    data <- bank
    data
  })

  # Résumé des données
  output$total_clients <- renderValueBox({
    valueBox(nrow(bank), "Total des clients", icon = icon("users"), color = "navy")
  })
  
  output$positive_responses <- renderValueBox({
    valueBox(sum(bank$y == "yes"), "Réponses positives", icon = icon("thumbs-up"), color = "olive")
  })
  
  output$response_rate <- renderValueBox({
    rate <- round(mean(bank$y == "yes") * 100, 2)
    valueBox(paste0(rate, "%"), "Taux de réponse", icon = icon("percent"), color = "purple")
  })
  
  # Taux de réponse global
  output$global_response_rate <- renderText({
    response_rate <- round(mean(bank$y == "yes") * 100, 2)
    paste("Taux : ", response_rate, "%")
  })
  
  # Durée des appels par réponse
  output$duration_plot <- renderPlot({
    ggplot(bank, aes(x = y, y = duration)) +
      geom_boxplot(fill = c("skyblue", "orange")) +
      theme_minimal() +
      labs(title = "Durée des appels et souscription", x = "Réponse (y)", y = "Durée (en secondes)")
  })
  
  # Effet des contacts précédents
  output$contacts_summary <- renderTable({
    bank %>%
      group_by(poutcome) %>%
      summarise(
        Total = n(),
        Réponses_positives = sum(y == "yes"),
        Taux_de_réponse = round(mean(y == "yes") * 100, 2)
      )%>%
      mutate(
        poutcome = substr(poutcome, 1, 15)  # Raccourcir les valeurs
      )
  }, rownames = TRUE)
  
  
  
  # Suppression des variables fortement corrélées et constantes par groupe
  filtered_data_reduced <- reactive({
    data <- filtered_data()
    
    # Identifier les variables numériques
    numeric_vars <- data %>% select_if(is.numeric)
    
    # Vérifier que chaque variable a une variance non nulle dans chaque groupe
    non_constant_vars <- numeric_vars %>%
      summarise(across(everything(), ~ all(tapply(., data$y, var, na.rm = TRUE) > 0))) %>%
      select(where(~ . == TRUE)) %>%
      names()
    
    # Filtrer les données
    numeric_vars <- numeric_vars %>% select(all_of(non_constant_vars))
    
    # Inclure 'y' dans les données finales
    data <- cbind(numeric_vars, y = data$y)
    
    return(data)
  })
  
  # Rééquilibrage des classes avec ROSE
  balanced_data <- reactive({
    data <- filtered_data_reduced()
    
    # Rééquilibrage des classes si nécessaire
    if (sum(data$y == "yes") / nrow(data) < 0.3) {  # Si la classe minoritaire est < 30%
      data <- ROSE(y ~ ., data = data, seed = 123)$data
    }
    
    return(data)
  })
  
  # Modèle d'analyse discriminante linéaire avec validation des données
  lda_model <- reactive({
    data <- balanced_data()  # Utiliser les données rééquilibrées
    
    # Vérification qu'il reste des variables après réduction
    if (ncol(data) <= 1) {
      stop("Pas assez de variables pour effectuer l'analyse discriminante.")
    }
    
    MASS::lda(y ~ ., data = data)
  })
  
  # Résumé de l'analyse discriminante linéaire
  output$lda_summary <- renderPrint({
    lda <- lda_model()
    print(lda)
  })
  
  # Commentaires sur les résultats de l'AFD
  output$comment <- renderPrint({
    lda <- lda_model()
    means <- as.data.frame(lda$means)
    prior <- lda$prior
    coefficients <- as.data.frame(lda$scaling)
    
    cat("===== Commentaires sur les résultats de l'AFD =====\n\n")
    
    # Introduction
    cat("L'Analyse Factorielle Discriminante (AFD) a été utilisée pour identifier les variables qui discriminent le mieux entre les clients ayant répondu 'yes' et 'no'.\n\n")
    
    # Probabilités a priori
    cat("===== Probabilités a priori des groupes =====\n")
    cat("Les probabilités a priori montrent la répartition des groupes dans les données :\n")
    print(prior)
    cat("\nCommentaire : Les classes sont presque équilibrées (50.65 % 'no' vs 49.35 % 'yes'), ce qui signifie qu'il n'y a pas de biais important en faveur d'une classe. Cela est idéal pour entraîner un modèle, car il n'est pas nécessaire de rééquilibrer les données.\n\n")
    
    # Moyennes de groupe
    cat("===== Moyennes de groupe =====\n")
    cat("Les moyennes de groupe révèlent des différences clés entre ceux qui ont répondu 'yes' et 'no' :\n")
    print(means)
    cat("\nCommentaire :\n")
    cat("1. **Durée de l'appel (duration)** : Les clients ayant répondu 'yes' ont des durées d'appel beaucoup plus longues (556.25) que ceux ayant répondu 'no' (223.39). Cela suggère que les clients qui passent plus de temps au téléphone sont plus susceptibles de souscrire.\n")
    cat("2. **Jours depuis le dernier contact (pdays)** : Les clients ayant répondu 'yes' ont été contactés plus récemment (794.23 jours) que ceux ayant répondu 'no' (982.24 jours). Cela indique que les clients contactés plus récemment sont plus susceptibles de souscrire.\n")
    cat("3. **Nombre de contacts précédents (previous)** : Les clients ayant répondu 'yes' ont été contactés plus souvent (0.48 fois) que ceux ayant répondu 'no' (0.13 fois). Cela suggère que les clients contactés plusieurs fois sont plus susceptibles de souscrire.\n")
    cat("4. **Taux de variation de l'emploi (emp.var.rate)** : Les clients ayant répondu 'yes' sont dans des régions avec un taux de variation de l'emploi plus faible (-1.21) que ceux ayant répondu 'no' (0.23). Cela pourrait indiquer que les clients dans des environnements économiques plus stables sont plus susceptibles de souscrire.\n")
    cat("5. **Taux Euribor à 3 mois (euribor3m)** : Les clients ayant répondu 'yes' sont dans des régions avec un taux Euribor plus faible (2.13) que ceux ayant répondu 'no' (3.80). Cela suggère que les clients dans des environnements à taux d'intérêt plus bas sont plus susceptibles de souscrire.\n")
    cat("6. **Nombre d'employés (nr.employed)** : Les clients ayant répondu 'yes' sont dans des régions avec moins d'employés (5095.31) que ceux ayant répondu 'no' (5175.51). Cela pourrait indiquer que les clients dans des régions avec moins d'emplois sont plus susceptibles de souscrire.\n\n")
    
    # Coefficients des discriminants
    cat("===== Coefficients des discriminants linéaires (LD1) =====\n")
    cat("Les coefficients des discriminants montrent l'importance relative de chaque variable dans la séparation des groupes :\n")
    print(coefficients)
    cat("\nCommentaire :\n")
    cat("1. **Taux de variation de l'emploi (emp.var.rate)** : Le coefficient est négatif et élevé en valeur absolue (-0.146). Cela signifie qu'une augmentation du taux de variation de l'emploi réduit la probabilité de souscription.\n")
    cat("2. **Taux Euribor à 3 mois (euribor3m)** : Le coefficient est négatif et élevé en valeur absolue (-0.134). Cela signifie qu'une augmentation du taux Euribor à 3 mois réduit la probabilité de souscription.\n")
    cat("3. **Indice des prix à la consommation (cons.price.idx)** : Le coefficient est positif (0.075). Cela signifie qu'une augmentation de l'indice des prix à la consommation augmente la probabilité de souscription.\n")
    cat("4. **Durée de l'appel (duration)** : Le coefficient est positif (0.002). Cela signifie qu'une augmentation de la durée de l'appel augmente la probabilité de souscription.\n")
    cat("5. **Nombre de contacts pendant la campagne (campaign)** : Le coefficient est négatif (-0.016). Cela signifie qu'une augmentation du nombre de contacts pendant la campagne réduit la probabilité de souscription.\n")
    cat("6. **Nombre de contacts précédents (previous)** : Le coefficient est négatif (-0.016). Cela signifie qu'une augmentation du nombre de contacts précédents réduit la probabilité de souscription.\n\n")
    
    # Interprétation et recommandations
    cat("===== Interprétation et recommandations =====\n")
    cat("1. **Cibler les clients avec des durées d'appel plus longues** : Les clients qui passent plus de temps au téléphone sont plus susceptibles de souscrire. Essayez de prolonger la durée des appels ou de cibler les clients qui ont déjà eu des interactions plus longues.\n")
    cat("2. **Adapter les campagnes en fonction des indicateurs économiques** : Les clients dans des régions avec un taux de variation de l'emploi plus faible ou un taux Euribor plus bas sont plus susceptibles de souscrire. Adaptez vos campagnes en fonction de ces indicateurs économiques.\n")
    cat("3. **Éviter de contacter les clients trop souvent** : Les clients contactés trop souvent (nombre élevé de contacts pendant la campagne ou contacts précédents) sont moins susceptibles de souscrire. Limitez le nombre de contacts pour ces clients.\n")
    cat("4. **Surveiller l'indice des prix à la consommation** : Les clients dans des régions avec un indice des prix à la consommation plus élevé sont plus susceptibles de souscrire. Ciblez ces régions pour vos campagnes.\n")
  })
  
  # Plot des axes discriminants avec contours et meilleures couleurs
  output$lda_plot <- renderPlotly({
    lda <- lda_model()
    pred <- predict(lda, newdata = filtered_data_reduced())
    data <- filtered_data_reduced()
    data$LD1 <- pred$x[, 1]  # Utiliser uniquement LD1
    
    # Tracer un graphique de densité pour LD1
    plot_ly(data, x = ~LD1, color = ~y, colors = c("red", "blue"),
            type = "histogram", histnorm = "probability density", opacity = 0.6) %>%
      layout(
        title = "Distribution des scores discriminants (LD1)",
        xaxis = list(title = "LD1"),
        yaxis = list(title = "Densité"),
        barmode = "overlay"
      )
  })
  
  # Tableau de contingence
  contingency_table <- as.data.frame(table(bank$job, bank$cluster))
  colnames(contingency_table) <- c("Job", "Cluster", "Count")
  
  output$contingency_table <- renderDataTable({
    datatable(
      contingency_table,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # Visualisation de la répartition des emplois
  output$job_distribution_plot <- renderPlotly({
    plot_ly(contingency_table, labels = ~Job, values = ~Count, type = "pie", hole = 0.4, textinfo = "label+percent", insidetextorientation = "radial") %>%
      layout(
        title = "Proportion des emplois par segment",
        showlegend = FALSE
      )
  })
  
  output$recommandations <- renderTable({
    recommandations
  })
#=============================================== ===========================================  
  output$Profils_des_segments <- renderTable({
    Profils_des_segments
  })
#=============================================== Exploration des Données===========================================  
  output$Analyse_discriminante <- renderTable({
    Analyse_discriminante
  })
#=============================================== Exploration des Données==============================================================================
  
  
  # Filtrage des données
  filtered_data <- reactive({
    data <- bank
    if (input$month_filter != "Tous") {
      data <- data %>% filter(month == input$month_filter)
    }
    if (input$job_filter != "Tous") {
      data <- data %>% filter(job == input$job_filter)
    }
    data
  })
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "month_filter", selected = "Tous")
    updateSelectInput(session, "job_filter", selected = "Tous")
  })
  
  output$yes1_count <- renderValueBox({
    yes_count <- filtered_data() %>% filter(y == "yes") %>% nrow()
    valueBox(value = yes_count, subtitle = "Nombre de réponses 'Yes'", icon = icon("check-circle"), color = "success")
  })
  
  output$no1_count <- renderValueBox({
    no_count <- filtered_data() %>% filter(y == "no") %>% nrow()
    valueBox(value = no_count, subtitle = "Nombre de réponses 'No'", icon = icon("times-circle"), color = "lightblue")
  })
  
  output$response1_rate <- renderValueBox({
    total <- nrow(filtered_data())
    yes <- filtered_data() %>% filter(y == "yes") %>% nrow()
    rate <- ifelse(total > 0, round((yes / total) * 100, 2), 0)
    valueBox(value = paste0(rate, "%"), subtitle = "Taux de Réponse", icon = icon("percent"), color = "maroon")
  })
  
  output$total1_clients <- renderValueBox({
    total <- nrow(filtered_data())
    valueBox(value = total, subtitle = "Total des Clients Filtrés", icon = icon("users"), color = "purple")
  })
  
  output$average1_duration <- renderValueBox({
    avg <- filtered_data() %>% summarise(avg = mean(duration, na.rm = TRUE)) %>% pull(avg)
    valueBox(value = paste0(round(avg, 2), " sec"), subtitle = "Durée Moyenne des Appels", icon = icon("clock"), color = "teal")
  })
  
  output$average1_campaign <- renderValueBox({
    avg <- filtered_data() %>% summarise(avg = mean(campaign, na.rm = TRUE)) %>% pull(avg)
    valueBox(value = avg, subtitle = "Nombre Moyen de Campagnes", icon = icon("chart-line"), color = "orange")
  })
  
  output$data_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(scrollX = TRUE, scrollY = "400px", pageLength = 10, lengthMenu = c(10, 25, 50, 100))
    )
  })
#====================================================================================================================================================
  
  # Taux de Conversion Global
  output$global_conversion <- renderPlotly({
    conversion_rate <- bank %>%
      summarise(Taux = mean(y == "yes")) %>%
      pull(Taux) * 100
    
    # Calcul des valeurs pour le graphique (pour les souscriptions et non-souscriptions)
    values <- c(100 - conversion_rate, conversion_rate)
    labels <- c("Non Souscriptions", "Souscriptions")
    
    plot_ly(
      labels = labels,
      values = values,
      type = "pie",
      textinfo = "label+percent",  # Affiche le label et le pourcentage
      hoverinfo = "label+percent"
    ) %>%
      layout(title = "Taux de Conversion Global")
  })
  
  
  # Distribution de y
  output$y_distribution <- renderPlotly({
    bank %>%
      group_by(housing, loan) %>%
      summarise(Taux = mean(y == "yes") * 100) %>%
      plot_ly(x = ~housing, y = ~Taux, color = ~loan, type = "bar") %>%
      layout(title = "Impact des Prêts sur Souscription", xaxis = list(title = "Prêt Immobilier"), yaxis = list(title = "Taux (%)"))
  })
  
  # Taux par Canal de Communication
  output$contact_conversion <- renderPlotly({
    bank %>%
      group_by(contact) %>%
      summarise(Taux = mean(y == "yes") * 100) %>%
      plot_ly(x = ~contact, y = ~Taux, type = "bar") %>%
      layout(title = "Taux par Canal de Communication", xaxis = list(title = "Contact"), yaxis = list(title = "Taux (%)"))
  })
  
  #  Performance par Mois
  output$month_conversion <- renderPlotly({
    bank %>%
      mutate(DurationGroup = cut(duration, breaks = c(0, 100, 300, 600, Inf), 
                                 labels = c("0-100s", "101-300s", "301-600s", ">600s"))) %>%
      group_by(DurationGroup) %>%
      summarise(Taux = mean(y == "yes") * 100) %>%
      plot_ly(x = ~DurationGroup, y = ~Taux, type = "bar") %>%
      layout(title = "Impact de la Durée des Appels", xaxis = list(title = "Durée (s)"), yaxis = list(title = "Taux (%)"))
  })

  # Affichage du tableau explicatif des variables
  output$variablesTable <- renderDT({
    datatable(variable_info, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
}

#  Lancer l'application
shinyApp(ui, server)
