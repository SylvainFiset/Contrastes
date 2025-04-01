# Application Shiny pour démontrer les contrastes linéaires orthogonaux
library(shiny)
library(DT)
library(knitr)
library(shinythemes)
library(shinyWidgets)

# Note: Créez un dossier 'www' dans le même répertoire que ce script
# et placez-y votre fichier logo (par exemple, 'logo_umce.png')

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .main-header {
        background-color: #287BA6;
        color: white;
        padding: 15px 0;
        margin-bottom: 20px;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.2);
      }
      .logo-container {
        display: flex;
        align-items: center;
        justify-content: flex-end;
      }
      .author-info {
        font-size: 16px;
        font-style: italic;
        margin-top: 5px;
        color: #c1dbeb;
      }
      .nav-tabs > li > a {
        background-color: #f8f8f8;
        color: #555;
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        background-color: #fff;
        color: #287BA6;
        font-weight: bold;
      }
      .btn-primary {
        background-color: #287BA6;
        border-color: #1a6890;
      }
      .btn-primary:hover {
        background-color: #1a6890;
        border-color: #155a7d;
      }
      .contrast-panel {
        background-color: #f8f8f8;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 15px;
        border: 1px solid #e3e3e3;
      }
      .contrast-title {
        color: #287BA6;
        margin-top: 0;
        margin-bottom: 10px;
      }
      .interpretation-box {
        background-color: #edf7fd;
        border-left: 5px solid #287BA6;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 0 5px 5px 0;
      }
      .footer {
        margin-top: 30px;
        padding-top: 15px;
        border-top: 1px solid #eee;
        text-align: center;
        font-size: 12px;
        color: #777;
      }
    "))
  ),
  
  div(class = "main-header",
      fluidRow(
        column(8, 
               h2("Démonstration des Contrastes Linéaires Orthogonaux", 
                  style = "margin-left: 15px; margin-top: 10px;"),
               div(class = "author-info", "Développé par: Sylvain Fiset, Ph.D.", 
                   style = "margin-left: 15px;")
        ),
        column(4, 
               div(class = "logo-container",
                   # Utilisation du logo local depuis le dossier www
                   tags$img(src = "logo_umce.png", 
                            height = "70px", 
                            style = "margin-right: 15px;",
                            alt = "Logo de l'Université de Moncton, campus d'Edmundston")
               )
        )
      )
  ),
  
  fluidRow(
    column(3,
           wellPanel(
             style = "background-color: #f8f8f8; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
             h4("Paramètres", style = "color: #287BA6; border-bottom: 1px solid #ddd; padding-bottom: 10px;"),
             
             sliderInput("n_groups", "Nombre de groupes:", 
                        min = 3, max = 8, value = 4, step = 1,
                        ticks = TRUE),
             
             uiOutput("contrast_inputs"),
             
             actionButton("check_orthogonal", "Vérifier orthogonalité", 
                         class = "btn-primary btn-block", 
                         icon = icon("check-circle")),
             
             hr(),
             
             h4("Instructions", style = "color: #287BA6; border-bottom: 1px solid #ddd; padding-bottom: 10px;"),
             tags$ol(
               tags$li("Sélectionnez le nombre de groupes"),
               tags$li("Définissez les coefficients pour chaque contraste"),
               tags$li("Cliquez sur 'Vérifier orthogonalité'")
             ),
             tags$ul(
               tags$li("Un contraste est valide si la somme des coefficients est 0"),
               tags$li("Deux contrastes sont orthogonaux si leur produit scalaire est 0")
             )
           )
    ),
    
    column(9,
           tabsetPanel(
             tabPanel("Matrice des contrastes", 
                     br(),
                     DTOutput("contrast_matrix"),
                     verbatimTextOutput("sum_check")),
             
             tabPanel("Orthogonalité", 
                     br(),
                     div(class = "interpretation-box",
                         h4("Interprétation", style = "color: #287BA6; margin-top: 0;"),
                         p(HTML("<b>Note:</b> La matrice ci-dessous montre le produit scalaire entre chaque paire de contrastes. 
                                Deux contrastes sont orthogonaux si leur produit scalaire est égal à 0.")),
                         tags$ul(
                           tags$li(HTML("<b>Valeurs sur la diagonale:</b> Somme des carrés des coefficients de chaque contraste.")),
                           tags$li(HTML("<b>Valeurs hors diagonale:</b> Produit scalaire entre deux contrastes différents.")),
                           tags$li(HTML("<b>Si une valeur hors diagonale est égale (ou très proche) à 0:</b> Les deux contrastes correspondants sont orthogonaux.")),
                           tags$li(HTML("<b>Si une valeur hors diagonale est différente de 0:</b> Les deux contrastes ne sont pas orthogonaux."))
                         )
                     ),
                     DTOutput("orthogonality_check", width = "100%"),
                     verbatimTextOutput("orthogonality_text")),
             
             tabPanel("Visualisation", 
                     br(),
                     plotOutput("contrast_plot", height = "600px")),
             
             tabPanel("Explication",
                     br(),
                     div(
                       HTML("
                       <div class='panel panel-primary'>
                         <div class='panel-heading'><h3 class='panel-title'>Contrastes linéaires</h3></div>
                         <div class='panel-body'>
                           <p>Un contraste linéaire est une combinaison linéaire des moyennes de groupe, où la somme des coefficients est égale à zéro.</p>
                         </div>
                       </div>
                       
                       <div class='panel panel-primary'>
                         <div class='panel-heading'><h3 class='panel-title'>Orthogonalité</h3></div>
                         <div class='panel-body'>
                           <p>Deux contrastes sont orthogonaux si leur produit scalaire est égal à zéro:</p>
                           <p>Soient c₁ = (c₁₁, c₁₂, ..., c₁ₖ) et c₂ = (c₂₁, c₂₂, ..., c₂ₖ) deux contrastes.</p>
                           <p>Ils sont orthogonaux si: c₁₁×c₂₁ + c₁₂×c₂₂ + ... + c₁ₖ×c₂ₖ = 0</p>
                         </div>
                       </div>
                       
                       <div class='panel panel-primary'>
                         <div class='panel-heading'><h3 class='panel-title'>Propriétés</h3></div>
                         <div class='panel-body'>
                           <ul>
                             <li>Pour k groupes, on peut avoir au maximum k-1 contrastes orthogonaux</li>
                             <li>Les contrastes orthogonaux permettent de décomposer la variabilité entre groupes en composantes indépendantes</li>
                             <li>Ils sont particulièrement utiles dans l'ANOVA pour tester des hypothèses spécifiques</li>
                           </ul>
                         </div>
                       </div>
                       
                       <div class='panel panel-primary'>
                         <div class='panel-heading'><h3 class='panel-title'>Applications</h3></div>
                         <div class='panel-body'>
                           <p>Les contrastes orthogonaux sont utilisés pour:</p>
                           <ul>
                             <li>Décomposer les effets dans des plans expérimentaux</li>
                             <li>Tester des tendances linéaires, quadratiques, etc.</li>
                             <li>Comparer des groupes de traitements spécifiques</li>
                           </ul>
                         </div>
                       </div>
                       ")
                     )
             )
           ),
           div(class = "footer",
               hr(),
               p("© 2025 Université de Moncton, campus d'Edmundston. Tous droits réservés.")
           )
    )
  )
)

server <- function(input, output, session) {
  # Création dynamique des entrées pour les contrastes
  output$contrast_inputs <- renderUI({
    n_groups <- input$n_groups
    n_contrasts <- n_groups - 1
    
    contrast_inputs <- lapply(1:n_contrasts, function(i) {
      div(
        class = "contrast-panel",
        h4(paste("Contraste", i), class = "contrast-title"),
        div(
          style = "display: flex; flex-wrap: wrap;",
          lapply(1:n_groups, function(j) {
            div(
              style = "width: 70px; margin-right: 5px;",
              numericInput(
                inputId = paste0("c", i, "_", j),
                label = paste("Grp", j),
                value = 0,
                step = 0.5
              )
            )
          })
        )
      )
    })
    
    do.call(tagList, contrast_inputs)
  })
  
  # Fonction pour récupérer les contrastes
  get_contrasts <- reactive({
    n_groups <- input$n_groups
    n_contrasts <- n_groups - 1
    
    contrasts <- matrix(0, nrow = n_contrasts, ncol = n_groups)
    
    for (i in 1:n_contrasts) {
      for (j in 1:n_groups) {
        input_id <- paste0("c", i, "_", j)
        if (!is.null(input[[input_id]])) {
          contrasts[i, j] <- input[[input_id]]
        }
      }
    }
    
    rownames(contrasts) <- paste("Contraste", 1:n_contrasts)
    colnames(contrasts) <- paste("Groupe", 1:n_groups)
    
    return(contrasts)
  })
  
  # Matrice des contrastes
  output$contrast_matrix <- renderDT({
    datatable(get_contrasts(), 
              options = list(
                dom = 't', 
                ordering = FALSE,
                autoWidth = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                pageLength = 15
              ),
              rownames = TRUE,
              class = 'cell-border stripe') %>%
      formatStyle(columns = names(get_contrasts()),
                  backgroundColor = '#f8f8f8')
  })
  
  # Vérification des sommes
  output$sum_check <- renderText({
    contrasts <- get_contrasts()
    sums <- rowSums(contrasts)
    
    output <- "Vérification des sommes (doivent être = 0):\n"
    for (i in 1:nrow(contrasts)) {
      output <- paste0(output, "Contraste ", i, ": ", 
                       round(sums[i], 4), 
                       ifelse(abs(sums[i]) < 1e-10, " ✓", " ✗"),
                       "\n")
    }
    return(output)
  })
  
  # Vérification de l'orthogonalité
  output$orthogonality_check <- renderDT({
    contrasts <- get_contrasts()
    n_contrasts <- nrow(contrasts)
    
    orthogonality_matrix <- matrix(0, nrow = n_contrasts, ncol = n_contrasts)
    
    for (i in 1:n_contrasts) {
      for (j in 1:n_contrasts) {
        orthogonality_matrix[i, j] <- sum(contrasts[i, ] * contrasts[j, ])
      }
    }
    
    rownames(orthogonality_matrix) <- paste("Contraste", 1:n_contrasts)
    colnames(orthogonality_matrix) <- paste("Contraste", 1:n_contrasts)
    
    datatable(round(orthogonality_matrix, 4),
              options = list(
                dom = 't', 
                ordering = FALSE,
                autoWidth = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = TRUE,
              class = 'cell-border stripe') %>%
      formatStyle(
        columns = 1:n_contrasts,
        color = styleInterval(c(-0.00001, 0.00001), c('red', 'green', 'red')),
        backgroundColor = styleEqual(
          diag(round(orthogonality_matrix, 4)), 
          rep('#f0f8ff', n_contrasts)
        ),
        fontWeight = styleEqual(
          diag(round(orthogonality_matrix, 4)), 
          rep('bold', n_contrasts)
        )
      )
  })
  
  # Texte d'explication de l'orthogonalité
  output$orthogonality_text <- renderText({
    contrasts <- get_contrasts()
    n_contrasts <- nrow(contrasts)
    
    output <- "Vérification de l'orthogonalité:\n"
    
    if (n_contrasts < 2) {
      return("Définissez au moins 2 contrastes pour vérifier l'orthogonalité.")
    }
    
    for (i in 1:(n_contrasts-1)) {
      for (j in (i+1):n_contrasts) {
        dot_product <- sum(contrasts[i, ] * contrasts[j, ])
        output <- paste0(output, "Contraste ", i, " · Contraste ", j, " = ", 
                         round(dot_product, 4), 
                         ifelse(abs(dot_product) < 1e-10, " (orthogonaux) ✓", " (non orthogonaux) ✗"),
                         "\n")
      }
    }
    return(output)
  })
  
  # Visualisation des contrastes
  output$contrast_plot <- renderPlot({
    contrasts <- get_contrasts()
    n_contrasts <- nrow(contrasts)
    n_groups <- ncol(contrasts)
    
    # Création du layout pour les graphiques
    par(mfrow = c(min(n_contrasts, 3), ceiling(n_contrasts/3)),
        mar = c(4, 4, 3, 1),
        bg = "white")
    
    for (i in 1:n_contrasts) {
      barplot(contrasts[i, ], 
              main = paste("Contraste", i),
              xlab = "Groupes", 
              ylab = "Coefficient",
              names.arg = 1:n_groups,
              col = "#4DA6FF",
              border = "white",
              ylim = c(min(contrasts) - 1, max(contrasts) + 1),
              cex.names = 1.2,
              cex.axis = 1.1,
              cex.lab = 1.1,
              cex.main = 1.3)
      abline(h = 0, lty = 2, col = "darkgray", lwd = 1.5)
      # Ajouter les valeurs sur les barres
      text(x = seq(0.7, by = 1.2, length.out = n_groups), 
           y = contrasts[i, ] + ifelse(contrasts[i, ] >= 0, 0.2, -0.4),
           labels = contrasts[i, ],
           cex = 0.9)
    }
  })
  
  # Action lors du clic sur le bouton
  observeEvent(input$check_orthogonal, {
    # Cette action met en évidence les résultats
    contrasts <- get_contrasts()
    sums <- rowSums(contrasts)
    all_valid <- all(abs(sums) < 1e-10)
    
    if (all_valid) {
      showNotification("Tous les contrastes sont valides (somme = 0)!", 
                       type = "message",
                       duration = 5)
    } else {
      showNotification("Attention : Certains contrastes ne sont pas valides (somme ≠ 0)!", 
                       type = "warning",
                       duration = 5)
    }
  })
}

shinyApp(ui = ui, server = server)
