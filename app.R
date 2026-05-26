library(shiny)
library(bslib)

# Výpočty
vypocitej_ph_kyseliny <- function(c0, Ka) {
  h_plus <- sqrt(Ka * c0) 
  return(-log10(h_plus)) 
}
vypocitej_ph_zasady <- function(c0, Kb) {
  oh_minus <- sqrt(Kb * c0)
  pOH <- -log10(oh_minus)
  return(14 - pOH)
}

# Define UI for application
ui <- fluidPage(
  # barvičky
  theme = bs_theme(bootswatch = "cerulean"),
  # Application title
  titlePanel("Výpočet pH slabé kyseliny/zásady"),
  
  # Čudlíky a Inputy
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "vyber_AB", label="Vyberte typ látky", choiceNames = c("Slabá kyselina","Slabá zásada"), choiceValues = c("A","B")),
      numericInput(inputId = "vstupc0", value = 0.1, label = "Počáteční koncentrace (mol/l)"),
      uiOutput("dynamicke_pole_konstanty"),
      actionButton(inputId = "tl_vypocet", label = "Vypočítat")
    ),
    
    # Tabulka s výsledky a graf
    mainPanel(
      h3("Výsledek:"),
      textOutput("text_vysledek"),
      tableOutput("tabulka_vysledku"),
      
      br(),
      h4("Graf závislosti pH na koncentraci"),
      plotOutput("graf_zavislosti")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Co se pocita v zavislosti na zvolene konstante
  output$dynamicke_pole_konstanty <- renderUI({
    if (input$vyber_AB == "A") {
      numericInput("konstanta", HTML("Disociační konstanta K<sub>a</sub>"), value = 0.0000175)
    } else {
      numericInput("konstanta", HTML("Disociační konstanta K<sub>b</sub>"), value = 0.000018)
    }
  })
  
  # Vypocet, ceka na tlacitko
  vypoctena_data <- eventReactive(input$tl_vypocet, {
    
    if (input$vyber_AB == "A") {
      ph <- vypocitej_ph_kyseliny(input$vstupc0, input$konstanta)
      typ <- "Kyselina"
    } else {
      ph <- vypocitej_ph_zasady(input$vstupc0, input$konstanta)
      typ <- "Zásada"
    }
    
    # Hodnoty v listu aby byly pristupne
    list(
      ph = ph,
      typ = typ,
      c0 = input$vstupc0,
      konstanta = input$konstanta,
      vyber_AB = input$vyber_AB
    )
  })
  
  # 2. Text
  output$text_vysledek <- renderText({
    data <- vypoctena_data()
    paste("Vypočtené pH", sub("a$", "y", data$typ), "je:", round(data$ph, 2))
  })
  
  # 3. Tabulka
  output$tabulka_vysledku <- renderTable({
    data <- vypoctena_data()
    data.frame(Parametr = c("Typ", "pH"), Typ = c(data$typ, round(data$ph, 2)))
  })
  
  # 4. Graf
  output$graf_zavislosti <- renderPlot({
    data <- vypoctena_data()
    
    max_c <- max(0.1, data$c0 * 2) 
    c_seq <- seq(0.0001, max_c, length.out = 200)
    
    if (data$vyber_AB == "A") {
      ph_seq <- vypocitej_ph_kyseliny(c_seq, data$konstanta)
      barva_krivky <- "blue"
    } else {
      ph_seq <- vypocitej_ph_zasady(c_seq, data$konstanta)
      barva_krivky <- "violet"
    }
    
    plot(c_seq, ph_seq, type = "l", col = barva_krivky, lwd = 3,
         xlab = expression("Počáteční koncentrace " * c[0] * " (mol/l)"), 
         ylab = "pH",
         bty = "n", las = 1)
    
    grid(col = "lightgray", lty = "dotted")
    
    points(data$c0, data$ph, col = "pink", pch = 19, cex = 1.8)
    
    pozice_textu <- ifelse(data$c0 > max_c * 0.8, 2, 4)
    text(data$c0, data$ph, labels = paste0("pH = ", round(data$ph, 2)),
         pos = pozice_textu, col = "red", font = 2, offset = 0.8)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)