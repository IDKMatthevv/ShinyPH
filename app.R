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
# Y na konci slov v out textu
y_na_konci <- "y"


# Define UI for application that draws a histogram
ui <- fluidPage(
  #barvičky
  theme = bs_theme(bootswatch = "cerulean"),
  # Application title
  titlePanel("Výpočet pH slabé kyseliny/zásady"),
  
  # Čudlíky a Inputy
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "vyber_AB", label="Vyberte typ látky", choiceNames = c("Slabá kyselina","Slabá zásada"), choiceValues = c("A","B")),
      numericInput(inputId = "vstupc0", value = 0.1, label = "Počáteční koncentrace (mol/l)"),
      uiOutput("dynamicke_pole_konstanty"),
      
      actionButton(inputId = "tl_vypocet", label = "Vypočítat"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Výsledek:"),
      textOutput("text_vysledek"),
      tableOutput("tabulka_vysledku")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Generování vstupního pole na straně serveru
  output$dynamicke_pole_konstanty <- renderUI({
    if (input$vyber_AB == "A") {
      numericInput("konstanta", HTML("Disociační konstanta K<sub>a</sub>"), value = 0.0000175)
    } else {
      numericInput("konstanta", HTML("Disociační konstanta K<sub>b</sub>"), value = 0.000018)
    }
  })
  
  # Výpočet podle tlačítka A nebo B
  observeEvent(input$tl_vypocet, {

    
    # Použití správné rovnice podle výběru A nebo B
    if (input$vyber_AB == "A") {
      ph <- vypocitej_ph_kyseliny(input$vstupc0, input$konstanta)
      typ <- "Kyselina"
    } else {
      ph <- vypocitej_ph_zasady(input$vstupc0, input$konstanta)
      typ <- "Zásada"
    }
    
    # Výstup textu s interpretací 
    output$text_vysledek <- renderText({
      paste("Vypočtené pH", sub("a$", "y", typ), "je:", round(ph, 2))
    })
    
    # Výstup tabulky
    output$tabulka_vysledku <- renderTable({
      data.frame(Parametr = c("Typ", "pH"), Typ = c(typ, round(ph, 2)))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
