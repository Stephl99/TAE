#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        # Input: Simple integer interval ----
        
       
        
        sidebarPanel(
            numericInput("I_HOGAR", "Ingreso mensual total del hogar en pesos colombianos:", 100000),
            numericInput("I_UGASTO","Gasto mensual total del hogar en pesos colombianos:", 100000),
            numericInput("arriendo","Arriendo mensual del hogar en pesos colombianos:", 100000),
            numericInput("CANT_PERSONAS_HOGAR","Numero de personas que viven en el hogar", 1),
            numericInput("num_dormitorios","Numero de dormitorios del hogar:", 3),
            numericInput("num_cuartos","Numero de cuartos del hogar:", 2),
            selectInput("tipo_ser_sanitario","Tipo de servicio sanitario:", c("Inodoro conectado a alcantarillado"="1",
                                                                              "Inodoro conectado a pozo septico"="2",
                                                                              "Inodoro sin conexion"="3",
                                                                              "Letrina"="4",
                                                                              "Inodoro con descarga directa a fuentes de agua (bajamar)"="5",
                                                                              "No tiene servicio sanitario","6")),
            
            selectInput("conyuges","El conyugue vive en el hogar con usted:", c("No tiene"="0",
                                                                                  "Si vive en el hogar"="1",
                                                                                  "No vive en el hogar"="2")),
            
            selectInput("obtencion_agua_alimento","De donde proviene el agua de la vivienda",c("Acueducto publico"="3",
                                                                                                 "Acueducto comunal o veredal"="4",
                                                                                                 "Pozo con bomba"="5",
                                                                                                 "Pozo sin bomba, aljibe, jagUey o barreno"="6",
                                                                                                 "Agua lluvia"="7",
                                                                                                 "RIo, quebrada, manantial o nacimiento"="8",
                                                                                                 "Pila pUblica"="9",
                                                                                                 "Carro tanque"="10",
                                                                                                 "Aguatero"="11",
                                                                                                 "Agua embotellada o en bolsa"="12")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

#loadRDS

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
