# Aplicación para predecir el numero de hijos en un hogar según la información del mismo
# Técnicas en Aprendizaje Estadístico - Semestre 01 2021
# Universidad Nacional de Colombia
#
#' @Authors:#' 
#'     * Santiago Franco Valencia
#'     * Stephany Lobo
#'     * Isabela Luján Jaramillo
#'     * Daniel Alexander Naranjo Ríos
#'     * Ana María Sánchez Henao
#'
#'

library(shiny)
library(randomForest)
source("./predRF.R")

# Define UI for application that draws a histogram
ui <- fluidPage(title="Predicción de hijos",
    
    #CSS
    
    tags$head(# Note the wrapping of the string in HTML()
        tags$style(
            HTML("

        @import url('https://fonts.googleapis.com/css2?family=Kiwi+Maru:wght@300&family=Raleway:wght@200&display=swap');

        #title_panel{
            font-family: 'Raleway', sans-serif;
            font-weight: bold;
            font-size: 40px;
            text-align: center;   
        }
        body {
            margin-top: 40px;
            background-color: #f2f2f2;
            color: black;
            font-family: 'Raleway', sans-serif;
        }
        #description{
            font-size: 20px;
        }
        hr{
            border-top: 3px dotted;
            border-width: 5px;
        }
        #texto_predicc, #link1, #link2, #link3{
            font-size: 20px;
        }
        #predicc{
            font-size: 20px;
            margin-top:3px;
        }
        #SideBar{
            margin-top: 40px;
            background-color: #a3297a;
            color: white
        }
        #predecir_hijos{
            background-color: white;
            color: black;
            
        }
        #predecir_hijos:hover{
            background-color: #ebadd6
        }
        #links{
            font-family: 'Raleway', sans-serif;
            font-weight: bold;
            font-size: 20px;
        }
        
        "
            ))),
    
    #APP CODE
    
    # Application title
    titlePanel( 
        # textOutput("title_panel"),
        tags$div(class = "title_panel", checked = NA,
                 tags$h2("RAMILIA", style = "font-weight: bold; font-size: 40px; text-align: center;"),
                 tags$p("Aplicación para predecir el número de hijos de un hogar", style = "font-size: 30px; text-align: center;"),
        ),
    ),
    sidebarLayout(
        sidebarPanel (
            id = "SideBar",
            br(),
            #Input del gasto mensual del hogar
            numericInput(
                "I_UGASTO",
                "Gasto mensual total del hogar en pesos colombianos:",
                100000.0,
                min = 0
            ),
            #Input del ingreso mensual del hogar
            numericInput(
                "I_HOGAR",
                "Ingreso mensual total del hogar en pesos colombianos:",
                100000,
                min = 0
            ),
            #Input de cuánto cuesta el arriendo
            numericInput(
                "arriendo",
                "Arriendo mensual del hogar en pesos colombianos:",
                100000.0,
                min = 0
            ),
            #Input de cuanta gente vive en el hogar
            numericInput(
                "CANT_PERSONAS_HOGAR",
                "Numero de personas que viven en el hogar:",
                1 ,
                min = 0
            ),
            #Input de cuantas piezas tiene el hogar
            numericInput("num_dormitorios", "Numero de dormitorios del hogar:", 3 , min =
                             0),
            #Input para el tipo de sanitario con el que cuenta la vivienda
            selectInput(
                "tipo_serv_sanitario",
                "Tipo de servicio sanitario:",
                c(
                    "Inodoro conectado a alcantarillado" = "1",
                    "Inodoro conectado a pozo septico" =
                        "2",
                    "Inodoro sin conexion" =
                        "3",
                    "Letrina" =
                        "4",
                    "Inodoro con descarga directa a fuentes de agua (bajamar)" =
                        "5",
                    "No tiene servicio sanitario" =
                        "6"
                )
            ),
            #Input para saber si el conyugue vive o no en el hogar
            selectInput(
                "conyuges",
                "¿El conyugue vive en el hogar con usted?",
                c(
                    "No tiene conyugue" = "0",
                    "Sí vive en el hogar" =
                        "1",
                    "No vive en el hogar" =
                        "2"
                )
            ),
            #Input de cuantos cuartos (dormitorios y otros) tiene la casa
            numericInput("num_cuartos", "Numero de cuartos del hogar:", 2 , min =
                             0),
            #Input para saber de donde obtienen el agua de la casa para los alimentos
            selectInput(
                "obtencion_agua_alimento",
                "¿De dónde proviene el agua de la vivienda?",
                c(
                    "Acueducto público" = 1,
                    "Acueducto comunal o veredal" =
                        2,
                    "Pozo con bomba" =
                        3,
                    "Pozo sin bomba, aljibe, jagüey o barreno" =
                        4,
                    "Agua lluvia" =
                        5,
                    "Río, quebrada, manantial o nacimiento" =
                        6,
                    "Pila pública" =
                        7,
                    "Carro tanque" =
                        8,
                    "Aguatero" =
                        9,
                    "Agua embotellada o en bolsa" =
                        10
                )
            ),
            actionButton("predecir_hijos", "¡Predecir!")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            br(),
            br(),
            br(),
            textOutput("description"),
            br(),
            br(),
            hr(),
            br(),
            textOutput("texto_predicc"),
            textOutput("predicc"),
            br(),
            hr(),
            br(),
            textOutput("link1"),
            textOutput("linkVideo"),
            br(),
            textOutput("link2"),
            textOutput("linkRPubs"),
            br(),
            textOutput("link3"),
            textOutput("linkGithub"),
            br(),
            br(),
            br(),
            #Excavadora goes brrrrr
            tags$div(class = "links", checked = NA,
                     tags$p("Link al video:", style="font-size:20px"),
                     tags$a(href = "https://guthib.com", "Click aquí para ir al video promocional de la App", style="font-size:15px"), 
                     br(),
                     br(),
                     tags$p("Link al reporte técnico:", style="font-size:20px"),
                     tags$a(href = "https://rpubs.com/StephanyL/749703", "Click aquí para ir al reporte técnico en RPubs", style="font-size:15px"),
                     br(),
                     br(),
                     tags$p("Link al repositorio de GitHub:", style="font-size:20px"),
                     tags$a(href = "https://github.com/Stephl99/TAE", "Click aquí para ir al repositorio de GitHub", style="font-size:15px"),
            ),
            br(),
            br(),
            br(),
            textOutput("Authors"),
            br(),
            img(src = "LogoUNAL.png", height = 88.5, width = 200, alt="Logo de la Universidad Nacional de Colombia"),
            br(),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$title_panel <- renderText({
        "RAMILIA"
    })
    output$description <- renderText({
        "Por favor proporcione la información que se le pide en el formulario, 
        con base en ella se predecirá el número de hijos que habita en la vivienda."
    })
    observeEvent(input$predecir_hijos, {
        # Texto natural para la respuesta
        output$texto_predicc <- renderText({
            pred_text <-
                "Con base en la información proporcionada, el sistema estima
            que el número de hijos que habita la vivienda es:"
        })
        
        output$predicc <- renderText({
            # Predicción del modelo:
            evaluables <-
                c(
                    as.double(input$I_UGASTO),
                    as.double(input$I_HOGAR),
                    as.double(input$arriendo),
                    as.double(input$CANT_PERSONAS_HOGAR),
                    as.double(input$num_dormitorios),
                    as.integer(input$tipo_serv_sanitario),
                    as.integer(input$conyuges),
                    as.double(input$num_cuartos),
                    as.integer(input$obtencion_agua_alimento),
                    0
                )
            prediccion <- as.integer(predRF(evaluables)) - 1
        })
    })
    #Autores
    output$Authors <- renderText({
            "Hecho Por:
            Santiago Franco Valencia,
            Stephany Lobo,
            Isabela Lujan Jaramillo,
            Daniel Alexander Naranjo Ríos y 
            Ana María Sánchez Henao"
    })
}

# Run the application
shinyApp(ui = ui, server = server)
