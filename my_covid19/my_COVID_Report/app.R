#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse) # %>% el resultado de la izquierda se aplica a la derecha
library(magrittr) # %<>% el rdo de la izda se aplica a la dcha y viceversa
library(lubridate) # quizas no sea necesario importarla, ya viene incluida en tidyverse
library(plotly)
library(xts)
library(dygraphs)


# Define UI for application that draws a histogram
# Visualización en el navegador, genera javascript
ui <- fluidPage(
    # shinythemes::shinytheme("cerulean")
    shinythemes::themeSelector(),
    titlePanel("Análisis del COVID-19"),
    
    # Forma de organizar los paneles
    sidebarLayout(
        # Panel lateral
        sidebarPanel(
            dateInput("date1", "Fecha Inicio:", value = "2020-01-22"),
            dateInput("date2", "Fecha Fin:", value = today()),
            uiOutput("pais"), ## Rellenar desde server con países
            checkboxInput("logscale", "Log Y", value = FALSE),
            sliderInput("alpha", "Selecciona el nivel de transparencia:", 
                        min = 0, max = 1, value = 0.2)
        ),
        # Panel principal
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Tabla", tableOutput("contents")),
                        tabPanel("Plot 1", plotOutput("plot1")),
                        tabPanel("Plot 2", dygraphOutput("plot2")),
                        tabPanel("Plot 3", plotOutput("plot3")),
                        tabPanel("Plot 4", plotlyOutput("plot4"))
                        ) 
        )
    )
)

# Define server logic required to draw a histogram
# Código para proveer info al iu
# Añadimos el parámetro 'session'
server <- function(input, output, session) {
  
    myoriginaldata <- reactive({
      datos <- read.csv("../covid_19_clean_complete.csv", stringsAsFactors = FALSE)
      
      colnames(datos) = c("Provincia_Estado", # Cualitativo
                          "Pais_Region", # Cualitativo
                          "Latitud", # Norte+ o Sur- Cuantitativo
                          "Longitud", # Este+ u Oeste- Cuantitativo
                          "Fecha", # Ordinal
                          "Casos_Confirmados", # Cuantitativo
                          "Casos_Muertos", # Cuantitativo
                          "Casos_Recuperados" # Cuantitativo
      )
      
      datos$Provincia_Estado %<>% factor()
      datos$Pais_Region %<>% factor()
      datos$Fecha %<>% mdy()
      
      return(datos)
    })
    
  
     mydata <- reactive({
       # Al devolver los datos, los filtramos por los datos seleccionados por el usuario en pantalla
       # Con input$ accedemos a campos del 'ui'
       return(myoriginaldata() %>% filter(between(Fecha, input$date1, input$date2))) 
    })
    
     primer_contagio <- reactive({
       myoriginaldata() %>%
         group_by(Pais_Region) %>%
         filter(Casos_Confirmados > 0) %>%
         summarise(Primer_Contagio = min(Fecha)-1)
     })
     
    output$pais <- renderUI({
        countries <- mydata() %>% # Partiendo de los datos del csv, sacamos los posible países para el desplegable
            select(Pais_Region) %>%
            arrange(Pais_Region) %>%
            unique()
        selectInput("pais", "Selecciona el país:", choices = countries)
    })
    
    
    output$contents <- renderTable({
        mydata() %>% filter(Pais_Region == input$pais)
    })
    
    output$plot1 <- renderPlot({
        datos_por_fecha = aggregate(
            cbind(Casos_Confirmados, Casos_Muertos, Casos_Recuperados) ~ Fecha,
            data = mydata() %>% # Indicamos que los datos estan en my_data 
            filter(Pais_Region == input$pais), # Añadimos este filtro para mostrar solo el país seleccionado   
            FUN = sum
        )
        datos_por_fecha$Casos_Enfermos = datos_por_fecha$Casos_Confirmados - 
            datos_por_fecha$Casos_Muertos - datos_por_fecha$Casos_Recuperados
        
        logy = ""
        # Definimos el eje y, forzando que siempre empiece en 0 hasta el valor máximo
        # Lo ampliamos un 0,5% para que no llege la línea hasta el marco
        lims = c(0, 1.05*max(datos_por_fecha$Casos_Confirmados))
        
        if(input$logscale){
            logy = "y"
            datos_por_fecha %<>%
                filter(Casos_Confirmados > 0) # Si el usuario elige 'log' filtramos para no hacer log(0)
            lims = c(1, 1.05*max(datos_por_fecha$Casos_Confirmados))
        }
        
        
        plot(Casos_Confirmados ~ Fecha, data = datos_por_fecha, 
             col = "blue", type = "l", ylim = lims,
             main = paste0("Casos documentados por día en ", input$pais),
             xlab = "Fecha", ylab ="Número de personas", log = logy)
        lines(Casos_Muertos ~ Fecha, data = datos_por_fecha, col = "red")
        lines(Casos_Recuperados ~ Fecha, data = datos_por_fecha, col = "green")
        
        legend("topleft", c("Confirmados", "Muertos", "Recuperados"),
               col = c("blue", "red", "green"), pch = 1, lw = 2)
  
    })
    
    output$plot2 <- renderDygraph({
      
      datos_por_fecha = aggregate(
        cbind(Casos_Confirmados, Casos_Muertos, Casos_Recuperados) ~ Fecha,
        data = mydata() %>% # Indicamos que los datos estan en my_data 
          filter(Pais_Region == input$pais), # Añadimos este filtro para mostrar solo el país seleccionado   
        FUN = sum
      )
      datos_por_fecha$Casos_Enfermos = datos_por_fecha$Casos_Confirmados - 
        datos_por_fecha$Casos_Muertos - datos_por_fecha$Casos_Recuperados
      
      datos_por_fecha_ts <- xts(x = datos_por_fecha[ ,2:5],
                                order.by = datos_por_fecha$Fecha)
      
      # Con la libreria dygraphs asociada a xts pintamos un diagrama, con diferentes opciones
      dygraph(datos_por_fecha_ts) %>%
        dyOptions(labelsUTC = TRUE, labelsKMB = TRUE,
                  fillGraph = TRUE, fillAlpha = input$alpha, 
                  drawGrid = FALSE, colors = "red") %>%
        dyRangeSelector() %>%
        dyCrosshair(direction = "vertical") %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE) %>%
        dyRoller(rollPeriod = 2)
      
    })
      
    output$plot3 <- renderPlot({
        mydata() %>%
          filter(Fecha == ymd(input$date1)) %>% 
          ggplot(aes(Longitud, Latitud)) +
          geom_point(aes(size = log(Casos_Confirmados+1), 
                         colour = log(Casos_Muertos+1)))  +
          coord_fixed() +
          theme(legend.position = "bottom")

    })
      
    output$plot4 <- renderPlotly({
      
      data_first = mydata() %>%
        inner_join(primer_contagio(), by = "Pais_Region") %>%
        mutate(Dias_Desde_PC = as.numeric(Fecha - Primer_Contagio)) %>%
        filter(Dias_Desde_PC >= 0) %>%
        group_by(Dias_Desde_PC, Pais_Region) %>%
        summarise(Casos_Confirmados = sum(Casos_Confirmados),
                  Casos_Muertos = sum(Casos_Muertos),
                  Casos_Recuperados = sum(Casos_Recuperados)
                  )
      
      data_first %>%
        # Dejamos unos países fijos, y añadimos el seleccionado por el usuario
        filter(Pais_Region %in% c("Spain", "Italy", "China", 
                                  "US", "Germany", input$pais)) %>%
        ggplot(aes(x = Dias_Desde_PC, y = Casos_Confirmados)) + 
        geom_line(aes(col = Pais_Region)) +
        xlab("Días desde el primer contagio") +
        ylab("Número de personas contagiadas") +
        ggtitle("Análisis de Cohortes") +
        theme(legend.position = "none") -> g
      
      # Lo asignamos a un gráfico ggplot y lo transformamos con ggplotly
      ggplotly(g)
    
    })


}

# Run the application 
shinyApp(ui = ui, server = server)
