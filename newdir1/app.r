library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(scales)

dane <- read.table("data/Danedlapolski.csv", header = TRUE, sep = ";")


ui<-fluidPage(
  
  theme = shinytheme("simplex"),
  
  titlePanel(
    
    h1(br(), "Wskaźniki makroekonomiczne dla Polski", align="center")
    
  ),
  
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
    
      selectInput(
        
        "zmienna",
        
        label = h5("Wybierz wskaźnik jaki chcesz zobaczyć", br()),
        
        choices = c(
          
          
          "GDP",
          
          "Inflation",
          
          "Unemployment"
        
        ),
        
                  
      ),
      
      br(),
      
      sliderInput(
        
        "zakres",
        
        label = h5("Wybierz zakres lat", br()),
        
        min = min(dane$ROK),
        
        max = max(dane$ROK),
        
        step = 1,
        
        value = c(min(dane$ROK),max(dane$ROK)),
        
        sep = ""
                
      ),
      
      br(),
      
      conditionalPanel(
        
        condition = "input.zmienna == 'Unemployment'",
        
        radioButtons(
          
          "zmiennaBez",
          
          label = h5("Wybierz dla kogo chcesz zobaczyć dane", br()),
          
          choices = c(
          
            "Male",
          
            "Female",
          
            "Total"
          )
          
        )
        
      ),
      
      conditionalPanel(
        
        condition = "input.zmienna == 'GDP'",
                       
        radioButtons(
          
          "zmiennaPKB",
                                     
          label = h5("Wybierz jaki rodzaj GDP chcesz zobaczyć", br()),
                                     
          choices = c(
                                       
            "GDP per capita growth (annual %)",
                                        
            "GDP growth (annual %)"
                                     
          )
                       
        ) 
                       
      )
      
    ),
    
    mainPanel(
      
      width = 9,
      
      fluidRow(
        
        align = "center",
        
        h3(textOutput("Pierwszyoutput"), align = "center"),
        
        br(),
        
        plotOutput("pierwszywykres", height = 500, width = "80%"),
        
        br(),
        
      ),
      
      fluidRow(
        
        align = "justify",
        
        column(width = 1),
        
        column(
          
          width = 10,
        
          p(strong("Pochodzenie danych: "), " ",
          
            a(
            
              "World Data Bank, WDI", 
            
              href = "https://databank.worldbank.org/reports.aspx?source=world-development-indicators# "
              
            )
          
          ),
        
          br(),
             
          p(strong("Opis zmiennej: ")),
            
          p(textOutput("Opiszmiennych")),
          
          br(),
          br(),
        
        )
      
      )
    
    )
    
  )
  
)

server <- function(input, output) {
  
  output$Pierwszyoutput <- renderText({
    
    if(input$zmienna == "GDP"){
    
      paste(input$zmiennaPKB)
        
    }
    
    else if(input$zmienna == "Inflation"){
      
      paste("Inflation, consumer prices (annual %)")
      
    }
    
    else if(input$zmienna == "Unemployment"){
      
      if(input$zmiennaBez == "Male"){
        
        paste("Unemployment, male (% of male labor force)")
        
      }
      
      else if(input$zmiennaBez == "Female"){
        
        paste("Unemployment, female (% of female labor force)")
        
      }
      
      else if(input$zmiennaBez == "Total"){
        
        paste("Unemployment, total (% of total labor force)")
        
      }
      
    }
    
  })
  
  output$pierwszywykres <- renderPlot({
    
    if(input$zmienna == "GDP"){
      
      if(input$zmiennaPKB == "GDP per capita growth (annual %)"){
        
        dane %>% filter(between(ROK, input$zakres[1], input$zakres[2])) %>%
          ggplot(aes(x = ROK, y = GDPPCgrowth)) +
          geom_col(fill = "steelblue") +
          xlab("Year") +
          ylab("GDP per capita growth (%)") +
          scale_x_continuous(breaks = ~round(unique(pretty(.))))
        
      }
      
      else if(input$zmiennaPKB == "GDP growth (annual %)"){
        
        dane %>% filter(between(ROK, input$zakres[1], input$zakres[2])) %>%
          ggplot(aes(x = ROK, y = GDPgrowth)) +
          geom_col(fill = "steelblue") +
          xlab("Year") +
          ylab("GDP growth (%)") +
          scale_x_continuous(breaks = ~round(unique(pretty(.))))
        
      }
      
    }
    
    else if(input$zmienna == "Inflation"){
      
      dane %>% filter(between(ROK, input$zakres[1], input$zakres[2])) %>%
        ggplot(aes(x = ROK, y = INFLATION)) +
        geom_col(fill = "steelblue") +
        xlab("Year") +
        ylab("Inflation (%)") +
        scale_x_continuous(breaks = ~round(unique(pretty(.))))
      
    }
    
    else if(input$zmienna == "Unemployment"){
      
      if(input$zmiennaBez == "Male"){
        
        dane %>% filter(between(ROK, input$zakres[1], input$zakres[2])) %>%
          ggplot(aes(x = ROK, y = Unmale)) +
          geom_col(fill = "steelblue") +
          xlab("Year") +
          ylab("Unemployment (%)") +
          scale_x_continuous(breaks = ~round(unique(pretty(.))))
        
      }
      
      else if(input$zmiennaBez == "Female"){
        
        dane %>% filter(between(ROK, input$zakres[1], input$zakres[2])) %>%
          ggplot(aes(x = ROK, y = Unfemale)) +
          geom_col(fill = "steelblue") +
          xlab("Year") +
          ylab("Unemployment (%)") +
          scale_x_continuous(breaks = ~round(unique(pretty(.))))
        
      }
      
      else if(input$zmiennaBez == "Total"){
        
        dane %>% filter(between(ROK, input$zakres[1], input$zakres[2])) %>%
          ggplot(aes(x = ROK, y = Untotal)) +
          geom_col(fill = "steelblue") +
          xlab("Year") +
          ylab("Unemployment (%)") +
          scale_x_continuous(breaks = ~round(unique(pretty(.))))
        
      }
      
    }
    
  })
  
  output$Opiszmiennych <- renderText({
    
    if(input$zmienna == "GDP"){
      
      if(input$zmiennaPKB == "GDP per capita growth (annual %)"){
        
        paste("Roczna procentowa stopa wzrostu PKB na mieszkańca w oparciu o stałą walutę lokalną. 
              PKB per capita to produkt krajowy brutto podzielony przez liczbę ludności w połowie roku. 
              PKB w cenach nabywcy to suma wartości dodanej brutto wytworzonej przez wszystkich krajowych producentów 
              w gospodarce powiększona o wszelkie podatki od produktów i pomniejszona o wszelkie dotacje nieuwzględnione w wartości produktów. 
              Jest obliczany bez dokonywania odliczeń z tytułu amortyzacji sfabrykowanych aktywów lub wyczerpania i degradacji zasobów naturalnych.")
        
      }
      
      else if(input$zmiennaPKB == "GDP growth (annual %)"){

        paste("Roczna procentowa stopa wzrostu PKB w cenach rynkowych w oparciu o stałą walutę lokalną. 
              Agregaty są oparte na cenach stałych z 2015 r., wyrażonych w dolarach amerykańskich. 
              PKB to suma wartości dodanej brutto wytworzonej przez wszystkich krajowych producentów 
              w gospodarce powiększona o podatki od produktów i pomniejszona o wszelkie dotacje nieuwzględnione w wartości produktów. 
              Jest obliczany bez dokonywania odliczeń z tytułu amortyzacji sfabrykowanych aktywów lub wyczerpania i degradacji zasobów naturalnych.")
        
      }
      
    }
    
    else if(input$zmienna == "Inflation"){
      
      paste("Inflacja mierzona indeksem cen towarów i usług konsumenckich 
            odzwierciedla roczną procentową zmianę kosztu nabycia koszyka towarów i usług przez przeciętnego konsumenta, 
            które mogą być ustalane lub zmieniane w określonych odstępach czasu, na przykład rocznych. 
            Powszechnie stosowana jest formuła Laspeyresa.")
      
    }
    
    else if(input$zmienna == "Unemployment"){
      
      if(input$zmiennaBez == "Male"){
        
        paste("Bezrobocie odnosi się do części siły roboczej, która nie ma pracy, ale jest dostępna i szuka zatrudnienia.")
        
      }
      
      else if(input$zmiennaBez == "Female"){
        
        paste("Bezrobocie odnosi się do części siły roboczej, która nie ma pracy, ale jest dostępna i szuka zatrudnienia.")
        
      }
      
      else if(input$zmiennaBez == "Total"){
        
        paste("Bezrobocie odnosi się do części siły roboczej, która nie ma pracy, ale jest dostępna i szuka zatrudnienia.")
        
      }
      
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
