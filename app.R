suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))
suppressMessages(library(shiny)) # shiny
suppressMessages(library(shinydashboard)) # shiny
suppressMessages(library(rsconnect)) # publicar 
suppressMessages(library(lubridate)) # manipular data
suppressMessages(library(DT)) # tabela
suppressMessages(library(geobr))
suppressMessages(library(sf))
suppressMessages(library(rio))

# dados por grão e estado
data_producao = read_csv("data_producao.csv") 
data_producao = data_producao[,-1]
data_area = read_csv("data_area.csv") 
data_area = data_area[,-1]
data_area$ano = gsub("/[0-9][0-9]","",data_area$ano)
data_producao$ano = gsub("/[0-9][0-9]","",data_producao$ano)
data_area = data_area %>% filter(ano != "2019 Previsão (¹)" & ano != "2020   Previsão (¹)"
                                 & ano != "2020 (¹) Lim. Inferior" & ano != "2020 (¹) Lim. Superior")
data_producao = data_producao %>% filter(ano != "2019 Previsão (¹)" & ano != "2020   Previsão (¹)"
                                 & ano != "2020 (¹) Lim. Inferior" & ano != "2020 (¹) Lim. Superior")

# dados do Brasil
produto = read_csv("produto.csv") 
produto = produto[,-1]
grao = read_csv("grao.csv") 
grao = grao[,-1]


states <- read_state(year="2017")
colnames(states)[2] <- "UF"
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())


#Shiny
ui <- dashboardPage(
    dashboardHeader(title = "Safra Brasileira"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Produção", tabName = "geral", icon = icon("line-chart")),
            menuItem("Área", tabName = "area", icon = icon("line-chart")),
            menuItem("Mapas", tabName = "mapas", icon = icon("line-chart")),
            menuItem("Brasil", tabName = "brasil", icon = icon("line-chart"),
                     menuSubItem("Por Produto", tabName = "produto", icon = icon("line-chart")),
                     menuSubItem("Por Grão", tabName = "grao", icon = icon("line-chart"))),
            menuItem("Por Matheus Duzzi", icon = icon("paper-plane"), href = "https://www.linkedin.com/in/matheusduzziribeiro/", newtab = T)
        )
    ),
    dashboardBody(    
        tabItems(
            tabItem("geral",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                selectInput("estado1", "Estado:", 
                                            choices = unique(data_producao$UF)),
                                hr(),
                                selectInput("grao1", "Grão:", 
                                            choices = unique(data_producao$name)),
                                hr(),
                                helpText("Escolha o Estado e o grão para sua análise"),
                                hr(),
                                helpText("Quantidade em mil toneladas")
                            ),
                            mainPanel(
                                plotOutput("prodgraf", width = "90%")  
                            )
                        ),
                        DT::dataTableOutput("tabprod")
                    )
            ),
            tabItem("area",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                selectInput("estado2", "Estado:", 
                                            choices = unique(data_producao$UF)),
                                hr(),
                                selectInput("grao2", "Grão:", 
                                            choices = unique(data_producao$name)),
                                hr(),
                                helpText("Escolha o Estado e o grão para sua análise"),
                                hr(),
                                helpText("Área em mil hectares")
                            ),
                            mainPanel(
                                plotOutput("areagraf", width = "90%")  
                            )
                        ),
                        DT::dataTableOutput("tabarea")
                    )
            ),
            tabItem("mapas",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                selectInput("ano1", "Ano:", 
                                            choices = unique(data_producao$ano)),
                                hr(),
                                selectInput("grao3", "Grão:", 
                                            choices = unique(data_producao$name)),
                                hr(),
                                helpText("Escolha o Ano e o Grão para sua análise"),
                                hr(),
                                helpText("Área em mil hectares e produção em mil toneladas (escala 1:1000)")
                            ),
                            mainPanel(
                                plotOutput("areaplan", width = "90%"),
                                plotOutput("areaprod", width = "90%")
                            )
                        )
                    )
            ),
            tabItem("produto",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                selectInput("sheet1", "Setor:", 
                                            choices = unique(produto$name)),
                                hr(),
                                selectInput("estado3", "Estado:", 
                                            choices = unique(produto$UF)),
                                hr(),
                                helpText("Escolha o Estado e entre Produção, Área ou Produtividade"),
                                hr(),
                                helpText("Área em mil hectares e produção em mil toneladas (escala 1:1000)")
                            ),
                            mainPanel(
                                plotOutput("produto1", width = "90%"),
                                plotOutput("produto2", width = "90%")
                            )
                        )
                    )
            ),
            tabItem("grao",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                selectInput("sheet2", "Setor:", 
                                            choices = unique(grao$name)),
                                hr(),
                                selectInput("grao4", "Grão:", 
                                            choices = unique(grao$PRODUTO)),
                                hr(),
                                helpText("Escolha o Grão e entre Produção, Área ou Produtividade"),
                                hr(),
                                helpText("Área em mil hectares e produção em mil toneladas (escala 1:1000)")
                            ),
                            mainPanel(
                                plotOutput("plotgrao", width = "90%")  
                            )
                        ),
                        DT::dataTableOutput("tabgrao")
                    )
            )
        )
    )
)




server <- function(input, output){ 
    
    output$prodgraf = renderPlot(
        ggplot(data_producao %>% filter(UF == input$estado1 & name == input$grao1),
               aes(ano,rend)) + geom_point() + theme(axis.text.x = element_text(angle = 45))
    )
    
    output$tabprod = DT::renderDataTable({
        DT::datatable(data_producao %>% filter(UF == input$estado1 & name == input$grao1) %>% select(ano,rend), options = list(paging = FALSE,searching = FALSE))
    })
    
    output$areagraf = renderPlot(
        ggplot(data_area %>% filter(UF == input$estado2 & name == input$grao2),
                                        aes(ano,rend)) + geom_point() + theme(axis.text.x = element_text(angle = 45))
    )
    
    output$tabarea = DT::renderDataTable({
        DT::datatable(data_producao %>% filter(UF == input$estado2 & name == input$grao2) %>% select(ano,rend), options = list(paging = FALSE,searching = FALSE))
    })

    output$areaplan = renderPlot(
        ggplot() +
            geom_sf(data= st_as_sf(data_area %>% filter(ano == input$ano1 & name == input$grao3) %>% select("UF","rend") 
                    %>% left_join(states)), 
                    aes(fill= rend/1000), color= NA, size=.15) +
            labs(subtitle="Área plantada no ano escolhido", size=8) +
            scale_fill_distiller(palette = "Set1", name="Área Plantada") +
            theme_minimal() + no_axis
    )
    
    output$areaprod = renderPlot(
        ggplot() +
            geom_sf(data= st_as_sf(data_producao %>% filter(ano == input$ano1 & name == input$grao3) %>% select("UF","rend") 
                    %>% left_join(states)), 
                    aes(fill=rend/1000), color= NA, size=.15) +
            labs(subtitle="Produção no ano escolhido", size=8) +
            scale_fill_distiller(palette = "Set1", name="Produção") +
            theme_minimal() + no_axis
    )
    
    output$produto1 = renderPlot(
        ggplot() +
            geom_sf(data= st_as_sf(produto %>% filter(UF == input$estado3 & name == input$sheet1) %>% select("UF","rend") 
                                   %>% left_join(states)), 
                    aes(fill=rend/1000), color= NA, size=.15) +
            labs(subtitle="Desempenho do Estado escolhido", size=8) +
            scale_fill_distiller(palette = "Set1", name="Produção") +
            theme_minimal() + no_axis
    )
    
    output$produto2 = renderPlot(
        ggplot(produto %>% filter(UF == input$estado3 & name == input$sheet1),
               aes(ano,rend)) + geom_point() + theme(axis.text.x = element_text(angle = 45))
    )
    
    output$plotgrao = renderPlot(
        ggplot(grao %>% filter(PRODUTO == input$grao4 & name == input$sheet2),
               aes(ano,rend)) + geom_point() + theme(axis.text.x = element_text(angle = 45))
    )
    
    output$tabgrao = DT::renderDataTable({
        DT::datatable(grao %>% filter(PRODUTO == input$grao4 & name == input$sheet2) %>% select(ano,rend), options = list(paging = FALSE,searching = FALSE))
    })
    
}

shinyApp(ui, server)


