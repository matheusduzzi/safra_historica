suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))
suppressMessages(library(shiny)) 
suppressMessages(library(shinydashboard)) 
suppressMessages(library(rsconnect)) 
suppressMessages(library(lubridate)) 
suppressMessages(library(DT)) 
suppressMessages(library(geobr))
suppressMessages(library(sf))
suppressMessages(library(rio))
suppressWarnings(library(plotly))
suppressWarnings(library(dygraphs))
suppressWarnings(library(shinythemes))
suppressWarnings(library(ggthemes))
suppressWarnings(library(Quandl))
suppressWarnings(library(zoo))
suppressWarnings(library(forecast))
suppressWarnings(library(readxl))

cepea <- read_excel("cepea.xlsx")
cepea_code <- paste0("CEPEA/",cepea$code)
Quandl.api_key("GoaHpqiK3dLspe3MXHDL")
#mydata = Quandl("CEPEA/CALF", type="zoo")

# dados por grão e estado
data_producao = read_csv("data_producao.csv") 
data_producao = data_producao[,-1]
data_area = read_csv("data_area.csv") 
data_area = data_area[,-1]
data_area$ano = gsub("/[0-9][0-9]","",data_area$ano)
data_area$ano = gsub("201516","2015",data_area$ano)
data_producao$ano = gsub("/[0-9][0-9]","",data_producao$ano)

# dados de previsão
prev_area = na.exclude(data_area %>% filter(ano == "2019 Previsão (¹)" | ano == "2020   Previsão (¹)"
                                 | ano == "2020 (¹) Lim. Inferior" | ano == "2020 (¹) Lim. Superior"))
prev_prod = na.exclude(data_producao %>% filter(ano == "2019 Previsão (¹)" | ano == "2020   Previsão (¹)" 
                                     | ano == "2020 (¹) Lim. Inferior" | ano == "2020 (¹) Lim. Superior"))


# dados limpos 
data_area = na.exclude(data_area %>% filter(ano != "2019 Previsão (¹)" & ano != "2020   Previsão (¹)"
                                 & ano != "2020 (¹) Lim. Inferior" & ano != "2020 (¹) Lim. Superior"))
data_producao = na.exclude(data_producao %>% filter(ano != "2019 Previsão (¹)" & ano != "2020   Previsão (¹)"
                                 & ano != "2020 (¹) Lim. Inferior" & ano != "2020 (¹) Lim. Superior"))

# dados do Brasil
produto = read_csv("produto.csv") 
produto = produto[,-1]
grao = read_csv("grao.csv") 
grao = grao[,-1]

# shape
states <- read_state(year="2017")
colnames(states)[2] <- "UF"
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

colnames(data_area) <- c("UF","ano","quantidade","name")
colnames(data_producao) <- c("UF","ano","quantidade","name")
colnames(grao) <- c("PRODUTO","ano","quantidade","name")
colnames(produto) <- c("UF","ano","quantidade","name")
colnames(prev_area) <- c("UF","ano","quantidade","name")
colnames(prev_prod) <- c("UF","ano","quantidade","name")




ui <- dashboardPage(
    dashboardHeader(title = "Meu Manejo"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home-Page", tabName = "home", icon = icon("table")),
            menuItem("Descrição das cotações", tabName = "aba_0", icon = icon("line-chart")),
            menuItem("Cotações", tabName = "aba_6", icon = icon("line-chart")),
            menuItem("Produção", tabName = "aba_1", icon = icon("line-chart")),
            menuItem("Área", tabName = "aba_2", icon = icon("line-chart")),
            menuItem("Mapas", tabName = "aba_3", icon = icon("line-chart")),
            menuItem("Brasil", tabName = "aba_4", icon = icon("line-chart"),
                     menuSubItem("Por Produto", tabName = "aba1_brasil", icon = icon("line-chart")),
                     menuSubItem("Por Grão", tabName = "aba2_brasil", icon = icon("line-chart"))),
            menuItem("Previsões da CONAB", tabName = "aba_5", icon = icon("line-chart"),
                     menuSubItem("Produção", tabName = "aba1_prev", icon = icon("line-chart")),
                     menuSubItem("Área", tabName = "aba2_prev", icon = icon("line-chart"))),
            menuItem("Séries Temporais", tabName = "aba_series", icon = icon("line-chart"),
                     menuSubItem("Por Produção", tabName = "aba_7", icon = icon("line-chart")),
                     menuSubItem("Por Área", tabName = "aba_8", icon = icon("line-chart"))),
            menuItem("Por Matheus Duzzi", icon = icon("paper-plane"), href = "https://www.linkedin.com/in/matheusduzziribeiro/", newtab = T)
        )
    ),
    dashboardBody(
        tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #03b000;
                              }
        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #03b000;
                              }
        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #03b000;
                              }        
        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #03b000;
                              }
        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #03b000;
                              }
        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #03b000;
                              color: #000000;
                              }
        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #e63200;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #03b000;
                              }
                              '))),
        
        tabItems(
            tabItem("home",
                    fluidRow(
                        mainPanel(
                            h1("Meu Manejo",align = "left"),
                            hr(),
                            img(src = "img.png", height = 400, width = 500, style = "float:left;"),
                            hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),
                            hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),
                            h4(strong("O que eu posso ver nesse app?",align = "left")),
                            hr(),
                            h4("A área plantada, produtividade e produção de grãos em séries históricas",align = "left"),
                            hr(),
                            h4("Quanto foi produzido de cada tipo de grão por estado",align = "left"),
                            hr(),
                            h4("Cotações de commodities",align = "left"),
                            hr(),
                            h4("Predições de rendimento das safras em questão",align = "left"),
                        )
                    )
            ),
            tabItem("aba_0",
                    fluidRow(
                        mainPanel(
                            h2("O que cada símbolo diz?",align = "left"),
                            hr(),
                            hr(),
                            h4("Na aba de cotações, você terá a opção de escolha do commoditie que desejar analisar, para isso veja na
                               tabela abaixo do que se trata cada uma das opções:",align = "left"),
                            hr(),hr(),
                            DT::dataTableOutput("tab_aba0")
                        )
                    )
            ),
            tabItem("aba_1",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                h3("Produção por estado"),
                                selectInput("estado_aba1", "Estado:", 
                                            choices = unique(data_producao$UF)),
                                hr(),
                                selectInput("grao_aba1", "Grão:", 
                                            choices = unique(data_producao$name)),
                                hr(),
                                helpText("Escolha o estado e o grão para sua análise. Quantidade em mil toneladas"),
                                helpText("Erros podem aparecer se o estado não produz o grão escolhido")
                            ),
                            mainPanel(
                                dygraphOutput("plot_aba1")  
                            )
                        ),
                        hr(),
                        DT::dataTableOutput("tabgeral")
                    )
            ),
            tabItem("aba_2",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                h3("Área Plantada por Estado"),
                                selectInput("estado_aba2", "Estado:", 
                                            choices = unique(data_producao$UF)),
                                hr(),
                                selectInput("grao_aba2", "Grão:", 
                                            choices = unique(data_producao$name)),
                                hr(),
                                helpText("Escolha o estado e o grão para sua análise. Área em mil hectares"),
                                helpText("Erros podem aparecer se o estado não produz o grão escolhido")
                            ),
                            mainPanel(
                                dygraphOutput("areagraf")  
                            )
                        ),
                        hr(),
                        DT::dataTableOutput("tabarea")
                    )
            ),
            tabItem("aba_3",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                h3("Mapa por ano e grão"),
                                selectInput("ano_aba3", "Ano:", 
                                            choices = unique(data_producao$ano)),
                                hr(),
                                selectInput("grao_mapa3", "Grão:", 
                                            choices = unique(data_producao$name)),
                                hr(),
                                helpText("Escolha o ano e o grão para sua análise"),
                                helpText("Área em mil hectares e produção em mil toneladas (escala 1:1000)"),
                                helpText("Erros podem aparecer se o estado não produz o grão escolhido")
                            ),
                            mainPanel(
                                plotOutput("areaplan", width = "90%"),
                                plotOutput("areaprod", width = "90%")
                            )
                        )
                    )
            ),
            tabItem("aba1_brasil",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                h3("Desempenho do total de grãos por estado"),
                                selectInput("setor_aba1_brasil", "Setor:", 
                                            choices = unique(produto$name)),
                                hr(),
                                selectInput("estado_aba1_brasil", "Estado:", 
                                            choices = unique(produto$UF)),
                                hr(),
                                helpText("Escolha o estado e entre produção, área ou produtividade"),
                                helpText("Área em mil hectares e produção em mil toneladas (escala 1:1000)"),
                                helpText("Erros podem aparecer se o estado não produz o grão escolhido")
                            ),
                            mainPanel(
                                plotOutput("produto1", width = "90%"),
                                hr(),
                                plotOutput("produto2", width = "90%")
                            )
                        )
                    )
            ),
            tabItem("aba2_brasil",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                h3("Desempenho brasileiro por grão em cada estado"),
                                selectInput("estado_aba2_brasil", "Setor:", 
                                            choices = unique(grao$name)),
                                hr(),
                                selectInput("grao_aba2_brasil", "Grão:", 
                                            choices = unique(grao$PRODUTO)),
                                hr(),
                                helpText("Escolha o grão e entre produção, área ou produtividade"),
                                helpText("Área em mil hectares e produção em mil toneladas (escala 1:1000)"),
                                helpText("Erros podem aparecer se o estado não produz o grão escolhido")
                            ),
                            mainPanel(
                                plotOutput("plotgrao", width = "90%")  
                            )
                        ),
                        hr(),
                        DT::dataTableOutput("tabgrao")
                    )
            ),
            tabItem("aba1_prev",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                h3("Previsão da CONAB de produção brasileiro por estado"),
                                selectInput("grao_aba1_prev", "Grão:", 
                                            choices = unique(prev_prod$name)),
                                hr(),
                                helpText("Escolha o grão"),
                                hr(),
                                selectInput("estado_aba1_prev", "Estado:", 
                                            choices = unique(prev_prod$UF)),
                                hr(),
                                helpText("Escolha o estado"),
                                helpText("Erros podem aparecer se o estado não produz o grão escolhido")
                            ),
                            mainPanel(
                                DT::dataTableOutput("tab_prevprodgrao")
                            )
                        )

                    )
            ),
            tabItem("aba2_prev",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                h3("Previsão da CONAB de área plantada brasileiro por estado"),
                                selectInput("grao_aba2_prev", "Grão:", 
                                            choices = unique(prev_prod$name)),
                                hr(),
                                helpText("Escolha o grão"),
                                hr(),
                                selectInput("estado_aba2_prev", "Estado:", 
                                            choices = unique(prev_prod$UF)),
                                hr(),
                                helpText("Escolha o estado"),
                                helpText("Erros podem aparecer se o estado não produz o grão escolhido")
                            ),
                            mainPanel(
                                DT::dataTableOutput("tab_prevareagrao")
                            )
                        )

                    )
            ),
            tabItem("aba_6",
                    fluidRow(
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("code_aba6", "Produto:", 
                                            choices = cepea_code)),
                            mainPanel(
                                DT::dataTableOutput("tab_aba6")
                            )
                        )
                    )
            ),
            tabItem("aba_7",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                h3("Predição de produção por Séries Temporais"),
                                selectInput("estado_aba7", "Estado:", 
                                            choices = unique(times_prod$UF)),
                                hr(),
                                selectInput("grao_aba7", "Grão:", 
                                            choices = unique(times_prod$name)),
                                hr(),
                                helpText("Escolha o grão e o Estado"),
                                helpText("Erros podem aparecer se o estado não produz o grão escolhido")
                            ),
                            mainPanel(
                                DT::dataTableOutput("tab_aba7")
                            )
                        )
                    )
            ),
            tabItem("aba_8",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                h3("Predição de área por Séries Temporais"),
                                selectInput("estado_aba8", "Estado:", 
                                            choices = unique(times_prod$UF)),
                                hr(),
                                selectInput("grao_aba8", "Grão:", 
                                            choices = unique(times_prod$name)),
                                hr(),
                                helpText("Escolha o grão e o Estado"),
                                helpText("Erros podem aparecer se o estado não produz o grão escolhido")
                            ),
                            mainPanel(
                                DT::dataTableOutput("tab_aba8")
                            )
                        )
                    )
            )
        )
    )
)

server <- function(input, output){ 
    
    ##### ABA 0
    
    output$tab_aba0 = DT::renderDataTable({
        DT::datatable(cepea, options = list(paging = TRUE,searching = TRUE))
    })
    
    
    ##### ABA 1
    
    output$plot_aba1 = renderDygraph({
        
    dygraph(ts(data_producao %>% filter(UF == input$estado_aba1 & name == input$grao_aba1) 
             %>% select("quantidade"),start = c(as.numeric(data_producao %>% filter(UF == input$estado_aba1 & name == input$grao_aba1) 
                 %>% select(ano) %>% head(1))),end = c(as.numeric(data_producao 
                                    %>% filter(UF == input$estado_aba1 & name == input$grao_aba1) 
                                      %>% select(ano) %>% tail(1))), frequency = 1))
    })
    
    output$tabgeral = DT::renderDataTable({
        DT::datatable(data_producao %>% filter(UF == input$estado_aba1 & name == input$grao_aba1) %>% select(ano,quantidade), options = list(paging = TRUE,searching = TRUE))
    })
    
    ##### ABA 2
    
    output$areagraf = renderDygraph({
        
        dygraph(ts(data_area %>% filter(UF == input$estado_aba2 & name == input$grao_aba2)
                   %>% select("quantidade"),start = c(as.numeric(data_area %>% filter(UF == input$estado_aba2 & name == input$grao_aba2) 
                                                                 %>% select(ano) %>% head(1))),
                   end = c(as.numeric(data_area %>% filter(UF == input$estado_aba2 & name == input$grao_aba2) 
                                      %>% select(ano) %>% tail(1))), frequency = 1))
    })
    
    output$tabarea = DT::renderDataTable({
        DT::datatable(data_producao %>% filter(UF == input$estado_aba2 & name == input$grao_aba2) %>% select(ano,quantidade), options = list(paging = TRUE,searching = TRUE))
    })
    
    ##### SUB 3.1
    
    output$areaplan = renderPlot(
        ggplot() +
            geom_sf(data= st_as_sf(data_area %>% filter(ano == input$ano_aba3 & name == input$grao_mapa3) %>% select("UF","quantidade") 
                                   %>% left_join(states)), 
                    aes(fill= quantidade/1000), color= NA, size=.15) +
            labs(subtitle="Área plantada no ano escolhido", size=8) +
            scale_fill_distiller(palette = "Set1", name="Área Plantada") +
            theme_minimal() + no_axis
    )
    
    output$areaprod = renderPlot(
        ggplot() +
            geom_sf(data= st_as_sf(data_producao %>% filter(ano == input$ano_aba3 & name == input$grao_mapa3) %>% select("UF","quantidade") 
                                   %>% left_join(states)), 
                    aes(fill=quantidade/1000), color= NA, size=.15) +
            labs(subtitle="Produção no ano escolhido", size=8) +
            scale_fill_distiller(palette = "Set1", name="Produção") +
            theme_minimal() + no_axis
    )
    
    ##### sUB 3.2
    
    output$produto1 = renderPlot(
        ggplot() +
            geom_sf(data= st_as_sf(produto %>% filter(UF == input$estado_aba1_brasil & name == input$setor_aba1_brasil) %>% select("UF","quantidade") 
                                   %>% left_join(states)), 
                    aes(fill=quantidade/1000), color= NA, size=.15) +
            labs(subtitle="Desempenho do Estado escolhido", size=8) +
            scale_fill_distiller(palette = "Set1", name="Produção") +
            theme_minimal() + no_axis
    )
    
    output$produto2 = renderPlot(
        ggplot(produto %>% filter(UF == input$estado_aba1_brasil & name == input$setor_aba1_brasil),
               aes(ano,quantidade)) +  
            geom_point() +
            theme(axis.text.x=element_text(angle=60,vjust = 1, hjust=1,size=15)) + 
            theme(axis.text.y = element_text(size=13))  +
            theme_hc()
    )
    
    ##### ABA 4
    
    output$plotgrao = renderDygraph({
    
    dygraph(ts(grao %>% filter(PRODUTO == input$grao_aba2_brasil & name == input$estado_aba2_brasil)
               %>% select("quantidade"),start = c(as.numeric(grao %>% filter(PRODUTO == input$grao_aba2_brasil & name == input$estado_aba2_brasil)
                                                             %>% select(ano) %>% head(1))),
               end = c(as.numeric(grao %>% filter(PRODUTO == input$grao_aba2_brasil & name == input$estado_aba2_brasil)
                                  %>% select(ano) %>% tail(1))), frequency = 1))
    })
    
    output$plotgrao = renderPlot(
        ggplot(grao %>% filter(PRODUTO == input$grao_aba2_brasil & name == input$estado_aba2_brasil),
               aes(ano,quantidade)) +  
            geom_point() +
            theme(axis.text.x=element_text(angle=60,vjust = 1, hjust=1,size=15)) + 
            theme(axis.text.y = element_text(size=13))  +
            theme_hc()
    )
    
    output$tabgrao = DT::renderDataTable({
        DT::datatable(grao %>% filter(PRODUTO == input$grao_aba2_brasil & name == input$estado_aba2_brasil) %>% select(ano,quantidade), options = list(paging = TRUE,searching = TRUE))
    })
    
    ##### ABA 5.1
    
    output$tab_prevprodgrao = DT::renderDataTable({
        DT::datatable(prev_prod %>% filter(UF == input$estado_aba1_prev & name == input$grao_aba1_prev) %>% select(ano,quantidade), options = list(paging = TRUE,searching = TRUE))
    })
    
    ##### ABA 5.2
    
    output$tab_prevareagrao = DT::renderDataTable({
        DT::datatable(prev_area %>% filter(UF == input$estado_aba2_prev & name == input$grao_aba2_prev) %>% select(ano,quantidade), options = list(paging = TRUE,searching = TRUE))
    })
    
    ##### ABA 6
    
    output$tab_aba6 = DT::renderDataTable({
        DT::datatable(data.frame(Quandl(input$code_aba6, type="zoo")), options = list(paging = TRUE,searching = TRUE))
    })
    
    ##### ABA 8
    
    output$tab_aba7 = DT::renderDataTable({
        DT::datatable(as.data.frame(forecast(nnetar(ts(data_producao %>% filter(quantidade != 0) %>% filter(UF ==  input$estado_aba7 & name == input$grao_aba7) %>% select("quantidade"),start = c(1976), frequency = 1)),h=4)), options = list(paging = TRUE,searching = TRUE))
    })
    
    ##### ABA 9
    
    output$tab_aba8 = DT::renderDataTable({
        DT::datatable(as.data.frame(forecast(nnetar(ts(data_area %>% filter(quantidade != 0) %>% filter(UF ==  input$estado_aba8 & name == input$grao_aba8) %>% select("quantidade"),start = c(1976), frequency = 1)),h=4)), options = list(paging = TRUE,searching = TRUE))
    })
    

}

shinyApp(ui, server)


