suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))

setwd("C:/Users/mathe/Downloads/Projetos/Safrashist")

# Producao

clean_data <- function(x){
  df = c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS",
         "MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
  colnames(x) <- x[1,]
  colnames(x)[1] <- "UF"
  x <- x[-1,]
  x <- x[match(df, x$UF), ]
  x <- x %>% gather("ano", "rend", -UF)
  return(x)
}

algodao <- clean_data(read_excel("algodao.xls", 
                                 sheet = 5, 
                                 col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(algodao)),nrow(algodao)))
algodao <- cbind(algodao,name)

amendoim <- clean_data(read_excel("amendoim.xls", 
                                  sheet = 3, 
                                  col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(amendoim)),nrow(amendoim)))
amendoim <- cbind(amendoim,name)
data_producao = rbind(algodao,amendoim)

arroz_irrigado <- clean_data(read_excel("arroz_irrigado.xls", 
                                        sheet = 3, 
                                        col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(arroz_irrigado)),nrow(arroz_irrigado)))
arroz_irrigado <- cbind(arroz_irrigado,name)
data_producao = rbind(data_producao,arroz_irrigado)

arroz_sequeiro <- clean_data(read_excel("arroz_sequeiro.xls", 
                                        sheet = 3, 
                                        col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(arroz_sequeiro)),nrow(arroz_sequeiro)))
arroz_sequeiro <- cbind(arroz_sequeiro,name)
data_producao = rbind(data_producao,arroz_sequeiro)

canola <- clean_data(read_excel("canola.xls", 
                                sheet = 3, 
                                col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(canola)),nrow(canola)))
canola <- cbind(canola,name)
data_producao = rbind(data_producao,canola)

centeio <- clean_data(read_excel("centeio.xls", 
                                 sheet = 3, 
                                 col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(centeio)),nrow(centeio)))
centeio <- cbind(centeio,name)
data_producao = rbind(data_producao,centeio)

cevada <- clean_data(read_excel("cevada.xls", 
                                sheet = 3, 
                                col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(cevada)),nrow(cevada)))
cevada <- cbind(cevada,name)
data_producao = rbind(data_producao,cevada)

feijao <- clean_data(read_excel("feijao.xls", 
                                sheet = 3, 
                                col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(feijao)),nrow(feijao)))
feijao <- cbind(feijao,name)
data_producao = rbind(data_producao,feijao)

mamona <- clean_data(read_excel("mamona.xls", 
                                sheet = 3, 
                                col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(mamona)),nrow(mamona)))
mamona <- cbind(mamona,name)
data_producao = rbind(data_producao,mamona)

milho <- clean_data(read_excel("milho.xls", 
                               sheet = 3, 
                               col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(milho)),nrow(milho)))
milho <- cbind(milho,name)
data_producao = rbind(data_producao,milho)

soja <- clean_data(read_excel("soja.xls", 
                              sheet = 3, 
                              col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(soja)),nrow(soja)))
soja <- cbind(soja,name)
data_producao = rbind(data_producao,soja)

sorgo <- clean_data(read_excel("sorgo.xls", 
                               sheet = 3, 
                               col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(sorgo)),nrow(sorgo)))
sorgo <- cbind(sorgo,name)
data_producao = rbind(data_producao,sorgo)

trigo <- clean_data(read_excel("trigo.xls", 
                               sheet = 3, 
                               col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(trigo)),nrow(trigo)))
trigo <- cbind(trigo,name)
data_producao = rbind(data_producao,trigo)

triticale <- clean_data(read_excel("triticale.xls", 
                                   sheet = 3, 
                                   col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(triticale)),nrow(triticale)))
triticale <- cbind(triticale,name)
data_producao = rbind(data_producao,triticale)

cafe_arbica <- clean_data(read_excel("cafe_arbica.xls", 
                                     sheet = 4, 
                                     col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(cafe_arbica)),nrow(cafe_arbica)))
cafe_arbica <- cbind(cafe_arbica,name)
data_producao = rbind(data_producao,cafe_arbica)

cafe_conilon <- clean_data(read_excel("cafe_conilon.xls", 
                                      sheet = 4, 
                                      col_names = FALSE, na = "-", skip = 5))
cafe_conilon <- na.exclude(cafe_conilon)
name <- c(rep(deparse(substitute(cafe_conilon)),nrow(cafe_conilon)))
cafe_conilon <- cbind(cafe_conilon,name)
data_producao = rbind(data_producao,cafe_conilon)

data_producao <- data_producao %>% filter(UF != is.na(UF))

write.csv(data_producao,"data_producao.csv",fileEncoding = "UTF-8")


#############################################################################################


# Area

algodao <- clean_data(read_excel("algodao.xls", 
                                 sheet = 1, 
                                 col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(algodao)),nrow(algodao)))
algodao <- cbind(algodao,name)

amendoim <- clean_data(read_excel("amendoim.xls", 
                                  sheet = 1, 
                                  col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(amendoim)),nrow(amendoim)))
amendoim <- cbind(amendoim,name)
data_area = rbind(algodao,amendoim)

arroz_irrigado <- clean_data(read_excel("arroz_irrigado.xls", 
                                        sheet = 1, 
                                        col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(arroz_irrigado)),nrow(arroz_irrigado)))
arroz_irrigado <- cbind(arroz_irrigado,name)
data_area = rbind(data_area,arroz_irrigado)

arroz_sequeiro <- clean_data(read_excel("arroz_sequeiro.xls", 
                                        sheet = 1, 
                                        col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(arroz_sequeiro)),nrow(arroz_sequeiro)))
arroz_sequeiro <- cbind(arroz_sequeiro,name)
data_area = rbind(data_area,arroz_sequeiro)

canola <- clean_data(read_excel("canola.xls", 
                                sheet = 1, 
                                col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(canola)),nrow(canola)))
canola <- cbind(canola,name)
data_area = rbind(data_area,canola)

centeio <- clean_data(read_excel("centeio.xls", 
                                 sheet = 1, 
                                 col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(centeio)),nrow(centeio)))
centeio <- cbind(centeio,name)
data_area = rbind(data_area,centeio)

cevada <- clean_data(read_excel("cevada.xls", 
                                sheet = 1, 
                                col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(cevada)),nrow(cevada)))
cevada <- cbind(cevada,name)
data_area = rbind(data_area,cevada)

feijao <- clean_data(read_excel("feijao.xls", 
                                sheet = 1, 
                                col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(feijao)),nrow(feijao)))
feijao <- cbind(feijao,name)
data_area = rbind(data_area,feijao)

mamona <- clean_data(read_excel("mamona.xls", 
                                sheet = 1, 
                                col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(mamona)),nrow(mamona)))
mamona <- cbind(mamona,name)
data_area = rbind(data_area,mamona)

milho <- clean_data(read_excel("milho.xls", 
                               sheet = 1, 
                               col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(milho)),nrow(milho)))
milho <- cbind(milho,name)
data_area = rbind(data_area,milho)

soja <- clean_data(read_excel("soja.xls", 
                              sheet = 1, 
                              col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(soja)),nrow(soja)))
soja <- cbind(soja,name)
data_area = rbind(data_area,soja)

sorgo <- clean_data(read_excel("sorgo.xls", 
                               sheet = 1, 
                               col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(sorgo)),nrow(sorgo)))
sorgo <- cbind(sorgo,name)
data_area = rbind(data_area,sorgo)

trigo <- clean_data(read_excel("trigo.xls", 
                               sheet = 1, 
                               col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(trigo)),nrow(trigo)))
trigo <- cbind(trigo,name)
data_area = rbind(data_area,trigo)

triticale <- clean_data(read_excel("triticale.xls", 
                                   sheet = 1, 
                                   col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(triticale)),nrow(triticale)))
triticale <- cbind(triticale,name)
data_area = rbind(data_area,triticale)

cafe_arbica <- clean_data(read_excel("cafe_arbica.xls", 
                                     sheet = 1, 
                                     col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(cafe_arbica)),nrow(cafe_arbica)))
cafe_arbica <- cbind(cafe_arbica,name)
data_area = rbind(data_area,cafe_arbica)

cafe_conilon <- clean_data(read_excel("cafe_conilon.xls", 
                                      sheet = 1, 
                                      col_names = FALSE, na = "-", skip = 5))
cafe_conilon <- na.exclude(cafe_conilon)
name <- c(rep(deparse(substitute(cafe_conilon)),nrow(cafe_conilon)))
cafe_conilon <- cbind(cafe_conilon,name)
data_area = rbind(data_area,cafe_conilon)

data_area <- data_area %>% filter(UF != is.na(UF))

write.csv(data_area,"data_area.csv",fileEncoding = "UTF-8")


#############################################################################################


# Brasil produto

producao <- clean_data(read_excel("BrasilUFSerieHist.xls", 
                                 sheet = 3, 
                                 col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(producao)),nrow(producao)))
produto_producao <- cbind(producao,name)


area <- clean_data(read_excel("BrasilUFSerieHist.xls", 
                                  sheet = 1, 
                                  col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(area)),nrow(area)))
produto_area <- cbind(area,name)
produto = rbind(produto_producao,produto_area)

produtividade <- clean_data(read_excel("BrasilUFSerieHist.xls", 
                              sheet = 2, 
                              col_names = FALSE, na = "-", skip = 5))
name <- c(rep(deparse(substitute(produtividade)),nrow(produtividade)))
produto_produtiv <- cbind(produtividade,name)
produto = rbind(produto,produto_produtiv)

write.csv(produto,"produto.csv",fileEncoding = "UTF-8")


#############################################################################################

# Brasil Grão

producao <- read_excel("BrasilProdutoSerieHist.xls", 
                                  sheet = 3, 
                                  col_names = FALSE, na = "-", skip = 5)
colnames(producao) <- producao[1,]
producao <- producao[-1,]
producao <- na.exclude(producao %>% gather("ano", "rend", -PRODUTO))
name <- c(rep(deparse(substitute(producao)),nrow(producao)))
grao_producao <- cbind(producao,name)


area <- read_excel("BrasilProdutoSerieHist.xls", 
                   sheet = 1, 
                   col_names = FALSE, na = "-", skip = 5)
colnames(area) <- area[1,]
area <- area[-1,]
area <- na.exclude(area %>% gather("ano", "rend", -PRODUTO))
name <- c(rep(deparse(substitute(area)),nrow(area)))
grao_area <- cbind(area,name)
grao = rbind(grao_producao,grao_area)

produtividade <- read_excel("BrasilProdutoSerieHist.xls", 
                            sheet = 2, 
                            col_names = FALSE, na = "-", skip = 5)
colnames(produtividade) <- produtividade[1,]
produtividade <- produtividade[-1,]
produtividade <- na.exclude(produtividade %>% gather("ano", "rend", -PRODUTO))
name <- c(rep(deparse(substitute(produtividade)),nrow(produtividade)))
grao_produtividade <- cbind(produtividade,name)

grao = rbind(grao,grao_produtiv)

write.csv(grao,"grao.csv",fileEncoding = "UTF-8")

