# Análise de Dados Entregas

## Selecionando o working directory
setwd("~/Documents/data-science/husky")

## Libraries
library(data.table)
library(dplyr)
library(lubridate)
library(leaflet)
library(rgdal)
library(tidyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)


## Importando, limpando e formatando dados

### Importar data.table com encoding Latin-1 para interpretar acentos e cedilhas,
### e usar funcao clean_names para simplificar nomes em 'lowerCamel'.
dados <- fread("rest_order.csv",encoding="UTF-8",drop = c(1,2,4,5,9,14,15),
            na.strings = c("NULL","","0000-00-00 00:00:00"))
names(dados) <- c("idRestaurante","endereco","latitude","longitude","bairro",
                  "valorACobrar","valorRecolhidoCliente","taxaCartao","formaDePagamento",
                  "entregaCriada","entregaAgendada","entregaAceita","coletaIniciada",
                  "coletaFinalizada","chegadaNoCliente","entregaFinalizada","status",
                  "idCourier","operador","precoEntrega")


### simplificando nomes bairros, operador, formaDePagamento

#### Corrigindo os dados de bairro de linhas que continham valores incorretos. NAs do "bairro" cairam de 287 para 21.

narows <- dados[,which(bairro %in% c("Porto Alegre","Brazil","Brasil","Rio Grande do Sul")| is.na(bairro))]
nabairro <- dados %>%  
  filter(bairro %in% c("Porto Alegre","Brazil","Brasil","Rio Grande do Sul")| is.na(bairro)) %>%
  dplyr::select(endereco) %>%
  separate(endereco,into=c("end1","end2"),sep=c("-")) %>% 
  separate(end2,into=c("end3","end4"),sep=",") %>% 
  data.table() %>%
  set(j=c("end1","end4"),value = NULL)
set(x = dados, i = narows, j="bairro", value = nabairro)

##### Se livrando da variavel "endereco", usada apenas nas linhas acima
set(x = dados,j = "endereco",value = NULL)

#### Corrigindo variacoes nos nomes  dos bairros

bairros <- list(
  `Centro Histórico` = "Centro$|CENTRO$|• Centro Histórico|Centro Hist$|Centro/Azenha|Cais do Porto|Avenida Borges de Medeiros|Riachuelo|Rua Demétrio Ribeiro|Rua dos Andradas|Rua Duque de Caxias|Rua Siqueira Campos|Rua Voluntários da Pátria|Praça Osvaldo Cruz|Praça dos Açorianos",
  `Bom Fim` = "Bom Fim|Bonfim",
  Farroupilha = "Farropilha",
  Glória = "Glória|Gloria|Gl$",
  `Cel. Aparicio Borges` = "Coronel Aparicio Borges",
  Independência = "Independ$|Avenida Independência",
  `Jardim Botânico` = "Jardim Botanico|Jardim Bot$|Jardom Botanico",
  `Jardim Sabará` = "^Jardim Itu-Sabará$",
  `Jardim Lindóia` = "^Jardim Lindoia",
  `Santa Cecília` = "Santa Cecilia",
  `Santo Antônio` = "Santo Antonio",
  Partenon = "Partenon|Paternon",
  Petrópolis = "Petropolis|Petr$",
  `Menino Deus` = "Menino Deus|Marcilio Dias|Avenida Padre Cacique|Getúlio Vargas",
  `Montserrat` = "^Mont'Serrat",
  `Passo da Areia` = "Passo D' Areia",
  `Santa Tereza` = "Santa Teresa",
  `Praia de Belas` = "PR Belas",
  `Teresópolis` = "Teresopolis|^Alto Teresópolis$|^Alto Teresopolis$",
  Higienópolis = "Higien$",
  `Mário Quintana` = "Protásio Alves|^Alto Petropolis$|^Alto Petrópolis$" 
)
for (i in 1:length(bairros)){
  
  dados$bairro <- gsub(bairros[[i]],names(bairros)[i],dados$bairro)
}

##### Limpando excesso de white space remanescentes
dados$bairro <- trimws(dados$bairro,"both")

##### Forçando valores sem sentido da variavel "bairros" para NA
dumpbairro <- which(dados$bairro %in% c("Parque Amador","Paraíso","an 20","RS",
                                        "Acesso Oeste","201 C","2º Andar","Maringá"))
set(x=dados,i=dumpbairro,j="bairro",value=NA)
dados$bairro <- toupper(dados$bairro)
rm(i,bairros,nabairro,narows,dumpbairro)


#### Corrigindo dados da coluna 'operador'

##### Simplificando Rapiddo & RAPIDDO
set(x = dados, 
    i = which(dados$operador %in% c("RAPIDDO","Motoboy")| dados$idCourier == 16), 
    j = "operador", value = "Rapiddo")

##### Husky & HUSKY
set(x = dados, 
    i = which(dados$operador == "HUSKY"),
    j = "operador", value = "Husky")
#unique(dados$operador)


#### Transformando dados da coluna "status" em fatores, com significado
dados$status <- factor(dados$status,
                          labels = c("Aceito","Coleta","Finalizado","Cancelado"))
## Legenda da coluna status:
# 1. Pedido criado
# 2. Pedido aceito
# 3. Pedido sendo coletado
# 4. Pedido sendo entregue
# 5. Pedido no cliente
# 6. Pedido finalizado
# 7. Pedido cancelado


### Transformando variaveis em fator, numerico e data

#### Fator: restId,bairro,formaPag,status,motoId,operador
fac.nome <- c('idRestaurante','bairro','formaDePagamento','idCourier','operador')
dados[, (fac.nome) := lapply(.SD, as.factor), .SDcols = fac.nome]


#### Numerico: latitude, longitude, valCobrar, valRecol, taxaCard,precoEnt
num.nome <- c('latitude','longitude','valorACobrar','valorRecolhidoCliente',
              'taxaCartao','precoEntrega')
dados[, (num.nome) := lapply(.SD, as.numeric,na.rm=T), .SDcols = num.nome]
dados[valorACobrar < 0, valorACobrar := NA]


#### Data: entCriada, entAgend, entAceit, colIni, colFin, chegada, entFin 
data.nome <- c('entregaCriada','entregaAgendada','entregaAceita','coletaIniciada',
               'coletaFinalizada','chegadaNoCliente','entregaFinalizada')
dados[, (data.nome) := lapply(.SD, mdy_hm), .SDcols = data.nome]
rm(fac.nome,num.nome,data.nome)

##### Variaveis das datas
meses <- month(dados[!is.na(entregaAceita),entregaAceita], label = T)
meses16 <- month(dados[year(entregaAceita) == 2016 & !is.na(entregaAceita),entregaAceita], label = T)
meses17 <- month(dados[year(entregaAceita) == 2017 & !is.na(entregaAceita),entregaAceita], label = T)
meses18 <- month(dados[year(entregaAceita) == 2018 & !is.na(entregaAceita),entregaAceita], label = T)
semdia <- wday(dados[!is.na(entregaAceita),entregaAceita], label = T)
hora <- hour(dados[!is.na(entregaAceita),entregaAceita])


## DATA VIS

## Plotando mapa
poa <- readOGR('/mnt/data/data-science/husky/shapefiles/Bairros_LC12112_16.shp',verbose = F)
poat <- spTransform(poa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
dados$bairro <- ordered(dados$bairro, levels = dados[,.N,by=bairro][order(N)][[1]])
poapopup <- paste0("<b>",poat$NOME,"</b>")
poacor <-  colorFactor(colorRampPalette(brewer.pal(9,"Blues"),interpolate = "spline",bias=2)(length(levels(dados$bairro))), 
                       reverse=F,ordered = T, na.color = '#e0e0e0',
                       domain = dados$bairro,levels=dados[,.N,by=bairro][order(N)][[1]])
leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -51.228735, lat=-30.027704,zoom=12) %>%
  addPolygons(data=poat,stroke=T,weight=1,color="white", popup = poapopup,
              fillOpacity=0.9, dashArray = "3", smoothFactor = 0.5,
              fillColor = ~poacor(poat$NOME), 
              highlight = highlightOptions(
                weight = "2",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = T)
  ) %>%
  addLegend(position = 'bottomright', colors = brewer.pal(9,"Blues"), opacity = 1,
            labels = c('0','1','5','10','20','40','50','75','+100'),
            title = "Pedidos")


### Visualizações sobre número de pedidos

##### Histograma do Número Total de Pedidos/Mês
plot_ly(x = meses18, type = 'histogram', name="2018") %>%
  add_histogram(meses17, name="2017") %>%
  add_histogram(meses16, name = "2016") %>%
  layout(barmode = 'overlay',title = "Histograma do Número Total de Pedidos/Mês")

# pedidos/hora facet histograms
dtp <- data.frame(hora = hora, meses =meses)
ggplot(dtp) +
  scale_fill_distiller(type = 'div',palette = "BuPu",direction = 1,limits=c(-200,390)) +
  geom_bar(mapping = aes(x=hora,fill=..count..),alpha=1) +
  ggtitle("Pedidos por Hora", subtitle = "Mês a mês") +
  scale_x_discrete("Hora",limits = c(10,12,14,16,18,20,22)) +
  ylab("") +
  facet_wrap(~meses)

# pedidos/diasem facet histogram
dts <- data.frame(semdia = semdia, meses =meses)
ggplot(dts) +
  scale_fill_distiller(type = 'seq',palette = 'YlOrRd',direction = 1) +
  geom_bar(mapping = aes(x=semdia,fill=..count..),alpha=0.9) +
  ggtitle("Pedidos por Dia da Semana", subtitle = "Mês a mês") +
  xlab("Dias da Semana") +
  ylab("") +
  facet_wrap(~meses) 

##### Total de pedidos ao longo do tempo, ano a ano.
ggplot(dados,aes(x=entregaAceita)) + 
  geom_histogram(data = subset(dados,year(dados$entregaAceita) == 2016),
                 fill = "red",alpha=0.5,bins = 25) +
  geom_histogram(data = subset(dados,year(dados$entregaAceita) == 2017),
                 fill = "blue",alpha=0.5,bins = 25) +
geom_histogram(data = subset(dados,year(dados$entregaAceita) == 2018),
               fill = "green",alpha=0.5,bins = 25)

#### Gráfico dos Pedidos por dia da Semana (Mensal, 2016-2018)
mat <- as.matrix(table(meses,semdia))
# Gráfico dos Pedidos por dia da Semana (Mensal, 2016-2018)
mat <- as.matrix(table(meses,semdia))
plot_ly(x = colnames(mat),
        y = rownames(mat),
        z = mat,
        type = "heatmap",
        colors = rev(brewer.pal(11,"Spectral"))) %>%
  layout(title = "Pedidos por dia da Semana (Mensal, 2016-2018)")

plot_ly(x = colnames(mat),
        y = rownames(mat),
        z = mat,
        colors = rev(brewer.pal(11,"Spectral"))) %>%
  add_surface()

#### Gráfico dos Pedidos a cada hora (Mês, 2016-2018)
mat2 <- as.matrix(table(hora,meses))
# Gráfico dos Pedidos a cada hora (Mês, 2016-2018)
mat2 <- as.matrix(table(hora,meses))
plot_ly(x = colnames(mat2),
        y = rownames(mat2),
        z = mat2,
        type = "heatmap",
        colors = rev(brewer.pal(11,"Spectral"))) %>%
  layout(title = "Número de Pedidos a cada hora (Mês, 2016-2018)")

plot_ly(x = colnames(mat2),
        y = rownames(mat2),
        z = mat2,
        colors = rev(brewer.pal(11,"Spectral"))) %>%
  add_surface()

## Métricas tempoColeta, tempoEntrega 

##### Definindo variaveis tempoColeta e tempoEntrega
dados[, ':='(tempoColeta = as.numeric(coletaFinalizada - entregaAgendada),
             tempoEntrega = as.numeric(entregaFinalizada - coletaFinalizada),
             tempoTotal = as.numeric(entregaFinalizada - entregaAgendada))]

boys <- dados[tempoColeta>0 & !is.na(tempoColeta) & tempoColeta<(43200) & 
                tempoEntrega>0 & !is.na(tempoEntrega) & tempoEntrega<(43200) &
                tempoTotal>0 & !is.na(tempoTotal) & tempoTotal<(43200) & !is.na(idCourier),
              .(mediaColeta = mean(tempoColeta),
                mediaEntrega = mean(tempoEntrega),
                mediaTotal = mean(tempoTotal)),by=idCourier]

boys[,':='(Coleta = round((mediaColeta - mean(mediaColeta))/sd(mediaColeta),2),
           Entrega = round((mediaEntrega - mean(mediaEntrega))/sd(mediaEntrega),2),
           Total = round((mediaTotal - mean(mediaTotal))/sd(mediaTotal),2))]             
boys <- gather(boys,key="tipo",value="valor",c(Coleta,Entrega,Total)) %>% as.data.table()
boys[,sinal := ifelse(valor>0,"Acima","Abaixo")]

# Gráfico divergente  do tempo de Coleta, Entrega e Total, normalizados.
boys <- boys %>% ungroup() %>% arrange(tipo,desc(valor)) %>% mutate(ordem = row_number())
ggplot(boys, aes(x=ordem,y=valor, fill = sinal)) + 
  geom_bar(stat="identity",show.legend = F) + 
  facet_wrap(.~tipo,scales="free") +
  ggtitle("Tempo por Courier (Normalizado)") +
  xlab("Id do Courier") +
  ylab("Desvios em relação à Média") +
  scale_x_continuous(
    breaks = boys$ordem,
    labels = boys$idCourier,
    expand = c(0,0)) + coord_flip() 