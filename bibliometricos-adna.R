# Analisis de datos para el trabajo "Asimetrias globales en la produccion de conocimientos de los estudios de ADN antiguo en humanos"
# Russo M.G., Di Fabio Rocca F., Arencibia V., Bettera Marcat G., Dejean C.B., Avena S., Seldes V.

pkgs <- c("bibliometrix", "ggplot2", "ggpubr", "dplyr")
lapply(pkgs, library, character.only = T)

rm(list=ls())

# CONJUNTO DE DATOS: ARTICULOS DE ADN ANTIGUO EN HUMANOS
# Busqueda en Pubmed
# (((("ancient DNA"[Title/Abstract]) OR ("ancient genome"[Title/Abstract])) 
# OR ("ancient mitogenome"[Title/Abstract])) OR ("ancient mitochondrial DNA"[Title/Abstract])) 
# OR ("ancient human DNA"[Title/Abstract] OR "ADN antiguo"[Title/Abstract]) 
# OR ("genoma antiguo"[Title/Abstract])) OR ("mitogenoma antiguo"[Title/Abstract])) 
# OR ("ADN mitocondrial antiguo"[Title/Abstract])) OR ("ADN humano antiguo"[Title/Abstract]) 
# AND (Humans[Filter])

# Analisis descriptivo ####
#conversion del archivo a dataframe
filehum <- convert2df(file = "hum-ancientDNA-dataset.txt", dbsource = 'pubmed', format = "pubmed")

descripthum <- biblioAnalysis(filehum, sep = ";") #creacion de objeto bibliometrix

Shum <- summary(object = descripthum, k = 10, pause = FALSE)
plot(x = descripthum, k = 15, pause = FALSE)

# Grafico de articulos por anio ####
artxaniohum <- as.data.frame(Shum$AnnualProduction)
#modificacion de las fechas de publicacion en 8 articulos (2 de 2023 y 5 de 2024)
artxaniohum_modif <- as_tibble(artxaniohum)
artxaniohum_modif <- artxaniohum_modif %>% 
  filter(!(`Year   ` %in% c(2023, 2024, 2025)))
artxaniohum_modif <- artxaniohum_modif %>%
  add_row(`Year   ` = "2023", Articles = 104)
artxaniohum_modif <- artxaniohum_modif %>%
  add_row(`Year   ` = "2024", Articles = 102)

attach(artxaniohum_modif)
ggplot2::ggplot(artxaniohum_modif, aes(x = `Year   `, y = Articles)) + 
  geom_bar(stat = "identity", fill = "#009eb2ff") +
  theme_classic() + xlab("Año") + ylab("Artículos") +
  theme(axis.text.x = element_text(angle = 90))

# Grafico de articulos por revista ####
df_revistas <- as.data.frame(descripthum$Sources)

#modificacion de nombres para graficar y juntar publicaciones de Am.J.Phys.Anthropol. con Am.J.Biol.Anthropol.
#write.csv(df_revistas, file = "articulosxrevistas.csv")
df_20revistas <- df_revistas[1:20,] 
df_20revistas <- df_20revistas %>% 
  filter(!(SO %in% "AMERICAN JOURNAL OF PHYSICAL ANTHROPOLOGY"))
df_20revistas <- df_20revistas %>% 
  mutate(Revistas = c("PLOS ONE", "PROC. NATL. ACAD. SCI. USA", "CURRENT BIOLOGY", "SCIENCE", "SCIENTIFIC REPORTS", "NATURE", "MOL. BIOL. EVOL.", "GENES", "ANTHROPOLOGISCHER ANZEIGER", "HUMAN BIOLOGY", "CELL", "FORENSIC SCI. INT. GEN.", "NATURE COMMUNICATIONS", "NUCLEIC ACIDS RESEARCH", "PHILOS. TRANS. R. SOC. LOND. B BIOL. SCI.", "PROC. BIOL. SCI.", "ANNALS OF HUMAN BIOLOGY", "METHODS IN MOLECULAR BIOLOGY", "AM. J. HUM. GENET."))
df_20revistas <- df_20revistas %>% 
  add_row(SO = "AMERICAN JOURNAL OF PHYSICAL ANTHROPOLOGY", Freq = 92, Revistas = "AM. J. PHYS. ANTHROPOL.", .before = 1)

x_nombres <- df_20revistas$Revistas # vector para ordenar las barras con limits
ggplot2::ggplot(df_20revistas, aes(Revistas, Freq)) + 
  geom_bar(stat = "identity", fill = "#e69f00ff") +
  theme_classic() +
  scale_x_discrete(name = "Revistas", limits = x_nombres) + ylab("Artículos") +
  theme(axis.text.x = element_text(size = 10, vjust = 1, hjust = 1, angle = 70))

# Grafico 15 autores mas productivos a lo largo del tiempo ####
topAUhum <- authorProdOverTime(filehum, k = 15, graph = TRUE) #TC es la tasa de citacion que no esta disponible en el formato PubMed

#% del total de publicaciones que tienen los 15 primeros autores
df_autores <- as.data.frame(descripthum$Authors)
sum(df_autores$Freq[1:15])/descripthum$Articles*100
#write.csv(df_autores, file="df_autores_hum.csv")

# Grafico para paises Latinoamerica ####
plot(x = descripthum, k = 56, pause = FALSE) #grafico total de paises

df_prod_countryhum <- descripthum$CountryCollaboration
df_country_latam <- df_prod_countryhum %>% 
  filter(Country %in% 
           c("ARGENTINA", "BRAZIL", "MEXICO", "COLOMBIA", "CHILE", "URUGUAY", "ECUADOR"))
#armado del df para graficar
Country <- c("ARGENTINA","ARGENTINA","BRAZIL","BRAZIL","MEXICO","MEXICO","COLOMBIA","COLOMBIA","CHILE","CHILE","URUGUAY","URUGUAY","ECUADOR","ECUADOR")
Colab <- c("UP", "MP", "UP", "MP", "UP", "MP", "UP", "MP", "UP", "MP", "UP", "MP","UP", "MP")
Documents <- c(7,5,8,2,3,4,2,1,1,1,2,0,0,1)
paises_latam <- tibble(Country, Colab, Documents)

ggplot(paises_latam, aes(fill=Colab, y=Documents, x=Country)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits = c("ECUADOR", "CHILE", "URUGUAY", "COLOMBIA", "MEXICO", "BRAZIL", "ARGENTINA")) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12)) +  
  coord_flip() +
  #theme_linedraw() + ylab("Artículos") + xlab("Países")
  theme_classic2() + ylab("Artículos") + xlab("Países")

sum(paises_latam$Documents) #nro. de articulos con autores de correspondencia latinoamericanos

# Red de colaboracion entre paises ####
Mhum <- metaTagExtraction(filehum, Field = "AU_CO", sep = ";")

#traduccion de nombres para visualizar nodos
write.csv(Mhum$AU_CO, "AU_CO.csv") #archivo para traducir manualmente
AU_CO_es <- read.csv("AU_CO_es.csv") #archivo traducido
Mhum_es <- Mhum
Mhum_es$AU_CO <- AU_CO_es$x

#obtencion de la red
NetMatrixhum <- biblioNetwork(Mhum_es, analysis = "collaboration", network = "countries", sep = ";")

#grafico de la red
nethum <- networkPlot(NetMatrixhum, n = dim(NetMatrixhum)[1], type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")
net2VOSviewer(nethum, vos.path = 'C:/Users/-/Programas/VOSviewer') #path a VOSviewer.jar

