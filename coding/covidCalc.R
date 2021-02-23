covid_19_bauru_mortes <- read.csv("./dados/covid_19_bauru_mortes.csv", 
                                  sep=";", encoding = "UTF-8")
head(covid_19_bauru_mortes)

covid_19_bauru_casos_geral <- read.csv("./dados/covid_19_bauru_casos_geral.csv",
                                       sep=";")
head(covid_19_bauru_casos_geral)

if (!require(readxl)) install.packages("readxl")

df.mortes <- read_excel("./dados/covid_19_bauru_mortes.xlsx")
head(df.mortes) 

df.geral <- read_excel("./dados/covid_19_bauru_casos_geral.xlsx")
head(df.geral) 

temp.sexo <- df.mortes$sexo[!is.na(df.mortes$sexo)]
table(temp.sexo)

unname(table(temp.sexo))

unname(table(temp.sexo))[1]

unname(table(temp.sexo))[2]

sum(unname(table(temp.sexo)))

pct.sexo = round(unname(table(temp.sexo)) / sum(unname(table(temp.sexo)))*100,1)
pct.sexo

pct.sexo = round(unname(table(temp.sexo)) / sum(unname(table(temp.sexo)))*100,0)
pct.sexo

pct.sexo = paste0(pct.sexo, "%")
pct.sexo


cbind("fr" = addmargins(prop.table(table(temp.sexo))))

graph.sex <- barplot(table(temp.sexo), main = "Grfico 1. Gnero",
                     ylab = "Nmero de bitos", ylim = c(0,sum(unname(table(temp.sexo)))),
                     #legend.text = pct.sexo,
                     names.arg = c("Feminino", "Masculino"))
text(x = graph.sex, y = table(temp.sexo), label = pct.sexo, pos = 3, cex = 1.25, col = "purple")
axis(1, at=graph.sex, labels=table(temp.sexo), tick=F, las=1, line=-4.5, cex.axis=1.25)

temp.hosp <- df.mortes$tipo_hosp[!is.na(df.mortes$tipo_hosp)]
temp.hosp
pct.hosp = round(unname(table(temp.hosp)) / sum(unname(table(temp.hosp)))*100,0)
pct.hosp

pct.hosp = paste0(pct.hosp, "%")
pct.hosp


graph.hosp<- barplot(table(temp.hosp), main = "Grfico 2. Tipo de Hospitalizao",
                     ylab = "Nmero de bitos", ylim = c(0,sum(unname(table(temp.hosp)))),
                     #legend.text = pct.sexo,
                     names.arg = c("Privado", "Pblico"))
text(x = graph.hosp, y = table(temp.hosp), label = pct.hosp, pos = 3, cex = 1.25, col = "purple")
axis(1, at=graph.hosp, labels=table(temp.hosp), tick=F, las=1, line=-4.5, cex.axis=1.25)

graph.hosp.pie<-pie(table(temp.hosp), main = "Grfico 3. Tipo de Hospitalizao",
                    edges = 200, radius = 1.0,
                    clockwise = T, 
                    density = NULL, angle = 90, col = NULL)

temp.idade <- df.mortes$idade[!is.na(df.mortes$idade)]

idade.tb <- table(temp.idade)
cbind("f" = idade.tb)

cbind("f" = addmargins(idade.tb))

maior.freq.idade = max(idade.tb)
maior.freq.idade

round(cbind("fr" = prop.table(idade.tb))*100,1)

cbind("fr" = addmargins(prop.table(idade.tb)))

summary(temp.idade)

boxplot(temp.idade, horizontal=T, xlab="Faixa etria", main = "Grfico 4. Distribuio dos bitos por idade")

indice.1<-which.max(idade.tb > 5)
indice_2<-which.max(idade.tb > 10)
idade.tb[indice.1:indice_2]

sum(idade.tb[indice.1:indice_2])


pct.idade<-round(idade.tb / sum(idade.tb)*100,1)
pct.idade


graph.age <- barplot(table(temp.idade), 
                     main = "Grfico 5. Faixa etria dos bitos",
                     ylab = "Nmero de bitos",
                     xlab = "Idade dos pacientes",
                     ylim = c(0,maior.freq.idade + 5))
