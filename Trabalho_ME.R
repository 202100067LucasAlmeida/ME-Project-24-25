##########################
######              ######
##### Trabalho de ME #####
######              ######
##########################

# Alunos:
#
# Lucas Almeida - 202100067
# Diana Francisco - 202100673
# Rita Pereira - 202200170

# Docente:
#
# Profª Vanda Rosado

###################
######       ######
##### 1° Fase #####
######       ######
###################

# Recolha da base de dados

# Fonte: https://archive.ics.uci.edu/dataset/320/student+performance
# Site recomendado no enunciado do trabalho.

dados <- read.csv("D:/Lucas Alexandre/IPS/ME-Project-24-25/data.csv", sep= ";", header = TRUE)
dados <- read.csv("C:/Users/diana/Desktop/IPS/LEI/2ºANO/2ºSEMESTRE/ME/PROJETO/ME-Project-24-25/data.csv", sep=";", header = TRUE)
dados <- read.csv("/Users/rita/Documents/IPS/3ºano/2º\ semestre/ME/trabalho/ME-Project-24-25/data.csv", sep=";", header = TRUE)

# Variáveis escolhidas da base de dados original 
escola <- dados$school
idade <- dados$age
estudo_semanal <- dados$studytime
aulas_extras_pagas <- dados$paid
num_faltas <- dados$absences
nota_final <- dados$G3

# Nova tabela a partir das variáveis escolhidas
estudantes <- data.frame(aluno=c(1:nrow(dados)), 
                         escola,
                         idade,
                         estudo_semanal,
                         aulas_extras_pagas,
                         num_faltas,
                         nota_final)

# Sem valores omissos.
is.na(estudantes) 

#######################
###                 ###
## Análise dos dados ##
###                 ###
#######################

# O nosso trabalho irá abordar um estudo do desempenho dos alunos em matemática 
# no ensino secundário de duas escolas portuguesas.

# População:
# Alunos do ensino secundário de duas escolas portuguesas.
#
# Unidade Estatística: Alunos


# Amostra: 395 alunos (de duas escolas diferentes)
amostra <- nrow(dados)

########################################
###                                  ###
## Variáveis estatísticas para estudo ##
###                                  ###
########################################

# Variável estatística: escola
# Descrição: Escola que o aluno frequenta.
# Dados estatísticos: "GP" - Gabriel Pereira, "MS" - Mousinho da Silveira
# Classificação: Qualitativa Nominal
estudantes$escola

# Variável estatística: idade
# Descrição: Idade do aluno.
# Dados estatísticos: de 15 a 22
# Classificação: Quantitativa Contínua
estudantes$idade

# Variável estatística: estudo_semanal
# Descrição: Número de horas de estudo semanais dos alunos.
# Dados estatísticos: 1 - <2 horas, 2 - 2 a 5 horas, 3 - 5 a 10 horas, 4 - >10 horas
# Classificação: Qualitativa Ordinal
estudantes$estudo_semanal

# Variável estatística: aulas_extras_pagas
# Descrição: Aulas extras pagas dentro da disciplina do curso.
# Dados estatísticos: sim ou não (yes ou no)
# Classificação: Qualitativa Nominal
estudantes$aulas_extras_pagas

# Variável estatística: num_faltas
# Descrição: Número de faltas escolares dos alunos.
# Dados estatísticos: de 0 a 93
# Classificação: Quantitativa Discreta
estudantes$num_faltas

# Variável estatística: nota_final
# Descrição: Nota final dos alunos.
# Dados estatísticos: de 0 a 20 
# Classificação: Quantitativa Contínua
estudantes$nota_final

###############################################################
###############################################################
###############################################################

###################
######       ######
##### 2° Fase #####
######       ######
###################

##########################
#####                #####
### Análise descritiva ###
#####                #####
##########################

# Tabelas  de frequências

# Variável: escola

ni.e <- table(estudantes$escola) # Frequência absoluta
fi.e <- prop.table(ni.e) # Frequência relativa

tabela.frequencia.escola <- data.frame(i=c(1,nrow(ni.e)),
                                       xi = names(ni.e),
                                       ni = as.integer(ni.e),
                                       fi = round(as.numeric(fi.e),4))


# Variável: estudo_semanal
ni.es <- table(estudantes$estudo_semanal) #frequência absoluta
fi.es <- prop.table(ni.es) #frequência relativa
Ni.es <- cumsum(ni.es)
Fi.es <- round(cumsum(fi.es),4)

tabela.frequencia.estudo_semanal <- data.frame(i = 1:length(ni.es),
                                               xi = c("<2h", "entre 2 e 5 h", "entre 5 e 10 h", ">10h"),
                                               ni = as.integer(ni.es),
                                               fi = round(as.numeric(fi.es), 4),
                                               Ni = as.integer(Ni.es),
                                               Fi = as.numeric(Fi.es))


# Variável: aulas_extra_pagas
ni.aep <- table(estudantes$aulas_extras_pagas) #frequência absoluta
fi.aep <- prop.table(ni.aep) #frequência relativa

tabela.frequencia.aulas_extras_pagas <- data.frame(i = 1:length(ni.aep),
                                                   xi = names(ni.aep),
                                                   ni = as.integer(ni.aep),
                                                   fi = round(as.numeric(fi.aep), 4))

# Tabelas de frequências que necessitam de criação de classes

# Variável: idade
ni.i = table(estudantes$idade)
fi.i = prop.table(ni.id)
Ni.i = cumsum(ni.id)
Fi.i = round(cumsum(fi.id), 4)

tabela.frequencia.idade <- data.frame(i=c(1:length(ni.i)),
                                   xi = names(ni.i),
                                   ni = as.integer(ni.i),
                                   fi = as.numeric(fi.i),
                                   Ni = as.integer(Ni.i),
                                   Fi = as.numeric(Fi.i))

# Variável: num_faltas

# Regra de Sturges

(k.f = trunc(1+log(amostra)/log(2))) # Número de classes -> 9
(h.f = (max(estudantes$num_faltas)-min(estudantes$num_faltas))/k.f) # Amplitude das classes
(min.classe.f = min(estudantes$num_faltas)) # Inicio da primeira classe
(max.classe.f = min.classe.f + h.f * k.f) # Fim da última classe

cortes.faltas = seq(min.classe.f, max.classe.f, by = h.f)

classes.faltas <- cut(estudantes$num_faltas,
                     breaks = cortes.faltas,
                     right = FALSE,
                     include.lowest = TRUE) 

ni.f = table(classes.faltas)
fi.f = prop.table(ni.f)
Ni.f = cumsum(ni.f)
Fi.f = cumsum(fi.f)

tabela.frequencia.num_faltas <- data.frame(i = c(1:nrow(ni.f)),
                                           classe = names(ni.f),
                                           ni = as.integer(ni.f),
                                           fi = round(as.numeric(fi.f), 4),
                                           Ni = as.integer(Ni.f),
                                           Fi = round(as.numeric(Fi.f), 4))


# Variável: nota_final

estudantes$nota_final_2 = (((dados$G1)+(dados$G2))/2) # Criamos novos valores por questões de ter variedade de dados

(min(estudantes$nota_final_2)) # Verificar o min dos dados
(max(estudantes$nota_final_2)) # Verificar o max dos dados

(k.nf = trunc(1+log(amostra)/log(2))) # Número de classes -> 9
(h.nf = (max(estudantes$nota_final_2)-min(estudantes$nota_final_2))/k.nf) # Amplitude das classes
(min.classe.nf = min(estudantes$nota_final_2)) # Inicio da primeira classe
(max.classe.nf = min.classe.nf + h.nf * k.nf) # Fim da última classe

cortes.nota.final = seq(min.classe.nf, max.classe.nf, by = h.nf)

classes.nota.final <- cut(estudantes$nota_final_2,
                   breaks = cortes.nota.final,
                   right = FALSE, 
                   include.lowest = TRUE) 

ni.n = table(classes.nota.final)
fi.n = prop.table(ni.n)
Ni.n = cumsum(ni.n)
Fi.n = cumsum(fi.n)

tabela.frequencia.nota_final <- data.frame(i = c(1:nrow(ni.n)),
                                           classe = names(ni.n),
                                           ni = as.integer(ni.n),
                                           fi = round(as.numeric(fi.n), 4),
                                           Ni = as.integer(Ni.n),
                                           Fi = round(as.numeric(Fi.n), 4))


# Gráficos

# Variável: escola
# Tipo de gráfico: circular

pie(ni.e, 
    labels=paste(ni.e),  
    col=rainbow(length(ni.e)), 
    main="Frequências absolutas das Escolas")

legend("topleft", 
       legend=c("GP - Gabriel Pereira", "MS - Mousinho da Silveira"), 
       fill=rainbow(length(ni.e)))

# Variável: estudo semanal
# Tipo de gráfico: barras

barplot(ni.es, 
        xlab="Horas", 
        ylab="Frequências absolutas", 
        main="Classificação do estudo semanal",
        col=c("red", "yellow", "blue", "green"), 
        yaxt="n")

axis(side=2, at=c(0,ni.es))

# Variável: aulas extra
# Tipo de gráfico: circular

pie(ni.aep, 
    labels=paste(ni.aep),  
    col=rainbow(length(ni.aep)), 
    main="Frequências absolutas das aulas extra pagas")

legend("topleft", 
       legend=c("Não", "Sim"), 
       fill=rainbow(length(ni.aep)))


# Variável: idade
# Tipo de gráfico: barras

barplot(ni.i, 
        xlab="Idade (em anos)", 
        ylab="Frequências absolutas", 
        main="Idade dos alunos",
        col=rainbow(ni.i), 
        yaxt="n")

axis(side=2, at=c(0,ni.i))

# Variável: número de faltas
# Tipo de gráfico: histograma

hist(x=estudantes$num_faltas, 
     breaks=cortes.faltas,
     right=TRUE, 
     include.lowest=TRUE,
     freq=TRUE, 
     main="Histograma do número de faltas",
     xlab="Número de faltas",
     ylab="frequências absolutas",
     col="red",
     ylim=c(0,300),
     xaxt="n")

axis(side = 1, at=c(0, round(cortes.faltas,1)))


# Variável: nota final
# Tipo de gráfico: histograma

hist(x=estudantes$nota_final_2, 
     breaks=cortes.nota.final,
     right=TRUE, 
     include.lowest=TRUE,
     freq=TRUE, 
     main="Histograma da nota final",
     xlab="Nota final",
     ylab="frequências absolutas",
     col="blue",
     ylim=c(0,200),
     xaxt="n")

axis(side = 1, at=c(0, round(cortes.nota.final,1)))

###############################################################
###############################################################
###############################################################

###################
######       ######
##### 3° Fase #####
######       ######
###################

#######################
# todas as variáveis

# escola              -> Qualitativa Nominal
# idade               -> Quantitativa Contínua
# estudo_semanal      -> Qualitativa Ordinal
# aulas_extras_pagas  -> Qualitativa Nominal
# num_faltas          -> Quantitativa Discreta
# nota_final          -> Quantitativa Contínua
#
#######################
#######################


# Medidas estatísticas por Variável
 
# Quantitativas
#
# Moda
(ni.i)
if(min(ni.i)==max(ni.i)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.i)[ni.i==max(ni.i)]))
}

(ni.f)
if(min(ni.f)==max(ni.f)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.f)[ni.f==max(ni.f)]))
}

(ni.n)
if(min(ni.n)==max(ni.n)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.n)[ni.n==max(ni.n)]))
}

#
# Mediana
(median(estudantes$idade))

(median(estudantes$num_faltas))

(median(estudantes$nota_final_2))

#
# Média
(mean(estudantes$idade))

(mean(estudantes$num_faltas))

(mean(estudantes$nota_final_2))

#
# Quartis
(quantile(estudantes$idade, probs = c(0.25, 0.50, 0.75)))

(quantile(estudantes$num_faltas, probs = c(0.25, 0.50, 0.75)))

(quantile(estudantes$nota_final_2, probs = c(0.25, 0.50, 0.75)))
round(cumsum(fi.n),4)

#
# Amplitude total
(max(estudantes$idade)-min(estudantes$idade))

(max(estudantes$num_faltas)-min(estudantes$num_faltas))

(max(estudantes$nota_final_2)-min(estudantes$nota_final_2))

#
# Amplitude interquartis
(IQR(estudantes$idade))

(IQR(estudantes$num_faltas))

(IQR(estudantes$nota_final_2))

#
# Variância
(var(estudantes$idade))

(var(estudantes$num_faltas))

(var(estudantes$nota_final_2))

#
# Desvio padrão
(sd(estudantes$idade))

(sd(estudantes$num_faltas))

(sd(estudantes$nota_final_2))

#
# Coeficiente de variação
(sd(estudantes$idade)/mean(estudantes$idade))*100

(sd(estudantes$num_faltas)/mean(estudantes$num_faltas))*100

(sd(estudantes$nota_final_2)/mean(estudantes$nota_final_2))*100

#
############
# Qualitativas (frequências absolutas e relativas feitas anteriormente)
#
# Moda
#
# escola
if(min(ni.e)==max(ni.e)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.e)[ni.e==max(ni.e)]))
}

# estudo semanal
if(min(ni.es)==max(ni.es)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.es)[ni.es==max(ni.es)]))
}

# aulas extra pagas
if(min(ni.aep)==max(ni.aep)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.aep)[ni.aep==max(ni.aep)]))
}


##########################
##########################
# Representação do boxplot
boxplot(estudantes$idade, col=2, main="Diagrama de extremos e quartis", horizontal=TRUE, xlab="Idade", range=0) #sem outliers

boxplot(estudantes$estudo_semanal, col=2, main="Diagrama de extremos e quartis", horizontal=TRUE, xlab="Estudo semanal", range=0) #sem outliers

boxplot(estudantes$num_faltas, col=2, main="Diagrama de extremos e quartis", horizontal=TRUE, xlab="Número de faltas", range=0) #sem outliers

boxplot(estudantes$nota_final_2, col=2, main="Diagrama de extremos e quartis", horizontal=TRUE, xlab="Nota final", range=0) #sem outliers


##########################
##########################
# Análise da simetria
#
# Coeficiente de assimetria
e1071::skewness(estudantes$idade)

e1071::skewness(estudantes$num_faltas)

e1071::skewness(estudantes$nota_final_2)


##########################
##########################
# Testes de ajustamento

# Variáveis pros testes:
#     idade (assimétrica positiva - moderado), 
#     nº faltas (assimétrica positiva - extremo), 
#     nota final (assimétrica positiva - ligeira)

# distribuições possíveis
#     idade -> normal, n>50
#     nº faltas -> exp, n>50
#     nota final -> normal, n>50

# testes

# H0: idade ~ N(16.6962,1.276043)
# H1: idade ~/~ N(16.6962,1.276043)
# n: 395
# nível de significância: a=0.05
library(nortest)
lillie.test(estudantes$idade)
# 2.2e-16<0.05 então rejeita-se H0


# H0: nºfaltas ~ P(5.708861)
# H1: nºfaltas ~/~ P(5.708861)
# n: 395
# nível de significância: a=0.05
# 0.05 então  H0
lambda <- mean(estudantes$num_faltas)
obs <- table(estudantes$num_faltas)
x_vals <- as.integer(names(obs))
esp <- dpois(x_vals, lambda) * length(estudantes$num_faltas)
grupo <- ifelse(esp < 5, "outros", as.character(x_vals))
obs_grouped <- tapply(as.numeric(obs), grupo, sum)
esp_grouped <- tapply(esp, grupo, sum)
chisq.test(x = obs_grouped, p = esp_grouped / sum(esp_grouped))
#conclusão: rejeita se H0


# H0: nota ~ N(10.81139,3.407479)
# H1: nota ~/~ N(10.81139,3.407479)
# n: 395
# nível de significância: a=0.05
lillie.test(estudantes$nota_final_2)
# 0.001671<0.05 então rejeita-se H0


# H0: nota ~ U(0,20)
# H1: nota ~/~ U(0,20)
# n: 395
# nível de significância: a=0.05
# 0.05 então  H0
n <- nrow(estudantes)
obs_nf <- tabela.frequencia.nota_final$ni
esp_nf <- rep(n / length(obs_nf), length(obs_nf))
chisq.test(x = obs_nf, p = esp_nf / sum(esp_nf))
#conclusão: rejeita se H0


##########################
##########################
# Testes de independência




