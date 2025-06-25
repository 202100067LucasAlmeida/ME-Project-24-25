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
fi.i = prop.table(ni.i)
Ni.i = cumsum(ni.i)
Fi.i = round(cumsum(fi.i), 4)

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

#################################
#########                 #######
##### 3° Fase - refactoring #####
#########                 #######
#################################

#######################
# todas as variáveis

# escola              -> Qualitativa Nominal
# estudo_semanal      -> Qualitativa Ordinal
# aulas_extras_pagas  -> Qualitativa Nominal

#
#######################
#######################

# DEFINIR AS HIPÓTESES

# nº de faltas - Quantitativa Discreta
# H0: o nº de faltas segue uma distribuíção Binomial Negativa
# H1: o nº de faltas NÃO segue uma distribuíção Binomial Negativa


# nota_final - Quantitativa Contínua
# H0: a nota final segue uma distribuíção Normal
# H1: a nota final NÃO segue uma distribuíção Normal


# idade - Quantitativa Contínua
# H0: a idade segue uma distribuíção Qui-Quadrado
# H1: a idade NÃO segue uma distribuíção Qui-Quadrado


#######################
#######################

# ESTATISTICA DE TESTE (α=0.05)
nrow(estudantes)

# nº de faltas - qui-quadrado
media_faltas <- mean(estudantes$num_faltas)
variancia_faltas <- var(estudantes$num_faltas)

p_est <- media_faltas / variancia_faltas
r_est <- media_faltas^2 / (variancia_faltas - media_faltas)

(Oi_faltas <- table(classes_faltas))

(cortes2_faltas = c(cortes.faltas[1:5], cortes.faltas[9+1]))
(classes2_faltas = cut(estudantes$num_faltas, breaks=cortes2_faltas, right=TRUE, 
                         include.lowest=TRUE, dig.lab = 3))

(Oi2_faltas = table(classes2_faltas))
sum(Oi2_faltas)

pi_faltas <- c(
  pnbinom(8.33, size = r_est, prob = p_est),
  pnbinom(16.7, size = r_est, prob = p_est) - pnbinom(8.33, size = r_est, prob = p_est),
  pnbinom(25, size = r_est, prob = p_est) - pnbinom(16.7, size = r_est, prob = p_est),
  pnbinom(33.3, size = r_est, prob = p_est) - pnbinom(25, size = r_est, prob = p_est),
  1 - pnbinom(33.3, size = r_est, prob = p_est)
)
sum(pi_faltas) # tem de dar 1

(teste_nfaltas = chisq.test(x=Oi2_faltas,p=pi_faltas))


# nota_final - Lilliefors
library(nortest)
teste_nota_final <- lillie.test(estudantes$nota_final_2)
teste_nota_final$statistic


# idade - qui-quadrado
(min(estudantes$idade)) # Verificar o min dos dados
(max(estudantes$idade)) # Verificar o max dos dados

(k.i = 5)
(h.i = (max(estudantes$idade)-min(estudantes$idade))/k.i)
(min.classe.i = min(estudantes$idade))
(max.classe.i = min.classe.i + h.i * k.i)

cortes.idade = seq(min.classe.i, max.classe.i, by = h.i)

classes_idade <- cut(estudantes$idade,
                          breaks = cortes.idade,
                          right = FALSE, 
                          include.lowest = TRUE) 

(Oi_idade = table(classes_idade))
sum(Oi_idade)

(cortes2_idade = c(cortes.idade[1:4], cortes.idade[5+1]))
(classes2_idade = cut(estudantes$idade, breaks=cortes2_idade, right=TRUE, 
                       include.lowest=TRUE, dig.lab = 3))

(Oi2_idade = table(classes2_idade))
sum(Oi2_idade)

pchisq(16.4,n)-pchisq(15,n)
pchisq(17.8,n)-pchisq(16.4,n)
pchisq(19.2,n)-pchisq(17.8,n)
pchisq(22,n)-pchisq(19.2,n)

(pi_idade <- c())
sum(pi_idade) # tem de dar 1

(teste_idade = chisq.test(x=Oi_idade,p=pi_idade))
teste_idade$statistic

gl_idade = k_idade-1-
qchisq(0.95,gl_idade) #RC [, +...[


#######################
#######################

# P-VALUE

# nº de faltas
# p-value (ajustado): 0.6518869 
gl_faltas = k_faltas-1-2
(pvalue_faltas <- 1 - pchisq(teste_nfaltas$statistic, gl_faltas))
qchisq(0.95,gl_faltas) #RC [, +...[


# nota_final
# p-value: 0.001670715
teste_nota_final$p.value


# idade
# p-value (ajustado):
teste_idade$p.value


#######################
#######################

# DECISÃO
alpha <- 0.05

# nº de faltas
# não rejeitar H0 - o nºfaltas segue uma distribuição binomial negativa
if(teste_nfaltas$p.value <= alpha) {
  print("Rejeitar H0: os dados não são binomais negativos")
} else {
  print("Não rejeitar H0: os dados podem ser binomais negativos")
}


# nota_final
# rejeitar H0 - a nota final não segue uma distribuíção normal
if(teste_nota_final$p.value <= alpha) {
  print("Rejeitar H0: os dados não são normais")
} else {
  print("Não rejeitar H0: os dados podem ser normais")
}


# idade
# 
alpha <- 0.05
if(teste_idade$p.value <= alpha) {
  print("Rejeitar H0: os dados não são qui-quadrado")
} else {
  print("Não rejeitar H0: os dados podem ser qui-quadrado")
}


###################
######       ######
##### 4° Fase #####
######       ######
###################


# Regressão Linear

idade=estudantes$idade
faltas=estudantes$num_faltas
notas=estudantes$nota_final_2

# Diagrama de dispersão
plot(x=faltas,
     y=notas,
     pch=20,
     col="blue",
     xlab="Num faltas",
     ylab="Nota final",
     main="Diagrama de dispersão")

# Coeficiente de correlação linear de Pearson
cor(x=faltas,y=notas)

modelo <- lm(formula = notas~faltas)

b <- modelo$coefficients[2]
a <- modelo$coefficients[1]

# Reta de regressão linear

plot(x=faltas,
     y=notas,
     pch=20,
     col="blue",
     xlab="Num faltas",
     ylab="Nota final",
     main="Diagrama de dispersão",
     abline(a=a,b=b,col="red"))

# Residuos
residuos <- modelo$residuals
sum(residuos)

plot(x=faltas,
     y=residuos,
     pch=20,
     col="blue",
     xlab="Num faltas",
     ylab="Residuos",
     main="Gráficos de residuos",
     abline(h=0))

# Conclusões:
# Pelo diagrama de Dispersão conclui-se que não existe associação linear entre o número de faltas
# e a nota final dos alunos, sua correlação linear é muito fraca, praticamente nula.

# Pela análise dos resíduos, conclui-se que o modelo ajustado não é bom.

# Previsões:
# Nota final de um aluno que falta 10 vezes e quando falta 70
# y(10) ~ 11
y1 <- a - b*10

# y(70) ~ 12
y2 <- a - b*70

# Como não há correlação, não há possibilidade de fazer previsões da nota final com base no 
# número de faltas.

# Outliers: outliers não influentes. Apesar de haver observações muito destacadas, não
# alteram o modelo linear ajustado.
