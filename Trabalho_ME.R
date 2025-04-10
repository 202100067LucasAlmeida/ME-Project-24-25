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