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
# dados <- read.csv("C:/Users/diana/Desktop/IPS/LEI/2ºANO/2ºSEMESTRE/ME/PROJETO/ME-Project-24-25/data.csv", sep=";", header = TRUE)
# dados <- read.csv("/Users/rita/Documents/IPS/3ºano/2º\ semestre/ME/trabalho/ME-Project-24-25/data.csv", sep=";", header = TRUE)
 
escola <- dados$school
idade <- dados$age
estudo_semanal <- dados$studytime
aulas_extras_pagas <- dados$paid
num_faltas <- dados$absences
nota_final <- dados$G3

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

# Nosso trabalho irá abordar um estudo do desempenho dos alunos em matemática 
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

# Variável: idade
k.i = 4 # 4 classes
h.i = 2 # amplitude de 2 anos

valor.min.i  = 15
valor.max.i = valor.min.i + k.i*h.i

cortes.idade = seq(valor.min.i, valor.max.i, by=h.i)

classe.idade <- cut(estudantes$idade, breaks = cortes.idade, right = FALSE, include.lowest = TRUE)

ni.i = table(classe.idade)
fi.i = prop.table(ni.i)
Ni.i = cumsum(ni.i)
Fi.i = cumsum(fi.i)

tabela.frequencia.idade <- data.frame(i=c(1:nrow(ni.i)),
                                  classe = names(ni.i),
                                  ni = as.integer(ni.i),
                                  fi = round(as.numeric(fi.i),4),
                                  Ni = as.integer(Ni.i),
                                  Fi = round(as.numeric(Fi.i),4))

