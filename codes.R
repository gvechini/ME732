setwd("~/Desktop/UNICAMP/Atividade 2 - multi2")
install.packages("stringr")

library(stringr)
library(readr)
library(dplyr)

Beethoven_instruments <- read_csv("Beethoven_instruments.csv")
Beethoven_compositions <- read_csv("Beethoven_compositions.csv", 
                                   locale = locale(encoding = "latin1"))
head(Beethoven_compositions)
unique(Beethoven_compositions$Key)

# considerando data importante, vamos considerar apenas o primeiro ano de conposição

Beethoven_compositions$Date <- str_trunc(Beethoven_compositions$Date, 4, "right",ellipsis = "")

# em key temos 76 NA em nossas 243 observaçõa, ou vamos retirala, pois compoem 1/3 da bade de dados 

# ou vamos considerar uma medida onde podemos usar NA como distancia

# scoring, vamos separar os instruimisntos existenets

inst <- Beethoven_compositions$Scoring 

# junta tudo em uma string só
p <- function(..., sep='') {
  paste(..., sep=sep, collapse=sep)
}

test <- p(inst, ' ')
teste2 <- str_split_fixed(test, ' ', n = Inf)
inst_unique_v1 <- unique(teste2[1,])

# agora que sei as possiveis colunas, vou separar cada string e criar as colunas

# remover numero começo string @"^[0-9]+"

inst_unique_v2 <- str_replace_all(inst_unique_v1, "^[0-9]+", "")

inst_unique_v2  <- unique(inst_unique_v2)

inst_unique_v2[13] <- 'pf'

inst_unique_v2 <- inst_unique_v2[-c(24, 33)]

# criando a a tabela de isntuimentos
qtde_inst <- setNames(data.frame(matrix(ncol = 31, nrow = 243)), inst_unique_v2)

# tirando os numeros
# vou fazer binário
scoring <- str_replace_all(Beethoven_compositions$Scoring , "^[0-9]+", "")
# andando pelas colunas
passei = 0
opa = 0
i = 0
j = 0
for (i in 1:243){
  lista <- scoring[i]
  lista <- str_split_fixed(lista, ' ', n = Inf)[1,]
  passei = passei +1
  # andando pelas coluans
  for(j in 1:31){
    tem <- str_detect(lista, inst_unique_v2[j])
    if (sum(tem)) {
      qtde_inst[i, j] = 1
    } else {
      qtde_inst[i, j] = 0
    }
    opa = opa+1
  }
}

qtde_inst[100, 13] <- 1
qtde_inst <- qtde_inst[, -23]

# juntando os bancos
all_bee <- cbind(Beethoven_compositions[,-c(1,5)], qtde_inst)

# a coluna Op. não será incuida pois, representa diferentes versões dos manuscritos e em Titulo já temos essa informação da versão do manuscrito.
# Atraves da coluna Key, podemos observar que há diferença entre eles, nuca uma sequencia Op n/ possui mesma Key. Sendo assim, removeremos a coluna Key tbm.
# pois 1) em nossos dados representam cerca de 1/3 das informações faltantes e poderiamos dizer que que em Titulo já considaramos aue são partituras diferenets.

#Observamos que em Data, A musica piano trio, op. 63 é a unica que não temos infromação de ano.
# Pesquisando rapidamente, encontramos que foi compsta em 1807(via wiki), logo completei esta coluna. (mais rapido qo que pensar em como vou lidar com esse NA)

write_csv(all_bee, "dataset_beethoven.csv")

