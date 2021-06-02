setwd("~/Desktop/UNICAMP/Atividade 2 - multi2")

install.packages("factoextra")
library(factoextra)

install.packages("ggplot2")
library(ggplot2)

install.packages("cluster")
library(cluster)


library(readr)
beethoven <- read_csv("datasets/dataset_beethoven.csv")
beethoven$Title <- factor(beethoven$Title)
beethoven$Key <- factor(beethoven$Key)
beethoven$Genre <- factor(beethoven$Genre)


res.agnes <- agnes(x = beethoven, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "gower", # metric for distance matrix
                   method = "ward" # Linkage method
)

fviz_dend(res.agnes, cex = 0.6, k = 4)

fviz_nbclust(beethoven, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Gráfico de cotovelo")

fviz_nbclust(beethoven, hcut, method = "silhouette")+
  labs(subtitle = "Análise de Silhouette")

fviz_nbclust(beethoven, hcut, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

res.hc2 <- hclust(res.agnes, method = "average")

grp <- cutree(res.hc, k = 4)
head(grp, n = 4)

