library(tidyverse)

# Odczyt danych z pliku csv i przypisanie do zmiennej "dane"
dane <- read_delim("~/Desktop/Licencjat/tabela.csv", 
                   delim = "\t", escape_double = FALSE, trim_ws = TRUE, quote = "")

# Podział danych ze względu na pozycję nadrzędnika
dane_0 <- dane[dane$'pozycja nadrzędnika'=="0", ]
dane_L <- dane[dane$'pozycja nadrzędnika'=="L", ]
dane_M <- dane[dane$'pozycja nadrzędnika'=="M", ]
dane_R <- dane[dane$'pozycja nadrzędnika'=="R", ]

# Wyświetlenie liczby wierszy w każdej z podzielonych grup
nrow(dane_0)
nrow(dane_L)
nrow(dane_M)
nrow(dane_R)
# Funkcja tworząca tabelkę z wynikami mediany, średniej, testu Wilcoxona i wartości p
t1 <- function(data){
  median_left <- c(median(data$'znaki pierwszego członu'), median(data$'sylaby pierwszego członu'), 
                   median(data$'słowa pierwszego członu'), median(data$'tokeny pierwszego członu'))
  median_right <- c(median(data$'znaki ostatniego członu'), median(data$'sylaby ostatniego członu'), 
                    median(data$'słowa ostatniego członu'), median(data$'tokeny ostatniego członu'))
  mean_left <- c(mean(data$'znaki pierwszego członu'), mean(data$'sylaby pierwszego członu'), 
                 mean(data$'słowa pierwszego członu'), mean(data$'tokeny pierwszego członu'))
  mean_right <- c(mean(data$'znaki ostatniego członu'), mean(data$'sylaby ostatniego członu'), 
                  mean(data$'słowa ostatniego członu'), mean(data$'tokeny ostatniego członu'))
  wilcoxon <- c(wilcox.test(data$'znaki pierwszego członu', data$'znaki ostatniego członu')$statistic,
                wilcox.test(data$'sylaby pierwszego członu', data$'sylaby ostatniego członu')$statistic,
                wilcox.test(data$'słowa pierwszego członu', data$'słowa ostatniego członu')$statistic,
                wilcox.test(data$'tokeny pierwszego członu', data$'tokeny ostatniego członu')$statistic)
  p_value <- c(wilcox.test(data$'znaki pierwszego członu', data$'znaki ostatniego członu')$p.value,
               wilcox.test(data$'sylaby pierwszego członu', data$'sylaby ostatniego członu')$p.value,
               wilcox.test(data$'słowa pierwszego członu', data$'słowa ostatniego członu')$p.value,
               wilcox.test(data$'tokeny pierwszego członu', data$'tokeny ostatniego członu')$p.value)
  df_result <- data.frame(cbind(median_left, median_right, mean_left, mean_right, wilcoxon, p_value))
  rownames(df_result) <- c('znaki', 'sylaby', 'słowa', 'tokeny')
  colnames(df_result) <- c('mediana-l', 'mediana-p', 'średnia-l', 'średnia-p', 'W', 'p')
  return(df_result)
}

tab1a <- t1(dane)
tab1b <- t1(dane_0)
tab1c <- t1(dane_L)
tab1d <- t1(dane_M)
tab1e <- t1(dane_R)
tab1 <- rbind(tab1a, tab1b, tab1c, tab1d, tab1e)

data <- dane
#znalezienie proporcji 
data <- cbind(data, ifelse(data$'znaki pierwszego członu'<data$'znaki ostatniego członu', 1, 0)) 
data <- cbind(data, ifelse(data$'sylaby pierwszego członu'<data$'sylaby ostatniego członu', 1, 0)) 
data <- cbind(data, ifelse(data$'słowa pierwszego członu'<data$'słowa ostatniego członu', 1, 0)) 
data <- cbind(data, ifelse(data$'tokeny pierwszego członu'<data$'tokeny ostatniego członu', 1, 0))

colnames(data) <- c(colnames(dane), 'znaki', 'sylaby', 'słowa', 'tokeny')

data_L <- data[data$'pozycja nadrzędnika'=='L', ]
data_0 <- data[data$'pozycja nadrzędnika'=='0', ]
data_R <- data[data$'pozycja nadrzędnika'=='R', ]


#pomocnicze do drugiej i trzeciej tabeli - wyrzucam te, gdzie dlugosc mierzona w X jest rowna
pobierz <- function(data, co){
  data1 <- data[data$'znaki pierwszego członu' != data$'znaki ostatniego członu', ]
  data2 <- data[data$'sylaby pierwszego członu' != data$'sylaby ostatniego członu', ]
  data3 <- data[data$'słowa pierwszego członu' != data$'słowa ostatniego członu', ]
  data4 <- data[data$'tokeny pierwszego członu' != data$'tokeny ostatniego członu', ]
  return(c(co(data1$'znaki'), 
           co(data2$'sylaby'), 
           co(data3$'słowa'), 
           co(data4$'tokeny'),
           nrow(data1), nrow(data2), nrow(data3), nrow(data4)))
}
lewy_proportion <- pobierz(data_L, mean)[1:4]
lewy_number <- pobierz(data_L, sum)[1:4]
pop_L <- pobierz(data_L, sum)[5:8]
proportion_0 <- pobierz(data_0, mean)[1:4]
number_0 <- pobierz(data_0, sum)[1:4]
pop_0 <- pobierz(data_0, sum)[5:8]
prawy_proportion <- pobierz(data_R, mean)[1:4]
prawy_number <- pobierz(data_R, sum)[1:4]
pop_R <- pobierz(data_R, sum)[5:8]

#prop test
test <- function(data1, data2){
  data1a <- data1[data1$'znaki pierwszego członu' != data1$'znaki ostatniego członu', ]
  data1b <- data1[data1$'sylaby pierwszego członu' != data1$'sylaby ostatniego członu', ]
  data1c <- data1[data1$'słowa pierwszego członu' != data1$'słowa ostatniego członu', ]
  data1d <- data1[data1$'tokeny pierwszego członu' != data1$'tokeny ostatniego członu', ]
  data2a <- data2[data2$'znaki pierwszego członu' != data2$'znaki ostatniego członu', ]
  data2b <- data2[data2$'sylaby pierwszego członu' != data2$'sylaby ostatniego członu', ]
  data2c <- data2[data2$'słowa pierwszego członu' != data2$'słowa ostatniego członu', ]
  data2d <- data2[data2$'tokeny pierwszego członu' != data2$'tokeny ostatniego członu', ]
  return(c(prop.test(x = c(sum(data1a$znaki), sum(data2a$znaki)), n = c(nrow(data1a), nrow(data2a)))$statistic,
           prop.test(x = c(sum(data1b$sylaby), sum(data2b$sylaby)), n = c(nrow(data1b), nrow(data2b)))$statistic,
           prop.test(x = c(sum(data1c$słowa), sum(data2c$słowa)), n = c(nrow(data1c), nrow(data2c)))$statistic,
           prop.test(x = c(sum(data1d$tokeny), sum(data2d$tokeny)), n = c(nrow(data1d), nrow(data2d)))$statistic,
           prop.test(x = c(sum(data1a$znaki), sum(data2a$znaki)), n = c(nrow(data1a), nrow(data2a)))$p.value,
           prop.test(x = c(sum(data1b$sylaby), sum(data2b$sylaby)), n = c(nrow(data1b), nrow(data2b)))$p.value,
           prop.test(x = c(sum(data1c$słowa), sum(data2c$słowa)), n = c(nrow(data1c), nrow(data2c)))$p.value,
           prop.test(x = c(sum(data1d$tokeny), sum(data2d$tokeny)), n = c(nrow(data1a), nrow(data2d)))$p.value))
}

tab2 <- data.frame(lewy_proportion, lewy_number, pop_L, prawy_proportion, prawy_number, pop_R, test(data_L, data_R)[1:4], test(data_L, data_R)[5:8])
colnames(tab2) <- c('%_L', '#_L', 'z ilu', '%_R', '#_R', 'z ilu', 'chi_2', 'p_value')
rownames(tab2) <- c('znaki', 'sylaby', 'słowa', 'tokeny')
tab2

tab3 <- data.frame(proportion_0, number_0, pop_0, test(data_L, data_0)[1:4], test(data_L, data_0)[5:8], test(data_R, data_0)[1:4], test(data_R, data_0)[5:8])
colnames(tab3) <- c('%_0', '#_0', 'z ilu', 'chi2 Lvs0', 'p_val Lvs0', 'chi2 Rvs0', 'p_val Rvs0')
rownames(tab3) <- c('znaki', 'sylaby', 'słowa', 'tokeny')
tab3

#GLM
library(ggplot2)
library(gridExtra)
library(ggExtra)
wykres <- function(data){
  data1 <- data[data$'znaki pierwszego członu' != data$'znaki ostatniego członu', ]
  data2 <- data[data$'sylaby pierwszego członu' != data$'sylaby ostatniego członu', ]
  data3 <- data[data$'słowa pierwszego członu' != data$'słowa ostatniego członu', ]
  data4 <- data[data$'tokeny pierwszego członu' != data$'tokeny ostatniego członu', ]
  
  # Adding a column of the absolute difference for each dataset
  data1$difference <- abs(data1$'znaki pierwszego członu' - data1$'znaki ostatniego członu')
  data2$difference <- abs(data2$'sylaby pierwszego członu' - data2$'sylaby ostatniego członu')
  data3$difference <- abs(data3$'słowa pierwszego członu' - data3$'słowa ostatniego członu')
  data4$difference <- abs(data4$'tokeny pierwszego członu' - data4$'tokeny ostatniego członu')
  
  p1 <- ggplot(data1, aes(x=difference, y=znaki)) + geom_point(alpha = 0.2) +
    geom_smooth(method="glm", formula=y~x, se=TRUE, fill = 'deepskyblue', method.args = list(family = binomial), colour = 'red') + 
    xlab("Absolute Difference of Conjunct Lengths") + 
    ylab("Proportion of Shorter Left Conjuncts") + 
    ggtitle("znaki") + theme_bw()
  
  p2 <- ggplot(data2, aes(x=difference, y=sylaby)) + geom_point(alpha = 0.2) +
    geom_smooth(method="glm", formula=y~x, se=TRUE, fill = 'deepskyblue', method.args = list(family = binomial), colour = 'red') + 
    xlab("Absolute Difference of Conjunct Lengths") + 
    ylab("Proportion of Shorter Left Conjuncts") + 
    ggtitle("sylaby") + theme_bw()
  
  p3 <- ggplot(data3, aes(x=difference, y=słowa)) + geom_point(alpha = 0.2) +
    geom_smooth(method="glm", formula=y~x, se=TRUE, fill = 'deepskyblue', method.args = list(family = binomial), colour = 'red') + 
    xlab("Absolute Difference of Conjunct Lengths") + 
    ylab("Proportion of Shorter Left Conjuncts") + 
    ggtitle("słowa") + theme_bw()
  
  p4 <- ggplot(data4, aes(x=difference, y=tokeny)) + geom_point(alpha = 0.2) +
    geom_smooth(method="glm", formula=y~x, se=TRUE, fill = 'deepskyblue', method.args = list(family = binomial), colour = 'red') + 
    xlab("Absolute Difference of Conjunct Lengths") + 
    ylab("Proportion of Shorter Left Conjuncts") + 
    ggtitle("tokeny") + theme_bw()
  p1 <- ggMarginal(p1, margins = 'x', colour = 'black')
  p2 <- ggMarginal(p2, margins = 'x', colour = 'black')
  p3 <- ggMarginal(p3, margins = 'x', colour = 'black')
  p4 <- ggMarginal(p4, margins = 'x', colour = 'black')
  return(list(p1=p1, p2=p2, p3=p3, p4=p4))
}
par(mfrow=c(4,3))
a1 <- wykres(data_0)$p1
a2 <- wykres(data_0)$p2
a3 <- wykres(data_0)$p3
a4 <- wykres(data_0)$p4
a5 <- wykres(data_L)$p1
a6 <- wykres(data_L)$p2
a7 <- wykres(data_L)$p3
a8 <- wykres(data_L)$p4
a9 <- wykres(data_R)$p1
a10 <- wykres(data_R)$p2
a11 <- wykres(data_R)$p3
a12 <- wykres(data_R)$p4
plots_list = list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
grid.arrange(grobs = plots_list, ncol = 4, nrow = 3)
# model wieloczynnikowy
library(emmeans)
datax <- data[data$`pozycja nadrzędnika`!='M',]
datax$'pozycja' <- datax$`pozycja nadrzędnika`

data1 <- datax[datax$'znaki pierwszego członu' != datax$'znaki ostatniego członu', ]
data1$difference <- abs(data1$'znaki pierwszego członu' - data1$'znaki ostatniego członu')
g1 <- glm(znaki ~ difference * pozycja, family = binomial, data = data1)
g1_emm <- emmeans(g1, specs = pairwise ~ pozycja)
summary(g1)
summary(g1_emm)


data2 <- datax[datax$'sylaby pierwszego członu' != datax$'sylaby ostatniego członu', ]
data3 <- datax[datax$'słowa pierwszego członu' != datax$'słowa ostatniego członu', ]
data4 <- datax[datax$'tokeny pierwszego członu' != datax$'tokeny ostatniego członu', ]
data2$difference <- abs(data2$'sylaby pierwszego członu' - data2$'sylaby ostatniego członu')
data3$difference <- abs(data3$'słowa pierwszego członu' - data3$'słowa ostatniego członu')
data4$difference <- abs(data4$'tokeny pierwszego członu' - data4$'tokeny ostatniego członu')
g2 <- glm(sylaby~difference*pozycja,family=binomial,data=data2)
g3 <- glm(słowa~difference*pozycja,family=binomial,data=data3)
g4 <- glm(tokeny~difference*pozycja,family=binomial,data=data4)
summary(g2)
summary(g3)
summary(g4)
g1_RasRef <- glm(znaki ~ difference * pozycja, family = binomial, data = data1,
                 contrasts = list(pozycja = contr.treatment(c("R", "L", "0"))))
g1_LasRef <- glm(znaki ~ difference * pozycja, family = binomial, data = data1,
                 contrasts = list(pozycja = contr.treatment(c("L", "0", "R"))))
summary(g1_RasRef)
summary(g1_LasRef)