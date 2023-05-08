library(tidyverse)

# Odczyt danych z pliku csv i przypisanie do zmiennej "dane"
dane <- read_delim("~/Desktop/Licencjat/tabela.csv", 
                   delim = "\t", escape_double = FALSE, trim_ws = TRUE, quote = "")

table(dane$conjunction.word)
table(dane$governor.position)
table(dane$no.conjuncts)
# Podział danych ze względu na pozycję nadrzędnika
dane_0 <- dane[dane$'governor.position'=="0", ]
dane_L <- dane[dane$'governor.position'=="L", ]
dane_M <- dane[dane$'governor.position'=="M", ]
dane_R <- dane[dane$'governor.position'=="R", ]

# Wyświetlenie liczby wierszy w każdej z podzielonych grup
nrow(dane)
nrow(dane_0)
nrow(dane_L)
nrow(dane_M)
nrow(dane_R)
table(dane$conjunction.word)
# Funkcja tworząca tabelkę z wynikami mediany, średniej, testu Wilcoxona i wartości p
t1 <- function(data){
  median_left <- c(median(data$'L.chars'), median(data$'L.syllables'), 
                   median(data$'L.words'), median(data$'L.tokens'))
  median_right <- c(median(data$'R.chars'), median(data$'R.syllables'), 
                    median(data$'R.words'), median(data$'R.tokens'))
  mean_left <- c(mean(data$'L.chars'), mean(data$'L.syllables'), 
                 mean(data$'L.words'), mean(data$'L.tokens'))
  mean_right <- c(mean(data$'R.chars'), mean(data$'R.syllables'), 
                  mean(data$'R.words'), mean(data$'R.tokens'))
  wilcoxon <- c(wilcox.test(data$'L.chars', data$'R.chars')$statistic,
                wilcox.test(data$'L.syllables', data$'R.syllables')$statistic,
                wilcox.test(data$'L.words', data$'R.words')$statistic,
                wilcox.test(data$'L.tokens', data$'R.tokens')$statistic)
  p_value <- c(wilcox.test(data$'L.chars', data$'R.chars')$p.value,
               wilcox.test(data$'L.syllables', data$'R.syllables')$p.value,
               wilcox.test(data$'L.words', data$'R.words')$p.value,
               wilcox.test(data$'L.tokens', data$'R.tokens')$p.value)
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
data <- cbind(data, ifelse(data$'L.chars'<data$'R.chars', 1, 0)) 
data <- cbind(data, ifelse(data$'L.syllables'<data$'R.syllables', 1, 0)) 
data <- cbind(data, ifelse(data$'L.words'<data$'R.words', 1, 0)) 
data <- cbind(data, ifelse(data$'L.tokens'<data$'R.tokens', 1, 0))

colnames(data) <- c(colnames(dane), 'znaki', 'sylaby', 'słowa', 'tokeny')

data_L <- data[data$'governor.position'=='L', ]
data_0 <- data[data$'governor.position'=='0', ]
data_R <- data[data$'governor.position'=='R', ]


#pomocnicze do drugiej i trzeciej tabeli - wyrzucam te, gdzie dlugosc mierzona w X jest rowna
pobierz <- function(data, co){
  data1 <- data[data$'L.chars' != data$'R.chars', ]
  data2 <- data[data$'L.syllables' != data$'R.syllables', ]
  data3 <- data[data$'L.words' != data$'R.words', ]
  data4 <- data[data$'L.tokens' != data$'R.tokens', ]
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
testy <- function(data1, data2){
  data1a <- data1[data1$'L.chars' != data1$'R.chars', ]
  data1b <- data1[data1$'L.syllables' != data1$'R.syllables', ]
  data1c <- data1[data1$'L.words' != data1$'R.words', ]
  data1d <- data1[data1$'L.tokens' != data1$'R.tokens', ]
  data2a <- data2[data2$'L.chars' != data2$'R.chars', ]
  data2b <- data2[data2$'L.syllables' != data2$'R.syllables', ]
  data2c <- data2[data2$'L.words' != data2$'R.words', ]
  data2d <- data2[data2$'L.tokens' != data2$'R.tokens', ]
  return(c(prop.test(x = c(sum(data1a$znaki), sum(data2a$znaki)), n = c(nrow(data1a), nrow(data2a)))$statistic,
           prop.test(x = c(sum(data1b$sylaby), sum(data2b$sylaby)), n = c(nrow(data1b), nrow(data2b)))$statistic,
           prop.test(x = c(sum(data1c$słowa), sum(data2c$słowa)), n = c(nrow(data1c), nrow(data2c)))$statistic,
           prop.test(x = c(sum(data1d$tokeny), sum(data2d$tokeny)), n = c(nrow(data1d), nrow(data2d)))$statistic,
           prop.test(x = c(sum(data1a$znaki), sum(data2a$znaki)), n = c(nrow(data1a), nrow(data2a)))$p.value,
           prop.test(x = c(sum(data1b$sylaby), sum(data2b$sylaby)), n = c(nrow(data1b), nrow(data2b)))$p.value,
           prop.test(x = c(sum(data1c$słowa), sum(data2c$słowa)), n = c(nrow(data1c), nrow(data2c)))$p.value,
           prop.test(x = c(sum(data1d$tokeny), sum(data2d$tokeny)), n = c(nrow(data1d), nrow(data2d)))$p.value))
}

tab2 <- data.frame(lewy_proportion, lewy_number, pop_L, prawy_proportion, prawy_number, pop_R, testy(data_L, data_R)[1:4], testy(data_L, data_R)[5:8])
colnames(tab2) <- c('%_L', '#_L', 'z ilu', '%_R', '#_R', 'z ilu', 'chi_2', 'p_value')
rownames(tab2) <- c('znaki', 'sylaby', 'słowa', 'tokeny')
tab2

tab3 <- data.frame(proportion_0, number_0, pop_0, testy(data_L, data_0)[1:4], testy(data_L, data_0)[5:8], testy(data_R, data_0)[1:4], testy(data_R, data_0)[5:8])
colnames(tab3) <- c('%_0', '#_0', 'z ilu', 'chi2 Lvs0', 'p_val Lvs0', 'chi2 Rvs0', 'p_val Rvs0')
rownames(tab3) <- c('znaki', 'sylaby', 'słowa', 'tokeny')
tab3

#GLM
library(ggplot2)
library(gridExtra)
library(ggExtra)
# wykres <- function(data){
#   data1 <- data[data$'L.chars' != data$'R.chars', ]
#   data2 <- data[data$'L.syllables' != data$'R.syllables', ]
#   data3 <- data[data$'L.words' != data$'R.words', ]
#   data4 <- data[data$'L.tokens' != data$'R.tokens', ]
#   
#   # Adding a column of the absolute difference for each dataset
#   data1$difference <- abs(data1$'L.chars' - data1$'R.chars')
#   data2$difference <- abs(data2$'L.syllables' - data2$'R.syllables')
#   data3$difference <- abs(data3$'L.words' - data3$'R.words')
#   data4$difference <- abs(data4$'L.tokens' - data4$'R.tokens')
#   
#   p1 <- ggplot(data1, aes(x=difference, y=znaki)) + geom_point(alpha = 0.2) +
#     geom_smooth(method="glm", formula=y~x, se=TRUE, fill = 'deepskyblue', method.args = list(family = binomial), colour = 'red') + 
#     xlab("Absolute Difference of Conjunct Lengths") + 
#     ylab("Proportion of Shorter Left Conjuncts") + 
#     ggtitle("znaki") + theme_bw()
#   
#   p2 <- ggplot(data2, aes(x=difference, y=sylaby)) + geom_point(alpha = 0.2) +
#     geom_smooth(method="glm", formula=y~x, se=TRUE, fill = 'deepskyblue', method.args = list(family = binomial), colour = 'red') + 
#     xlab("Absolute Difference of Conjunct Lengths") + 
#     ylab("Proportion of Shorter Left Conjuncts") + 
#     ggtitle("sylaby") + theme_bw()
#   
#   p3 <- ggplot(data3, aes(x=difference, y=słowa)) + geom_point(alpha = 0.2) +
#     geom_smooth(method="glm", formula=y~x, se=TRUE, fill = 'deepskyblue', method.args = list(family = binomial), colour = 'red') + 
#     xlab("Absolute Difference of Conjunct Lengths") + 
#     ylab("Proportion of Shorter Left Conjuncts") + 
#     ggtitle("słowa") + theme_bw()
#   
#   p4 <- ggplot(data4, aes(x=difference, y=tokeny)) + geom_point(alpha = 0.2) +
#     geom_smooth(method="glm", formula=y~x, se=TRUE, fill = 'deepskyblue', method.args = list(family = binomial), colour = 'red') + 
#     xlab("Absolute Difference of Conjunct Lengths") + 
#     ylab("Proportion of Shorter Left Conjuncts") + 
#     ggtitle("tokeny") + theme_bw()
#   p1 <- ggMarginal(p1, margins = 'x', colour = 'black')
#   p2 <- ggMarginal(p2, margins = 'x', colour = 'black')
#   p3 <- ggMarginal(p3, margins = 'x', colour = 'black')
#   p4 <- ggMarginal(p4, margins = 'x', colour = 'black')
#   return(list(p1=p1,p2=p2,p3=p3,p4=p4))
# }
# par(mfrow=c(4,3))
# a1 <- wykres(data_0)$p1
# a2 <- wykres(data_0)$p2
# a3 <- wykres(data_0)$p3
# a4 <- wykres(data_0)$p4
# a5 <- wykres(data_L)$p1
# a6 <- wykres(data_L)$p2
# a7 <- wykres(data_L)$p3
# a8 <- wykres(data_L)$p4
# a9 <- wykres(data_R)$p1
# a10 <- wykres(data_R)$p2
# a11 <- wykres(data_R)$p3
# a12 <- wykres(data_R)$p4
# plots_list = list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
# grid.arrange(grobs = plots_list, ncol = 4, nrow = 3)
# model wieloczynnikowy
library(emmeans)

# znaki
data1 <- data[data$'L.chars' != data$'R.chars', ]
data1 <- data1[data1$'governor.position'!='M',]
data1$difference <- abs(data1$'L.chars' - data1$'R.chars')
g1 <- glm(znaki ~ difference * governor.position, family = binomial, data = data1)
g1_emm <- emmeans(g1, specs = pairwise ~ governor.position)  # sprawdzić, czy same nachylenia są istotnie różne
g1_emm2 <- emmeans(g1, pairwise ~ governor.position * difference)
summary(g1)
summary(g1_emm)
summary(g1_emm2)
emt1 <- emtrends(g1, specs=pairwise~'governor.position', var='difference')
sum1 <- summary((emt1),infer=TRUE)
sum1

# sylaby
data2 <- data[data$'L.syllables' != data$'R.syllables', ]
data2 <- data2[data2$'governor.position'!='M',]
data2$difference <- abs(data2$'L.syllables' - data2$'R.syllables')
g2 <- glm(sylaby ~ difference * governor.position, family = binomial, data = data2)
g2_emm <- emmeans(g2, specs = pairwise ~ governor.position)
summary(g2)
summary(g2_emm)
emt2 <- emtrends(g2, specs=pairwise~'governor.position', var='difference')
sum2 <- summary((emt2),infer=TRUE)
sum2

# słowa
data3 <- data[data$'L.words' != data$'R.words', ]
data3 <- data3[data3$'governor.position'!='M',]
data3$difference <- abs(data3$'L.words' - data3$'R.words')
g3 <- glm(słowa ~ difference * governor.position, family = binomial, data = data3)
g3_emm <- emmeans(g3, specs = pairwise ~ governor.position)
summary(g3)
summary(g3_emm)
emt3 <- emtrends(g3, specs=pairwise~'governor.position', var='difference')
sum3 <- summary((emt3),infer=TRUE)
sum3

# tokeny
data4 <- data[data$'L.tokens' != data$'R.tokens', ]
data4 <- data4[data4$'governor.position'!='M',]
data4$difference <- abs(data4$'L.tokens' - data4$'R.tokens')
g4 <- glm(tokeny ~ difference * governor.position, family = binomial, data = data4)
g4_emm <- emmeans(g4, specs = pairwise ~ governor.position)
summary(g4)
summary(g4_emm)
emt4 <- emtrends(g4, specs=pairwise~'governor.position', var='difference')
sum4 <- summary((emt4),infer=TRUE)
sum4

quartz(type = 'pdf', file = '~/Desktop/Licencjat/analizy/wykresy1.pdf')
l1 <- ggplot(data1, aes(x=difference, y=znaki)) + geom_point(alpha = 0.2) +
  geom_smooth(method="glm", formula=y~x, se=TRUE, lwd = 0.35, fill = 'deepskyblue', method.args = list(family = binomial), colour = 'red') + 
  xlab("Moduł z róźnicy długości członów") + ylab("") + facet_wrap(~ governor.position) +
  ggtitle("Znaki") + theme_bw()
l2 <- ggplot(data2, aes(x=difference, y=sylaby)) + geom_point(alpha = 0.2) +
  geom_smooth(method="glm", formula=y~x, se=TRUE, lwd = 0.35, fill = 'deepskyblue', method.args = list(family = binomial), colour = 'red') + 
  xlab("Moduł z róźnicy długości członów") + ylab("") + facet_wrap(~ governor.position) +
  ggtitle("Sylaby") + theme_bw()
l3 <- ggplot(data3, aes(x=difference, y=słowa)) + geom_point(alpha = 0.2) +
  geom_smooth(method="glm", formula=y~x, se=TRUE, lwd = 0.35, fill = 'deepskyblue', method.args = list(family = binomial), colour = 'red') + 
  xlab("Moduł z róźnicy długości członów") + ylab("") + facet_wrap(~ governor.position) +
  ggtitle("Słowa") + theme_bw()
l4 <- ggplot(data4, aes(x=difference, y=tokeny)) + geom_point(alpha = 0.2) +
  geom_smooth(method="glm", formula=y~x, se=TRUE, lwd = 0.35, fill = 'deepskyblue', method.args = list(family = binomial), colour = 'red') + 
  xlab("Moduł z róźnicy długości członów") + ylab("") + facet_wrap(~ governor.position) +
  ggtitle("Tokeny") + theme_bw()
plots = list(l1, l2, l3, l4)
grid.arrange(grobs = plots, ncol = 1, nrow = 4, left = "Proporcja krótszego członu z lewej strony")
dev.off()

library(ggpubr)
quartz(type = 'pdf', file = '~/Desktop/Licencjat/analizy/wykresy2.pdf', width = 9, height = 5)
k1 <- ggplot(data = data1, mapping = aes(x = difference, y = znaki, color = governor.position, fill = governor.position, linetype = governor.position)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method="glm", formula=y~x, se=TRUE, lwd = 0.35, alpha = 0.3, method.args = list(family = binomial)) + 
  xlab("Moduł z róźnicy długości członów") + 
  ylab("Proporcja krótszego członu z lewej") +
  ggtitle("Znaki") + theme_bw() + scale_color_manual(labels = c('brak', 'z lewej strony', 'z prawej strony'), values = c("red", "green", "royalblue")) + scale_fill_manual(labels = c('brak', 'z lewej strony', 'z prawej strony'), values = c("red", "green", "royalblue")) + scale_linetype_manual(labels = c('brak', 'z lewej strony', 'z prawej strony'), values = c('solid', 'twodash', 'dotted')) + 
  guides(fill=guide_legend(title="Nadrzędnik:"), color=guide_legend(title="Nadrzędnik:"), linetype=guide_legend(title="Nadrzędnik:"))
k2 <- ggplot(data = data2, mapping = aes(x = difference, y = sylaby, color = governor.position, fill = governor.position, linetype = governor.position)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method="glm", formula=y~x, se=TRUE, lwd = 0.35, alpha = 0.3, method.args = list(family = binomial)) + 
  xlab("Moduł z róźnicy długości członów") + 
  ylab("Proporcja krótszego członu z lewej") +
  ggtitle("Sylaby") + theme_bw() + scale_color_manual(labels = c('brak', 'z lewej strony', 'z prawej strony'), values = c("red", "green", "royalblue")) + scale_fill_manual(labels = c('brak', 'z lewej strony', 'z prawej strony'), values = c("red", "green", "royalblue")) + scale_linetype_manual(labels = c('brak', 'z lewej strony', 'z prawej strony'), values = c('solid', 'twodash', 'dotted')) +
  guides(fill=guide_legend(title="Nadrzędnik:"), color=guide_legend(title="Nadrzędnik:"), linetype=guide_legend(title="Nadrzędnik:"))
k3 <- ggplot(data = data3, mapping = aes(x = difference, y = słowa, color = governor.position, fill = governor.position, linetype = governor.position)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method="glm", formula=y~x, se=TRUE, lwd = 0.35, alpha = 0.3, method.args = list(family = binomial)) + 
  xlab("Moduł z róźnicy długości członów") + 
  ylab("Proporcja krótszego członu z lewej") +
  ggtitle("Słowa") + theme_bw() + scale_color_manual(labels = c('brak', 'z lewej strony', 'z prawej strony'), values = c("red", "green", "royalblue")) + scale_fill_manual(labels = c('brak', 'z lewej strony', 'z prawej strony'), values = c("red", "green", "royalblue")) + scale_linetype_manual(labels = c('brak', 'z lewej strony', 'z prawej strony'), values = c('solid', 'twodash', 'dotted')) +
  guides(fill=guide_legend(title="Nadrzędnik:"), color=guide_legend(title="Nadrzędnik:"), linetype=guide_legend(title="Nadrzędnik:"))
k4 <- ggplot(data = data4, mapping = aes(x = difference, y = tokeny, color = governor.position, fill = governor.position, linetype = governor.position)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method="glm", formula=y~x, se=TRUE, lwd = 0.35, alpha = 0.3, method.args = list(family = binomial)) + 
  xlab("Moduł z róźnicy długości członów") + 
  ylab("Proporcja krótszego członu z lewej") +
  ggtitle("Tokeny") + theme_bw() + scale_color_manual(labels = c('brak', 'z lewej strony', 'z prawej strony'), values = c("red", "green", "royalblue")) + scale_fill_manual(labels = c('brak', 'z lewej strony', 'z prawej strony'), values = c("red", "green", "royalblue")) + scale_linetype_manual(labels = c('brak', 'z lewej strony', 'z prawej strony'), values = c('solid', 'twodash', 'dotted')) +
  guides(fill=guide_legend(title="Nadrzędnik:"), color=guide_legend(title="Nadrzędnik:"), linetype=guide_legend(title="Nadrzędnik:"))
plots = list(k1, k2, k3, k4)
#grid.arrange(grobs = plots, ncol = 2, nrow = 2, common.legend = T, legend = "bottom")
ggarrange(k1, k2, k3, k4, ncol=2, nrow=2, common.legend = T, legend = "right")
dev.off()
# grid.arrange(grobs = plots, ncol = 2, nrow = 4)



