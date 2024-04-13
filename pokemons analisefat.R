###análise fatorial

library(readr)
pokemon <- read_csv("C:/Users/W0KU/Downloads/pokemon.csv")
#View(pokemon)

library(dplyr)
#colnames(pokemon)
pokemon_numeric<- pokemon %>%
  select_if(is.double)
#View(pokemon_numeric)

pokemon_numeric<-pokemon_numeric[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-21,-22,-23,-28,-33,-34)]
#View(pokemon_numeric)

#View(var(na.omit(pokemon_numeric)))
pokemon_numeric<-na.omit(pokemon_numeric)
#View(cor(pokemon_numeric))

###criando a matriz de cargas fatoriais L
hist(pokemon_numeric$sp_defense)
hist(pokemon_numeric$sp_attack)


###não vamos fazer por maxima verossimilhança pois supor normalidade
###para esse conjunto de dados não é possivel


####por componentes pricipais, temos:

R<-cor(pokemon_numeric)
eigen(R)
lambda=eigen(R)$values[1:4]
e=eigen(R)$vectors[,1:4]
L=cbind(sqrt(lambda[1])*e[,1],sqrt(lambda[2])*e[,2], sqrt(lambda[3])*e[,3], sqrt(lambda[4])*e[,4])
#View(L)
LL=L%*%t(L)  ## a diag é a var explicada de cada var

psi=diag(R)-diag(LL) #variab não explicada de cada var
psi
L_acp<- L
S<-LL+psi  ##a matriz de correlação dos dados é aproximadamente essa decomposição, quanto maior o 
##númeoro de fatores, mais próximo da matriz real de correlação
R
traco_R<-sum(diag(cor(R)))


##verificando o percentual de variab. explicada por 4 fatores 
R<-cor(pokemon_numeric)
traco_R<-sum(diag(cor(R)))
perc_exp4fat<-sum(lambda)/traco_R

rot4<-varimax(L)
rot<-rot4$loadings
rownames(rot)<-colnames(pokemon_numeric)
rot
# boxplot(pokemon_numeric$base_egg_steps)
# boxplot(pokemon_numeric$weight_kg)
# boxplot(pokemon_numeric$experience_growth)

#o primeiro fator é fortemente relacionado com peso, altura, saude
# cor fraca com tempo de eclosão do ovo, experiencia, percentual de machos, ataque, defesa e velocidade

###1 vitalidade e físico

#o segundo fator é frotemente relacionado com ataque defesa e velocidade 
#cor fraca com tempo de ecolsão do ovo, altura, saúde,percentual de machos, e peso

##2 combate e agilidade

#o terceiro fator é fortemente relacionado com o tempo de eclosão do ovo, experiencia 
#cor fraca com altura, saúde, percentual de machos, ataque defesa, velocidade e peso

##3  desnvolvimento e crecimento 

#o quarto fator é fortemente relacionado com o percetual de machos
#cor fraca com tempo de eclosão dos ovos, experiencia, altura, saude, ataque, defesa, velocidade e peso

##4 equilibrio de gêneros






###########testando mais fatores para aumentar a var explicada 


lambda=eigen(R)$values[1:6]
e=eigen(R)$vectors[,1:6]
L=cbind(sqrt(lambda[1])*e[,1],sqrt(lambda[2])*e[,2], sqrt(lambda[3])*e[,3], sqrt(lambda[4])*e[,4],sqrt(lambda[5])*e[,5], sqrt(lambda[6])*e[,6])
View(L)
LL=L%*%t(L)

psi=diag(R)-diag(LL)
psi
L_acp<- L
S<-LL+psi
R
traco_R<-sum(diag(cor(R)))

perc_exp6fat<-sum(lambda)/traco_R

###com 6 fatores já melhorou bastante o percentual de variab. explicada

####hora de analisar as cargas fatoriais e dar uma interpretação pra elas

L
####não fica claro quais quais variáveis pertecem a quais fatores para podermos interpretar
###vamos realizar uma rotação ortogonal no intuito de deixar mais evidente a relação das var com os fatores



varimax(L)
