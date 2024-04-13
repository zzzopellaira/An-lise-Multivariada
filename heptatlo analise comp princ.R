##aula 28-03 Mult var

# brut=c(9893,8776,13572,6455,5129,5432,3807,3423,3708,3294,5433,6287) 
# liq=c(564,389,1103,743,203,215,385,187,127,297,432,451)
# pat=c(17689,17359,18597,8745,14397,3467,4679,6754,2275,6754,5589,8972) 
# dados=cbind(brut,liq,pat)

# #obtendo a matriz de cov 
# s<-(var(dados))
# o<-eigen(s)$vectors #matriz de autovetores
# lambda<-eigen(s)$values #matriz diag de autovalores
# s1<-lambda[1] * o[,1]%*%t(o[,1]) #lambda1 x e1 x t(e1) primeira parcela da matriz
# s2<-s1+(lambda[2] * o[,2]%*%t(o[,2])) #A SOMA CONVERGE PARA A MATRIZ SIMILAR
# s3<-s2+(lambda[3] * o[,3]%*%t(o[,3]))
# s
# s2
# s1

# sum(diag(s1))
# sum(diag(s2))
# sum(diag(s3))  #matriz similar
# sum(diag(s))  #traço da matriz de cov dos dados
# 
# det(s)
# det(s3)
##base de dados real

library(scatterplot3d)
View(heptatlo)
heptatlo <- read.csv("C:/Users/W0KU/Downloads/heptatlo.txt",header=T, sep="")

##o que fariamos se não tivessemos a coluna de scores e quisessemos saber 
## quem merece o ouro? 

#a primeira coluna é o tempo, qunato menor, melhor 
#a segunda esta em metros, quanto maior melhor, 
#hurdels, run200 e run800 quanto menor melhor 

View(heptatlo)
#modificando as colunas em que a pontução quanto menor melhor
heptatlo<- subset(heptatlo,select = -score) #retirando a col de score
heptatlo$hurdles<-heptatlo$hurdles*(-1)
heptatlo$run200m<-heptatlo$run200m*(-1)
heptatlo$run800m<-heptatlo$run800m*(-1)

scatterplot3d(heptatlo, type = "h")

View(var(heptatlo))  ###a var run800 varia muito entao ela comanda a análise
#premiação injusta dar o ouro para o melhor somente na run800

comps1<-princomp(x=heptatlo )
summary(comps1, loadings = T)


#solução: usar a matriz de correlação 




#crindo as componentes principais
comps2<-princomp(x=heptatlo, cor=T )
summary(comps2, loadings = T)

#calculando os novos scores baseados em componentes principais
escores<-comps2$scores

comps2$scores

heptatlo$novos_scores<-comps2$scores
##todos os coeficientes grandes exceto o lançamento de dardos
##bem mais razoavel pois considera mais provas
##o percentual de explicação dos dados é bem menor mas  
##



##atencao, uma pessoa foi muito mal nas provas e mexeu nos dados
##ela não tem como ganhar a medalha entao nao
##atrapalhara a analise retirala
boxplot(heptatlo$run800m, main="Boxplot corrida de 800m", ylab = "Tempo dos participantes em segundos")
#possivel ver que essa pessoa atrapalhou os dados por se outlier


#refazer a análise sem o outlier

heptatlo2<-heptatlo[-25,]
#View(heptatlo2)
View(var(heptatlo2)) ##a var da corrida de 800m continua muito maior
heptatlo<-heptatlo2
## para que ela não comamnde a análise novamente 

comps<-princomp(x=heptatlo )
summary(comps, loadings = T)

#com a cov os pesos ficam maiores para apenas 3 variaveis

##fazendo com a cor para evitar a dominancia da variavel run800


comps<-princomp(x=heptatlo , cor=T )
scores<-comps$scores
summary(comps, loadings = T)



scores<-comps$scores
View(scores)
#mesmo explicando menos a variabilidade dos dados, o score considera
#todas as provas
##posso pegar duas componentes para calcular os scores pra tentar
#explicar mais a variab dos dados???
#não existe o resultados de somar os scores, o melhor procedimento seria pegar
#somente a primeira componente






##como escolher o meljor?
#como multiplicamos por menor 1 as variaveis em que o menor tempo significa que o atleta é melhor condicionado,
# vamos ranquear os scores do maior pro menor e o primeiro recebe a medalha de ouro 
#
##obs utilizamos as variaveis centradas para calcular os scores, o que não é
#um problema visto que a analise de comp princip considera as variancias. Lembrando
#que a variancia de uma variavel não é afetada ao somar ou subtrair uma constante



