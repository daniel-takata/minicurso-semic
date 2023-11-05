#lendo os dados do campeonato brasileiro 2023 até a 30a rodada

dados <- read.csv('C:\\Users\\danie\\Documents\\minicurso_futebol\\BRA2023.csv')

#histograma do número de gols por partida

dados$gols=dados$gols_mandante+dados$gols_visitante
hist(dados$gols,main=NULL,ylab="Frequência de jogos",xlab="Gols")
mean(dados$gols)
x=1:11-1
y=dpois(x,2.539474)

hist(dados$gols,main=NULL,ylab="Frequência de jogos",
     xlab="Gols",freq=FALSE,
     ylim=c(0,0.3),xlim=c(0,10))
par(new=T)
plot(x,y,type="l",ylim=c(0,0.3),xlim=c(0,10))

#histograma dos gols do flamengo
flamengo=c(0,0,0,0,0,0,
           1,1,1,1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,
           3,3,3,3,3,
           4)

hist(flamengo,breaks=c(-1,0,1,2,3,4),
     main=NULL,ylab="Frequência de jogos",
     xlab="Gols")

#cálculos das frequencias esperadas de gols do flamengo
dpois(0,1.47)
dpois(1,1.47)
dpois(2,1.47)
dpois(3,1.47)
dpois(4,1.47)


#média de gols por time
mandantes=dados$mandante
visitantes=dados$visitante
times=c(mandantes,visitantes)
gols_mandantes=dados$gols_mandante
gols_visitantes=dados$gols_visitante
gols=c(gols_mandantes,gols_visitantes)
aggregate(gols, list(times), FUN=mean) 

#cálculo da probabilidade de Internacional vencer Fluminense por 1 x 0

exp(-1.16)*1.16*exp(-1.35)

dpois(1, 1.16)*dpois(0, 1.35)


max_gols = 10
probabilidades <- dpois(0:max_gols, 1.16) %*% t(dpois(0:max_gols, 1.35))
#matriz de probabilidades, soma 1 (ou quase um, pq vai ate 10 gols).
#prob. numero de gols da equipe mandante na vertical, e da visitante na horizontal
prob_mandante <- sum(probabilidades[lower.tri(probabilidades)])
prob_empate <- sum(diag(probabilidades))
prob_visitante <- sum(probabilidades[upper.tri(probabilidades)])

#médias de gols por time quando o time é mandante
aggregate(dados$gols_mandante, list(dados$mandante), FUN=mean) 

#médias de gols por time quando o time é visitante
aggregate(dados$gols_visitante, list(dados$visitante), FUN=mean) 


#cálculo da probabilidade de Internacional mandante vencer Fluminense visitante por 1 x 0

exp(-1.63)*1.63*exp(-0.93)

dpois(1, 1.63)*dpois(0, 0.93)


max_gols = 10
probabilidades <- dpois(0:max_gols, 1.63) %*% t(dpois(0:max_gols, 0.93))
prob_mandante <- sum(probabilidades[lower.tri(probabilidades)])
prob_empate <- sum(diag(probabilidades))
prob_visitante <- sum(probabilidades[upper.tri(probabilidades)])


#modelo de Maher (1982)

#gols marcados por mandantes
aggregate(gols_mandante ~ mandante, data=dados, sum)
#gols sofridos por visitantes
aggregate(gols_mandante ~ visitante, data=dados, sum)

Sx=sum(dados$gols_mandante)

alphas=aggregate(gols_mandante ~ mandante, data=dados, sum)
alphas$gols_mandante=alphas$gols_mandante/sqrt(Sx)
betas=aggregate(gols_mandante ~ visitante, data=dados, sum)
betas$gols_mandante=betas$gols_mandante/sqrt(Sx)
colnames(alphas)[2]="alpha"
colnames(betas)[2]="beta"

alpha_emv=alphas
beta_emv=betas
for (j in 1:10){
  for (i in 1:20){
    aux1=dados[dados$mandante==alpha_emv[i,1],]
    aux2=beta_emv[beta_emv$visitante!=alpha_emv[i,1],]
    alpha_emv[i,2]=sum(aux1$gols_mandante)/sum(aux2$beta)
  }
  for (i in 1:20){
    aux1=dados[dados$visitante==beta_emv[i,1],]
    aux2=alpha_emv[alpha_emv$mandante!=beta_emv[i,1],]
    beta_emv[i,2]=sum(aux1$gols_mandante)/sum(aux2$alpha)
  }}

#parametros gamma e delta

#gols tomados por mandantes
aggregate(gols_visitante ~ mandante, data=dados, sum)
#gols marcados por visitantes
aggregate(gols_visitante ~ visitante, data=dados, sum)

Sy=sum(dados$gols_visitante)

gammas=aggregate(gols_visitante ~ mandante, data=dados, sum)
gammas$gols_visitante=gammas$gols_visitante/sqrt(Sy)
deltas=aggregate(gols_visitante ~ visitante, data=dados, sum)
deltas$gols_visitante=deltas$gols_visitante/sqrt(Sy)
colnames(gammas)[2]="gamma"
colnames(deltas)[2]="delta"

gamma_emv=gammas
delta_emv=deltas

for (j in 1:10){
  for (i in 1:20){
    aux1=dados[dados$mandante==gamma_emv[i,1],]
    aux2=delta_emv[delta_emv$visitante!=gamma_emv[i,1],]
    gamma_emv[i,2]=sum(aux1$gols_visitante)/sum(aux2$delta)
  }
  for (i in 1:20){
    aux1=dados[dados$visitante==delta_emv[i,1],]
    aux2=gamma_emv[gamma_emv$mandante!=delta_emv[i,1],]
    delta_emv[i,2]=sum(aux1$gols_visitante)/sum(aux2$gamma)
  }
} 

final=cbind(alpha_emv,beta_emv$beta,gamma_emv$gamma,delta_emv$delta)
final




#inter x fluminense
#xij gols de inter x flu, poisson com media alpha_i x beta_j
#yij gols do flu x inter fora de casa, poisson com media gamma_i x delta_j

lambda1=final[15,2]*final[11,3]
lambda2=final[15,4]*final[11,5]
#calculando as probabilidades
max_gols = 10 
probabilidades <- dpois(0:max_gols, lambda1) %*% t(dpois(0:max_gols, lambda2))
prob_mandante <- sum(probabilidades[lower.tri(probabilidades)])
prob_empate <- sum(diag(probabilidades))
prob_visitante <- sum(probabilidades[upper.tri(probabilidades)])

#MODELO 2 MAHER

eta2=sum(dados$gols_visitante)/sum(dados$gols_mandante)

S=sum(dados$gols_mandante)+sum(dados$gols_visitante)

gols_mandante=aggregate(gols_mandante ~ mandante, data=dados, sum)
gols_visitante=aggregate(gols_visitante ~ visitante, data=dados, sum)

alphas1=(gols_mandante$gols_mandante+gols_visitante$gols_visitante)/((1+eta2)*sqrt(S))
alphas=gols_mandante
alphas$gols_mandante=alphas1
colnames(alphas)[2]="alpha"

gols_mandante_sofridos=aggregate(gols_visitante ~ mandante, data=dados, sum)
gols_visitante_sofridos=aggregate(gols_mandante ~ visitante, data=dados, sum)

betas1=(gols_mandante_sofridos$gols_visitante+gols_visitante_sofridos$gols_mandante)/((1+eta2)*sqrt(S))
betas=gols_mandante_sofridos
betas$gols_visitante=betas1
colnames(betas)[2]="beta"

alpha_emv=alphas
beta_emv=betas

for (j in 1:10){
  for (i in 1:20){
    aux1=dados[dados$mandante==alpha_emv[i,1],]
    aux2=dados[dados$visitante==alpha_emv[i,1],]
    aux3=beta_emv[beta_emv$mandante!=alpha_emv[i,1],]
    alpha_emv[i,2]=(sum(aux1$gols_mandante)+sum(aux2$gols_visitante))/((1+eta2)*sum(aux3$beta))
  }
  
  
  for (i in 1:20){
    aux1=dados[dados$visitante==beta_emv[i,1],]
    aux2=dados[dados$mandante==beta_emv[i,1],]
    aux3=alpha_emv[alpha_emv$mandante!=beta_emv[i,1],]
    beta_emv[i,2]=(sum(aux1$gols_mandante)+sum(aux2$gols_visitante))/((1+eta2)*sum(aux3$alpha))
  } 
}

final=cbind(alpha_emv,beta_emv$beta)
final$forca=final[,2]/final[,3]
final1=final[order(final$forca,decreasing=TRUE),]

final1$posicao=1:20
final1$posicao_campeonato=c(1,2,5,3,7,6,4,16,9,10,8,12,13,11,14,17,18,15,20,19)
final1

eta2

#inter x flu
#xij gols do inter x flu, poisson com media alpha_i x beta_j
#yij gols do flu x inter fora de casa, poisson com media gamma_i x delta_j

lambda1=final[15,2]*final[11,3]
lambda2=final[11,2]*final[15,3]*eta2

#calculando as probabilidades

max_gols = 10 
probabilidades <- dpois(0:max_gols, lambda1) %*% t(dpois(0:max_gols, lambda2))
prob_mandante <- sum(probabilidades[lower.tri(probabilidades)])
prob_empate <- sum(diag(probabilidades))
prob_visitante <- sum(probabilidades[upper.tri(probabilidades)])


#cálculo das probabilidades do campeao, por simulacao

jogos_restantes <- read.csv('C:\\Users\\danie\\Documents\\minicurso_futebol\\jogos_restantes.csv')



a0=matrix(0,11,1)
a1=matrix(1,11,1)
a2=matrix(2,11,1)
a3=matrix(3,11,1)
a4=matrix(4,11,1)
a5=matrix(5,11,1)
a6=matrix(6,11,1)
a7=matrix(7,11,1)
a8=matrix(8,11,1)
a9=matrix(9,11,1)
a10=matrix(10,11,1)
res1=c(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
seq=0:10
res2=c(seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq)

simulacoes=1000
campeao=matrix(1,simulacoes,2)
for (j in 1:simulacoes){
resultados_simulados=jogos_restantes
for (i in 1:73){
lambda1=final[resultados_simulados$cod_mandante[i],2]*final[resultados_simulados$cod_visitante[i],3]
lambda2=final[resultados_simulados$cod_visitante[i],2]*final[resultados_simulados$cod_mandante[i],3]*eta2
prob_resultado=dpois(res1, lambda1)*dpois(res2,lambda2)
simulacao=sample(x = 1:121, 1, replace = T, prob = prob_resultado) 
resultados_simulados$simul_mandante[i]=res1[simulacao]
resultados_simulados$simul_visitante[i]=res2[simulacao]}

mandante=c(resultados_simulados$cod_mandante,dados$cod_mandante)
visitante=c(resultados_simulados$cod_visitante,dados$cod_visitante)
gols_feitos_mandante=c(resultados_simulados$simul_mandante,dados$gols_mandante)
gols_feitos_visitante=c(resultados_simulados$simul_visitante,dados$gols_visitante)
gols_tomados_mandante=c(resultados_simulados$simul_visitante,dados$gols_visitante)
gols_tomados_visitante=c(resultados_simulados$simul_mandante,dados$gols_mandante)
pontos_mandante=matrix(0,380,1)
pontos_visitante=matrix(0,380,1)
vitoria_mandante=matrix(0,380,1)
vitoria_visitante=matrix(0,380,1)

for (i in 1:380){
  if (gols_feitos_mandante[i]>gols_feitos_visitante[i]){
    pontos_mandante[i]=3
    vitoria_mandante[i]=1}
  if (gols_feitos_mandante[i]<gols_feitos_visitante[i]){
    pontos_visitante[i]=3
    vitoria_visitante[i]=1}
  if (gols_feitos_mandante[i]==gols_feitos_visitante[i]){
    pontos_mandante[i]=1
    pontos_visitante[i]=1
  }
}

times=c(mandante,visitante)
gols_feitos=c(gols_feitos_mandante,gols_feitos_visitante)
gols_tomados=c(gols_tomados_mandante,gols_tomados_visitante)
pontos=c(pontos_mandante,pontos_visitante)
vitoria=c(vitoria_mandante,vitoria_visitante)
resultados_finais=data.frame(times,gols_feitos,gols_tomados,pontos,vitoria)

tabela_final=aggregate(cbind(pontos,vitoria,gols_feitos,gols_tomados) ~ times, resultados_finais, sum)
tabela_final$saldo=tabela_final$gols_feitos-tabela_final$gols_tomados
tabela_final1=tabela_final[order(tabela_final$pontos,
        tabela_final$vitoria,tabela_final$saldo,decreasing=TRUE),]
head(tabela_final1)
campeao[j]=tabela_final1[1,1]}

campeao1=data.frame(campeao)
campeao2=aggregate(X2 ~ X1, campeao1, sum)
campeao2[order(campeao2$X2,decreasing=TRUE),]
