amostra = c(14.4,12.9,7,13.7,13.5)

#Cálculos
n <- length(amostra)
s <- sd(amostra)
t <- mean(amostra)

mu <- 12
d <- t - mu
alpha <- 0.02

z <- (d) / (s/sqrt(n- 1))
t <- qt(1-alpha,n-1)

#Distribuicao Padrao t-Student(n-1)
x1 <- seq(-4,4,by=.1) #eixo dos quantis
y1 <- dt(x1, n-1) #eixo das distribuições

plot(x1,y1,
     type="l", 
     xlab="Valores de t", 
     ylab="Probabilidade", 
     main="Poder do teste bilateral",
     col="red"
)

#Distribuição da amostra
lines(x1, dt(x1, z, df=n-1),
      lwd=2,
      col='green')

#Graficando erro do tipo 1 (alpha)
polygon(c(t, seq(t, 4,by=.1) ,4), 
        c(0, dt(seq(t,4,by=.1), n-1),0),
        density=70,
        col = "blue")

#Graficando erro do tipo 2 (beta)
polygon(c(-4, seq(-4, t,by=.1), t),
        c(0, dt(seq(-4, t,by=.1), n-1, z), 0),
        density=70, col = "orange")

#Cálculo do Poder
beta <- pt(t, n-1, d*sqrt(n)/s)
poder = 1-beta
poder

#Função verificação (nativa do R)
power.t.test(n = 5,
             delta = d,
             sd = s,
             sig.level = alpha,
             type = "one.sample",
             alternative = "one.sided")