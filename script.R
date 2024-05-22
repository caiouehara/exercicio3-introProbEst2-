n <- 5
s <- 3.011
alpha = 0.06
mi <- 12
mi_amostra <- 12.3
d <- mi_amostra - mi
z <- (d*sqrt(n))/s

x1 <- seq(-4,10,by=.1)
y1 <- dt(x1, n-1)

plot(x1,y1,
     type="l", 
     xlab="Valores de t", 
     ylab="Probabilidade", 
     main="Poder do teste bilateral",
     col="red"
     )

t_alpha <- qt(1-alpha,n-1)

polygon(c(t_alpha, seq(t_alpha,10,by=.1) ,10),
        c(0, dt(seq(t_alpha,10,by=.1), n-1),0),
        density=70, col = "blue")

lines(x1, dt(x1,n-1,d*sqrt(n)/s),lwd=2,col='green')

polygon(c(-4, seq(-4, t_alpha,by=.1), t_alpha),
        c(0, dt(seq(-4, t_alpha,by=.1), n-1, z), 0),
        density=70, col = "orange")

beta <- pt(t_alpha, n-1, z)
poder <- 1-beta
poder

power.t.test(n=n,
             delta = d,
             sd = s,
             sig.level = alpha,
             type = "one.sample",
             alternative = "one.sided")
