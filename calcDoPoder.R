# Amostra
amostra <- c(14.4, 12.9, 7, 13.7, 13.5)

# Cálculos
n <- length(amostra)
s <- sd(amostra)
t <- mean(amostra)

mu <- 12
d <- t - mu
alpha <- 0.05

# Função para cálculo de Poder
poderT <- function(n, alpha, type){
  t_alpha <- if(type == "unilateral") 
    qt(1 - alpha*2, n - 1) else 
      qt(1 - alpha, n - 1)
  z <- d / s * sqrt(n)
  beta <- pt(t_alpha, n - 1, ncp = z)
  return(1 - beta)
}

# Gráficos do Poder 
curve(poderT(x, alpha, "unilateral"),
      from = 1,
      to = 30,
      col = "green",
      lwd = 2,
      ylabel="Poder",
      xlabel="n",
      main="Curva de poder do teste",
      ylim = c(0, 0.3) #Ajuste do gráfico em Y
)

curve(poderT(x, alpha, "bilateral"),
      from = 1,
      to = 30,
      col = "blue",
      lwd = 2,
      add = TRUE
)

#Legenda
legend("bottomright", 
       legend = c("Unilateral", "Bilateral"),
       col = c("green", "blue"), lwd = 2)

# Cálculo do poder usando power.t.test
power.t.test(n = 5,
             delta = d,
             sd = s,
             sig.level = alpha,
             type = "one.sample",
             alternative = "one.sided")
