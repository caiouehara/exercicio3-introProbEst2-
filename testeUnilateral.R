n <- 5
s <- 3.011
alpha <- 0.06
mi <- 12
mi_amostra <- 12.3
d <- mi_amostra - mi
z <- (d * sqrt(n)) / s

# Range of t-values
x1 <- seq(-4, 10, by = 0.1)
y1 <- dt(x1, n - 1)

# Plotting the t-distribution
plot(x1, y1,
     type = "l", 
     xlab = "Valores de t", 
     ylab = "Probabilidade", 
     main = "Poder do teste e tamanho amostral",
     col = "red"
)

# Critical t-value for a one-sided test
t_alpha <- qt(1 - alpha, n - 1)

# Shading the rejection region for a one-sided test
polygon(c(t_alpha, seq(t_alpha, 10, by = 0.1), 10),
        c(0, dt(seq(t_alpha, 10, by = 0.1), n - 1), 0),
        density = 70, col = "blue"
)

# Overlaying the non-central t-distribution
lines(x1, dt(x1, n - 1, ncp = d * sqrt(n) / s), lwd = 2, col = 'green')

# Shading the acceptance region under the non-central t-distribution
polygon(c(-4, seq(-4, t_alpha, by = 0.1), t_alpha),
        c(0, dt(seq(-4, t_alpha, by = 0.1), n - 1, ncp = z), 0),
        density = 70, col = "orange"
)

# Calculate beta and power
beta <- pt(t_alpha, n - 1, ncp = z)
poder <- 1 - beta
poder

# Using power.t.test function for a one-sided test
power.t.test(n = n,
             delta = d,
             sd = s,
             sig.level = alpha,
             type = "one.sample",
             alternative = "one.sided")
