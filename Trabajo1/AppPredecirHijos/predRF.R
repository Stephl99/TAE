load("./modeloB.RData")
load("./df_aux.RData")

predRF = function(eval) {
  df_aux2 <- rbind(df_aux, eval)
  prediction <- predict(modelo, df_aux2[2, 1:9])
  return(prediction)
}

# Dummies
# iolore <- c(500000, 500000, 500000, 5, 4, 1, 1, 7, 1, 0)
# iolore2 <- c(100000, 100000, 100000, 1, 3, 2, 1, 0, 2, 1)
# predRF(iolore2)
