load("./df_aux.RData")
modelo <- readRDS(file = "./definitive_model.rds")

predRF = function(eval) {
  df_aux2 <- rbind(df_aux, eval)
  prediction <- predict(modelo, df_aux2[2, 1:8])
  return(prediction)
}

# Dummies
# iolore <- c(500000, 500000, 500000, 4, 1, 1, 7, 1, 0)
# iolore2 <- c(100000, 100000, 100000, 3, 2, 1, 0, 2, 0)
# iolore3 <- c(1200000, 1200000, 1200000, 4, 2, 2, 4, 2, 0)
# predRF(iolore2)
