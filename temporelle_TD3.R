# Chargement des bibliothèques
library(forecast)
library(TSA)
library(ggplot2)
# Chauqe modèle partie 1 ----
# Fonction pour afficher la série et ses ACF/PACF
plot_series_acf_pacf <- function(series, title) {

  # Tracé de la série
  plot(series, type="l", main=paste("Série simulée -", title), col="blue", ylab="Valeurs", xlab="Temps")

  # Autocorrélogramme (ACF)
  acf(series, main="ACF", col="red")

  # Autocorrélogramme partiel (PACF)
  pacf(series, main="PACF", col="darkgreen")

  par(mfrow=c(1,1))  # Reset affichage
}

set.seed(123)  # Assurer la reproductibilité

### 🔹 Modèle 1 : ARMA saisonnier (périodicité 4)
Y_t1 <- arima.sim(n = 500,
                  model = list(ar = c(0.4), ma = c(0.3, 0.3),
                               seasonal = list(ar = c(0.4), ma = c(0.3), period = 4))) # car  L^4

# Affichage
plot_series_acf_pacf(Y_t1, "Modèle 1 (ARMA saisonnier, périodicité 4)")

### 🔹 Modèle 2 : ARMA saisonnier (périodicité 6)
Y_t2 <- arima.sim(n = 500,
                  model = list(ar = c(0.4), ma = c(0.4),
                               seasonal = list(ar = c(0.4), ma = c(0.4), period = 6)))

# Affichage
plot_series_acf_pacf(Y_t2, "Modèle 2 (ARMA saisonnier, périodicité 6)")

### 🔹 Modèle 3 : AR(2) avec saisonnalité annuelle (périodicité 12)
Y_t3 <- arima.sim(n = 500,
                  model = list(ar = c(0.6, -0.3),
                               seasonal = list(ma = c(0.8), period = 12)))

# Affichage
plot_series_acf_pacf(Y_t3, "Modèle 3 (AR(2) saisonnier, périodicité 12)")



# --- Consommation de gaz naturel en France ---

# Import des données
data_gaz <- load("gas_consumption.RData")


# Transformation en série temporelle
gaz_ts <- ts(data_gaz$consumption, start = c(2008, 1), frequency = 12)

# Affichage de la série temporelle
plot.ts(gaz_ts, main="Consommation de gaz naturel en France (2008-2024)", ylab="Consommation", col="blue")







