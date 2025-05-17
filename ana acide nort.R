# 1. Criando o dataframe manualmente
acidentes_nordeste <- data.frame(
  ESTADO = c("AL", "SE", "PE", "PB", "RN", "CE", "PI", "MA", "BA"),
  ACIDENTES = c(3058, 3021, 22758, 12308, 7922, 10848, 14025, 11628, 32284)
)

# 2. Visualizando os dados
print(acidentes_nordeste)

# 3. Estatísticas descritivas básicas
cat("\n=== ESTATÍSTICAS DESCRITIVAS ===\n")


# Quartis e extremos
cat("\n\nQuartis:\n")
quantile(acidentes_nordeste$ACIDENTES)

# 4. Análise por estado (ordenado)
cat("\n\nEstados ordenados por número de acidentes:\n")
acidentes_ordenados <- acidentes_nordeste[order(-acidentes_nordeste$ACIDENTES), ]
print(acidentes_ordenados)

# 5. Visualização gráfica

library(ggplot2)

# Gráfico de barras
ggplot(acidentes_ordenados, aes(x = reorder(ESTADO, -ACIDENTES), y = ACIDENTES)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Acidentes no Nordeste",
       x = "Estado",
       y = "Número de Acidentes") +
  theme_minimal()

# Boxplot para mostrar a distribuição
ggplot(acidentes_nordeste, aes(y = ACIDENTES)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribuição de Acidentes no Nordeste",
       y = "Número de Acidentes") +
  theme_minimal()

# Análise adicional - Proporção do total
acidentes_nordeste$PROPORCAO <- round(acidentes_nordeste$ACIDENTES/sum(acidentes_nordeste$ACIDENTES)*100, 1)
cat("\n\nProporção em relação ao total:\n")
print(acidentes_nordeste[, c("ESTADO", "PROPORCAO")])


# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(knitr)


# Criar dataframe com os dados fornecidos
acidentes_nordeste <- data.frame(
  ESTADO = c("AL", "SE", "PE", "PB", "RN", "CE", "PI", "MA", "BA"),
  ACIDENTES = c(3058, 3021, 22758, 12308, 7922, 10848, 14025, 11628, 32284)
)

# Calcular estatísticas descritivas com intervalos de confiança
estatisticas <- acidentes_nordeste %>%
  summarise(
    media_acidentes = mean(ACIDENTES),
    sd_acidentes = sd(ACIDENTES),
    n = n(),
    se = sd_acidentes / sqrt(n),
    ci_lower = media_acidentes - qt(0.975, n-1) * se,
    ci_upper = media_acidentes + qt(0.975, n-1) * se
  )

# Visualização da média de acidentes por estado com intervalos de confiança
ggplot(acidentes_nordeste, aes(x = reorder(ESTADO, -ACIDENTES), y = ACIDENTES)) +
  geom_bar(aes(y = ACIDENTES, fill = ESTADO), stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = ACIDENTES * 0.9, ymax = ACIDENTES * 1.1), 
                width = 0.2, color = "black") +
  geom_text(aes(y = ACIDENTES, label = ACIDENTES), 
            vjust = -0.5, color = "black", size = 3.5) +
  labs(
    title = "Número de Acidentes por Estado no Nordeste",
    x = "Estado",
    y = "Número de Acidentes"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25)
  ) +
  scale_fill_brewer(palette = "Set1")

# Tabela descritiva
acidentes_nordeste 
  mutate(
    Proporcao = round(ACIDENTES/sum(ACIDENTES)*100, 2),
    Desvio_Media = round(ACIDENTES - mean(ACIDENTES), 2)
  ) %>%
  arrange(desc(ACIDENTES)) %>%
  kable(
    caption = "Estatísticas de Acidentes por Estado no Nordeste",
    col.names = c("Estado", "N° Acidentes", "% do Total", "Diferença da Média"),
    align = c("l", "c", "c", "c"),
    format.args = list(big.mark = ".")
  )
