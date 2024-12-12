# Instalar e carregar as bibliotecas necessárias
# Descomente a linha abaixo para instalar as bibliotecas caso ainda não estejam instaladas
# install.packages(c("tidyverse"))

library(tidyverse)  # Inclui ggplot2, dplyr, readr, entre outros

# Ignorar avisos
options(warn = -1)

# Configurar o estilo dos gráficos para ggplot2
theme_set(theme_bw())  # Define o tema como "Black and White"

# Carregar o conjunto de dados Titanic diretamente do repositório do Seaborn
titanic <- read_csv("https://raw.githubusercontent.com/mwaskom/seaborn-data/master/titanic.csv")

# Exibir as primeiras linhas do conjunto de dados
head(titanic)

# Plota um histograma das idades dos passageiros
ggplot(titanic, aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  labs(
    title = "Distribuição de Idades dos Passageiros do Titanic",
    x = "Idade",
    y = "Número de Passageiros"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


# Criar um boxplot da tarifa por classe de passageiro
ggplot(titanic, aes(x = factor(pclass), y = fare, fill = factor(pclass))) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribuição da Tarifa por Classe de Passageiro",
    x = "Classe de Passageiro",
    y = "Tarifa"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


# Plota um violin plot da idade por gênero
ggplot(titanic, aes(x = sex, y = age, fill = sex)) +
  geom_violin(trim = FALSE, palette = "Pastel1") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Distribuição de Idades por Gênero",
    x = "Gênero",
    y = "Idade"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"  # Remove a legenda, já que a cor está mapeada para 'sex'
  )



# Plota um scatterplot da idade vs. tarifa por sobrevivência
ggplot(titanic, aes(x = age, y = fare, color = factor(survived))) +
  geom_point(alpha = 0.6, size = 3) +  # Adiciona os pontos com transparência e tamanho definidos
  scale_color_brewer(palette = "RdBu", labels = c("Não Sobreviveu", "Sobreviveu")) +  # Define a paleta de cores
  labs(
    title = "Idade vs. Tarifa por Sobrevivência",
    x = "Idade",
    y = "Tarifa",
    color = "Sobreviveu"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centraliza e formata o título
    axis.title = element_text(size = 14),  # Define o tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),   # Define o tamanho dos textos dos eixos
    legend.title = element_text(size = 14),  # Define o tamanho do título da legenda
    legend.text = element_text(size = 12)    # Define o tamanho dos textos da legenda
  )


# Instalar e carregar as bibliotecas necessárias
# Descomente a linha abaixo para instalar as bibliotecas caso ainda não estejam instaladas
# install.packages(c("tidyverse", "gridExtra"))

library(gridExtra)    # Para organizar múltiplos gráficos em uma única visualização

# Definir a semente para reprodutibilidade
set.seed(42)

# Número de amostras
n <- 100

# Correlação Positiva
x_pos <- rnorm(n, mean = 50, sd = 10)
y_pos <- x_pos + rnorm(n, mean = 0, sd = 10)

# Correlação Negativa
x_neg <- rnorm(n, mean = 50, sd = 10)
y_neg <- -x_neg + rnorm(n, mean = 0, sd = 10)

# Sem Correlação
x_none <- rnorm(n, mean = 50, sd = 10)
y_none <- rnorm(n, mean = 50, sd = 10)

# Calcular os coeficientes de correlação de Pearson
corr_pos <- cor(x_pos, y_pos, method = "pearson")
corr_neg <- cor(x_neg, y_neg, method = "pearson")
corr_none <- cor(x_none, y_none, method = "pearson")

# Exibir os coeficientes de correlação
cat(sprintf("Coeficiente de Pearson (Positiva): %.2f\n", corr_pos))
cat(sprintf("Coeficiente de Pearson (Negativa): %.2f\n", corr_neg))
cat(sprintf("Coeficiente de Pearson (Sem Correlação): %.2f\n", corr_none))

# Criar dataframes para cada conjunto de dados
df_pos <- data.frame(Variavel_X = x_pos, Variavel_Y = y_pos)
df_neg <- data.frame(Variavel_X = x_neg, Variavel_Y = y_neg)
df_none <- data.frame(Variavel_X = x_none, Variavel_Y = y_none)

# Criar o Scatterplot com Correlação Positiva
plot_pos <- ggplot(df_pos, aes(x = Variavel_X, y = Variavel_Y)) +
  geom_point(color = "green", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(
    title = sprintf("Correlação Positiva\nr = %.2f", corr_pos),
    x = "Variável X",
    y = "Variável Y"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Criar o Scatterplot com Correlação Negativa
plot_neg <- ggplot(df_neg, aes(x = Variavel_X, y = Variavel_Y)) +
  geom_point(color = "red", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = sprintf("Correlação Negativa\nr = %.2f", corr_neg),
    x = "Variável X",
    y = "Variável Y"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Criar o Scatterplot sem Correlação
plot_none <- ggplot(df_none, aes(x = Variavel_X, y = Variavel_Y)) +
  geom_point(color = "blue", alpha = 0.6, size = 3) +
  labs(
    title = sprintf("Sem Correlação\nr = %.2f", corr_none),
    x = "Variável X",
    y = "Variável Y"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Organizar os três plots lado a lado
grid.arrange(plot_pos, plot_neg, plot_none, ncol = 3)


#################################
### 6.ESTATÍSTICA DESCRITIVA ####
#################################

# Seleciona a coluna idade e remove valores ausentes
age <- titanic$age[!is.na(titanic$age)]

# Função para calcular a moda
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calcula as estatísticas
mean_age <- mean(age)
median_age <- median(age)
mode_age <- get_mode(age)
std_dev_age <- sd(age)
range_age <- max(age) - min(age)
mean_deviation_age <- mean(abs(age - mean_age))  # Cálculo manual do desvio médio

# Exibe os resultados
cat(sprintf("Média de Idade: %.2f\n", mean_age))
cat(sprintf("Mediana de Idade: %.2f\n", median_age))
cat(sprintf("Moda de Idade: %.2f\n", mode_age))
cat(sprintf("Desvio Padrão da Idade: %.2f\n", std_dev_age))
cat(sprintf("Amplitude de Idade: %.2f\n", range_age))
cat(sprintf("Desvio Médio da Idade: %.2f\n", mean_deviation_age))


#################################
######## 7. QUARTIS #############
#################################

# Define os rótulos dos quartis
quartile_labels <- c("Q1", "Q2", "Q3", "Q4")

# Cria a nova coluna 'fare_quartile' usando a função cut() com quebras baseadas nos quartis
titanic <- titanic %>%
  mutate(
    fare_quartile = cut(
      fare,
      breaks = quantile(fare, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
      include.lowest = TRUE,
      labels = quartile_labels
    )
  )

# Exibe as primeiras linhas com a nova coluna
head(titanic %>% select(fare, fare_quartile))


#################################
##### 8. GRÁFICO DE BARRAS ######
#################################

# Plota um gráfico de barras da contagem de passageiros por classe
ggplot(titanic, aes(x = factor(pclass), fill = factor(pclass))) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Número de Passageiros por Classe",
    x = "Classe de Passageiro",
    y = "Contagem",
    fill = "Classe de Passageiro"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


#################################
##### 9. GRÁFICO DE PIZZA #######
#################################

# Calcula as contagens de sobrevivência
survival_counts <- titanic %>%
  filter(!is.na(survived)) %>%  # Remove valores ausentes na coluna 'survived'
  count(survived)

# Define os rótulos e as cores dos quartis
labels <- c("Não Sobreviveu", "Sobreviveu")
colors <- c("lightcoral", "lightskyblue")

# Adiciona os rótulos personalizados ao dataframe
survival_counts <- survival_counts %>%
  mutate(
    label = labels[survived + 1]  # 'survived' é 0 ou 1; adicionamos 1 para indexar corretamente
  )

# Plota um gráfico de pizza
ggplot(survival_counts, aes(x = "", y = n, fill = label)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +
  labs(
    title = "Taxa de Sobrevivência no Titanic",
    fill = "Sobreviveu"
  ) +
  theme_void() +  # Remove eixos e fundo
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
