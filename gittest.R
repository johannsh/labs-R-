# this code str from rstudio

# Представлен временной ряд прибыли, полученной промышленным
# предприятием (в млн. руб.) по месяцам с 2007 по 2022 гг. Необходимо
# построить прогнозную модель на основе имеющихся данных и осуществить
# прогнозирование прибыли предприятия на январь-март 2023

# гружу данные
df_lr <- read.table(
  "C:/Users/morfe/Мой диск/EDUCATION/КТАиОД/7 вариант/l.r.2.csv", header = TRUE,
  sep = ";", fileEncoding = "windows-1251"
)

# выцепляю данные с нужного столбца "7 вариант"
tsData <- df_lr[,10]

# ставtsData# ставлю либы
install.packages("tseries")
install.packages("forecast")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("broom")
install.packages("ggpubr")
install.packages("prophet")
install.packages("ollamar")

# подключаю либы
library(tseries)
library(forecast)
library(ggplot2)
library(gridExtra)
library(broom)
library(ggpubr)
library(prophet)
library(ollamar)

# преобразую массив в ts, с частотой 12, начиная с 2007-01-01
tsData <- ts(tsData, frequency = 12, start = c(2007,1))

# создаем датафрейм
df_ts <- data.frame(
  Date = seq(as.Date("2007-01-01"), as.Date("2022-12-01"), by = "month"),
  Value = as.numeric(tsData)
)

# строим график ВР
ggplot(df_ts, aes(x = Date, y = Value)) +
  geom_line(color = "mediumspringgreen", size = 1) +
  labs(x = "Время, год", y = "Прибыль, млн. руб.") +
  scale_x_date(
    breaks = as.Date(c("2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01",
                       "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01",
                       "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
                       "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01")),
    labels = c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
               "2015", "2016","2017", "2018", "2019", "2020", "2021", "2022",
               "2023"),
    limits = as.Date(c("2007-01-01", "2023-01-01"))
  ) +
  theme_minimal()

# декомпозирую ВР
ts_data_comp <- decompose(tsData, type = "additive", filter = NULL)

decomposed_df <- data.frame(
  Date = seq(as.Date("2007-01-01"), by = "month", length.out = length(tsData)),
  Original = as.numeric(tsData),
  Trend = as.numeric(ts_data_comp$trend),
  Seasonal = as.numeric(ts_data_comp$seasonal),
  Random = as.numeric(ts_data_comp$random)
)

# График для оригинального ряда
p1 <- ggplot(decomposed_df, aes(x = Date)) +
  geom_line(aes(y = Original, color = "Original"), size = 1) +
  labs(x = "Дата", y = "Original") +
  scale_color_manual(values = c("Original" = "blue")) +
  scale_x_date(
    breaks = as.Date(c("2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01",
                       "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01",
                       "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
                       "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01")),
    labels = c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
               "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
               "2023"),
    limits = as.Date(c("2007-01-01", "2023-01-01"))
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# График для тренда
p2 <- ggplot(decomposed_df, aes(x = Date)) +
  geom_line(aes(y = Trend, color = "Trend"), size = 1) +
  labs(x = "Дата", y = "Trend") +
  scale_color_manual(values = c("Trend" = "red")) +
  scale_x_date(
    breaks = as.Date(c("2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01",
                       "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01",
                       "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
                       "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01")),
    labels = c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
               "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
               "2023"),
    limits = as.Date(c("2007-01-01", "2023-01-01"))
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# График для сезонной составляющей
p3 <- ggplot(decomposed_df, aes(x = Date)) +
  geom_line(aes(y = Seasonal, color = "Seasonal"), size = 1) +
  labs(x = "Дата", y = "Seasonal") +
  scale_color_manual(values = c("Seasonal" = "green")) +
  scale_x_date(
    breaks = as.Date(c("2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01",
                       "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01",
                       "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
                       "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01")),
    labels = c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
               "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
               "2023"),
    limits = as.Date(c("2007-01-01", "2023-01-01"))
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# График для остаточной составляющей
p4 <- ggplot(decomposed_df, aes(x = Date)) +
  geom_line(aes(y = Random, color = "Random"), size = 1) +
  labs(x = "Дата", y = "Random") +
  scale_color_manual(values = c("Random" = "purple")) +
  scale_x_date(
    breaks = as.Date(c("2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01",
                       "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01",
                       "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
                       "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01")),
    labels = c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
               "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
               "2023"),
    limits = as.Date(c("2007-01-01", "2023-01-01"))
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Размещение графиков один над другим
grid.arrange(p1, p2, p3, p4, ncol = 1)

# строю график ACF
ggAcf(tsData) +
  geom_segment(color = "mediumspringgreen", size = 1.2) +
  ggtitle("Автокорреляционная функция (ACF)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# строю график PACF
ggPacf(tsData) +
  geom_segment(color = "slateblue", size = 1.2) +
  ggtitle("Автокорреляционная функция (ACF)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# идентификация тренда
# модель логистической кривой
start_params <- list(k = max(decomposed_df$Original, na.rm = TRUE), a = 1,
                     b = 0.1)


# создание логистической модели
logistic_model <- nls(Original ~ k / (1 + a * exp(-b * (1:nrow(decomposed_df))))
                      , data = decomposed_df, start = start_params)
summary(logistic_model)

# Получение предсказанных значений
decomposed_df$Logistic_Trend <- predict(logistic_model)

diagnostic_data <- augment(logistic_model)

# Q-Q Plot of Residuals
p1 <- ggplot(diagnostic_data, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
  )

# Residuals vs Fitted
p2 <- ggplot(diagnostic_data, aes(x = .fitted, y = .resid)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
  )

# Scale-Location (Residuals vs. Fitted)
p3 <- ggplot(diagnostic_data, aes(x = .fitted, y = sqrt(abs(.resid)))) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Scale-Location Plot", x = "Fitted Values",
       y = "Sqrt(|Residuals|)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
  )

ggarrange(p1, p2, p3, ncol = 1, nrow = 3)

# график остатков
residuals_df <- data.frame(
  Index = 1:length(r2),
  Residuals = r2
)

ggplot(residuals_df, aes(x = Index, y = Residuals)) +
  geom_point(color = "skyblue1", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "График остатков", x = "Индекс", y = "Остатки") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# расчет точности модели (хар-ки остатков)
# Предсказанные значения
fitted_values <- fitted(logistic_model)

# фактические значения
actual_values <- decomposed_df$Original

# остатки
residuals <- actual_values - fitted_values

# ME - средняя ошибка
me <- mean(residuals, na.rm = TRUE)

# MSE - средняя квадратичная ошибка
mse <- mean(residuals^2, na.rm = TRUE)

# RMSE - среднеквадратичная ошибка
rmse <- sqrt(mse)

# MAE - средняя абсолютная ошибка
mae <- mean(abs(residuals), na.rm = TRUE)

# MPE - средняя процентная ошибка
mpe <- mean(residuals / actual_values, na.rm = TRUE) * 100

# MAPE - средняя абсолютная процентная ошибка
mape <- mean(abs(residuals / actual_values), na.rm = TRUE) * 100

# MASE - средняя абсолютная масштабированная ошибка
naive_residuals <- diff(actual_values)
mae_naive <- mean(abs(naive_residuals), na.rm = TRUE)
mase <- mae / mae_naive

# R^2 - коэффициент детерминации
ss_total <- sum((actual_values - mean(actual_values, na.rm = TRUE))^2,
                na.rm = TRUE)
ss_residual <- sum(residuals^2, na.rm = TRUE)
r_squared <- 1 - (ss_residual / ss_total)

# получаем минимальные и максимальные остатки
min_residual <- min(residuals)
max_residual <- max(residuals)


# вывод результатов
accuracy_metrics <- data.frame(
  ME = me,
  MSE = mse,
  RMSE = rmse,
  MAE = mae,
  MPE = mpe,
  MAPE = mape,
  MASE = mase,
  R_squared = r_squared,
  Min_Residual = min_residual,
  Max_Residual = max_residual
)

print(accuracy_metrics)

# наложение предсказаний на исходный ВР
plot_data <- data.frame(
  Date = decomposed_df$Date,
  Original = decomposed_df$Original,
  Predicted = decomposed_df$Logistic_Trend
)

ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Original, color = "Оригинальный ряд"), size = 1) +
  geom_line(aes(y = Predicted, color = "Предсказанная модель"), size = 1,
            linetype = "dashed") +
  labs(title = "График ВР + Модель 1",
       x = "Дата",
       y = "Прибыль, млн. руб.") +
  scale_color_manual(values = c("Оригинальный ряд" = "mediumspringgreen",
                                "Предсказанная модель" = "red")) +
  scale_x_date(
    breaks = as.Date(c("2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01",
                       "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01",
                       "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
                       "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01")),
    labels = c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
               "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
               "2023"),
    limits = as.Date(c("2007-01-01", "2023-01-01"))
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

spectrum(residuals_df)

residuals_filtered <- residuals[residuals > 0]

# проверка, остались ли значения после фильтрации
if(length(residuals_filtered) > 0) {
  
  # построение периодограммы остатков
  periodogram_data <- spec.pgram(residuals_filtered, plot = FALSE)
  
  # преобразуем в data.frame для ggplot
  periodogram_df <- data.frame(Frequency = periodogram_data$freq, 
                               Spectrum = periodogram_data$spec)
  
  # построение графика
  ggplot(periodogram_df, aes(x = Frequency, y = Spectrum)) +
    geom_line(color = "palegreen2", size = 1) +
    labs(title = "Периодограмма остатков", x = "Частота", y = "Спектр") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

# строим acf и pacf для остатков

# строю график ACF
ggAcf(r2) +
  geom_segment(color = "mediumspringgreen", size = 1.2) +
  ggtitle("Автокорреляционная функция (ACF)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# строю график PACF
ggPacf(r2) +
  geom_segment(color = "slateblue", size = 1.2) +
  ggtitle("Частная втокорреляционная функция (PACF)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

acf(r2)

# модель сезонной структуры

start_params_2 <- list(k = max(decomposed_df$Original, na.rm = TRUE), 
                       a = 1, 
                       b = 0.1,
                       A = 10,
                       f = 1 / 12,
                       phi = 0) 

trend_season_model <- nls(
  Original ~ k / (1 + a * exp(-b * (1:nrow(decomposed_df)))) + 
    A * sin(2 * pi * f * (1:nrow(decomposed_df)) + phi),
  data = decomposed_df,
  start = start_params_2
)

# результаты модели
summary(trend_season_model)

# Получение предсказанных значений
decomposed_df$.fitted <- predict(trend_season_model)
decomposed_df$.resid <- decomposed_df$Original - decomposed_df$.fitted

# Q-Q Plot of Residuals
p1.1 <- ggplot(decomposed_df, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
  )

# Residuals vs Fitted
p2.2 <- ggplot(decomposed_df, aes(x = .fitted, y = .resid)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
  )

# Scale-Location (Residuals vs. Fitted)
p3.3 <- ggplot(decomposed_df, aes(x = .fitted, y = sqrt(abs(.resid)))) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Scale-Location Plot", x = "Fitted Values",
       y = "Sqrt(|Residuals|)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
  )

ggarrange(p1.1, p2.2, p3.3, ncol = 1, nrow = 3)

# график остатков
residuals_df_season <- data.frame(
  Index = 1:nrow(decomposed_df),
  Residuals = decomposed_df$.resid
)

ggplot(residuals_df_season, aes(x = Index, y = Residuals)) +
  geom_point(color = "skyblue1", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "График остатков", x = "Индекс", y = "Остатки") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# Предсказанные значения
fitted_values <- fitted(trend_season_model)  # предполагается, что модель называется trend_season_model

# Фактические значения
actual_values <- decomposed_df$Original  # исходные данные

# Остатки
residuals <- actual_values - fitted_values

# Средняя ошибка (ME)
me <- mean(residuals, na.rm = TRUE)

# Средняя квадратичная ошибка (MSE)
mse <- mean(residuals^2, na.rm = TRUE)

# Среднеквадратичная ошибка (RMSE)
rmse <- sqrt(mse)

# Средняя абсолютная ошибка (MAE)
mae <- mean(abs(residuals), na.rm = TRUE)

# Средняя процентная ошибка (MPE)
mpe <- mean(residuals / actual_values, na.rm = TRUE) * 100

# Средняя абсолютная процентная ошибка (MAPE)
mape <- mean(abs(residuals / actual_values), na.rm = TRUE) * 100

# Средняя абсолютная масштабированная ошибка (MASE)
naive_residuals <- diff(actual_values)  # разности для наивной модели
mae_naive <- mean(abs(naive_residuals), na.rm = TRUE)
mase <- mae / mae_naive

# Коэффициент детерминации (R^2)
ss_total <- sum((actual_values - mean(actual_values, na.rm = TRUE))^2, na.rm = TRUE)
ss_residual <- sum(residuals^2, na.rm = TRUE)
r_squared <- 1 - (ss_residual / ss_total)

# Минимальное и максимальное значения остатков
min_residual <- min(residuals, na.rm = TRUE)
max_residual <- max(residuals, na.rm = TRUE)

# Вывод результатов в таблицу
accuracy_metrics <- data.frame(
  ME = me,
  MSE = mse,
  RMSE = rmse,
  MAE = mae,
  MPE = mpe,
  MAPE = mape,
  MASE = mase,
  R_squared = r_squared,
  Min_Residual = min_residual,
  Max_Residual = max_residual
)

# Печать результатов
print(accuracy_metrics)

# наложение модели 2 на исходный ВР
decomposed_df$Trend_Season <- predict(trend_season_model,
                                      newdata = decomposed_df)

plot_data <- data.frame(
  Date = decomposed_df$Date,
  Original = decomposed_df$Original,
  Predicted_Trend_Season = decomposed_df$Trend_Season)



ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Original, color = "Оригинальный ряд"), size = 1) +
  geom_line(aes(y = Predicted_Trend_Season,
                color = "Модель 2 (Тренд + сезонность)"), 
            size = 1, linetype = "dashed") +
  labs(title = "График ВР + Модель 2 (Тренд + сезонность)",
       x = "Дата",
       y = "Прибыль, млн. руб.") +
  scale_color_manual(values = c("Оригинальный ряд" = "mediumspringgreen",
                                "Модель 2 (Тренд + сезонность)" = "red")) +
  scale_x_date(
    breaks = as.Date(c("2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01",
                       "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01",
                       "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
                       "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01")),
    labels = c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
               "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
               "2023"),
    limits = as.Date(c("2007-01-01", "2023-01-01"))
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# периодограмма остатков
spectrum_df <- data.frame(
  frequency = periodogram_data$freq,
  spec = periodogram_data$spec
)

periodogram_data <- spec.pgram(residuals_df_season, plot = FALSE)

ggplot(spectrum_df, aes(x = frequency, y = spec.2)) +
  geom_line(color = "palegreen2", size = 1) +
  labs(title = "Периодограмма остатков", x = "Частота", y = "Спектр") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Построение ACF и PACF для остатков

# уровень значимости для ACF и PACF
n <- nrow(decomposed_df)
conf_level <- 1.96 / sqrt(n)

acf_residuals <- acf(decomposed_df$.resid, plot = FALSE)

acf_df <- data.frame(
  Lag = acf_residuals$lag[-1],
  ACF = acf_residuals$acf[-1]
)

pacf_residuals <- pacf(decomposed_df$.resid, plot = FALSE)

pacf_df <- data.frame(
  Lag = pacf_residuals$lag[-1],
  PACF = pacf_residuals$acf[-1]
)

# ACF
ggplot(acf_df, aes(x = Lag, y = ACF)) +
  geom_segment(aes(xend = Lag, yend = 0), color = "mediumspringgreen",
               size = 1.2) +
  geom_hline(yintercept = c(-conf_level, conf_level), color = "blue",
             linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  ggtitle("Автокорреляционная функция (ACF)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# PACF
ggplot(pacf_df, aes(x = Lag, y = PACF)) +
  geom_segment(aes(xend = Lag, yend = 0), color = "blue",
               size = 1.2) +
  geom_hline(yintercept = c(-conf_level, conf_level), color = "blue",
             linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  ggtitle("Частная автокорреляционная Функция (PACF)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Убедитесь, что необходимые библиотеки загружены
library(ggplot2)

# 1. Построение ACF и PACF для остатков
# Остатки модели тренда и сезонности
residuals <- decomposed_df$.resid

# ACF
acf_residuals <- acf(residuals, plot = FALSE)

# PACF
pacf_residuals <- pacf(residuals, plot = FALSE)

# Создание датафреймов для ACF и PACF
acf_df <- data.frame(Lag = acf_residuals$lag[-1], ACF = acf_residuals$acf[-1])
pacf_df <- data.frame(Lag = pacf_residuals$lag[-1], PACF = pacf_residuals$acf[-1])

# ACF Plot
acf_plot <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
  geom_segment(aes(xend = Lag, yend = 0), color = "mediumspringgreen", size = 1.2) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  labs(title = "Автокорреляционная функция (ACF)", x = "Запаздывание", y = "ACF") +
  theme_minimal()

# PACF Plot
pacf_plot <- ggplot(pacf_df, aes(x = Lag, y = PACF)) +
  geom_segment(aes(xend = Lag, yend = 0), color = "blue", size = 1.2) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  labs(title = "Частная автокорреляционная функция (PACF)", x = "Запаздывание", y = "PACF") +
  theme_minimal()

# Отображение графиков
library(gridExtra)
grid.arrange(acf_plot, pacf_plot, ncol = 1)

# Определите порядок p на основании ACF и PACF
# Обычно порядок p выбирается по PACF, где PACF обрезается
# 3. Построение модели AR
p <- 2  # Замените 2 на порядок, определенный из графиков ACF и PACF
ar_model <- arima(decomposed_df$.resid, order = c(p, 0, 0))

# 4. Получение предсказаний
decomposed_df$.fitted_ar <- fitted(ar_model)

# График остатков модели AR
ggplot(decomposed_df, aes(x = seq_along(.resid), y = .resid)) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Остатки авторегрессионной модели", x = "Индекс", y = "Остатки") +
  theme_minimal()

# 5. Анализ остатков AR
# Q-Q Plot для остатков
ggplot(decomposed_df, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot остатков AR модели") +
  theme_minimal()

ar_model <- arima(decomposed_df$Original, order = c(2, 0, 0))

original_values <- decomposed_df$Original
fitted_values <- decomposed_df$.fitted_model3
SS_res <- sum((original_values - fitted_values) ^ 2, na.rm = TRUE)
SS_tot <- sum((original_values - mean(original_values, na.rm = TRUE)) ^ 2, na.rm = TRUE)
R_squared <- 1 - (SS_res / SS_tot)

# Результаты модели
summary(ar_model)

# Остатки модели
decomposed_df$.resid_ar <- residuals(ar_model)

# График остатков
ggplot(decomposed_df, aes(x = seq_along(.resid_ar), y = .resid_ar)) +
  geom_point(color = "skyblue1") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Остатки модели AR(2)", x = "Индекс", y = "Остатки") +
  theme_minimal()

# Проверка нормальности остатков
shapiro_test_result <- shapiro.test(decomposed_df$.resid_ar)

# Проверка на автокорреляцию остатков
# Используем тест Бокса-Люнга
box_test_result <- Box.test(decomposed_df$.resid_ar, lag = 20, type = "Ljung-Box")

# Вывод результатов
print(shapiro_test_result)
print(box_test_result)



# создние модели SARIMA
p <- 1  # Порядок авторегрессии
d <- 1  # Порядок интегрирования
q <- 1  # Порядок скользящего среднего
P <- 1  # Порядок сезонной авторегрессии
D <- 1  # Порядок сезонного интегрирования
Q <- 1  # Порядок сезонного скользящего среднего
s <- 12 # Период

# Построение модели SARIMA
sarima_model <- Arima(ts_data, order = c(p, d, q), seasonal = c(P, D, Q), period = s)

summary(sarima_model)
box_test_result_sarima <- Box.test(residuals(sarima_model), lag = 20, type = "Ljung-Box")
print(box_test_result_sarima)

# Остатки модели
decomposed_df$.resid_sarima <- residuals(sarima_model)

# Преобразование остатков в вектор
residuals_vector <- as.numeric(decomposed_df$.resid_sarima)

# График остатков
ggplot(decomposed_df, aes(x = seq_along(residuals_vector), y = residuals_vector)) +
  geom_point(color = "skyblue1") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Остатки модели SARIMA", x = "Индекс", y = "Остатки") +
  theme_minimal()

# Удаление выбросов
Q1 <- quantile(decomposed_df$Original, 0.25)
Q3 <- quantile(decomposed_df$Original, 0.75)
IQR <- Q3 - Q1

# Фильтруем данные
cleaned_data <- decomposed_df$Original[decomposed_df$Original > (Q1 - 1.5 * IQR) & decomposed_df$Original < (Q3 + 1.5 * IQR)]

# Заполнение пропусков
decomposed_df$Original <- na.approx(decomposed_df$Original)

decomposed_df$Log_Original <- log(decomposed_df$Original)

library(forecast)
sarima_model <- auto.arima(decomposed_df$Original, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(sarima_model)

# Проверка нормальности остатков
shapiro_test_result_sarima <- shapiro.test(decomposed_df$.resid_sarima)

# Проверка на автокорреляцию остатков
box_test_result_sarima <- Box.test(decomposed_df$.resid_sarima, lag = 20, type = "Ljung-Box")

# Вывод результатов
print(shapiro_test_result_sarima)
print(box_test_result_sarima)

ts_data <- ts(decomposed_df$Original, frequency = 12)  

# настройка параметров SARIMA (p, d, q)(P, D, Q)
p <- 2  # Порядок авторегрессии
d <- 1  # Порядок интегрирования
q <- 1  # Порядок скользящего среднего
P <- 1  # Порядок сезонной авторегрессии
D <- 1  # Порядок сезонного интегрирования
Q <- 1  # Порядок сезонного скользящего среднего
s <- 12 # Период

# построение модели SARIMA
sarima_model <- Arima(ts_data, order = c(p, d, q), seasonal = c(P, D, Q))

decomposed_df$.fitted_sarima <- fitted(sarima_model)
decomposed_df$.resid_sarima <- decomposed_df$Original -
  decomposed_df$.fitted_sarima

# cоздание модели 3 (тренд + сезонность + SARIMA)
decomposed_df$.fitted_model3 <- decomposed_df$.fitted_trend_season +
  (decomposed_df$.fitted_sarima - mean(decomposed_df$.fitted_sarima,
                                       na.rm = TRUE))
summary(decomposed_df$.fitted_model3)
# График итоговой модели
plot_data <- data.frame(
  Date = decomposed_df$Date,
  Original = decomposed_df$Original,
  Fitted_Trend_Season = decomposed_df$.fitted_trend_season,
  Fitted_SARIMA = decomposed_df$.fitted_sarima,
  Fitted_Model3 = decomposed_df$.fitted_model3
)


# получение предсказанных значений для тренда и сезонности
decomposed_df$.fitted_trend_season <- predict(trend_season_model)

# создание модели SARIMA
sarima_model <- auto.arima(decomposed_df$Original, seasonal = TRUE)
decomposed_df$.fitted_sarima <- fitted(sarima_model)

# Проверка размеров
cat("Length of fitted SARIMA:", length(decomposed_df$.fitted_sarima), "\n")

# 4. Создание модели 3 (тренд + сезонность + SARIMA)
# Убедитесь, что обе модели имеют одинаковую длину
if (length(decomposed_df$.fitted_trend_season) == length(decomposed_df$.fitted_sarima)) {
  decomposed_df$.fitted_model3 <- decomposed_df$.fitted_trend_season + 
    (decomposed_df$.fitted_sarima - mean(decomposed_df$.fitted_sarima, na.rm = TRUE))
} else {
  cat("Ошибка: длины предсказанных значений не совпадают!\n")
}

# Проверка результата
cat("Length of fitted model 3:", length(decomposed_df$.fitted_model3), "\n")

# 5. Визуализация
plot_data <- data.frame(
  Date = decomposed_df$Date,
  Original = decomposed_df$Original,
  Fitted_Trend_Season = decomposed_df$.fitted_trend_season,
  Fitted_SARIMA = decomposed_df$.fitted_sarima,
  Fitted_Model3 = decomposed_df$.fitted_model3
)

ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Original, color = "Оригинальный ряд"), size = 1) +
  geom_line(aes(y = Fitted_Model3, color = "Модель 3 (Тренд + Сезонность + SARIMA)"), size = 1, linetype = "twodash") +
  labs(title = "График Оригинального Ряда и Модели 3",
       x = "Дата",
       y = "Значение") +
  scale_color_manual(values = c("Оригинальный ряд" = "blue",
                                "Модель 3 (Тренд + Сезонность + SARIMA)" = "purple")) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

decomposed_df$.resid_model3 <- decomposed_df$Original - decomposed_df$.fitted_model3
decomposed_df$.fitted_model3 <- decomposed_df$.fitted_trend_season + (decomposed_df$.fitted_sarima - mean(decomposed_df$.fitted_sarima, na.rm = TRUE))

diagnostic_data <- data.frame(
  .fitted = decomposed_df$.fitted_model3,
  .resid = decomposed_df$.resid_model3
)


diagnostic_data$.fitted <- as.numeric(diagnostic_data$.fitted)
diagnostic_data$.resid <- as.numeric(diagnostic_data$.resid)

# Q-Q Plot of Residuals
p1 <- ggplot(diagnostic_data, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals for Model 3") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90")
  )

# Residuals vs Fitted
p2 <- ggplot(diagnostic_data, aes(x = .fitted, y = .resid)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Residuals vs Fitted for Model 3", x = "Fitted Values",
       y = "Residuals") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90")
  )

# Scale-Location (Residuals vs. Fitted)
p3 <- ggplot(diagnostic_data, aes(x = .fitted, y = sqrt(abs(.resid)))) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Scale-Location Plot", x = "Fitted Values",
       y = "Sqrt(|Residuals|)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90")
  )

grid.arrange(p1, p2, p3, ncol = 1)



original_values <- decomposed_df$Original
fitted_model3 <- decomposed_df$.fitted_model3

original_values <- na.omit(original_values)
fitted_model3 <- na.omit(fitted_model3)

# Вычисление R²
SS_res <- sum((original_values - fitted_model3) ^ 2)
SS_tot <- sum((original_values - mean(original_values)) ^ 2)
R_squared <- 1 - (SS_res / SS_tot)

# График остатков модели
ggplot(diagnostic_data, aes(x = .fitted, y = .resid)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "График остатков для Модели 3", x = "Предсказанные значения", y = "Остатки") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90")
  )

# Остатки модели
residuals <- decomposed_df$.resid_model3

# Минимальный остаток
min_residual <- min(residuals, na.rm = TRUE)

# Максимальный остаток
max_residual <- max(residuals, na.rm = TRUE)

# Средняя ошибка
mean_error <- mean(residuals, na.rm = TRUE)

# Стандартное отклонение ошибки (СКО)
sd_error <- sd(residuals, na.rm = TRUE)

# Средняя абсолютная ошибка (MAE)
mae <- mean(abs(residuals), na.rm = TRUE)

# Средняя ошибка в процентах (MPE)
mpe <- mean((residuals / decomposed_df$Original) * 100, na.rm = TRUE)

# Средняя абсолютная ошибка в процентах (MAPE)
mape <- mean(abs(residuals / decomposed_df$Original) * 100, na.rm = TRUE)

# Средний квадрат ошибки (MSE)
mse <- mean(residuals^2, na.rm = TRUE)

# Коэффициент детерминации (R²)
original_values <- decomposed_df$Original
fitted_values <- decomposed_df$.fitted_model3
SS_res <- sum((original_values - fitted_values) ^ 2, na.rm = TRUE)
SS_tot <- sum((original_values - mean(original_values, na.rm = TRUE)) ^ 2, na.rm = TRUE)
R_squared <- 1 - (SS_res / SS_tot)


# Вывод результатов в таблицу
accuracy_metrics <- data.frame(
  ME = mean_error,
  MSE = mse,
  RMSE = rmse,
  MAE = mae,
  MPE = mpe,
  MAPE = mape,
  MASE = mase,
  R_squared = R_squared,
  Min_Residual = min_residual,
  Max_Residual = max_residual
)

# Печать результатов
print(accuracy_metrics)
# Вывод результатов
cat("Минимальный остаток:", min_residual, "\n")
cat("Максимальный остаток:", max_residual, "\n")
cat("Средняя ошибка:", mean_error, "\n")
cat("Стандартное отклонение ошибки:", sd_error, "\n")
cat("Средняя абсолютная ошибка (MAE):", mae, "\n")
cat("Средняя ошибка в процентах (MPE):", mpe, "\n")
cat("Средняя абсолютная ошибка в процентах (MAPE):", mape, "\n")
cat("Средний квадрат ошибки (MSE):", mse, "\n")
cat("Коэффициент детерминации (R²):", R_squared, "\n")

# Вывод R²
print(paste("Коэффициент детерминации (R²) для модели 3:", R_squared))

# проба ETS
model_3.1 <- ets(tsData)
# Прогноз на следующие 12 периодов
forecast_ets <- forecast(model_3.1, h=12)

# Визуализация прогноза
plot(forecast_ets)
print(forecast_ets)
accuracy(forecast_ets)

# ETS модель
ets_model <- ets(ts_data)
ets_forecast <- forecast(ets_model, h=12) # Прогноз на 12 месяцев
plot(ets_forecast)

# ARIMA модель
auto_arima_model <- auto.arima(ts_data)
arima_forecast <- forecast(auto_arima_model, h=12)
plot(arima_forecast)

# Сравнение прогнозов
accuracy(ets_forecast)
accuracy(arima_forecast)


# Фактические значения
actual_values <- decomposed_df$values

# Общая сумма квадратов
SS_tot_ets <- sum((actual_values - mean(actual_values))^2)
SS_tot_arima <- sum((actual_values - mean(actual_values))^2)

# Сумма квадратов остатков для ETS
SS_res_ets <- sum(ets_residuals^2)

# Сумма квадратов остатков для ARIMA
SS_res_arima <- sum(arima_residuals^2)

# Коэффициент детерминации
R_squared_ets <- 1 - (SS_res_ets / SS_tot_ets)
R_squared_arima <- 1 - (SS_res_arima / SS_tot_arima)

# Вывод
cat("R^2 для модели ETS:", R_squared_ets, "\n")
cat("R^2 для модели ARIMA:", R_squared_arima, "\n")



library(ggplot2)

ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Original, color = "Оригинальный ряд"), size = 1) +
  geom_line(aes(y = Fitted_Model3, color = "Модель 3 (Тренд + Сезонность + SARIMA)"), size = 1, linetype = "twodash") +
  labs(title = "График Оригинального Ряда и Модели 3",
       x = "Дата",
       y = "Значение") +
  scale_x_date(
    breaks = as.Date(c("2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01",
                       "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01",
                       "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
                       "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01")),
    labels = c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
               "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
               "2023"),
    limits = as.Date(c("2007-01-01", "2023-01-01"))
  ) +
  scale_color_manual(values = c("Оригинальный ряд" = "blue",
                                "Модель 3 (Тренд + Сезонность + SARIMA)" = "purple")) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Предположим, что ваш вектор остатков для модели 3 называется residuals_model3
# Замените 'residuals_model3' на фактическое имя вашего вектора остатков

ggplot(data.frame(Index = seq_along(residuals_model3), Residuals = fitted_model3), aes(x = Index, y = Residuals)) +
  geom_point(color = "skyblue1") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Остатки модели 3 (Тренд + Сезонность + AR)", x = "Индекс", y = "Остатки") +
  theme_minimal()

# Остатки модели 3
residuals_model3 <- decomposed_df$.resid_model3

# Построение периодограммы остатков
spectrum(residuals_model3, main = "Периодограмма остатков модели 3 (Тренд + Сезонность + SARIMA)",
         xlab = "Частота", ylab = "Спектральная плотность", col = "blue")

# Опционально: добавление линии для визуализации
lines(spectrum(residuals_model3)$frequency, spectrum(residuals_model3)$spec, col = "red")

if(length(residuals_filtered) > 0) {
  
  # Построение периодограммы остатков
  periodogram_data <- spec.pgram(residuals_filtered, plot = FALSE)
  
  # Преобразуем в data.frame для ggplot
  periodogram_df <- data.frame(Frequency = periodogram_data$freq, 
                               Spectrum = periodogram_data$spec)
  
  # Построение графика
  ggplot(periodogram_df, aes(x = Frequency, y = Spectrum)) +
    geom_line(color = "palegreen2", size = 1) +
    labs(title = "Периодограмма остатков", x = "Частота", y = "Спектр") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

# ACF
ggplot(acf_df, aes(x = Lag, y = ACF)) +
  geom_segment(aes(xend = Lag, yend = 0), color = "mediumspringgreen",
               size = 1.2) +
  geom_hline(yintercept = c(-conf_level, conf_level), color = "blue",
             linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  ggtitle("Автокорреляционная функция (ACF)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# PACF
ggplot(pacf_df, aes(x = Lag, y = PACF)) +
  geom_segment(aes(xend = Lag, yend = 0), color = "blue",
               size = 1.2) +
  geom_hline(yintercept = c(-conf_level, conf_level), color = "blue",
             linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  ggtitle("Частная автокорреляционная Функция (PACF)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Параметры для ACF и PACF
conf_level <- 1.96 / sqrt(length(ts_data))

# Построение ACF
acf_values <- acf(ts_data, plot = FALSE)
acf_df <- data.frame(Lag = acf_values$lag, ACF = acf_values$acf)

# Построение PACF
pacf_values <- pacf(ts_data, plot = FALSE)
pacf_df <- data.frame(Lag = pacf_values$lag, PACF = pacf_values$acf)

# График ACF
ggplot(acf_df, aes(x = Lag, y = ACF)) +
  geom_segment(aes(xend = Lag, yend = 0), color = "steelblue", size = 1.2) +
  geom_hline(yintercept = c(-conf_level, conf_level), color = "blue", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  ggtitle("Автокорреляционная Функция (ACF)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# График PACF
ggplot(pacf_df, aes(x = Lag, y = PACF)) +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkred", size = 1.2) +
  geom_hline(yintercept = c(-conf_level, conf_level), color = "blue", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  ggtitle("Частичная автокорреляционная функция (PACF)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))





lag_max <- 25  # Можно настроить на желаемое количество лагов

# Вычисление ACF и PACF
acf_values <- acf(decomposed_df$.resid_model3, lag.max = lag_max, plot = FALSE)
pacf_values <- pacf(decomposed_df$.resid_model3, lag.max = lag_max, plot = FALSE)

# Преобразование в датафреймы для ggplot
acf_df <- data.frame(Lag = acf_values$lag, ACF = acf_values$acf)
pacf_df <- data.frame(Lag = pacf_values$lag, PACF = pacf_values$acf)

# Построение графиков ACF и PACF
conf_level <- qnorm(0.975) / sqrt(length(decomposed_df$.resid_model3))  # 95% доверительный интервал

# График ACF
ggplot(acf_df, aes(x = Lag, y = ACF)) +
  geom_segment(aes(xend = Lag, yend = 0), color = "mediumspringgreen", size = 1.2) +
  geom_hline(yintercept = c(-conf_level, conf_level), color = "blue", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  labs(title = "Автокорреляционная функция (ACF)", x = "Лаги", y = "ACF") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# График PACF
ggplot(pacf_df, aes(x = Lag, y = PACF)) +
  geom_segment(aes(xend = Lag, yend = 0), color = "blue", size = 1.2) +
  geom_hline(yintercept = c(-conf_level, conf_level), color = "blue", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  labs(title = "Частная автокорреляционная функция (PACF)", x = "Лаги", y = "PACF") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )    





# Проверим, есть ли NA в forecast_df
any(is.na(forecast_df$Forecast))      # TRUE, если есть NA

# Проверим минимальные и максимальные значения для Forecast
min(forecast_df$Forecast, na.rm = TRUE)
max(forecast_df$Forecast, na.rm = TRUE)

# Проверим диапазон дат для forecast_df
range(forecast_df$Date)

# Проверим диапазон дат для decomposed_df
range(decomposed_df$Date)

# Фильтруем forecast_df по допустимому диапазону Y и синхронизируем даты
forecast_df <- forecast_df %>%
  filter(!is.na(Forecast)) %>%               # Убираем NA
  filter(Date >= min(decomposed_df$Date) &   # Ограничиваем даты
           Date <= max(decomposed_df$Date)) %>%
  filter(Forecast >= 0, Forecast <= 100)     # Ограничиваем значения по Y, если нужно

# Построение графика
ggplot() +
  geom_line(data = decomposed_df, aes(x = Date, y = Original), color = "mediumspringgreen", size = 1) +
  geom_line(data = decomposed_df, aes(x = Date, y = .fitted), color = "red", linetype = "dashed") +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "blue", size = 1) +
  labs(title = "Прогноз временного ряда на 2023 год",
       x = "Дата",
       y = "Значение временного ряда") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Создание даты для прогноза только на первые 3 месяца 2023 года
forecast_dates <- data.frame(
  Date = seq.Date(as.Date("2023-01-01"), as.Date("2023-03-31"), by = "month")
)

# Обновление индексов для прогноза
forecast_dates$Index <- (nrow(decomposed_df) + 1):(nrow(decomposed_df) + nrow(forecast_dates))

# Выполнение прогноза
forecast_values <- predict(trend_season_model, newdata = forecast_dates)

# Применение сглаживания к прогнозируемым значениям
smoothed_forecast <- stats::filter(forecast_values, rep(1/3, 3), sides = 2)

# Формирование датафрейма для графика прогноза
forecast_df <- data.frame(
  Date = forecast_dates$Date,
  Forecast = smoothed_forecast
)

# Построение обновленного графика
ggplot() +
  geom_line(data = decomposed_df, aes(x = Date, y = Original), color = "mediumspringgreen", size = 1) +
  geom_line(data = decomposed_df, aes(x = Date, y = .fitted), color = "red", linetype = "dashed") +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "blue", size = 1) +
  labs(title = "Прогноз временного ряда на первые месяцы 2023 года",
       x = "Дата",
       y = "Значение временного ряда") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

forecast_value <- predict(trend_season_model, n.ahead = 12)
print(forecast_value$pred)

predict(logistic_model)
plot(pred1)

predict(trend_season_model)

plot(predict(trend_season_model, n.ahead = 3))     


forecast_values <- forecast(trend_season_model)
# Построение графика прогноза
autoplot(forecast_values) +
  labs(title = "Прогноз временного ряда",
       x = "Дата",
       y = "Значение временного ряда")

# создаем датафрейм
df_ts_1 <- data.frame(
  Date = seq(as.Date("2023-01-01"), as.Date("2023-04-01"), by = "month")
)

plot(predict(trend_season_model, newdata = df_ts_1))

fct <- forecast(trend_season_model)
autoplot()

install.packages("lubridate")
library(ggplot2)
library(lubridate)

# Создание данных для прогноза на период с 2023-01-01 по 2023-04-01
forecast_dates <- data.frame(Date = seq(as.Date("2023-01-01"), as.Date("2023-04-01"), by = "month"))

# Индексирование дат в forecast_dates
forecast_dates$Index <- (nrow(decomposed_df) + 1):(nrow(decomposed_df) + nrow(forecast_dates))

# Выполнение прогноза
forecast_values <- predict(trend_season_model, newdata = forecast_dates)

# Применение сглаживания к прогнозируемым значениям
smoothed_forecast <- stats::filter(forecast_values, rep(1/3, 3), sides = 2)

# Формирование датафрейма для графика прогноза
forecast_df <- data.frame(
  Date = forecast_dates$Date,
  Forecast = smoothed_forecast
)

# Построение обновленного графика
ggplot() +
  geom_line(data = decomposed_df, aes(x = Date, y = Original), color = "mediumspringgreen", size = 1) +
  geom_line(data = decomposed_df, aes(x = Date, y = .fitted), color = "red", linetype = "dashed") +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "blue", size = 1) +
  labs(title = "Прогноз временного ряда на период с 2023-01-01 по 2023-04-01",
       x = "Дата",
       y = "Значение временного ряда") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

library(ggplot2)
library(forecast)
library(lubridate)

# Создание данных для прогноза на период с 2023-01-01 по 2023-04-01
forecast_dates <- data.frame(Date = seq(as.Date("2023-01-01"), as.Date("2023-04-01"), by = "day"))

# Индексирование дат в forecast_dates
forecast_dates$Index <- (nrow(decomposed_df) + 1):(nrow(decomposed_df) + nrow(forecast_dates))

# Выполнение прогноза с использованием функции forecast()
# Предполагаем, что trend_season_model - это модель, подходящая для forecast
forecast_values <- forecast(trend_season_model, h = nrow(forecast_dates))

# Извлечение прогнозируемых значений и их сглаживание
smoothed_forecast <- stats::filter(forecast_values$mean, rep(1/3, 3), sides = 2)

# Формирование датафрейма для графика прогноза
forecast_df <- data.frame(
  Date = forecast_dates$Date,
  Forecast = smoothed_forecast
)

# Построение обновленного графика
ggplot() +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "blue", size = 1) +
  labs(title = "Прогноз временного ряда на период с 2023-01-01 по 2023-04-01",
       x = "Дата",
       y = "Значение временного ряда") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

autoplot(forecast_df$Forecast) +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "blue", size = 1) +
  ggtitle("Прогноз прибыли") +
  xlab("Дата") +
  ylab("Прибыль") + 
  theme_minimal()



profit <- diagnostic_data$.fitted

require(prophet)
M0 <- prophet(diagnostic_data)

pred <- forecast(decomposed_df$.fitted, a.head = 3)
plot(pred) + 
  ggplot(trend_season_model)

prophet_plot_components(pred)


plot(pred)
ggplot() +
  geom_line(data = decomposed_df, aes(x = Date, y = Original), color = "mediumspringgreen", size = 1) +
  geom_line(data = decomposed_df, aes(x = Date, y = .fitted), color = "red", linetype = "dashed") +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "blue", size = 1) +
  labs(title = "Прогноз временного ряда на период с 2023-01-01 по 2023-04-01",
       x = "Дата",
       y = "Значение временного ряда") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)

# Убедитесь, что ваш объект pred содержит столбцы ds (даты) и yhat (предсказанные значения)
# Вы можете извлечь их следующим образом:
pred_values <- data.frame(Date = decomposed_df$Date, Forecast = decomposed_df$.fitted)

# Объединяем исходные данные с предсказаниями
ggplot() +
  # Исходные данные
  geom_line(data = decomposed_df, aes(x = Date, y = Original, color = "Исходные данные"), size = 1) +
  geom_line(data = decomposed_df, aes(x = Date, y = .fitted, color = "Фитированное значение"), linetype = "dashed", size = 1) +
  # Предсказания
  geom_line(data = pred_values, aes(x = Date, y = Forecast, color = "Прогноз"), size = 1) +
  # Настройка заголовка и подписей
  labs(title = "Прогноз временного ряда на период с 2023-01-01 по 2023-04-01",
       x = "Дата",
       y = "Значение временного ряда") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_color_manual(name = "Легенда", 
                     values = c("Исходные данные" = "mediumspringgreen", 
                                "Фитированное значение" = "red", 
                                "Прогноз" = "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


# преобразую массив в ts, с частотой 12, начиная с 2007-01-01
tsData_2 <- ts(tsData, frequency = 12, start = c(2007,1))

# создаем датафрейм
df_ts_2 <- data.frame(
  ds = seq.Date(from = as.Date("2007-01-01"), as.Date("2022-12-01"), by = "month"),
  y = df_ts$Value
)


pred <- predict(trend_season_model, df_ts)

q_1 <- prophet(df_ts_2)

# Создание датафрейма для будущих дат (например, на 3 месяца вперед)
future <- make_future_dataframe(q_1, periods = 3, freq = "month")

pred <- predict(q_1, future)
plot(pred)
forecast(pred)
pred

predicted_values <- predict(trend_season_model)
forecast(trend_season_model)


# Добавление предсказанных значений в датафрейм
predicted_values <- c(rep(NA, nrow(predicted_values) - length(predicted_values)), predicted_values)

# Визуализация
ggplot(df, aes(x = Date)) +
  geom_line(aes(y = Original, color = "Исходные данные"), size = 1) +
  geom_line(aes(y = Predicted, color = "Предсказанные значения"), size = 1, linetype = "dashed") +
  labs(title = "Сравнение фактических и предсказанных значений",
       x = "Дата",
       y = "Значение") +
  scale_color_manual(values = c("Исходные данные" = "mediumspringgreen", "Предсказанные значения" = "red")) +
  theme_minimal()


# Пример исходных значений (замените на ваши фактические данные)
your_original_values <- df_ts$Date  # Фактические данные (пример)
predicted_values <- predict(trend_season_model)  # Ваши предсказания из модели

# Создание датафрейма с датами
tsData_2 <- ts(tsData, frequency = 12, start = c(2007,1))

# Создание итогового датафрейма
dff <- data.frame(Date = tsData_2, Original = your_original_values)
dff$Predicted_1 <- c(rep(NA, length(your_original_values) - length(predicted_values)), predicted_values)  # Добавляем предсказанные значения


library(ggplot2)

# Построение графика
ggplot(dff, aes(x = Date)) +
  geom_line(aes(y = Original, color = "Исходные данные"), size = 1) +  # Фактические данные
  geom_line(aes(y = Predicted_1, color = "Предсказанные значения"), size = 1, linetype = "dashed") +  # Предсказанные значения
  labs(title = "Сравнение фактических и предсказанных значений",
       x = "Дата",
       y = "Значение") +
  scale_color_manual(values = c("Исходные данные" = "mediumspringgreen", "Предсказанные значения" = "red")) +
  theme_minimal()

str(dff)
dff$Date <- as.Date(dff$Date, format = "%Y-%m-%d")

plot(decomposed_df$.fitted_trend_season)
prophet(trend_season_model)

autoplot(decomposed_df$.fitted_trend_season)

