
library(quantmod)
library(rugarch)
library(tseries)
library(xts)
library(PerformanceAnalytics)
library(dplyr)
library(tibble)
library(knitr)
library(TTR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)

# Загрузка данных по тикерам
# ESG и Non-ESG тикеры
esg_tickers <- c("MSFT", "NVDA", "INTU", "IDXX", "LRCX", "ADBE", "CRM", "AMAT", "CSCO", "TXN")
non_esg_tickers <- c("TSLA", "XOM", "META", "CVX", "JNJ", "BA", "GOOGL", "AMD", "MSI", "RF")

all_tickers <- c(esg_tickers, non_esg_tickers)

# Загрузка котировок
getSymbols(all_tickers, from = "2018-01-01", to = Sys.Date(), src = "yahoo")

# Функция лог-доходности
get_log_returns <- function(ticker) {
        price <- Cl(get(ticker))
        log_ret <- dailyReturn(price, type = "log")
        colnames(log_ret) <- ticker
        return(log_ret)
}

# Построение таблицы доходностей
returns_list <- lapply(all_tickers, get_log_returns)
returns_xts <- do.call(merge, returns_list)
returns_xts <- na.omit(returns_xts)
colnames(returns_xts) <- all_tickers
head(returns_xts)

# График сравнения кумулятивной доходности по группам ESG и Non-ESG.
# Разделим по группам
returns_esg <- returns_xts[, esg_tickers]
returns_non_esg <- returns_xts[, non_esg_tickers]

# Средняя доходность группы (равновзвешенный портфель)
esg_portfolio <- xts(rowMeans(returns_esg), order.by = index(returns_xts))
non_esg_portfolio <- xts(rowMeans(returns_non_esg), order.by = index(returns_xts))

# Построим xts для портфелей
portfolios <- merge(esg_portfolio, non_esg_portfolio)
colnames(portfolios) <- c("ESG", "Non-ESG")

# Кумулятивная доходность
cumulative_returns <- cumprod(1 + portfolios)
# Убираем строки с NA, NaN, Inf
cumulative_returns <- cumulative_returns[complete.cases(cumulative_returns) & is.finite(rowSums(cumulative_returns)), ]
plot.xts(
        cumulative_returns,
        main = "📈 Cumulative returns: ESG vs Non-ESG",
        col = c("green4", "red3"),
        legend.loc = "topleft",
        screens = 1,
        major.ticks = "years",
        grid.ticks.on = "years",
        ylab = "Cumulative Return"
)


# Сравним, какая из групп более волатильна по годовой скользящей волатильности (252 дня).

# Скользящее стандартное отклонение за 252 дня (annualized)

esg_vol <- runSD(esg_portfolio, n = 252) * sqrt(252)
non_esg_vol <- runSD(non_esg_portfolio, n = 252) * sqrt(252)
rolling_volatility <- merge(esg_vol, non_esg_vol)
colnames(rolling_volatility) <- c("ESG", "Non-ESG")
rolling_volatility <- na.omit(rolling_volatility)

# График скользящей волатильности
plot.xts(
        rolling_volatility,
        col = c("darkgreen", "darkred"),
        lwd = 2,
        main = "Rolling volatility (252 days)",
        ylab = "Volatility",
        xlab = "Date",
        legend.loc = NULL
)


# ESG статистики
esg_stats <- table.Stats(returns_xts[, esg_tickers])
# Non-ESG статистики
non_esg_stats <- table.Stats(returns_xts[, non_esg_tickers])

# ESG
cat("ESG\n")
kable(round(esg_stats, 4), format = "simple")

# Non-ESG
cat("Non-ESG\n")
kable(round(non_esg_stats, 4), format = "simple")



# ESG
esg_df <- esg_stats %>%
        as.data.frame() %>%
        rownames_to_column("Metric") %>%
        mutate(Group = "ESG")

# Non-ESG
non_esg_df <- non_esg_stats %>%
        as.data.frame() %>%
        rownames_to_column("Metric") %>%
        mutate(Group = "Non-ESG")

# Объединяем и переупорядочим
summary_stats <- bind_rows(esg_df, non_esg_df) %>%
        relocate(Group, Metric)

# Копия для округления только чисел
summary_stats_rounded <- summary_stats
summary_stats_rounded[ , !(names(summary_stats_rounded) %in% c("Group", "Metric"))] <-
        round(summary_stats_rounded[ , !(names(summary_stats_rounded) %in% c("Group", "Metric"))], 4)


stdev_df <- summary_stats_rounded %>%
        filter(Metric == "Stdev") %>%
        select(-Metric) %>%
        pivot_longer(cols = -Group, names_to = "Ticker", values_to = "Stdev")

ggplot(na.omit(stdev_df), aes(x = reorder(Ticker, -Stdev), y = Stdev, fill = Group)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Volatility by Ticker", x = "Ticker", y = "Standard Deviation") +
        scale_fill_manual(values = c("ESG" = "forestgreen", "Non-ESG" = "firebrick")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))




# ADF-test (Random walk)
# в какой группе поведение доходностей ближе к случайному блужданию

# ADF-тест по ESG
adf_esg <- sapply(esg_tickers, function(tk) {
        tryCatch(
                round(adf.test(returns_xts[, tk], alternative = "stationary")$p.value, 4),
                error = function(e) NA
        )
})

# ADF-тест по Non-ESG
adf_non_esg <- sapply(non_esg_tickers, function(tk) {
        tryCatch(
                round(adf.test(returns_xts[, tk], alternative = "stationary")$p.value, 4),
                error = function(e) NA
        )
})

# Собираем в таблицу
adf_df <- data.frame(
        Ticker = c(esg_tickers, non_esg_tickers),
        Group = c(rep("ESG", length(esg_tickers)), rep("Non-ESG", length(non_esg_tickers))),
        ADF_p_value = c(adf_esg, adf_non_esg)
)

kable(adf_df)
# Все ряды отвергают гипотезу случайного блуждания при уровне значимости 5% (p < 0.05);
# То есть, лог-доходности (как ты использовал) — стационарны и не соответствуют случайному блужданию;
# Это ожидаемый результат, т.к. лог-доходности почти всегда стационарны, 
# а случайное блуждание характерно для цен, а не доходностей.

ggplot(adf_df, aes(x = reorder(Ticker, -ADF_p_value), y = ADF_p_value, fill = Group)) +
        geom_bar(stat = "identity") +
        labs(title = "ADF Test p-values by Ticker", x = "Ticker", y = "ADF p-value") +
        scale_fill_manual(values = c("ESG" = "forestgreen", "Non-ESG" = "firebrick")) +
        geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
        annotate("text", x = 1, y = 0.055, label = "0.05 threshold", hjust = 0, size = 3.5) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Сравнение частот: дневная, недельная, месячная волатильность
returns_data <- do.call(merge, returns_list)
head(returns_data)
# Перевод в weekly и monthly
weekly_returns <- apply.weekly(returns_data, colSums)
monthly_returns <- apply.monthly(returns_data, colSums)

# Вычисление стандартного отклонения (волатильности)
vol_daily <- apply(returns_data, 2, sd, na.rm = TRUE)
vol_weekly <- apply(weekly_returns, 2, sd, na.rm = TRUE)
vol_monthly <- apply(monthly_returns, 2, sd, na.rm = TRUE)

t.test(vol_daily, vol_weekly, paired = TRUE)
t.test(vol_weekly, vol_monthly, paired = TRUE)

# Сравнение
vol_comparison <- data.frame(Daily = vol_daily, Weekly = vol_weekly, Monthly = vol_monthly)
kable(round(vol_comparison, 4), format = "simple", caption = "Volatility Comparison: Daily vs Weekly vs Monthly")

vol_comparison %>%
        mutate(Ticker = rownames(vol_comparison)) %>%
        arrange(desc(Monthly)) %>%
        kable(format = "simple", caption = "Sorted Monthly Volatility (High to Low)")


# Добавим тикеры как колонку, если они в rownames
vol_df <- vol_comparison %>%
        mutate(Ticker = rownames(.)) %>%
        pivot_longer(cols = c(Daily, Weekly, Monthly),
                     names_to = "Frequency",
                     values_to = "Volatility")

ggplot(vol_df, aes(x = reorder(Ticker, -Volatility), y = Volatility, fill = Frequency)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ Frequency, scales = "free_y") +
        labs(title = "Volatility Comparison by Frequency", x = "Ticker", y = "Volatility") +
        scale_fill_manual(values = c("Daily" = "skyblue", "Weekly" = "steelblue", "Monthly" = "navy")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Статическая корреляционная матрица (лог-доходности)

# Корреляция всех доходностей
cor_matrix <- cor(returns_xts, use = "pairwise.complete.obs")
round(cor_matrix, 2)
library(corrplot)

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = "black", addCoef.col = "black")

# Корреляция по группам

cor_esg <- cor(returns_xts[, esg_tickers], use = "pairwise.complete.obs")
cor_non_esg <- cor(returns_xts[, non_esg_tickers], use = "pairwise.complete.obs")
corrplot(cor_esg, method = "circle", title = "ESG Correlation", mar = c(1,1,1,1))
corrplot(cor_non_esg, method = "circle", title = "Non-ESG Correlation", mar = c(1,1,1,1))

# скользящая корреляция между MSFT и GOOGL по времени
roll_cor <- runCor(returns_xts$MSFT, returns_xts$GOOGL, n = 60)
plot(roll_cor, main = "Rolling Correlation: MSFT vs GOOGL (60 days)", 
     ylab = "Correlation", col = "darkblue", lwd = 2)

# скользящая корреляция между групппами по времени
# Скользящая корреляция с окном 60 дней
rolling_corr <- runCor(esg_portfolio, non_esg_portfolio, n = 60)
rolling_corr <- na.omit(rolling_corr)
plot(rolling_corr,
     main = "Rolling 60-day Correlation: ESG vs Non-ESG",
     ylab = "Correlation",
     col = "darkblue",
     lwd = 2)



# Оценка волатильности через GARCH
# GARCH(1,1) спецификация
spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
        mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
        distribution.model = "std"
)


# Пример оценки для MSFT
fit_msft <- ugarchfit(spec, returns_xts$MSFT)
show(fit_msft)
plot(fit_msft)

# Пример оценки для NVDA
fit_nvda <- ugarchfit(spec, returns_xts$NVDA)
show(fit_nvda)
plot(fit_nvda)

# Пример оценки для ADBE
fit_adbe <- ugarchfit(spec, returns_xts$ADBE)
show(fit_adbe)
plot(fit_adbe)

# Пример оценки для TSLA
fit_tsla <- ugarchfit(spec, returns_xts$TSLA)
show(fit_tsla)
plot(fit_tsla)

# Пример оценки для XOM
fit_xom <- ugarchfit(spec, returns_xts$XOM)
show(fit_xom)
plot(fit_xom)


# PCA (Principal Component Analysis) 

# Применим PCA к лог-доходностям
# Убедимся, что в данных нет пропусков
pca_data <- na.omit(returns_xts[, all_tickers])
# Центровка и масштабирование — важно для PCA
pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)
# Смотрим на результаты PCA
summary(pca_result)

explained_var <- summary(pca_result)$importance[2, ]
barplot(explained_var[1:5], main = "Variance Explained by PCs",
        xlab = "principal component", ylab = "proportion of variance",
        col = "skyblue")

# График для первых двух главных компонент
pca_data <- as.data.frame(pca_result$x)
pca_data$Group <- ifelse(rownames(pca_data) %in% esg_tickers, "ESG", "Non-ESG")

# PCA для групп ESG и Non-ESG
pca_esg <- prcomp(returns_xts[, esg_tickers], center = TRUE, scale. = TRUE)
pca_non_esg <- prcomp(returns_xts[, non_esg_tickers], center = TRUE, scale. = TRUE)
# График для первых двух главных компонент
pca_esg_data <- as.data.frame(pca_esg$x)
pca_esg_data$Group <- "ESG"
pca_non_esg_data <- as.data.frame(pca_non_esg$x)
pca_non_esg_data$Group <- "Non-ESG"
pca_combined <- rbind(pca_esg_data, pca_non_esg_data)
ggplot(pca_combined, aes(x = PC1, y = PC2, color = Group)) +
        geom_point() +
        labs(title = "PCA. First Two Principal Components (ESG vs Non-ESG)", x = "PC1", y = "PC2") +
        scale_color_manual(values = c("ESG" = "forestgreen", "Non-ESG" = "firebrick")) +
        theme_minimal()



# нужно выйти за рамки чистого GARCH и рассмотреть внешние предикторы (экзогенные переменные), 
# которые могут объяснять поведение волатильности.
# В этом примере мы будем использовать макроэкономические предикторы, такие как VIX, 
# 10-летняя доходность казначейских облигаций, индекс S&P 500, ставка ФРС и индекс потребительских цен (CPI).

# загрузим и подготовил макроэкономические предикторы волатильности с FRED

start_date <- index(first(returns_xts))
end_date <- index(last(returns_xts))

library(quantmod)

getSymbols(c("VIXCLS", "GS10", "SP500", "FEDFUNDS", "CPIAUCSL"), src = "FRED")

predictors_xts <- merge(VIXCLS, GS10, SP500, FEDFUNDS, CPIAUCSL)
colnames(predictors_xts) <- c("VIX", "Rate10Y", "SP500", "FedRate", "CPI")

# Преобразуем SP500 в лог-доходность
predictors_xts$SP500_ret <- diff(log(predictors_xts$SP500))

# Удаляем исходный SP500
predictors_xts <- predictors_xts[, c("VIX", "Rate10Y", "FedRate", "CPI", "SP500_ret")]
# Удалим пропуски и заполним недостающие значения
predictors_xts <- na.omit(predictors_xts)
predictors_xts <- na.locf(predictors_xts)
predictors_xts

summary(predictors_xts)

library(PerformanceAnalytics)

monthly_returns_list <- list()
# По всем тикерам
for (tk in colnames(returns_xts)) {
        ret <- na.omit(returns_xts[, tk])
        price <- exp(cumsum(ret))
        price_monthly <- apply.monthly(price, last)
        logret_monthly <- diff(log(price_monthly))
        monthly_returns_list[[tk]] <- logret_monthly
}

monthly_returns <- do.call(merge, monthly_returns_list)
monthly_returns <- na.omit(monthly_returns)
monthly_returns

head(monthly_returns)
summary(monthly_returns)

library(rugarch)
library(dplyr)
library(lubridate)
index(predictors_xts) <- ceiling_date(index(predictors_xts), unit = "month") - days(1)
predictors_monthly <- apply.monthly(predictors_xts, last)

# Список тикеров
tickers <- colnames(monthly_returns)
predictor_names <- colnames(predictors_xts)

# Сохраняем результаты
results <- data.frame()

library(TTR)

# for ESG
vol30_esg <- runSD(esg_portfolio, n = 30) * sqrt(252)
vol_data_esg <- merge(vol30_esg, predictors_xts)
vol_data_esg <- na.omit(vol_data_esg)
model_esg <- lm(vol30_esg ~ ., data = as.data.frame(vol_data_esg))
summary(model_esg)

# for non-ESG
vol30_non_esg <- runSD(non_esg_portfolio, n = 30) * sqrt(252)
vol_data_non_esg <- merge(vol30_non_esg, predictors_xts)
vol_data_non_esg <- na.omit(vol_data_non_esg)
model_non_esg <- lm(vol30_non_esg ~ ., data = as.data.frame(vol_data_non_esg))
summary(model_non_esg)


library(xts)
library(zoo)
library(dplyr)

# Скользящая волатильность for ESG
vol_esg <- runSD(esg_portfolio, n = 30) * sqrt(252)
colnames(vol_esg) <- "Volatility"
predictors_lagged <- predictors_xts
colnames(predictors_lagged) <- paste0("Lag_", colnames(predictors_lagged))
data_all <- merge(vol_esg, predictors_lagged, join = "inner")
data_all <- na.omit(data_all)

# Сдвигаем Volatility назад на 1 (чтобы прогнозировать t+1)
data_all$Volatility_t1 <- lag.xts(data_all$Volatility, k = -1)
model_data <- na.omit(data_all)

# Регрессия: Vol_{t+1} ~ Vol_{t} + макро предикторы
model_esg <- lm(Volatility_t1 ~ Volatility + ., data = as.data.frame(model_data))
summary(model_esg)



# Скользящая волатильность for ESG
vol_non_esg <- runSD(non_esg_portfolio, n = 30) * sqrt(252)
colnames(vol_non_esg) <- "Volatility"
predictors_lagged <- predictors_xts
colnames(predictors_lagged) <- paste0("Lag_", colnames(predictors_lagged))
data_all_non_esg <- merge(vol_non_esg, predictors_lagged, join = "inner")
data_all_non_esg <- na.omit(data_all_non_esg)

# Сдвигаем Volatility назад на 1 (чтобы прогнозировать t+1)
data_all_non_esg$Volatility_t1 <- lag.xts(data_all_non_esg$Volatility, k = -1)
model_data_non_esg <- na.omit(data_all_non_esg)

# Регрессия: Vol_{t+1} ~ Vol_{t} + макро предикторы
model_non_esg <- lm(Volatility_t1 ~ Volatility + ., data = as.data.frame(model_data_non_esg))
summary(model_non_esg)


# включаем лаги макро-предикторов в ESG
predictors_lag2 <- lag.xts(predictors_xts, k = 2)
data_lag2_esg <- merge(vol_esg, predictors_lag2)
data_lag2_esg$Volatility_t1 <- lag.xts(data_lag2_esg$Volatility, k = -1)
data_lag2_esg <- na.omit(data_lag2_esg)
summary(lm(Volatility_t1 ~ ., data = as.data.frame(data_lag2_esg)))

# включаем лаги макро-предикторов в non-ESG
predictors_lag2 <- lag.xts(predictors_xts, k = 2)
data_lag2_non_esg <- merge(vol_non_esg, predictors_lag2)
data_lag2_non_esg$Volatility_t1 <- lag.xts(data_lag2_non_esg$Volatility, k = -1)
data_lag2_non_esg <- na.omit(data_lag2_non_esg)
summary(lm(Volatility_t1 ~ ., data = as.data.frame(data_lag2_non_esg)))













