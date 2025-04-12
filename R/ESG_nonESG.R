
library(quantmod)
library(rugarch)
library(tseries)
library(xts)
library(PerformanceAnalytics)
library(dplyr)
library(tibble)
library(knitr)

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

# Сначала построим график сравнения кумулятивной доходности по группам ESG и Non-ESG.
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
library(TTR)
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

library(dplyr)
library(tidyr)
library(ggplot2)

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
library(tseries)

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

library(ggplot2)
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



