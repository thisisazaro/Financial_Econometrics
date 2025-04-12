

library(quantmod)
library(zoo) 

# Определяем тикеры фондовых индексов на Yahoo Finance
indices <- c("^AEX", "^GDAXI", "^HSI", "^FTSE", "^GSPC", "^FCHI", "^STI", "^N225")
getSymbols(indices, src = "yahoo", from = "2015-01-01", to = "2025-03-16")

str(AEX)
str(GDAXI)
str(HSI)
str(FTSE)
str(GSPC)
str(STI)
str(N225)

# Загружаем валютные пары на Yahoo Finance
forex <- c("AUDUSD=X", "GBPUSD=X", "USDCAD=X", "EURUSD=X", "USDJPY=X", "USDCHF=X")
getSymbols(forex, src = "yahoo", from = "2015-01-01", to = "2025-03-16")

str(`AUDUSD=X`)
str(`GBPUSD=X`)
str(`USDCAD=X`)
str(`EURUSD=X`)
str(`USDJPY=X`)
str(`USDCHF=X`)

assets <- c("AEX", "GDAXI", "HSI", "FTSE", "GSPC", "FCHI", "STI", "N225",
            "AUDUSD=X", "GBPUSD=X", "USDCAD=X", "EURUSD=X", "USDJPY=X", "USDCHF=X")

merged_data <- NULL

for (symbol in assets) {
    if (exists(symbol)) {  
        asset_data <- Cl(get(symbol))  
        colnames(asset_data) <- symbol  
        if (is.null(merged_data)) {
            merged_data <- asset_data
        } else {
            merged_data <- merge(merged_data, asset_data)
        }
    } else {
        print(paste("object", symbol, "not found"))
    }
}

str(merged_data)
head(merged_data)

# Заполняем пропущенные значения вперед
merged_data <- na.locf(merged_data, na.rm = FALSE)
# Заполняем пропущенные значения назад
merged_data <- na.locf(merged_data, fromLast = TRUE)





# S&P 500 ETF - SPY - Крупнейший ETF на индекс S&P 500
# Nasdaq 100 ETF - QQQ - ETF на крупнейшие технологические компании
# Small-Cap ETF	IWM - ETF - на малые компании США (Russell 2000)
# Europe ETF - VGK - ETF на европейский рынок
# Emerging Markets ETF - EEM - ETF на акции развивающихся рынков
# Gold ETF - GLD - ETF, отслеживающий цены на золото
# Real Estate ETF (REITs) - VNQ - ETF на недвижимость (Real Estate Investment Trusts)
# Oil & Gas ETF - XLE - ETF на энергетический сектор США (нефть и газ)

# Определяем тикеры для ETF и облигаций
etf_list <- c("SPY", "QQQ", "IWM", "VGK", "EEM", "GLD", "VNQ", "XLE")

# Long-Term US Treasury Bonds - TLT - Казначейские облигации США (долгосрочные, более 20 лет)
# Short-Term US Treasury Bonds - SHY - Казначейские облигации США (короткие, 1-3 года)
# Corporate Bonds ETF - LQD - ETF на корпоративные облигации инвестиционного уровня
# High-Yield Bonds ETF - HYG - ETF на высокодоходные (junk) облигации
# Inflation-Protected Bonds ETF (TIPS) - TIP - Облигации США, защищенные от инфляции
# Municipal Bonds ETF - MUB - ETF на муниципальные облигации
# Emerging Market Bonds ETF - EMB - ETF на облигации развивающихся стран
# Convertible Bonds ETF - CWB - ETF на конвертируемые облигации (ближе к акциям)

bonds_list <- c("TLT", "SHY", "LQD", "HYG", "TIP", "MUB", "EMB", "CWB")


# Определяем тикеры технологических акций
tech_stocks <- c("TSLA", "NVDA", "META", "AMZN", "GOOGL", "MSFT", "DASH", "UBER",
                 "LYFT", "AAPL", "TMUS", "INTC", "MCHP", "HPQ", "SAP")

# Объединяем все тикеры
assets <- c(etf_list, bonds_list, tech_stocks)

# Загружаем данные с Yahoo Finance
getSymbols(assets, src = "yahoo", from = "2015-01-01", to = "2025-02-28")

# Создаем объединенный датафрейм с ценами закрытия (Close)
merged_data <- NULL
for (ticker in assets) {
        asset_data <- Cl(get(ticker)) 
        colnames(asset_data) <- ticker 
        if (is.null(merged_data)) {
                merged_data <- asset_data
        } else {
                merged_data <- merge(merged_data, asset_data)
        }
}
# Заполняем пропущенные значения (если есть)
library(zoo)
merged_data <- na.locf(merged_data)

# Проверяем финальный набор данных
head(merged_data)


# Функция для расчета лог-доходностей
log_returns <- diff(log(merged_data))

# Удаляем первую строку (так как разность приводит к NA)
log_returns <- na.omit(log_returns)

# Просматриваем первые строки лог-доходностей
head(log_returns)


