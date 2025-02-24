# install.packages("here")
# install.packages("lubridate")
# install.packages("zoo")
# install.packages("Metrics")
# install.packages("mgcv")
# install.packages("ggplot2")
# install.packages("sf")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("naturalearthhires")
# install.packages("devtools")

library(here)
library(readxl)
library(zoo)
library(lubridate)
library(DIMORA)
library(forecast)
library(fpp2)
library(dplyr)
library(lmtest)
library(ggplot2)
library(tidyr)
library(Metrics)
library(prophet)
library(gam)
library(tseries)
library(scales)
library(gridExtra)
library(corrplot)
library(mgcv)

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

################################################
#         DATA LOADING AND PREPROCESSING
################################################

#______________________________Val di Fassa arrivals data__________________________________

  valdifassa_data <- read_excel(here("ValDiFassa_tourism_data.xlsx"), sheet = "ValDiFassa")
  
  # Extract the time and "Total arrivals" dropping rows with NA
  arrivals_data <- valdifassa_data[!is.na(valdifassa_data$`Total arrivals`), 
                                   c("Time", "Total arrivals")]
  
  names(arrivals_data)[names(arrivals_data) == "Total arrivals"] <- "Total_arrivals"
  
  #date into yyyy/mm/dd format
  arrivals_data$Time <- dmy(paste0("01 ", arrivals_data$Time)) 
  
  
  # #Create time indices and month columns
  arrivals_data$time_index <- seq_len(nrow(arrivals_data))
  arrivals_data$month      <- factor(format(arrivals_data$Time, "%m"))
  arrivals_data$month_num  <- as.numeric(format(arrivals_data$Time, "%m"))
  
  ts_arrivals <- ts(arrivals_data$Total_arrivals, start = c(2018, 12), frequency = 12)
  
  prophet_data <- arrivals_data %>%
    select(Time, Total_arrivals) %>%
    rename(ds = Time, y = Total_arrivals)

  
#___________________________Val di Fassa Hotels arrivals data_____________________________
  
  # Extract the time and "Hotel arrivals" dropping rows with NA
  hotel_arrivals_data <- valdifassa_data[95:nrow(valdifassa_data), 
                                         c("Time", "Hotel arrivals")]
  
  names(hotel_arrivals_data)[names(hotel_arrivals_data) == "Hotel arrivals"] <- "Hotel_arrivals"
  
  #date into yyyy/mm/dd format
  hotel_arrivals_data$Time <- dmy(paste0("01 ", hotel_arrivals_data$Time)) 
  
  
  # #Create time indices and month columns
  hotel_arrivals_data$time_index <- seq_len(nrow(hotel_arrivals_data))
  hotel_arrivals_data$month      <- factor(format(hotel_arrivals_data$Time, "%m"))
  hotel_arrivals_data$month_num  <- as.numeric(format(hotel_arrivals_data$Time, "%m"))
  
  ts_hotel_arrivals <- ts(hotel_arrivals_data$Hotel_arrivals, start = c(2018, 12), frequency = 12)
  
  prophet_data_hotel <- hotel_arrivals_data %>%
    select(Time, Hotel_arrivals) %>%
    rename(ds = Time, y = Hotel_arrivals)
  
#____________________________Trentino vs Val di Fassa data__________________________________
  
  data <- read_excel(here("ValDiFassa_tourism_data.xlsx"), sheet = "Comparison Trentino-VdF")
  str(data)
  
  # Formatting of Time column with lubridate
  data$Time <- dmy(paste0("01 ", data$Time))
  
  head(data)
  summary(data)
  
  difference_data <- data[,c(1, 18:25)]
  summary(difference_data)
  
  prophet_data_difference <- difference_data %>%
    select(Time, `DIFFERENCE TOTAL HOTEL arrivals`) %>%
    rename(ds = Time, y = `DIFFERENCE TOTAL HOTEL arrivals`)
  
#______________________________Foreigners proportion data___________________________________
  
  data_foreigners <- read_excel(here("ValDiFassa_tourism_data.xlsx"), sheet = "Nationality")
  str(data_foreigners)
  
  #Creating the overall proportion of foreigners time series
  foreigners_proportion_ts <- data_foreigners[, c(1, 4)] #select the proportion data
  foreigners_proportion_ts$Time <- dmy(paste0("01 ", foreigners_proportion_ts$Time))
  foreigners_proportion_ts <- ts(foreigners_proportion_ts$`Normalized foreigners arrivals`, start = c(2018, 12), frequency = 12)
  
  # Print the Time Series
  plot(foreigners_proportion_ts, 
       main = "Normalized Foreigners Arrivals", 
       xlab = "Time", 
       ylab = "Proportion", 
       col = "blue", 
       lwd = 2)
  tsdisplay(foreigners_proportion_ts)
  plot(decompose(foreigners_proportion_ts)) #PLOT separate trend and seasonality
  checkresiduals(foreigners_proportion_ts)

################################################
#                     EDA
################################################

  #______________________________Italian mountain provinces EDA___________________________________
  
  # Italy boundaries
  italy_sf <- ne_states(country = "Italy", returnclass = "sf")
  
  #Trentino-Alto Adige
  trentino_sf <- italy_sf[italy_sf$name == "Trentino-Alto Adige", ]
  
  # 2016-2023 oveerall tourists' arrivals
  data_provinces <- data.frame(
    city = c("Bolzano", "Trento", "Belluno", "Aosta", "Sondrio"),
    arrivals = c(55837984, 32371073, 7459781, 8930284, 6345599),
    lat = c(46.498, 46.066, 46.141, 45.737, 46.170), # Latitude
    lon = c(11.354, 11.121, 12.215, 7.317, 9.872),   # Longitude
    province = c("Other", "Autonomous province of Trento", "Other", "Other", "Other") # Trentino
  )
  
  
  # Map plot
  ggplot() +
    # Draw Italy
    geom_sf(data = italy_sf, fill = "aliceblue", color = "black") +
    geom_point(data = data_provinces, aes(x = lon, y = lat, color = province, size = arrivals), alpha = 0.9) +
    scale_size_continuous(range = c(2, 10)) + # Aumenta il range per ingrandire i cerchi
    # Cities names
    geom_text(data = data_provinces, aes(x = lon, y = lat, label = city), hjust = -0.2, vjust = -0.5, size = 3, color = "black") +
    scale_size_continuous(name = "Arrivals", range = c(8, 20)) +
    scale_color_manual(name = "Province", values = c("Autonomous province of Trento" = "red", "Others" = "lightblue")) +
    coord_sf(xlim = c(6.5, 13), ylim = c(44, 47)) +
    # Title
    labs(
      title = "Tourist Arrivals in mountain provinces map",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),  
      axis.title = element_blank(), 
      axis.ticks = element_blank() 
    )
  
  #exploded Pie chart
  if (!require(plotrix)) install.packages("plotrix")
  
  # load package
  library(plotrix)
  
  # same data for mountains provinces
  data_provinces <- c(55837984, 32371073, 7459781, 8930284, 6345599)
  labels <- c("Bolzano", "Trento", "Belluno", "Aosta", "Sondrio")
  
  # Percentages
  percentages <- round(100 * data_provinces / sum(data_provinces), 1)
  labels_with_percentages <- paste(labels, percentages, "%")
  
  explode <- c(0, 0.1, 0, 0, 0)
  
  # Color vector
  highlight_color <- "red" 
  default_colors <- hcl.colors(length(data), "Spectral")  
  colors <- ifelse(labels == "Trento", "red", "lightgray")
  
  # 3D pie graph
  pie3D(data_provinces,
        theta = 1,
        labels = labels_with_percentages,
        #explode = c(0, 0.1, 0, 0, 0), 
        main = "Mountain provinces of Italy arrivals pie chart (2016-2023)",
        #col = rainbow(length(data))
        # col = hcl.colors(length(data), "Spectral"),
        col = colors,
        border = "white",
        explode = 0.1
  )
  
  #________________________________________Trentino EDA___________________________________________
  trentino_data <- data[,c(1:9)]
  
  ###
  #DESCRIPTIVE STATISTICS
  summary(trentino_data)
  
  monthly_means <- trentino_data %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))
  print(monthly_means)
  
  ###
  #PLOT for TOTAL ARRIVALS distribution, Italian vs Foreigners
  #HOTEL Arrivals
  hotel_pie_data <- trentino_data %>%
    summarise(
      "Italian Hotel Arrivals" = sum(trentino_data$`TRENTINO Italian HOTEL arrivals`, na.rm = TRUE),
      "Foreigners Hotel Arrivals" = sum(trentino_data$`TRENTINO Foreigners HOTEL arrivals`, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Category", values_to = "Total") %>%
    mutate(Percentage = Total / sum(Total) * 100)
  
  hotel_pie <- ggplot(hotel_pie_data, aes(x = "", y = Total, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              position = position_stack(vjust = 0.5), size = 4) +
    labs(title = "Hotel Arrivals distribution, Autonomous Province of Trento (%)", fill = "") +
    scale_fill_brewer(palette = "Set2")
  
  #NON-HOTEL Arrivals
  non_hotel_pie_data <- trentino_data %>%
    summarise(
      "Italian Non-Hotel Arrivals" = sum(trentino_data$`TRENTINO Italian NON-HOTEL arrivals`, na.rm = TRUE),
      "Foreigners Non-Hotel Arrivals" = sum(trentino_data$`TRENTINO Foreigners NON-HOTEL arrivals`, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Category", values_to = "Total") %>%
    mutate(Percentage = Total / sum(Total) * 100)
  
  non_hotel_pie <- ggplot(non_hotel_pie_data, aes(x = "", y = Total, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              position = position_stack(vjust = 0.5), size = 4) +
    labs(title = "Non-Hotel Arrivals distribution, Autonomous Province of Trento (%)", fill = "") +
    scale_fill_brewer(palette = "Set2")
  
  grid.arrange(hotel_pie, non_hotel_pie, nrow = 2)
  
  ###
  #TIME SERIES for Italian and Foreigners in HOTELS
  italian_hotel_ts <- ts(trentino_data$`TRENTINO Italian HOTEL arrivals`, start = c(2018, 12), frequency = 12)
  foreigners_hotel_ts <- ts(trentino_data$`TRENTINO Foreigners HOTEL arrivals`, start = c(2018, 12), frequency = 12)
  total_hotel_ts <- ts(trentino_data$`TRENTINO TOTAL HOTEL arrivals`, start = c(2018, 12), frequency = 12)
  
  trentino_data_ts <- ts(data[, c("TRENTINO Italian HOTEL arrivals", "TRENTINO Foreigners HOTEL arrivals", "TRENTINO TOTAL HOTEL arrivals")],
                         start = c(2018, 12), frequency = 12)
  #Autoplot for the time series
  autoplot(trentino_data_ts, facets = TRUE) +
    labs(title = "Time Series: Hotel Arrivals",
         x = "Year", y = "Arrivals") +
    theme_minimal()
  
  #Multiple plot with the three ts together
  par(mar = c(5, 4, 4, 8), xpd = TRUE)
  ts.plot(italian_hotel_ts, foreigners_hotel_ts, total_hotel_ts, 
          col = c("blue", "red", "green"), lty = 1:3,
          main = "Hotel Arrivals in Autonomous Province of Trento", xlab = "Time", ylab = "Arrivals")
  legend("topright", inset = c(-0.25, 0), legend = c("Italian Hotel Arrivals", "Foreigners Hotel Arrivals", "Total Hotel Arrivals"), 
         col = c("blue", "red", "green"), lty = 1:3, box.lty = 0, cex = 0.8,
         title = "Legend")
  par(mar = c(5, 4, 4, 2))
  
  #Checking for residuals
  checkresiduals(total_hotel_ts)
  
  acf(total_hotel_ts)
  pacf(total_hotel_ts)
  
  ###
  #COMPARISON HOTEL and NON-HOTEL ARRIVALS (Trentino)
  comparison <- trentino_data %>%
    summarise(
      Italian_Hotel = sum(trentino_data$`TRENTINO Italian HOTEL arrivals`, na.rm = TRUE),
      Foreigners_Hotel = sum(trentino_data$`TRENTINO Foreigners HOTEL arrivals`, na.rm = TRUE),
      Italian_NonHotel = sum(trentino_data$`TRENTINO Italian NON-HOTEL arrivals`, na.rm = TRUE),
      Foreigners_NonHotel = sum(trentino_data$`TRENTINO Foreigners NON-HOTEL arrivals`, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Category", values_to = "Total")
  
  comparison_plot <- ggplot(comparison, aes(x = Category, y = Total, fill = Category)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Comparison of Arrivals: Hotel vs Non-Hotel, Autonomous Province of Trento", y = "Total Arrivals", x = "") +
    scale_fill_brewer(palette = "Set3")
  
  grid.arrange(hotel_pie, comparison_plot, nrow = 2)
  
  ###
  #CUMULATIVE SUM OF ARRIVALS OVER TIME
  data_cumsum <- trentino_data %>%
    mutate(
      Italian_Cumsum = cumsum(trentino_data$`TRENTINO Italian TOTAL arrivals`),
      Foreigners_Cumsum = cumsum(trentino_data$`TRENTINO Foreigners TOTAL arrivals`),
      Total_Cumsum = cumsum(trentino_data$`TRENTINO Italian TOTAL arrivals` + trentino_data$`TRENTINO Foreigners TOTAL arrivals`)
    )
  
  cumsum_plot <- ggplot(data_cumsum, aes(x = Time)) +
    geom_line(aes(y = Italian_Cumsum, color = "Italian Arrivals"), size = 1) +
    geom_line(aes(y = Foreigners_Cumsum, color = "Foreigners Arrivals"), size = 1) +
    geom_line(aes(y = Total_Cumsum, color = "Total Arrivals"), size = 1) +
    labs(
      title = "Cumulative Sum of Arrivals Over Time, Autonomous Province of Trento",
      x = "Time",
      y = "Cumulative Arrivals",
      color = "Category"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("blue", "red", "green"))
  
  print(cumsum_plot)
  
  ###
  #YEAR-OVER-YEAR COMPARISON OF ARRIVALS
  data_yoy <- trentino_data %>%
    mutate(Year = year(Time), Month = month(Time, label = TRUE)) %>%
    group_by(Year, Month) %>%
    summarise(
      Italian_Arrivals = sum(`TRENTINO Italian TOTAL arrivals`, na.rm = TRUE),
      Foreigners_Arrivals = sum(`TRENTINO Foreigners TOTAL arrivals`, na.rm = TRUE),
      Total_Arrivals = sum(`TRENTINO Italian TOTAL arrivals` + `TRENTINO Foreigners TOTAL arrivals`, na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = starts_with("Italian_Arrivals") | starts_with("Foreigners_Arrivals") | starts_with("Total_Arrivals"),
      names_to = "Category",
      values_to = "Arrivals"
    )
  
  yoy_plot <- ggplot(data_yoy, aes(x = Month, y = Arrivals, color = as.factor(Year), group = Year)) +
    geom_line(size = 1) +
    facet_wrap(~Category, scales = "free_y") +
    labs(
      title = "Year-over-Year Comparison of Arrivals, Autonomous Province of Trento",
      x = "Month",
      y = "Arrivals",
      color = "Year"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(yoy_plot)
  
  #_______________________________________ValdiFassa EDA__________________________________________
  
  valdifassa_data <- data[,c(1, 10:17)]
  
  ###
  #DESCRIPTIVE STATISTICS
  summary(valdifassa_data)
  
  monthly_means <- valdifassa_data %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))
  print(monthly_means)
  
  ###
  #PLOT for TOTAL ARRIVALS distribution, Italian vs Foreigners
  #HOTEL Arrivals
  hotel_pie_valdifassa_data <- valdifassa_data %>%
    summarise(
      "Italian Hotel Arrivals" = sum(`VALDIFASSA Italian HOTEL arrivals`, na.rm = TRUE),
      "Foreigners Hotel Arrivals" = sum(`VALDIFASSA Foreigner HOTEL arrivals`, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Category", values_to = "Total") %>%
    mutate(Percentage = Total / sum(Total) * 100)
  
  hotel_pie_valdifassa <- ggplot(hotel_pie_valdifassa_data, aes(x = "", y = Total, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              position = position_stack(vjust = 0.5), size = 4) +
    labs(title = "Hotel Arrivals distribution, Val di Fassa (%)", fill = "") +
    scale_fill_brewer(palette = "Set2")
  
  plot(hotel_pie_valdifassa)
  
  ###
  #TIME SERIES for Italian and Foreigners in HOTELS
  italian_hotel_valdifassa_ts <- ts(valdifassa_data$`VALDIFASSA Italian HOTEL arrivals`, start = c(2018, 12), frequency = 12)
  foreigners_hotel_valdifassa_ts <- ts(valdifassa_data$`VALDIFASSA Foreigner HOTEL arrivals`, start = c(2018, 12), frequency = 12)
  total_hotel_valdifassa_ts <- ts(valdifassa_data$`VALDIFASSA TOTAL HOTEL arrivals`, start = c(2018, 12), frequency = 12)
  
  valdifassa_data_ts <- ts(valdifassa_data[, c("VALDIFASSA Italian HOTEL arrivals", "VALDIFASSA Foreigner HOTEL arrivals", "VALDIFASSA TOTAL HOTEL arrivals")],
                           start = c(2018, 12), frequency = 12)
  
  autoplot(valdifassa_data_ts, facets = TRUE) +
    labs(title = "Time Series: Hotel Arrivals (Val di Fassa)",
         x = "Year", y = "Arrivals") +
    theme_minimal()
  
  par(mar = c(5, 4, 4, 8), xpd = TRUE)
  ts.plot(italian_hotel_valdifassa_ts, foreigners_hotel_valdifassa_ts, total_hotel_valdifassa_ts, 
          col = c(
            rgb(0, 0, 1, alpha = 0.5),  # Blu trasparente
            rgb(1, 0, 0, alpha = 0.5),  # Rosso trasparente
            rgb(0, 0, 0, alpha = 0.5)   # Nero trasparente
          ),
          lwd = 1.5 ,
          main = "Hotel Arrivals in Val di Fassa", xlab = "Time", ylab = "Arrivals")
  legend("topright", inset = c(-0.25, 0), legend = c("Italian Hotel Arrivals", "Foreigners Hotel Arrivals", "Total Hotel Arrivals"), 
         col = c("blue", "red", "black"), lty = 1, box.lty = 0, cex = 0.8,
         title = "Legend")
  par(mar = c(5, 4, 4, 2))
  
  #Checking for residuals
  checkresiduals(total_hotel_valdifassa_ts)
  
  acf(total_hotel_valdifassa_ts)
  pacf(total_hotel_valdifassa_ts)
  
  ###
  #COMPARISON HOTEL and NON-HOTEL ARRIVALS (Val di Fassa)
  comparison_valdifassa <- valdifassa_data %>%
    summarise(
      Italian_Hotel = sum(`VALDIFASSA Italian HOTEL arrivals`, na.rm = TRUE),
      Foreigners_Hotel = sum(`VALDIFASSA Foreigner HOTEL arrivals`, na.rm = TRUE),
      Italian_NonHotel = sum(`VALDIFASSA Italian NON-HOTEL arrivals`, na.rm = TRUE),
      Foreigners_NonHotel = sum(`VALDIFASSA Foreigner NON-HOTEL arrivals`, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Category", values_to = "Total")
  
  comparison_valdifassa_plot <- ggplot(comparison_valdifassa, aes(x = Category, y = Total, fill = Category)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Comparison of Arrivals: Hotel vs Non-Hotel, Val di Fassa", y = "Total Arrivals", x = "") +
    scale_fill_brewer(palette = "Set3")
  
  grid.arrange(hotel_pie_valdifassa, comparison_valdifassa_plot, nrow = 2)
  
  ###
  #CUMULATIVE SUM OF ARRIVALS OVER TIME
  data_cumsum_valdifassa <- valdifassa_data %>%
    mutate(
      Italian_Cumsum = cumsum(`VALDIFASSA Italian TOTAL arrivals`),
      Foreigners_Cumsum = cumsum(`VALDIFASSA Foreigners TOTAL arrivals`),
      Total_Cumsum = cumsum(`VALDIFASSA Italian TOTAL arrivals` + `VALDIFASSA Foreigners TOTAL arrivals`)
    )
  
  cumsum_valdifassa_plot <- ggplot(data_cumsum_valdifassa, aes(x = Time)) +
    geom_line(aes(y = Italian_Cumsum, color = "Italian Arrivals"), size = 1) +
    geom_line(aes(y = Foreigners_Cumsum, color = "Foreigners Arrivals"), size = 1) +
    geom_line(aes(y = Total_Cumsum, color = "Total Arrivals"), size = 1) +
    labs(
      title = "Cumulative Sum of Arrivals Over Time (Val di Fassa)",
      x = "Time",
      y = "Cumulative Arrivals",
      color = "Category"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("blue", "red", "green"))
  
  print(cumsum_valdifassa_plot)
  
  ###
  #YEAR-OVER-YEAR COMPARISON OF ARRIVALS
  data_yoy_valdifassa <- valdifassa_data %>%
    mutate(Year = year(Time), Month = month(Time, label = TRUE)) %>%
    group_by(Year, Month) %>%
    summarise(
      Italian_Arrivals = sum(`VALDIFASSA Italian TOTAL arrivals`, na.rm = TRUE),
      Foreigners_Arrivals = sum(`VALDIFASSA Foreigners TOTAL arrivals`, na.rm = TRUE),
      Total_Arrivals = sum(`VALDIFASSA Italian TOTAL arrivals` + `VALDIFASSA Foreigners TOTAL arrivals`, na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = starts_with("Italian_Arrivals") | starts_with("Foreigners_Arrivals") | starts_with("Total_Arrivals"),
      names_to = "Category",
      values_to = "Arrivals"
    )
  
  yoy_plot_valdifassa <- ggplot(data_yoy_valdifassa, aes(x = Month, y = Arrivals, color = as.factor(Year), group = Year)) +
    geom_line(size = 1) +
    facet_wrap(~Category, scales = "free_y") +
    labs(
      title = "Year-over-Year Comparison of Arrivals (Val di Fassa)",
      x = "Month",
      y = "Arrivals",
      color = "Year"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(yoy_plot_valdifassa)
  
  
  #______________________________Foreigners proportion data EDA___________________________________
  
  #Creating the multiple time series for each city
  foreigners_proportion_data_city <- data_foreigners[, -c(2, 3, 4)]
  foreigners_proportion_data_city$Time <- dmy(paste0("01", foreigners_proportion_data_city$Time))
  foreigners_proportion_data_city <- ts(foreigners_proportion_data_city[, -1], start = c(2018, 12), frequency = 12)
  
  # mts to dataframe conversion
  data_mts <- as.data.frame(foreigners_proportion_data_city)
  
  # time column
  data_mts$Time <- time(foreigners_proportion_data_city)
  
  # long format
  long_data_city <- melt(data_mts, id.vars = "Time", variable.name = "Category", value.name = "Arrivals")
  
  # Extract name and type from the column name
  long_data_city$City <- sub(" (.*)$", "", long_data_city$Category)
  long_data_city$Type <- sub(".* (Italian|Foreign).*", "\\1", long_data_city$Category)
  
  data_foreigners_locations <- data_foreigners[, -c(2, 3, 4)]
  data_foreigners_locations$Time <- dmy(paste0("01 ", data_foreigners_locations$Time))
  
  #Reshape the data to long format
  long_data <- data_foreigners_locations %>%
    pivot_longer(
      cols = -Time,
      names_to = c("Location", "Type", "Arrival_Type"),
      names_sep = "\\s+",
      values_to = "Arrivals"
    )
  
  # Months names in english
  long_data$Month <- month(long_data$Time, label = TRUE, abbr = TRUE)  # Usa i nomi abbreviati
  levels(long_data$Month) <- month.abb[1:12]  # Sovrascrive le etichette con i nomi completi in inglese
  
  #plots
  
  #Time series plot of total arrivals by type
  total_arrivals <- long_data %>%
    group_by(Time, Type) %>%
    summarize(Total = sum(Arrivals, na.rm = TRUE))
  
  ggplot(total_arrivals, aes(x = Time, y = Total, color = Type)) +
    geom_line(size = 1) +
    labs(
      title = "Time Series of Val di Fassa Arrivals by Type",
      x = "Time",
      y = "Total Arrivals"
    ) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_minimal()
  
  #Time series plot, italians vs foreigners for each city
  ggplot(long_data_city, aes(x = Time, y = Arrivals, color = Type, group = Type)) +
    geom_line(size = 1) +
    facet_wrap(~ City, scales = "free_y") +
    labs(
      title = "Italians and foreigners arrivals for each Val di Fassa city",
      x = "Time",
      y = "Arrivals",
      color = "Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  #Pie charts for each city

  summarized_data <- long_data_city %>%
    group_by(City, Type) %>%
    summarize(Total_Arrivals = sum(Arrivals, na.rm = TRUE), .groups = "drop") %>%
    group_by(City) %>%
    mutate(Percentage = Total_Arrivals / sum(Total_Arrivals) * 100) 
  
  ggplot(summarized_data, aes(x = "", y = Percentage, fill = Type)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    facet_wrap(~ City) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              position = position_stack(vjust = 0.5), size = 4) +
    labs(
      title = "Proportion of italians and foreigners for each city (2016-2023)",
      fill = "Type"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "bottom"
    )
  
  #stacked bar plot for each city
  ggplot(long_data, aes(x = Time, y = Arrivals, fill = Type)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = "Overall arrivals: italians and foreigners",
      x = "Time",
      y = "Arrivals",
      fill = "Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  #Seasonal Trends for Each Location
  ggplot(long_data, aes(x = Time, y = Arrivals, color = Arrival_Type)) +
    geom_line() +
    facet_wrap(~ Location, scales = "free_y") +
    labs(
      title = "Seasonal Trends in Arrivals by Location",
      x = "Time",
      y = "Number of Arrivals"
    ) +
    theme_minimal()
  
  #Boxplot of Arrivals by Location
  ggplot(long_data, aes(x = Location, y = Arrivals, fill = Arrival_Type)) +
    geom_boxplot() +
    labs(
      title = "Distribution of Arrivals by Location",
      x = "Location",
      y = "Number of Arrivals"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #Year-over-Year Comparison
  long_data$Year <- year(long_data$Time)
  long_data$Month <- month(long_data$Time, label = TRUE)
  
  yoy_comparison <- long_data %>%
    group_by(Location, Year, Month, Arrival_Type) %>%
    summarize(Total = sum(Arrivals, na.rm = TRUE))
  
  ggplot(yoy_comparison, aes(x = Month, y = Total, color = as.factor(Year), group = Year)) +
    geom_line() +
    facet_wrap(~ Location + Arrival_Type, scales = "free_y") +
    labs(
      title = "Year-over-Year Comparison of Arrivals",
      x = "Month",
      y = "Total Arrivals",
      color = "Year"
    ) +
    theme_minimal()
  
  #year-over-year area plot
  ggplot(yoy_comparison, aes(x = Month, y = Total, fill = as.factor(Year), group = Year)) +
    geom_area(position = "identity", alpha = 0.2) +  # Grafico ad area con trasparenza
    facet_wrap(~ Location + Arrival_Type, scales = "free_y") +
    labs(
      title = "Year-over-Year Comparison of Arrivals",
      x = "Month",
      y = "Total Arrivals",
      fill = "Year"
    ) +
    theme_minimal()
  
  
  # Comparison of Seasonal Patterns Across Locations
  long_data$Season <- case_when(
    month(long_data$Time) %in% c(12, 1, 2) ~ "Winter",
    month(long_data$Time) %in% c(3, 4, 5) ~ "Spring",
    month(long_data$Time) %in% c(6, 7, 8) ~ "Summer",
    TRUE ~ "Fall"
  )
  
  seasonal_data <- long_data %>%
    group_by(Location, Season, Arrival_Type) %>%
    summarize(Seasonal_Total = sum(Arrivals, na.rm = TRUE))
  
  ggplot(seasonal_data, aes(x = Season, y = Seasonal_Total, fill = Season)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ Location, scales = "free_y") +
    scale_fill_manual(
      values = c("Winter" = "#A6CEE3",  # Colore per Winter
                 "Spring" = "#B2DF8A",  # Colore per Spring
                 "Summer" = "#FDBF6F",  # Colore per Summer
                 "Fall"   = "#FB9A99")  # Colore per Fall
    ) +
    labs(
      title = "Seasonal Arrivals by Location",
      x = "Season",
      y = "Total Arrivals",
      fill = "Season"
    ) +
    theme_minimal()
  
  #Stacked area
  # Aggregate by time and location for stacked area plot
  stacked_data <- long_data %>%
    group_by(Time, Location) %>%
    summarize(Total_Arrivals = sum(Arrivals, na.rm = TRUE))
  
  ggplot(stacked_data, aes(x = Time, y = Total_Arrivals, fill = Location)) +
    geom_area() +
    labs(
      title = "Stacked Area Plot of Arrivals by Location",
      x = "Time",
      y = "Total Arrivals"
    ) +
    theme_minimal()
  
  #ROLLINIG VARIANCE
  # Calculate rolling variance (e.g., 30-day window)
  long_data <- long_data %>%
    group_by(Location, Arrival_Type) %>%
    mutate(Rolling_Variance = zoo::rollapply(Arrivals, 12, var, fill = NA, align = "right"))
  
  # Plot rolling variance
  ggplot(long_data, aes(x = Time, y = Rolling_Variance, color = Location)) +
    geom_line(size = 1) +
    facet_wrap(~ Arrival_Type, scales = "free_y") +
    labs(
      title = "30-Months Rolling Variance of Arrivals for each City",
      x = "Time",
      y = "Rolling Variance"
    ) +
    theme_minimal()
  
  #CUMULATIVE PLOT
  long_data <- long_data %>%
    group_by(Location, Arrival_Type) %>%
    mutate(Cumulative_Arrivals = cumsum(Arrivals))
  
  ggplot(long_data, aes(x = Time, y = Cumulative_Arrivals, color = Location)) +
    geom_line(size = 1) +
    facet_wrap(~ Arrival_Type) +
    labs(
      title = "Cumulative Sum of Arrivals Over Time",
      x = "Time",
      y = "Cumulative Arrivals"
    ) +
    theme_minimal()+ theme(axis.line=element_line(color = "black"))
  
################################################
#           VAL DI FASSA ANALYSIS
################################################

    #________________linear model_________________
    
    # Fit a linear model: trend + month factor
    lm_model <- tslm(
      ts_arrivals ~ trend + season
    )
    summary(lm_model)
    
    # Get fitted values
    fitted_values <- lm_model$fitted.values
    
    # Compute and plot residuals
    residuals_lm <- arrivals_data$Total_arrivals - fitted_values
    
    # Residuals plot
    tsdisplay(residuals(lm_model))
    
    # Compute RMSE, MAE
    mae(arrivals_data$Total_arrivals, fitted(lm_model))
    rmse(arrivals_data$Total_arrivals, fitted(lm_model))
    AIC(lm_model)
    
    
    # Forecast
    autoplot(forecast(lm_model, h = 12)) + 
      ggtitle("Linear model (trend + season) (12 months)") +
      xlab("Time") +
      ylab("Arrivals")
    
    #_______________Exponential smoothing__________________
    
    # Fit an ETS model
    ets_model <- ets(ts_arrivals)
    summary(ets_model)
    
    # Check residuals
    checkresiduals(ets_model)
    
    # Extract residuals
    residuals_ets <- residuals(ets_model)
    
    # Compute RMSE and MAE
    mae(arrivals_data$Total_arrivals, fitted(ets_model))
    rmse(arrivals_data$Total_arrivals, fitted(ets_model))
    AIC(ets_model)
    
    # Forecast
    autoplot(forecast(ets_model, h = 12)) + 
      ggtitle("Exponential smoothing forecasts (12 months)") +
      xlab("Time") +
      ylab("Arrivals")
    
    #______________________ARIMA__________________________
    
    arima_model <- auto.arima(ts_arrivals)
    print(arima_model)
    
    # Evaluate model residuals
    checkresiduals(arima_model)
    
    # Forecast for the next 12 months
    forecast_horizon <- 12
    forecasted_values <- forecast(arima_model, h = forecast_horizon)
    
    # Plot the forecast
    autoplot(forecasted_values) +
      ggtitle("Forecast of Total Arrivals (12 months) - SARIMA(0,1,0)(0,1,1)[12]") +
      ylab("Arrivals") +
      xlab("Time")

    print(forecasted_values)
    
    mae(arrivals_data$Total_arrivals, fitted(arima_model))
    rmse(arrivals_data$Total_arrivals, fitted(arima_model))
    AIC(arima_model)
    
    #______________________Prophet__________________________
    
    # Fit the Prophet model (using multiplicative seasonality)
    prophet_model <- prophet(
      prophet_data,
      seasonality.mode = 'multiplicative'
    )
    
    # Make predictions for the next 12 months
    future <- make_future_dataframe(prophet_model, periods = 12, freq = "month")
    forecast <- predict(prophet_model, future)
    
    # Plot the forecast
    plot(prophet_model, forecast) +
      ggtitle("Forecast of Total Arrivals (12 months) - Prophet") +
      ylab("Arrivals") +
      xlab("Time")
    
    # plot trend & seasonality components
    Sys.setlocale("LC_TIME", "English_United States.UTF-8")
    prophet_plot_components(prophet_model, forecast)
    
    actual    <- prophet_data$y
    predicted <- forecast$yhat[1:length(actual)]
    residuals <- actual - predicted
    
    # Plot residuals over time
    ggplot(data.frame(Date = prophet_data$ds, Residuals = residuals), aes(x = Date, y = Residuals)) +
      geom_line() +
      ggtitle("Residuals of Prophet Model") +
      xlab("Date") +
      ylab("Residuals")
    
    # ACF of residuals
    acf_residuals <- acf(residuals, main = "ACF of Residuals", lag.max = 36)
    
    # Compute RMSE, MAE
    mae(arrivals_data$Total_arrivals, predicted)
    rmse(arrivals_data$Total_arrivals, predicted)

    # AIC approximated by hand
    # Get residuals and log-likelihood approximation
    residuals <- actual - predicted
    n <- length(residuals)
    sigma2 <- mean(residuals^2)
    
    # Log-likelihood approximation
    log_likelihood <- -n / 2 * log(2 * pi * sigma2) - sum(residuals^2) / (2 * sigma2)
    
    # Number of parameters (approximated)
    k <- length(prophet_model$params)
    
    # AIC calculation
    AIC <- 2 * k - 2 * log_likelihood
    AIC
    
    #Prophet model with pandemic inclusion
    
    pandemic <- data.frame(
      ds = seq(as.Date("2020-11-01"), as.Date("2021-05-01"), by = "month"),
      holiday = "pandemic"
    )
    
    prophet_model_pandemic <- prophet(
      prophet_data,
      seasonality.mode = 'multiplicative',
      holidays = pandemic
    )
    
    # Make predictions for the next 12 months
    future <- make_future_dataframe(prophet_model_pandemic, periods = 12, freq = "month")
    forecast <- predict(prophet_model_pandemic, future)
    
    # Plot the forecast
    plot(prophet_model_pandemic, forecast) +
      ggtitle("Forecast of Total Arrivals (12 months) - Prophet with pandemic (2020/11/01 to 2021/05/01)") +
      ylab("Arrivals") +
      xlab("Time")
    
    # plot trend & seasonality components
    prophet_plot_components(prophet_model_pandemic, forecast)
    
    actual    <- prophet_data$y
    predicted <- forecast$yhat[1:length(actual)]
    residuals <- actual - predicted
    
    # Plot residuals over time
    ggplot(data.frame(Date = prophet_data$ds, Residuals = residuals), aes(x = Date, y = Residuals)) +
      geom_line() +
      ggtitle("Residuals of Prophet Model") +
      xlab("Date") +
      ylab("Residuals")
    
    # ACF of residuals
    acf_residuals <- acf(residuals, main = "ACF of Residuals", lag.max = 36)
    
    # Compute RMSE, MAE
    mae(arrivals_data$Total_arrivals, predicted)
    rmse(arrivals_data$Total_arrivals, predicted)
    
    # AIC approximated
    # Get residuals and log-likelihood approximation
    residuals <- actual - predicted
    n <- length(residuals)
    sigma2 <- mean(residuals^2)
    
    # Log-likelihood approximation
    log_likelihood <- -n / 2 * log(2 * pi * sigma2) - sum(residuals^2) / (2 * sigma2)
    
    # Number of parameters (approximated)
    k <- length(prophet_model_pandemic$params)
    
    # AIC calculation
    AIC <- 2 * k - 2 * log_likelihood
    AIC
    
    #________________________________GAM__________________________________
    
    # Fit the GAM (Option A: Gaussian)
    gam_model <- gam(
      Total_arrivals ~ s(time_index, k = 24) + s(month_num, bs = "cc", k = 12),
      data   = arrivals_data,
      method = "REML"
    )
    
    # Summaries, fitted values, and residuals
    summary(gam_model)
    fitted_values <- gam_model$fitted.values
    
    # Calculate residuals
    residuals_gam <- arrivals_data$Total_arrivals - fitted_values
    
    #check residuals behaviour
    checkresiduals(gam_model)
      
    
    # Compute RMSE, MAE
    mae(arrivals_data$Total_arrivals, fitted_values)
    rmse(arrivals_data$Total_arrivals, fitted_values)
    
    #compute AIC
    AIC(gam_model)
    
    # Make predictions
    last_time_index <- max(arrivals_data$time_index)
    future_horizon  <- 12
    new_time_index  <- (last_time_index + 1):(last_time_index + future_horizon)
    future_data     <- data.frame(time_index = new_time_index)
    
    last_date <- max(arrivals_data$Time)
    future_data$Date <- seq.Date(from = last_date + 1, by = "month", length.out = future_horizon)
    future_data$month     <- factor(format(future_data$Date, "%m"))
    future_data$month_num <- as.numeric(format(future_data$Date, "%m"))
    
    # Predict future arrivals
    predictions_gam <- predict(gam_model, newdata = future_data, type = "response")
    future_data$predicted_arrivals <- predictions_gam
    
    print(future_data)
    
    # Combine historical + future data for plotting
    history_df <- data.frame(
      Date       = arrivals_data$Time,
      Observed   = arrivals_data$Total_arrivals,
      Fitted     = fitted_values,
      time_index = arrivals_data$time_index
    )
    
    forecast_df <- future_data %>%
      mutate(Observed = NA,
             Fitted   = NA)
    
    plot_data <- bind_rows(
      history_df  %>% mutate(Source = "History"),
      forecast_df %>% mutate(Source = "Forecast")
    )
    
    ggplot(plot_data, aes(x = Date)) +
      geom_line(data = plot_data %>% filter(Source == "History"), 
                aes(y = Observed), color = "black") +
      geom_line(data = plot_data %>% filter(Source == "History"), 
                aes(y = Fitted), color = "blue") +
      geom_line(data = plot_data %>% filter(Source == "Forecast"), 
                aes(y = predicted_arrivals), color = "red") +
      labs(title = "Forecast of Total Arrivals (12 months) - GAM",
           x = "Date", y = "Total Arrivals") +
      theme_minimal()

    
    
################################################
#         VAL DI FASSA HOTELS ANALYSIS
################################################
    
    #________________linear model_________________
    
    # Fit a linear model: trend + month factor
    lm_model <- tslm(
      ts_hotel_arrivals ~ trend + season
    )
    summary(lm_model)
    
    # Get fitted values
    fitted_values <- lm_model$fitted.values
    
    # Compute and plot residuals
    residuals_lm <- hotel_arrivals_data$Hotel_arrivals - fitted_values
    
    # Residuals plot
    tsdisplay(residuals(lm_model))
    
    # Compute RMSE, MAE
    mae(hotel_arrivals_data$Hotel_arrivals, fitted(lm_model))
    rmse(hotel_arrivals_data$Hotel_arrivals, fitted(lm_model))
    AIC(lm_model)
    
    
    # Forecast
    autoplot(forecast(lm_model, h = 12)) + 
      ggtitle("Linear model (trend + season) (12 months)") +
      xlab("Time") +
      ylab("Arrivals")
    
    
    #_______________Exponential smoothing__________________
    
    # Fit an ETS model
    ets_model <- ets(ts_hotel_arrivals)
    summary(ets_model)
    
    # Check residuals
    checkresiduals(ets_model)
    
    # Extract residuals
    residuals_ets <- residuals(ets_model)
    
    # Compute RMSE and MAE
    mae(hotel_arrivals_data$Hotel_arrivals, fitted(ets_model))
    rmse(hotel_arrivals_data$Hotel_arrivals, fitted(ets_model))
    AIC(ets_model)
    
    # Forecast
    autoplot(forecast(ets_model, h = 12)) + 
      ggtitle("Exponential smoothing forecasts (12 months)") +
      xlab("Time") +
      ylab("Arrivals")
    
    #______________________ARIMA__________________________
    
    arima_model <- auto.arima(ts_hotel_arrivals)
    print(arima_model)
    
    # Evaluate model residuals
    checkresiduals(arima_model)
    
    # Forecast for the next 12 months
    forecast_horizon <- 12
    forecasted_values <- forecast(arima_model, h = forecast_horizon)
    
    # Plot the forecast
    autoplot(forecasted_values) +
      ggtitle("Forecast of Total Arrivals (12 months) - SARIMA(0,1,0)(0,1,1)[12]") +
      ylab("Arrivals") +
      xlab("Time")
    
    print(forecasted_values)
    
    mae(hotel_arrivals_data$Hotel_arrivals, fitted(arima_model))
    rmse(hotel_arrivals_data$Hotel_arrivals, fitted(arima_model))
    AIC(arima_model)
    
    #______________________Prophet__________________________
    
    # Fit the Prophet model (using multiplicative seasonality)
    prophet_model <- prophet(
      prophet_data_hotel,
      seasonality.mode = 'multiplicative'
    )
    
    # Make predictions for the next 12 months
    future <- make_future_dataframe(prophet_model, periods = 12, freq = "month")
    forecast <- predict(prophet_model, future)
    
    # Plot the forecast
    plot(prophet_model, forecast) +
      ggtitle("Forecast of Total Arrivals (12 months) - Prophet") +
      ylab("Arrivals") +
      xlab("Time")
    
    # plot trend & seasonality components
    Sys.setlocale("LC_TIME", "English_United States.UTF-8")
    prophet_plot_components(prophet_model, forecast)
    
    actual    <- prophet_data_hotel$y
    predicted <- forecast$yhat[1:length(actual)]
    residuals <- actual - predicted
    
    # Plot residuals over time
    ggplot(data.frame(Date = prophet_data_hotel$ds, Residuals = residuals), aes(x = Date, y = Residuals)) +
      geom_line() +
      ggtitle("Residuals of Prophet Model") +
      xlab("Date") +
      ylab("Residuals")
    
    # ACF of residuals
    acf_residuals <- acf(residuals, main = "ACF of Residuals", lag.max = 36)
    
    # Compute RMSE, MAE
    mae(hotel_arrivals_data$Hotel_arrivals, predicted)
    rmse(hotel_arrivals_data$Hotel_arrivals, predicted)
    
    # AIC approximated by hand
    # Get residuals and log-likelihood approximation
    residuals <- actual - predicted
    n <- length(residuals)
    sigma2 <- mean(residuals^2)
    
    # Log-likelihood approximation
    log_likelihood <- -n / 2 * log(2 * pi * sigma2) - sum(residuals^2) / (2 * sigma2)
    
    # Number of parameters (approximated)
    k <- length(prophet_model$params)  # You might refine this count
    
    # AIC calculation
    AIC <- 2 * k - 2 * log_likelihood
    AIC
    
    #Prophet model with pandemic inclusion
    
    pandemic <- data.frame(
      ds = seq(as.Date("2020-11-01"), as.Date("2021-05-01"), by = "month"),
      holiday = "pandemic"
    )
    
    prophet_model_pandemic <- prophet(
      prophet_data_hotel,
      seasonality.mode = 'multiplicative',
      holidays = pandemic
    )
    
    # Make predictions for the next 12 months
    future <- make_future_dataframe(prophet_model_pandemic, periods = 12, freq = "month")
    forecast <- predict(prophet_model_pandemic, future)
    
    # Plot the forecast
    plot(prophet_model_pandemic, forecast) +
      ggtitle("Forecast of Total Arrivals (12 months) - Prophet with pandemic (2020/11/01 to 2021/05/01)") +
      ylab("Arrivals") +
      xlab("Time")
    
    # plot trend & seasonality components
    prophet_plot_components(prophet_model_pandemic, forecast)
    
    actual    <- prophet_data_hotel$y
    predicted <- forecast$yhat[1:length(actual)]
    residuals <- actual - predicted
    
    # Plot residuals over time
    ggplot(data.frame(Date = prophet_data_hotel$ds, Residuals = residuals), aes(x = Date, y = Residuals)) +
      geom_line() +
      ggtitle("Residuals of Prophet Model") +
      xlab("Date") +
      ylab("Residuals")
    
    # ACF of residuals
    acf_residuals <- acf(residuals, main = "ACF of Residuals", lag.max = 36)
    
    # Compute RMSE, MAE
    mae(hotel_arrivals_data$Hotel_arrivals, predicted)
    rmse(hotel_arrivals_data$Hotel_arrivals, predicted)
    
    # AIC approximated by hand
    # Get residuals and log-likelihood approximation
    residuals <- actual - predicted
    n <- length(residuals)
    sigma2 <- mean(residuals^2)
    
    # Log-likelihood approximation
    log_likelihood <- -n / 2 * log(2 * pi * sigma2) - sum(residuals^2) / (2 * sigma2)
    
    # Number of parameters (approximated)
    k <- length(prophet_model_pandemic$params)  # You might refine this count
    
    # AIC calculation
    AIC <- 2 * k - 2 * log_likelihood
    AIC
    
    #________________________________GAM__________________________________
    
    # Fit the GAM (Option A: Gaussian)
    gam_model <- gam(
      Hotel_arrivals ~ s(time_index, k = 24) + s(month_num, bs = "cc", k = 12),
      data   = hotel_arrivals_data,
      method = "REML"
    )
    
    # Summaries, fitted values, and residuals
    summary(gam_model)
    fitted_values <- gam_model$fitted.values
    
    # Calculate residuals
    residuals_gam <- hotel_arrivals_data$Hotel_arrivals - fitted_values
    
    #check residuals behaviour
    checkresiduals(gam_model)
    
    
    # Compute RMSE, MAE
    mae(hotel_arrivals_data$Hotel_arrivals, fitted_values)
    rmse(hotel_arrivals_data$Hotel_arrivals, fitted_values)
    
    #compute AIC
    AIC(gam_model)
    
    # Make predictions
    last_time_index <- max(hotel_arrivals_data$time_index)
    future_horizon  <- 12
    new_time_index  <- (last_time_index + 1):(last_time_index + future_horizon)
    future_data     <- data.frame(time_index = new_time_index)
    
    last_date <- max(hotel_arrivals_data$Time)
    future_data$Date <- seq.Date(from = last_date + 1, by = "month", length.out = future_horizon)
    future_data$month     <- factor(format(future_data$Date, "%m"))
    future_data$month_num <- as.numeric(format(future_data$Date, "%m"))
    
    # Predict future arrivals
    predictions_gam <- predict(gam_model, newdata = future_data, type = "response")
    future_data$predicted_arrivals <- predictions_gam
    
    print(future_data)
    
    # Combine historical + future data for plotting
    history_df <- data.frame(
      Date       = hotel_arrivals_data$Time,
      Observed   = hotel_arrivals_data$Hotel_arrivals,
      Fitted     = fitted_values,
      time_index = hotel_arrivals_data$time_index
    )
    
    forecast_df <- future_data %>%
      mutate(Observed = NA,
             Fitted   = NA)
    
    plot_data <- bind_rows(
      history_df  %>% mutate(Source = "History"),
      forecast_df %>% mutate(Source = "Forecast")
    )
    
    ggplot(plot_data, aes(x = Date)) +
      geom_line(data = plot_data %>% filter(Source == "History"), 
                aes(y = Observed), color = "black") +
      geom_line(data = plot_data %>% filter(Source == "History"), 
                aes(y = Fitted), color = "blue") +
      geom_line(data = plot_data %>% filter(Source == "Forecast"), 
                aes(y = predicted_arrivals), color = "red") +
      labs(title = "Forecast of Hotel Arrivals (12 months) - GAM",
           x = "Date", y = "Total Arrivals") +
      theme_minimal()


################################################
#        TRENTINO without VAL DI FASSA Data
################################################
    
    #TIME SERIES for Italian and Foreigners in HOTELS
    difference_italian_hotel_ts <- ts(difference_data$`DIFFERENCE Italian HOTEL arrivals`, start = c(2018, 12), frequency = 12)
    difference_foreigners_hotel_ts <- ts(difference_data$`DIFFERENCE Foreigner HOTEL arrivals`, start = c(2018, 12), frequency = 12)
    difference_total_hotel_ts <- ts(difference_data$`DIFFERENCE TOTAL HOTEL arrivals`, start = c(2018, 12), frequency = 12)
    
    par(mar = c(5, 4, 4, 8), xpd = TRUE)
    ts.plot(difference_italian_hotel_ts, difference_foreigners_hotel_ts, difference_total_hotel_ts, 
            col = c("blue", "red", "green"), lty = 1:3,
            main = "Hotel Arrivals in Autonomous Province of Trento", xlab = "Time", ylab = "Arrivals")
    legend("topright", inset = c(-0.25, 0), legend = c("Italian Hotel Arrivals", "Foreigners Hotel Arrivals", "Total Hotel Arrivals"), 
           col = c("blue", "red", "green"), lty = 1:3, box.lty = 0, cex = 0.8,
           title = "Legend")
    par(mar = c(5, 4, 4, 2))
    
    #Checking for residuals
    checkresiduals(difference_total_hotel_ts)
    
    acf(difference_total_hotel_ts) 
    pacf(difference_total_hotel_ts)
    
    plot(difference_total_hotel_ts, main = "Total Hotel Arrivals", xlab = "Year", ylab = "Arrivals", col = "blue", type = "o")
    
    #LINEAR Model for the Difference total hotel arrivals
    
    #Linear model accounting for trend + seasonality
    linear_model <- tslm(difference_total_hotel_ts ~ trend + season, data = difference_data)
    summary(linear_model)
    
    fitted_values <- fitted(linear_model)
    
    plot(difference_total_hotel_ts, main = "Linear Model: Total Hotel Arrivals", xlab = "Year", ylab = "Arrivals", col = "blue")
    lines(fitted(linear_model), col = "red")
    
    res_linear <- residuals(linear_model)
    acf(res_linear, main = "ACF Residuals: Linear Model")
    AIC(linear_model)
    
    rmse_linear <- sqrt(mean(res_linear^2, na.rm = TRUE))
    mae_linear  <- mean(abs(res_linear), na.rm = TRUE)
    
    cat("Evaluation Metrics (Linear Model):\n")
    cat(" - RMSE:", rmse_linear, "\n")
    cat(" - MAE: ", mae_linear,  "\n")
    
    mae(difference_data$`DIFFERENCE TOTAL HOTEL arrivals`, fitted(linear_model))
    
    # Forecasts with Linear Model
    forecasts_linear_model <- forecast(linear_model)
    plot(forecasts_linear_model)
    
    #EXPONENTIAL SMOOTHING Model
    ets_model <- ets(difference_total_hotel_ts)
    summary(ets_model)
    AIC(ets_model)
    
    plot(difference_total_hotel_ts, main = "Exponential Smoothing Model: Total Hotel Arrivals", xlab = "Year", ylab = "Arrivals", col = "blue")
    
    checkresiduals(ets_model)
    res_ets <- residuals(ets_model)
    
    rmse_ets <- sqrt(mean(residuals_ets^2, na.rm = TRUE))
    mae_ets  <- mean(abs(residuals_ets), na.rm = TRUE)
    
    cat("Evaluation Metrics (ETS):\n")
    cat(" - RMSE:", rmse_ets, "\n")
    cat(" - MAE: ", mae_ets,  "\n")
    
    ets_forecast <- forecast(ets_model, h = 12)
    plot(ets_forecast, main = "Exponential Smoothing Forecast: Total Hotel Arrivals")
    
    #ARIMA Model
    auto_arima_model <- auto.arima(difference_total_hotel_ts)
    summary(auto_arima_model)
    
    checkresiduals(auto_arima_model)
    
    arima_forecast <- forecast(auto_arima_model, h = 12)
    plot(arima_forecast, main = "ARIMA Forecast: Total Hotel Arrivals")
    
    checkresiduals(auto_arima_model)
    res_arima <- residuals(auto_arima_model)
    
    rmse_arima <- sqrt(mean(res_arima^2, na.rm = TRUE))
    mae_arima  <- mean(abs(res_arima), na.rm = TRUE)
    
    cat("Evaluation Metrics (ARIMA):\n")
    cat(" - RMSE:", rmse_arima, "\n")
    cat(" - MAE: ", rmse_arima,  "\n")
    
    AIC(auto_arima_model)
    
    #PROPHET Model
    prophet_model <- prophet(prophet_data_difference)
    summary(prophet_model)
    
    future <- make_future_dataframe(prophet_model, periods = 12, freq = "month")
    forecast_prophet <- predict(prophet_model, future)
    
    plot(prophet_model, forecast_prophet) +
      ggtitle("Prophet Forecast: Total Hotel Arrivals")
    
    prophet_plot_components(prophet_model, forecast_prophet)
    
    actual <- prophet_data_difference$y
    predicted <- forecast_prophet$yhat[1:length(actual)]
    prophet_res <- actual - predicted
    
    prophet_rmse <- sqrt(mean(prophet_res^2, na.rm = TRUE))  
    prophet_mae  <- mean(abs(prophet_res), na.rm = TRUE)
    
    cat("Evaluation Metrics:\n")
    cat("Root Mean Square Error (RMSE):", prophet_rmse, "\n")
    cat("Mean Absolute Error (MAE):", prophet_mae, "\n")
    
    #######################
    #Prophet model with pandemic inclusion
    
    pandemic <- data.frame(
      ds = seq(as.Date("2020-11-01"), as.Date("2021-05-01"), by = "month"),
      holiday = "pandemic"
    )
    
    prophet_model_pandemic <- prophet(
      prophet_data_difference,
      seasonality.mode = 'multiplicative',
      holidays = pandemic
    )
    
    # Make predictions for the next 12 months
    future <- make_future_dataframe(prophet_model_pandemic, periods = 12, freq = "month")
    forecast <- predict(prophet_model_pandemic, future)
    
    # Plot the forecast
    plot(prophet_model_pandemic, forecast) +
      ggtitle("Forecast of Total Arrivals (12 months) - Prophet with pandemic (2020/11/01 to 2021/05/01)") +
      ylab("Arrivals") +
      xlab("Time")
    
    # plot trend & seasonality components
    prophet_plot_components(prophet_model_pandemic, forecast)
    
    actual    <- prophet_data_difference$y
    predicted <- forecast$yhat[1:length(actual)]
    residuals <- actual - predicted
    
    # Plot residuals over time
    ggplot(data.frame(Date = prophet_data_difference$ds, Residuals = residuals), aes(x = Date, y = Residuals)) +
      geom_line() +
      ggtitle("Residuals of Prophet Model") +
      xlab("Date") +
      ylab("Residuals")
    
    # ACF of residuals
    acf_residuals <- acf(residuals, main = "ACF of Residuals", lag.max = 36)
    
    # Compute RMSE, MAE
    mae(difference_data$`DIFFERENCE TOTAL HOTEL arrivals`, predicted)
    rmse(difference_data$`DIFFERENCE TOTAL HOTEL arrivals`, predicted)
    
    # AIC approximated by hand
    # Get residuals and log-likelihood approximation
    residuals <- actual - predicted
    n <- length(residuals)
    sigma2 <- mean(residuals^2)
    
    # Log-likelihood approximation
    log_likelihood <- -n / 2 * log(2 * pi * sigma2) - sum(residuals^2) / (2 * sigma2)
    
    # Number of parameters (approximated)
    k <- length(prophet_model_pandemic$params)  # You might refine this count
    
    # AIC calculation
    AIC <- 2 * k - 2 * log_likelihood
    AIC
    #################################à
    
    #GAM (Generalized Additive Model) 
    time_index <- as.numeric(time(difference_total_hotel_ts))
    difference_data$time_index <- 1:nrow(difference_data)
    difference_data$month_num <- as.numeric(format(difference_data$Time, "%m"))
    
    #GAUSSIAN Distribution
    gam_model <- gam(difference_total_hotel_ts ~ s(time_index, k=24) + s(month_num, bs = "cc", k = 12), 
                     data = difference_data, method = "REML")
    summary(gam_model)
    
    gam_res <- residuals(gam_model)
    
    rmse_gam <- sqrt(mean(gam_res^2, na.rm = TRUE))
    mae_gam <- mean(abs(gam_res), na.rm = TRUE)
    
    cat("Evaluation Metrics (GAM):\n")
    cat(" - RMSE:", rmse_gam, "\n")
    cat(" - MAE: ", mae_gam,  "\n")
    
    #Forecasts with GAM model
    last_time_index <- max(difference_data$time_index)
    future_horizon <- 12
    new_time_index <- (last_time_index + 1):(last_time_index + future_horizon)
    
    last_date <- max(difference_data$Time)
    future_dates <- seq.Date(from = last_date + months(1), by = "month", length.out = future_horizon)
    
    future_data <- data.frame(
      time_index = new_time_index,
      Time = future_dates,
      month_num = as.numeric(format(future_dates, "%m"))
    )
    
    future_data$predicted_arrivals <- predict(gam_model, newdata = future_data, type = "response")
    
    # Combination of Historical and Future data
    history_df <- data.frame(
      Time = difference_data$Time,
      Observed = as.numeric(difference_total_hotel_ts),
      Fitted = fitted(gam_model)
    )
    
    future_df <- data.frame(
      Time = future_dates,
      Observed = NA,
      Fitted = NA,
      Predicted = future_data$predicted_arrivals
    )
    
    plot_data <- bind_rows(
      history_df %>% mutate(Source = "Historical"),
      future_df %>% mutate(Source = "Forecast")
    )
    
    ggplot(plot_data, aes(x = Time)) +
      geom_line(data = plot_data %>% filter(Source == "Historical"), aes(y = Observed), color = "black", size = 1) +
      geom_line(data = plot_data %>% filter(Source == "Historical"), aes(y = Fitted), color = "blue", linetype = "dashed", size = 0.8) +
      geom_line(data = plot_data %>% filter(Source == "Forecast"), aes(y = Predicted), color = "red", size = 1) +
      labs(title = "GAM Model: Historical and Forecasted Data",
           x = "Date", y = "Hotel Arrivals") +
      theme_minimal()
    
    plot(difference_total_hotel_ts, main = "GAM: Total Hotel Arrivals", xlab = "Year", ylab = "Arrivals", col = "blue")
    lines(fitted(gam_model), col = "red")
    
    AIC(gam_model)

    
################################################
#   FOREIGNERS ARRIVALS PROPORTION ANALYSIS
################################################
    
    #_______________________Linear model_____________________________
    
    # fit a tslm model
    linear_model_foreigners <- tslm(foreigners_proportion_ts ~ trend + season, data = foreigners_proportion_ts)
    
    # Summary of the model
    summary(linear_model_foreigners)
    #Multiple R2 = 0.07971
    
    plot(foreigners_proportion_ts, main = "Linear model for foreigners proportion time series", xlab = "Year", ylab = "Arrivals proportion", col = "blue")
    lines(fitted(linear_model_foreigners), col = "red")
    
    residuals <- residuals(linear_model_foreigners)
    checkresiduals(linear_model_foreigners)
    
    abline(h = 0, col = "red", lty = 2) 
    qqnorm(residuals, main = "Normal Q-Q Plot")
    qqline(residuals, col = "red")
    
    acf(residuals, main = "ACF of Residuals")
    dwtest(linear_model_foreigners)
    AIC(linear_model_foreigners)
    
    
    mae(data_foreigners$`Normalized foreigners arrivals`, fitted(linear_model_foreigners))
    rmse(data_foreigners$`Normalized foreigners arrivals`, fitted(linear_model_foreigners))
    
    #___________________Exponential smoothing________________________
    
    # alternative applying ets for best additive model (based on AIC)
    fit3_hw<- ets(foreigners_proportion_ts)
    # 0.07038575 0.0471
    autoplot(forecast(fit3_hw))
    summary(fit3_hw)
    AIC(fit3_hw)
    
    plot(foreigners_proportion_ts, main = "Exponential Smoothing Model: Foreigners arrivals proportion", xlab = "Year", ylab = "Arrivals proportion", col = "blue")
    
    checkresiduals(fit3_hw)
    res_ets <- residuals(fit3_hw)
    
    rmse_ets <- sqrt(mean(res_ets^2, na.rm = TRUE))
    mae_ets  <- mean(abs(res_ets), na.rm = TRUE)
    
    cat("Evaluation Metrics (ETS):\n")
    cat(" - RMSE:", rmse_ets, "\n")
    cat(" - MAE: ", mae_ets,  "\n")
    
    ets_forecast <- forecast(fit3_hw, h = 12)
    plot(ets_forecast, main = "Exponential Smoothing Forecast: Total Hotel Arrivals")
    #detected alpha=0.2113
    #AIC = -65.51183
    
    #__________________________ARIMA_________________________________
    
    #AUTO-SARIMA
    fit1_SARIMA <- auto.arima(foreigners_proportion_ts, seasonal = TRUE)
    summary(fit1_SARIMA) 
    #estimated ARIMA(3,1,0)(1,1,0)[12]
    #AIC -74.08281
    autoplot(forecast(fit1_SARIMA, h = 6))
    checkresiduals(fit1_SARIMA) #significant lag 10
    
    #_________________________Prophet________________________________
    
    foreigners_proportion_data <- data_foreigners[, c(1, 4)]
    foreigners_proportion_data$Time <- dmy(paste0("01 ", foreigners_proportion_data$Time))
    
    foreigners_proportion_data_prophet <- foreigners_proportion_data[, c("Normalized foreigners arrivals", "Time")]
    foreigners_proportion_data_prophet$cap <- max(foreigners_proportion_data_prophet$`Normalized foreigners arrivals`)
    colnames(foreigners_proportion_data_prophet) <- c("y","ds","cap")
    #View(foreigners_proportion_data_prophet)
    
    prophet_foreigners_model <- prophet(foreigners_proportion_data_prophet, growth = "logistic")
    summary(prophet_foreigners_model)       
    
    ##create a future 'window' for prediction (out of sample period)
    future2 <- make_future_dataframe(prophet_foreigners_model, periods = 6, freq="month", include_history = T)
    tail(future2)
    future2$cap <- 0.567 #we manually insert the value cap for the added obs.
    
    #prophet forecasts
    forecast2 <- predict(prophet_foreigners_model, future2)
    tail(forecast2[c("ds", "yhat", "yhat_lower", "yhat_upper")])
    
    #prediction plot
    plot(prophet_foreigners_model, forecast2)
    
    #Dynamic plot
    dyplot.prophet(prophet_foreigners_model, forecast2) 
    
    #plot with change points
    plot(prophet_foreigners_model, forecast2)+add_changepoints_to_plot(prophet_foreigners_model, threshold=0)+
      ggtitle("Foreigners arrivals proportion - Prophet forecasts") + 
      xlab("Time") +
      ylab("Foreigners proportion") + 
      theme_minimal()
    
    
    #metrics
    forecast_prophet <- predict(prophet_foreigners_model, foreigners_proportion_data_prophet)
    y_predicted <- forecast_prophet$yhat
    mae(foreigners_proportion_data$`Normalized foreigners arrivals`, y_predicted)
    rmse(foreigners_proportion_data$`Normalized foreigners arrivals`, y_predicted)
    
    # AIC approximated
    
    actual <- foreigners_proportion_data_prophet$y
    predicted <- forecast_prophet$yhat[1:length(actual)]
    prophet_res <- actual - predicted
    
    # Get residuals and log-likelihood approximation
    n <- length(prophet_res)
    sigma2 <- mean(prophet_res^2)
    
    # Log-likelihood approximation
    log_likelihood <- -n / 2 * log(2 * pi * sigma2) - sum(prophet_res^2) / (2 * sigma2)
    
    # Number of parameters (approximated)
    k <- length(prophet_foreigners_model$params)
    
    # AIC calculation
    AIC <- 2 * k - 2 * log_likelihood
    AIC
    
    
    #prophet model with pandemic inclusion 
    
    pandemic <- data.frame(
      ds = seq(as.Date("2020-11-01"), as.Date("2021-05-01"), by = "month"),
      holiday = "pandemic"
    )
    
    prophet_model_pandemic_foreigners <- prophet(
      foreigners_proportion_data_prophet,
      seasonality.mode = 'multiplicative',
      holidays = pandemic
    )
    
    # Make predictions for the next 12 months
    future <- make_future_dataframe(prophet_model_pandemic_foreigners, periods = 12, freq = "month")
    forecast <- predict(prophet_model_pandemic_foreigners, future)
    
    # Plot the forecast
    plot(prophet_model_pandemic_foreigners, forecast) +
      ggtitle("Forecast of Difference Arrivals (12 months) - Prophet with pandemic (2020/11/01 to 2021/05/01)") +
      ylab("Arrivals") +
      xlab("Time")
    
    # plot trend & seasonality components
    prophet_plot_components(prophet_model_pandemic_foreigners, forecast)
    
    actual    <- foreigners_proportion_data_prophet$y
    predicted <- forecast$yhat[1:length(actual)]
    residuals <- actual - predicted
    
    # Plot residuals over time
    ggplot(data.frame(Date = foreigners_proportion_data_prophet$ds, Residuals = residuals), aes(x = Date, y = Residuals)) +
      geom_line() +
      ggtitle("Residuals of Prophet Model") +
      xlab("Date") +
      ylab("Residuals")
    
    # ACF of residuals
    acf_residuals <- acf(residuals, main = "ACF of Residuals", lag.max = 36)
    
    # Compute RMSE, MAE
    mae(foreigners_proportion_data$`Normalized foreigners arrivals`, forecast$yhat[1:length(actual)])
    rmse(foreigners_proportion_data$`Normalized foreigners arrivals`, forecast$yhat[1:length(actual)])
    
    # AIC approximated
    # Get residuals and log-likelihood approximation
    n <- length(residuals)
    sigma2 <- mean(residuals^2)
    
    # Log-likelihood approximation
    log_likelihood <- -n / 2 * log(2 * pi * sigma2) - sum(residuals^2) / (2 * sigma2)
    
    # Number of parameters (approximated)
    k <- length(prophet_model_pandemic_foreigners$params)
    
    # AIC calculation
    AIC <- 2 * k - 2 * log_likelihood
    AIC
    
    #___________________________GAM__________________________________
    
    # #Create time indices and month columns
    foreigners_proportion_data$time_index <- seq_len(nrow(foreigners_proportion_data))
    foreigners_proportion_data$month      <- factor(format(foreigners_proportion_data$Time, "%m"))
    foreigners_proportion_data$month_num  <- as.numeric(format(foreigners_proportion_data$Time, "%m"))
    names(foreigners_proportion_data)[names(foreigners_proportion_data) == "Normalized foreigners arrivals"] <- "Normalized_foreigners_arrivals"
    
    # Fit the GAM (Option A: Gaussian)
    gam_model_foreigners <- gam(
      Normalized_foreigners_arrivals ~ s(time_index, k = 24) + s(month_num, bs = "cc", k = 12),
      data   = foreigners_proportion_data,
      method = "REML"
    )
    
    # Summaries, fitted values, and residuals
    summary(gam_model_foreigners)
    fitted_values <- gam_model_foreigners$fitted.values
    
    # Calculate residuals
    residuals_gam <- foreigners_proportion_data$Normalized_foreigners_arrivals - fitted_values
    
    #check residuals behaviour
    checkresiduals(gam_model_foreigners)
    
    
    # Compute RMSE, MAE
    mae(foreigners_proportion_data$Normalized_foreigners_arrivals, fitted_values)
    rmse(foreigners_proportion_data$Normalized_foreigners_arrivals, fitted_values)
    
    #compute AIC
    AIC(gam_model_foreigners)
    
    # Make predictions
    last_time_index <- max(foreigners_proportion_data$time_index)
    future_horizon  <- 12
    new_time_index  <- (last_time_index + 1):(last_time_index + future_horizon)
    future_data     <- data.frame(time_index = new_time_index)
    
    last_date <- max(foreigners_proportion_data$Time)
    future_data$Date <- seq.Date(from = last_date + 1, by = "month", length.out = future_horizon)
    future_data$month     <- factor(format(future_data$Date, "%m"))
    future_data$month_num <- as.numeric(format(future_data$Date, "%m"))
    
    # Predict future arrivals
    predictions_gam <- predict(gam_model_foreigners, newdata = future_data, type = "response")
    future_data$predicted_arrivals <- predictions_gam
    
    print(future_data)
    
    # Combine historical + future data for plotting
    history_df <- data.frame(
      Date       = foreigners_proportion_data$Time,
      Observed   = foreigners_proportion_data$Normalized_foreigners_arrivals,
      Fitted     = fitted_values,
      time_index = foreigners_proportion_data$time_index
    )
    
    forecast_df <- future_data %>%
      mutate(Observed = NA,
             Fitted   = NA)
    
    plot_data <- bind_rows(
      history_df  %>% mutate(Source = "History"),
      forecast_df %>% mutate(Source = "Forecast")
    )
    
    ggplot(plot_data, aes(x = Date)) +
      geom_line(data = plot_data %>% filter(Source == "History"), 
                aes(y = Observed), color = "black") +
      geom_line(data = plot_data %>% filter(Source == "History"), 
                aes(y = Fitted), color = "blue") +
      geom_line(data = plot_data %>% filter(Source == "Forecast"), 
                aes(y = predicted_arrivals), color = "red") +
      labs(title = "Forecast of Proportion of foreigners (12 months) - GAM",
           x = "Date", y = "Total Arrivals") +
      theme_minimal()
    
    