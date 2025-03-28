library(tidyverse)
library(skimr)
library(nycflights13)

flights
?flights

str(flights)
glimpse(flights)

skim(flights)

flights_droprow <- flights %>%
  drop_na(tailnum)
skim(flights_droprow)

flights_dropcol <- flights %>%
  select(-c(dep_time, dep_delay)) %>%
  select(-air_time)
skim(flights_dropcol)

flights_airtime <- flights %>%
  mutate(air_time = replace_na(air_time, median(air_time, na.rm = TRUE)))
skim(flights_airtime)


# Outlier Handle
Q1 = quantile(flights$distance, 0.25, na.rm = T)
Q3 = quantile(flights$distance, 0.75, na.rm = T)
IQR <- Q3 - Q1

lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR

sum(flights$distance < lower_bound | flights$distance > upper_bound)

# Pake library
library(DescTools)
length(Outlier(flights$distance))

# Remove outlier
flights_no_outlier <- flights %>%
  filter(distance >= lower_bound & distance <= upper_bound)

# Transformation log
flights <- flights %>%
  mutate(distance_log = log(distance + 1))

# Transformation (box-cox)
lambda <- 0.1
flights <- flights %>%
  mutate(distance_bc = (distance ^ (lambda) - 1 / lambda))

length(Outlier(flights$distance_log))
length(Outlier(flights$distance_bc))

## Winsorizing (dari DescTools)
flights$distance <- Winsorize(flights$distance)
length(Outlier(flights$distance))


# Normalization
flights %>%
  mutate(distance_norm = scale(distance),
         .keep = "used")

# Incosistent Data
df <- data.frame(
  id = 1:5,
  tanggal = c("2024/03/01", "01-03-2024", "March 1, 2024", "2024.03.01", "01 Mar 2024"),
  jumlah_produk = c("10", "15", "20", "25.0", "30"),
  kategori = c("elektronik", "ELEKTRONIK", "Elektronik", "fashion", "Fashion"),
  harga = c("10000", "20.000", "30K", "40000", "50.000")
)

skim(df)
print(df)

# format date
library(lubridate)
df <- df %>%
  mutate(tanggal = parse_date_time(tanggal, orders = c("ymd", "dmy", "B d, Y", "Y.m.d", "d B Y")))
print(df)

# data types
df$jumlah_produk <- as.numeric(df$jumlah_produk)
print(df)

# Capitalize (proper)
df <- df %>%
  mutate(kategori = str_to_title(kategori))
print(df)

# Harga
df <- df %>%
  mutate(harga = str_replace_all(harga, "\\.", ""),
         harga = str_replace(harga, "K" , "000"),
         harga = as.numeric(harga))
print(df)
