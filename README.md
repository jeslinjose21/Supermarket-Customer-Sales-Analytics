# Supermarket-Customer-Sales-Analytics
This project analyses customer behaviour and sales performance using cleaned and aggregated transactional data. Techniques such as RFM analysis, k-means clustering, forecasting, and regression models are applied to identify trends and customer segments. Implemented using Python, R, Excel Solver, and data visualisation tools.

```{r}
library(readr)   # for Excel files
library(dplyr)
library(lubridate)

# ---- Function to read all Excel files in a folder ----
read_folder <- function(path) {
  files <- list.files(path = path,  pattern = "\\.csv$", full.names = TRUE)   # get all files
  df <- lapply(files, function(f) {read_csv(f, show_col_types = FALSE,col_types = cols(.default = "c"))   # all as character
  }) %>%
    bind_rows()
  return(df)
}

# ---- Read each year ---
df2013 <- read_folder("C:/customer analysis/2013/2013")
df2014 <- read_folder("C:/customer analysis/2014/2014")
df2015 <- read_folder("C:/customer analysis/2015/2015")

# ---- Combine into one dataset ----
raw_df <- bind_rows(df2013, df2014, df2015)
```

## Data Cleaning 
```{r}
raw_df$Sale_Date     <- as.Date(raw_df$Sale_Date, format = "%Y-%m-%d")
raw_df$Item_Value    <- as.numeric(raw_df$Item_Value)
raw_df$Quantity_Sold <- as.numeric(raw_df$Quantity_Sold)

# --- Step 1. Remove supplier payments (extreme negatives, keep normal returns)
raw_df <- raw_df[raw_df$Item_Value > -1000, ]

# --- Step 2. Identify generic loyalty cards (super customers)
cust_spend <- aggregate(Item_Value ~ UniSA_Customer_No, data = raw_df, sum, na.rm = TRUE)

# Define “super” customers as top 0.1% spenders
cutoff <- quantile(cust_spend$Item_Value, 0.999, na.rm = TRUE)
generic_ids <- cust_spend$UniSA_Customer_No[cust_spend$Item_Value > cutoff]

# --- Step 3. Remove them
clean_df <- raw_df[!(raw_df$UniSA_Customer_No %in% generic_ids), ]

# --- Step 4. Extra basic cleaning -------------------------

# Drop rows with missing critical IDs
clean_df <- clean_df[!is.na(clean_df$UniSA_Customer_No) & !is.na(clean_df$Sale_Date), ]

```


```{r}
#clean_df <- unique(clean_df)

# Ensure numeric columns are numeric
clean_df$Item_Value <- as.numeric(clean_df$Item_Value)
clean_df$Quantity_Sold <- as.numeric(clean_df$Quantity_Sold)

# Make sure Sale_Date is proper Date
clean_df$Sale_Date <- as.Date(clean_df$Sale_Date)

# --- Step 5. Quick checks --------------------------------
str(clean_df)           # confirm column types
#anyDuplicated(clean_df) # should return 0 if no duplicates
dim(clean_df)           # final rows x columns
head(clean_df)          # preview
```

```{r}
# Trading days
n_days <- n_distinct(clean_df$Sale_Date)

# Unique customers
n_customers <- n_distinct(clean_df$UniSA_Customer_No)

# Shopping trips (receipts)
n_trips <- n_distinct(clean_df$UniSA_Receipt_No1)

# Items (line-items)
n_items <- nrow(clean_df)

# Dollar volume (sum of line values; returns included)
total_sales <- sum(clean_df$Item_Value, na.rm = TRUE)

intro_tbl <- tibble::tibble(
  metric = c("Trading days (2013–2015)",
             "Unique customers",
             "Shopping trips (receipts)",
             "Line-items (rows)",
             "Total sales (sum of Item_Value)"),
  value  = c(n_days, n_customers, n_trips, n_items, total_sales)
)
intro_tbl

```


```{r}
library(dplyr)

yearly_summary <- clean_df %>%
  mutate(Year = format(Sale_Date, "%Y")) %>%
  group_by(Year) %>%
  summarise(
    Trading_Days   = n_distinct(Sale_Date),
    Customers      = n_distinct(UniSA_Customer_No),
    Trips          = n_distinct(UniSA_Receipt_No1),
    Line_Items     = n(),
    Total_Sales    = sum(Item_Value, na.rm = TRUE)
  )

print(yearly_summary)

```

## Step 1. Filter for “regular” customers
### customers who appear in 2013, 2014, and 2015
```{r}
# extract customer sets per year
cust2013 <- unique(clean_df %>% filter(year(Sale_Date) == 2013) %>% pull(UniSA_Customer_No))
cust2015 <- unique(clean_df %>% filter(year(Sale_Date) == 2015) %>% pull(UniSA_Customer_No))

# define "regular" = shopped both 2013 and 2015
regular_customers <- intersect(cust2013, cust2015)

# keep their 2014 data
df2014_regular <- clean_df %>%
  filter(year(Sale_Date) == 2014,
         UniSA_Customer_No %in% regular_customers)

```


## Step 1a. RFM Segmentation (no clustering)
```{r}

# reference date: last day of 2014
ref_date <- as.Date("2014-12-31")

rfm <- df2014_regular %>%
  group_by(UniSA_Customer_No) %>%
  summarise(
    Recency   = as.numeric(ref_date - max(Sale_Date)),  # days since last visit
    Frequency = n_distinct(UniSA_Receipt_No1),          # number of shopping trips
    Monetary  = sum(Item_Value, na.rm = TRUE)           # total spend
  )

# score each metric into quintiles (1 = low, 5 = high)


rfm <- rfm %>%
  mutate(
    R_score = ntile(-Recency, 5),   # invert so recent = high score
    F_score = ntile(Frequency, 5),
    M_score = ntile(Monetary, 5),
    RFM_Segment = paste0(R_score, F_score, M_score),
    RFM_Score   = R_score + F_score + M_score
  )

# Example segmentation mapping
rfm <- rfm %>%
  mutate(Segment = case_when(
    RFM_Score >= 12 ~ "Champions",
    RFM_Score >= 9  ~ "Loyal Customers",
    RFM_Score >= 6  ~ "Potential Loyalist",
    TRUE            ~ "At Risk"
  ))

print(rfm, width = Inf)

```


```{r}
table(rfm$Segment)
```


```{r}
rfm %>%
  group_by(Segment) %>%
  summarise(
    n_customers = n(),
    total_sales = sum(Monetary),
    avg_sales   = mean(Monetary)
  )
```


```{r}
library(ggplot2)

ggplot(rfm, aes(x = Segment, fill = Segment)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "RFM Segmentation: Customer Counts")

```

## Step 1b. Clustering on richer features
```{r}
# build behavioural features per customer
features <- df2014_regular %>%
  group_by(UniSA_Customer_No, UniSA_Receipt_No1) %>%
  summarise(
    trip_spend = sum(Item_Value, na.rm = TRUE),
    basket_size = sum(Quantity_Sold, na.rm = TRUE)
  ) %>%
  group_by(UniSA_Customer_No) %>%
  summarise(
    avg_basket_size = mean(basket_size),
    avg_spend_trip  = mean(trip_spend),
    total_spend     = sum(trip_spend),
    trips           = n()
  )

# add department proportions
dept_props <- df2014_regular %>%
  group_by(UniSA_Customer_No, Department_Name) %>%
  summarise(spend = sum(Item_Value, na.rm = TRUE)) %>%
  group_by(UniSA_Customer_No) %>%
  mutate(prop = spend / sum(spend)) %>%
  select(UniSA_Customer_No, Department_Name, prop) %>%
  tidyr::pivot_wider(names_from = Department_Name, values_from = prop, values_fill = 0)

features <- left_join(features, dept_props, by = "UniSA_Customer_No")

# scale features for clustering
mat <- features %>%
  select(-UniSA_Customer_No) %>%
  scale()

set.seed(123)
km <- kmeans(mat, centers = 4, nstart = 25)
features$cluster <- factor(km$cluster)

# 5) Profile clusters (means of features)
cluster_profile <- features |>
  group_by(cluster) |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
cluster_profile


```


```{r}
pca <- prcomp(mat, scale. = TRUE)
plot(pca$x[,1], pca$x[,2], col = km$cluster, pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Customer clusters (PCA projection)")

legend("topright", 
       legend = paste("Cluster", 1:4), 
       col = 1:4, pch = 19)


```

## Comparison
```{r}
comp <- rfm |>
  select(UniSA_Customer_No, Segment) |>
  left_join(features |> select(UniSA_Customer_No, cluster), by = "UniSA_Customer_No")

counts <- table(comp$Segment, comp$cluster)
props  <- prop.table(counts, margin = 1)

counts
round(props, 3)  # row-wise proportions


```


## 2.MONTHLY SALES ANALYSIS AND FORECASTING
```{r}
# --- Step 1. Ensure Sale_Date is Date -------------------
clean_df$Sale_Date <- as.Date(clean_df$Sale_Date)

# --- Step 2. Add Year, Month, YearMonth -----------------
clean_df$Year      <- format(clean_df$Sale_Date, "%Y")
clean_df$Month     <- format(clean_df$Sale_Date, "%m")
clean_df$YearMonth <- format(clean_df$Sale_Date, "%Y-%m")

# --- Step 3. Aggregate to Monthly Sales -----------------
monthly_sales <- aggregate(Item_Value ~ YearMonth + Year + Month,
                           data = clean_df,
                           sum, na.rm = TRUE)

monthly_sales$Year  <- as.numeric(monthly_sales$Year)
monthly_sales$Month <- as.numeric(monthly_sales$Month)

# --- Step 4. Calendar predictors ------------------------
library(lubridate)

# Days in month
monthly_sales$DaysInMonth <- days_in_month(as.Date(paste0(monthly_sales$YearMonth, "-01")))

# Weekend days per month
get_weekend_days <- function(year, month) {
  d <- seq(as.Date(paste0(year, "-", month, "-01")),
           as.Date(paste0(year, "-", month, "-", days_in_month(as.Date(paste0(year, "-", month, "-01"))))),
           by = "day")
  sum(weekdays(d) %in% c("Saturday", "Sunday"))
}
monthly_sales$WeekendDays <- mapply(get_weekend_days,
                                    monthly_sales$Year,
                                    monthly_sales$Month)

# --- Step 5. Regression model ---------------------------
model <- lm(Item_Value ~ Year + Month + DaysInMonth + WeekendDays,
            data = monthly_sales)
summary(model)

# --- Step 6. Future data (Jan–Mar 2016) -----------------
future <- data.frame(
  Year  = rep(2016, 3),
  Month = 1:3
)
future$YearMonth   <- paste0(future$Year, "-", sprintf("%02d", future$Month))
future$DaysInMonth <- days_in_month(as.Date(paste0(future$YearMonth, "-01")))
future$WeekendDays <- mapply(get_weekend_days, future$Year, future$Month)

# --- Step 7. Predictions with intervals -----------------
pred <- predict(model, newdata = future, interval = "prediction", level = 0.95)
forecast <- cbind(future, round(pred, 2))
print(forecast)

```

```{r}
library(ggplot2)

# Make sure YearMonth is Date for plotting
monthly_sales$YearMonth <- as.Date(paste0(monthly_sales$YearMonth, "-01"))
forecast$YearMonth      <- as.Date(paste0(forecast$YearMonth, "-01"))

ggplot() +
  geom_line(data = monthly_sales, aes(x = YearMonth, y = Item_Value),
            colour = "blue", size = 1) +
  geom_line(data = forecast, aes(x = YearMonth, y = fit),
            colour = "red", size = 1) +
  geom_ribbon(data = forecast,
              aes(x = YearMonth, ymin = lwr, ymax = upr),
              fill = "red", alpha = 0.2) +
  labs(title = "Monthly Sales with Forecast (2016 Q1)",
       x = "Month", y = "Sales ($)") +
  theme_minimal()


```


## 3. IMPACT OF PROMOTIONS ON ITEM SALES
```{r}
# Use positives only for units & price (exclude returns)
pos_lines <- clean_df |>
  filter(!is.na(Quantity_Sold), Quantity_Sold > 0,
         !is.na(Item_Value), Item_Value > 0)

# Build item-day aggregates
item_day <- pos_lines |>
  mutate(
    unit_price = Item_Value / Quantity_Sold,
    Offer_flag = ifelse(toupper(Offer) %in% c("YES","Y","TRUE","T","1"), 1L, 0L),
    dow        = wday(Sale_Date, label = TRUE, abbr = TRUE),
    mon        = month(Sale_Date, label = TRUE, abbr = TRUE)
  ) |>
  group_by(Barcode_Item, Sale_Date, Department_Name) |>
  summarise(
    units      = sum(Quantity_Sold, na.rm = TRUE),
    avg_price  = mean(unit_price, na.rm = TRUE),
    promo_any  = as.integer(any(Offer_flag == 1L)),
    dow        = first(dow),
    mon        = first(mon),
    .groups    = "drop"
  ) |>
  filter(is.finite(units), is.finite(avg_price))

# Quasi-Poisson: units ~ promo + log(price) + department + DOW + month
fit_promo <- glm(
  units ~ promo_any + log(avg_price) + Department_Name + dow + mon,
  data = item_day,
  family = quasipoisson(link = "log")
)
summary(fit_promo)

# Promotion effect (multiplicative on units)
promo_coef <- coef(fit_promo)["promo_any"]
promo_uplift_pct <- (exp(promo_coef) - 1) * 100
promo_uplift_pct

```

```{r}
library(broom)
library(ggplot2)

# Extract tidy coefficients
coef_df <- broom::tidy(fit_promo, conf.int = TRUE)

# Keep only main variables of interest (exclude polynomial terms if needed)
coef_df <- coef_df %>%
  filter(term %in% c("promo_any", "log(avg_price)", 
                     grep("Department_Name", term, value = TRUE)))

# Plot
ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(color = "blue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Effect of Promotion and Other Factors on Unit Sales",
       x = "Estimate (log scale)", y = "Predictor") +
  theme_minimal()

```

