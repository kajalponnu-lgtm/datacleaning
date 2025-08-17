$ oc_0_5           : num  3.549 0.522 0.752 1.075 3.196 ...
$ oc_5_15          : num  0.688 0.821 2.721 1.132 1.281 ...
$ oc_15_30         : num  1.095 0.765 0.557 0.211 0.55 ...
$ ph_mean          : num  5.96 6.54 6.17 6.61 6.6 ...
$ oc_mean          : num  1.777 0.702 1.343 0.806 1.676 ...
> summary(main90)
dem_class              DEM            slope            aspect        profile_curvature   
Length:90          Min.   :523.3   Min.   :0.5943   Min.   :  1.089   Min.   :-3.042e-03  
Class :character   1st Qu.:528.7   1st Qu.:1.6326   1st Qu.: 86.948   1st Qu.:-8.780e-04  
Mode  :character   Median :533.2   Median :2.5269   Median :228.244   Median :-1.495e-04  
Mean   :533.9   Mean   :2.7163   Mean   :198.618   Mean   :-9.004e-05  
3rd Qu.:537.2   3rd Qu.:3.4828   3rd Qu.:297.363   3rd Qu.: 5.652e-04  
Max.   :552.9   Max.   :8.1235   Max.   :356.492   Max.   : 2.767e-03  
plan_curvature            twi           longitude        latitude           n     
Min.   :-2.643e-03   Min.   : 6.498   Min.   :78.39   Min.   :17.31   Min.   :40  
1st Qu.:-6.312e-04   1st Qu.: 7.395   1st Qu.:78.40   1st Qu.:17.32   1st Qu.:40  
Median : 2.500e-06   Median : 8.076   Median :78.41   Median :17.32   Median :40  
Mean   : 5.876e-05   Mean   : 9.137   Mean   :78.41   Mean   :17.32   Mean   :40  
3rd Qu.: 6.990e-04   3rd Qu.: 9.652   3rd Qu.:78.41   3rd Qu.:17.32   3rd Qu.:40  
Max.   : 3.236e-03   Max.   :21.658   Max.   :78.42   Max.   :17.33   Max.   :40  
sample_n      ph_0_5         ph_5_15         ph_15_30         oc_0_5      
Min.   :28   Min.   :5.002   Min.   :5.026   Min.   :5.040   Min.   :0.5221  
1st Qu.:28   1st Qu.:5.753   1st Qu.:5.686   1st Qu.:5.588   1st Qu.:1.1279  
Median :28   Median :6.412   Median :6.214   Median :6.031   Median :2.1295  
Mean   :28   Mean   :6.524   Mean   :6.231   Mean   :6.048   Mean   :2.1696  
3rd Qu.:28   3rd Qu.:7.342   3rd Qu.:6.680   3rd Qu.:6.441   3rd Qu.:3.1255  
Max.   :28   Max.   :7.983   Max.   :7.461   Max.   :6.971   Max.   :3.9979  
oc_5_15          oc_15_30         ph_mean         oc_mean      
Min.   :0.3480   Min.   :0.1009   Min.   :5.314   Min.   :0.6249  
1st Qu.:0.9372   1st Qu.:0.5514   1st Qu.:5.997   1st Qu.:1.1934  
Median :1.4767   Median :0.9470   Median :6.336   Median :1.6145  
Mean   :1.6048   Mean   :1.0050   Mean   :6.268   Mean   :1.5931  
3rd Qu.:2.2717   3rd Qu.:1.5027   3rd Qu.:6.552   3rd Qu.:2.0432  
Max.   :2.9909   Max.   :1.9866   Max.   :7.127   Max.   :2.7333  
> 
  > # 3. Check missing values
  > na_counts <- colSums(is.na(main90))
> print(na_counts)
dem_class               DEM             slope            aspect profile_curvature 
0                 0                 0                 0                 0 
plan_curvature               twi         longitude          latitude                 n 
0                 0                 0                 0                 0 
sample_n            ph_0_5           ph_5_15          ph_15_30            oc_0_5 
0                 0                 0                 0                 0 
oc_5_15          oc_15_30           ph_mean           oc_mean 
0                 0                 0                 0 
> 
  > # 4. Detect outliers (basic check)
  > outlier_summary <- main90 %>%
  +     select_if(is.numeric) %>%
  +     summarise_all(~sum(. > (mean(.) + 3*sd(.)) | . < (mean(.) - 3*sd(.))))
> print(outlier_summary)
DEM slope aspect profile_curvature plan_curvature twi longitude latitude n sample_n ph_0_5
1   0     1      0                 0              0   1         0        0 0        0      0
ph_5_15 ph_15_30 oc_0_5 oc_5_15 oc_15_30 ph_mean oc_mean
1       0        0      0       0        0       0       0
> 
  > # 5. Save cleaned dataset (before removing any vars)
  > write.csv(main90, "DEM_variability_points_main90_cleaned.csv", row.names = FALSE)
Error in file(file, ifelse(append, "a", "w")) : 
  cannot open the connection
In addition: Warning message:
  In file(file, ifelse(append, "a", "w")) :
  cannot open file 'DEM_variability_points_main90_cleaned.csv': Permission denied

>  DATA CLEANING + EDA + CORRELATION SCRIPT
Error: unexpected symbol in " DATA CLEANING"

> # ==========================================================
> # DATA CLEANING + EDA + CORRELATION SCRIPT
  > # ==========================================================
> 
  > # Load libraries
  > library(tidyverse)
> library(psych)
> library(corrplot)
> library(GGally)
> 
  > # 1. Load dataset
  > main90 <- read.csv("DEM_variability_points_main90.csv")
> 
  > # 2. Check structure and summary
  > str(main90)
'data.frame':	90 obs. of  19 variables:
  $ dem_class        : chr  "[521,528]" "[521,528]" "[521,528]" "[521,528]" ...
$ DEM              : num  523 524 524 525 525 ...
$ slope            : num  5.69 1.3 3.78 3.02 1.41 ...
$ aspect           : num  41.6 16.4 290.2 222.2 263.6 ...
$ profile_curvature: num  -0.001811 -0.00144 -0.000896 -0.000372 0.001206 ...
$ plan_curvature   : num  0.000748 0.001071 -0.000507 -0.001677 -0.000049 ...
$ twi              : num  8.32 16.87 10.53 15.03 17.36 ...
$ longitude        : num  78.4 78.4 78.4 78.4 78.4 ...
$ latitude         : num  17.3 17.3 17.3 17.3 17.3 ...
$ n                : int  40 40 40 40 40 40 40 40 40 40 ...
$ sample_n         : int  28 28 28 28 28 28 28 28 28 28 ...
$ ph_0_5           : num  5.86 7.36 6.23 7.65 7.82 ...
$ ph_5_15          : num  5.33 6.63 5.86 6.64 5.8 ...
$ ph_15_30         : num  6.68 5.62 6.42 5.53 6.19 ...
$ oc_0_5           : num  3.549 0.522 0.752 1.075 3.196 ...
$ oc_5_15          : num  0.688 0.821 2.721 1.132 1.281 ...
$ oc_15_30         : num  1.095 0.765 0.557 0.211 0.55 ...
$ ph_mean          : num  5.96 6.54 6.17 6.61 6.6 ...
$ oc_mean          : num  1.777 0.702 1.343 0.806 1.676 ...
> summary(main90)
dem_class              DEM            slope            aspect        profile_curvature   
Length:90          Min.   :523.3   Min.   :0.5943   Min.   :  1.089   Min.   :-3.042e-03  
Class :character   1st Qu.:528.7   1st Qu.:1.6326   1st Qu.: 86.948   1st Qu.:-8.780e-04  
Mode  :character   Median :533.2   Median :2.5269   Median :228.244   Median :-1.495e-04  
Mean   :533.9   Mean   :2.7163   Mean   :198.618   Mean   :-9.004e-05  
3rd Qu.:537.2   3rd Qu.:3.4828   3rd Qu.:297.363   3rd Qu.: 5.652e-04  
Max.   :552.9   Max.   :8.1235   Max.   :356.492   Max.   : 2.767e-03  
plan_curvature            twi           longitude        latitude           n     
Min.   :-2.643e-03   Min.   : 6.498   Min.   :78.39   Min.   :17.31   Min.   :40  
1st Qu.:-6.312e-04   1st Qu.: 7.395   1st Qu.:78.40   1st Qu.:17.32   1st Qu.:40  
Median : 2.500e-06   Median : 8.076   Median :78.41   Median :17.32   Median :40  
Mean   : 5.876e-05   Mean   : 9.137   Mean   :78.41   Mean   :17.32   Mean   :40  
3rd Qu.: 6.990e-04   3rd Qu.: 9.652   3rd Qu.:78.41   3rd Qu.:17.32   3rd Qu.:40  
Max.   : 3.236e-03   Max.   :21.658   Max.   :78.42   Max.   :17.33   Max.   :40  
sample_n      ph_0_5         ph_5_15         ph_15_30         oc_0_5      
Min.   :28   Min.   :5.002   Min.   :5.026   Min.   :5.040   Min.   :0.5221  
1st Qu.:28   1st Qu.:5.753   1st Qu.:5.686   1st Qu.:5.588   1st Qu.:1.1279  
Median :28   Median :6.412   Median :6.214   Median :6.031   Median :2.1295  
Mean   :28   Mean   :6.524   Mean   :6.231   Mean   :6.048   Mean   :2.1696  
3rd Qu.:28   3rd Qu.:7.342   3rd Qu.:6.680   3rd Qu.:6.441   3rd Qu.:3.1255  
Max.   :28   Max.   :7.983   Max.   :7.461   Max.   :6.971   Max.   :3.9979  
oc_5_15          oc_15_30         ph_mean         oc_mean      
Min.   :0.3480   Min.   :0.1009   Min.   :5.314   Min.   :0.6249  
1st Qu.:0.9372   1st Qu.:0.5514   1st Qu.:5.997   1st Qu.:1.1934  
Median :1.4767   Median :0.9470   Median :6.336   Median :1.6145  
Mean   :1.6048   Mean   :1.0050   Mean   :6.268   Mean   :1.5931  
3rd Qu.:2.2717   3rd Qu.:1.5027   3rd Qu.:6.552   3rd Qu.:2.0432  
Max.   :2.9909   Max.   :1.9866   Max.   :7.127   Max.   :2.7333  
> 
  > # 3. Check missing values
  > na_counts <- colSums(is.na(main90))
> print(na_counts)
dem_class               DEM             slope            aspect profile_curvature 
0                 0                 0                 0                 0 
plan_curvature               twi         longitude          latitude                 n 
0                 0                 0                 0                 0 
sample_n            ph_0_5           ph_5_15          ph_15_30            oc_0_5 
0                 0                 0                 0                 0 
oc_5_15          oc_15_30           ph_mean           oc_mean 
0                 0                 0                 0 
> 
  > # 4. Detect outliers (basic check)
  > outlier_summary <- main90 %>%
  +     select_if(is.numeric) %>%
  +     summarise_all(~sum(. > (mean(.) + 3*sd(.)) | . < (mean(.) - 3*sd(.))))
> print(outlier_summary)
DEM slope aspect profile_curvature plan_curvature twi longitude latitude n sample_n ph_0_5
1   0     1      0                 0              0   1         0        0 0        0      0
ph_5_15 ph_15_30 oc_0_5 oc_5_15 oc_15_30 ph_mean oc_mean
1       0        0      0       0        0       0       0
> 
  > # 5. Save cleaned dataset (before removing any vars)
  > write.csv(main90, "DEM_variability_points_main90_cleaned.csv", row.names = FALSE)
> 
  > # ==========================================================
> # EXPLORATORY DATA ANALYSIS
  > # ==========================================================
> 
  > # 6. Histograms
  > num_cols <- main90 %>% select_if(is.numeric)
> 
  > pdf("EDA_Histograms.pdf")
> num_cols %>% gather() %>%
  +     ggplot(aes(value)) +
  +     facet_wrap(~key, scales = "free") +
  +     geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  +     theme_minimal()
> dev.off()
RStudioGD 
2 
> 
  > # 7. Boxplots
  > pdf("EDA_Boxplots.pdf")
> num_cols %>% gather() %>%
  +     ggplot(aes(x = key, y = value)) +
  +     geom_boxplot(fill = "orange", color = "black") +
  +     coord_flip() +
  +     theme_minimal()
> dev.off()
RStudioGD 
2 
> 
  > # 8. Descriptive statistics
  > desc_stats <- describe(num_cols)
> write.csv(desc_stats, "EDA_Descriptive_Statistics.csv")
> 
  > # ==========================================================
> # PEARSON CORRELATION
  > # ==========================================================
> 
  > # 9. Remove zero-variance columns (like n, sample_n)
  > zero_var <- sapply(num_cols, function(x) sd(x, na.rm = TRUE) == 0)
> num_cols_corr <- num_cols[, !zero_var]
> 
  > # 10. Compute correlation matrix
  > cor_matrix <- cor(num_cols_corr, use = "complete.obs", method = "pearson")
> write.csv(cor_matrix, "Pearson_Correlation_Matrix.csv")
> 
  > # 11. Heatmap
  > png("Correlation_Heatmap.png", width = 1000, height = 800)
> corrplot(cor_matrix, method = "color", type = "upper",
           +          tl.cex = 0.8, addCoef.col = "black")
> dev.off()
RStudioGD 
2 
> 
  > # 12. Pairplot
  > png("Correlation_Pairplot.png", width = 1200, height = 1000)
> ggpairs(num_cols_corr)
> dev.off()                                                                                  

RStudioGD 
2 
> 
  > # ==========================================================
> # DONE
  > # ==========================================================
> cat("✅ Data cleaning, EDA, and correlation analysis completed.\n")
✅ Data cleaning, EDA, and correlation analysis completed.
> cat("👉 Outputs saved as CSV + PNG/PDF files in your working directory.\n")
👉 Outputs saved as CSV + PNG/PDF files in your working directory.
> 
  > # Example dataframe: soil_data with columns N, P, K, pH, EC
  > library(e1071) # for skewness & kurtosis
> 
  > eda_summary <- function(x) {
    +     c(
      +         Min = min(x, na.rm = TRUE),
      +         Max = max(x, na.rm = TRUE),
      +         Mean = mean(x, na.rm = TRUE),
      +         Median = median(x, na.rm = TRUE),
      +         SD = sd(x, na.rm = TRUE),
      +         CV = (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100,
      +         Skewness = skewness(x, na.rm = TRUE),
      +         Kurtosis = kurtosis(x, na.rm = TRUE)
      +     )
    + }
> 
  > result <- as.data.frame(t(sapply(soil_data, eda_summary)))
Error: object 'soil_data' not found

> library(e1071) # for skewness & kurtosis
> 
  > eda_summary <- function(x) {
    +     c(
      +         Min = min(x, na.rm = TRUE),
      +         Max = max(x, na.rm = TRUE),
      +         Mean = mean(x, na.rm = TRUE),
      +         Median = median(x, na.rm = TRUE),
      +         SD = sd(x, na.rm = TRUE),
      +         CV = (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100, # fixed
      +         Skewness = skewness(x, na.rm = TRUE),
      +         Kurtosis = kurtosis(x, na.rm = TRUE)
      +     )
    + }
> 
  > # Apply on your dataset (numeric columns only)
  > result <- as.data.frame(t(sapply(main90 %>% select_if(is.numeric), eda_summary)))
> 
  > # Save to CSV
  > write.csv(result, "EDA_Descriptive_Statistics.csv", row.names = TRUE)
Error in file(file, ifelse(append, "a", "w")) : 
  cannot open the connection
In addition: Warning message:
  In file(file, ifelse(append, "a", "w")) :
  cannot open file 'EDA_Descriptive_Statistics.csv': Permission denied

> library(e1071) # for skewness & kurtosis
> 
  > eda_summary <- function(x) {
    +     c(
      +         Min = min(x, na.rm = TRUE),
      +         Max = max(x, na.rm = TRUE),
      +         Mean = mean(x, na.rm = TRUE),
      +         Median = median(x, na.rm = TRUE),
      +         SD = sd(x, na.rm = TRUE),
      +         CV = (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100, # fixed
      +         Skewness = skewness(x, na.rm = TRUE),
      +         Kurtosis = kurtosis(x, na.rm = TRUE)
      +     )
    + }
> 
  > # Apply on your dataset (numeric columns only)
  > result <- as.data.frame(t(sapply(main90 %>% select_if(is.numeric), eda_summary)))
> 
  > # Save to CSV
  > write.csv(result, "EDA_Descriptive_Statistics.csv", row.names = TRUE)
> 
  > # Load libraries
  > library(ggplot2)
> 
  > # Read your dataset
  > data <- read.csv("DEM_variability_points_main90.csv")
> 
  > # Check column names first
  > names(data)
[1] "dem_class"         "DEM"               "slope"             "aspect"           
[5] "profile_curvature" "plan_curvature"    "twi"               "longitude"        
[9] "latitude"          "n"                 "sample_n"          "ph_0_5"           
[13] "ph_5_15"           "ph_15_30"          "oc_0_5"            "oc_5_15"          
[17] "oc_15_30"          "ph_mean"           "oc_mean"          
> 
  > # Histogram for pH
  > ggplot(data, aes(x = pH)) +
  +     geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black", alpha = 0.7) +
  +     geom_density(aes(y = ..count..), color = "red", size = 1) +
  +     labs(title = "Frequency Distribution of pH",
             +          x = "pH", y = "Frequency") +
  +     theme_minimal()
Error in `geom_histogram()`:
  ! Problem while computing aesthetics.
ℹ Error occurred in the 1st layer.
Caused by error:
  ! object 'pH' not found
Run `rlang::last_trace()` to see where the error occurred.
Warning message:
  Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
ℹ Please use `linewidth` instead.
This warning is displayed once every 8 hours.
Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated. 

> ggplot(data, aes(x = ph_0_5)) +
  +     geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black", alpha = 0.7) +
  +     geom_density(aes(y = ..count..), color = "red", linewidth = 1) +
  +     labs(title = "Frequency Distribution of pH (0–5 cm)",
             +          x = "pH", y = "Frequency") +
  +     theme_minimal()
Warning message:
  The dot-dot notation (`..count..`) was deprecated in ggplot2 3.4.0.
ℹ Please use `after_stat(count)` instead.
This warning is displayed once every 8 hours.
Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated. 
> 
  > # Load libraries
  > library(dplyr)
> 
  > # Read your dataset
  > data <- read.csv("DEM_variability_points_main90.csv")
> 
  > set.seed(123)  # for reproducibility
> 
  > # Create training (80%) and validation (20%) sets
  > train_index <- sample(1:nrow(data), size = 0.8 * nrow(data))
> 
  > train_data <- data[train_index, ]
> valid_data <- data[-train_index, ]
> 
  > # Save them as CSVs
  > write.csv(train_data, "train_data.csv", row.names = FALSE)
> write.csv(valid_data, "valid_data.csv", row.names = FALSE)
> 
  > cat("Training set size:", nrow(train_data), "\n")
Training set size: 72 
> cat("Validation set size:", nrow(valid_data), "\n")
Validation set size: 18 
> 
  > library(sf)
Linking to GEOS 3.13.1, GDAL 3.11.0, PROJ 9.6.0; sf_use_s2() is TRUE
> library(blockCV)
Error in library(blockCV) : there is no package called ‘blockCV’

> install.packages("blockCV")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:
  
  https://cran.rstudio.com/bin/windows/Rtools/
  Warning in install.packages :
  package ‘blockCV’ is not available for this version of R

A version of this package for your version of R might be available elsewhere,
see the ideas at
https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages

> library(sf)
> library(dplyr)
> 
  > # Load your soil points
  > pts <- st_read("DEM_variability_points_main90.csv")
Reading layer `DEM_variability_points_main90' from data source 
  `C:\Users\User\Documents\R script\datacleaning\DEM_variability_points_main90.csv' 
using driver `CSV'
Warning message:
no simple feature geometries present: returning a data.frame or tbl_df 
> 
> # Make sure it’s in projected CRS (meters)
> pts <- st_transform(pts, 32644) # example: UTM Zone 44N, change CRS as needed
Error in UseMethod("st_transform") : 
  no applicable method for 'st_transform' applied to an object of class "data.frame"

> library(sp)
> 
> set.seed(123)
> n_val <- round(0.2 * nrow(pts))  # 20% validation
> val_idx <- sample(1:nrow(pts), n_val)
> 
> # training set excludes validation + nearby points (e.g., 1 km buffer)
> buffer <- 1000 # meters
> val_pts <- pts[val_idx, ]
> train_pts <- pts[-val_idx, ]
> 
> # Remove points within buffer of validation
> nearby <- st_is_within_distance(train_pts, val_pts, dist = buffer)
Error in UseMethod("st_geometry") : 
  no applicable method for 'st_geometry' applied to an object of class "data.frame"

> library(sf)
> 
> # Example: if your CSV has columns "lon" and "lat"
> pts <- read.csv("DEM_variability_points_main90.csv")
> 
> # Convert to sf object (change EPSG to your CRS, e.g., WGS84=4326 or UTM=32644)
> pts <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
Error in `[.data.frame`(x, coords) : undefined columns selected

> install.packages("usethis")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
also installing the dependencies ‘credentials’, ‘openssl’, ‘zip’, ‘gitcreds’, ‘httr2’, ‘ini’, ‘desc’, ‘gert’, ‘gh’, ‘rprojroot’, ‘whisker’
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/credentials_2.0.2.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/openssl_2.3.3.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/zip_2.3.3.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/gitcreds_0.1.2.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/httr2_1.2.1.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/ini_0.3.1.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/desc_1.4.3.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/gert_2.1.5.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/gh_1.5.0.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/rprojroot_2.1.0.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/whisker_0.4.1.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/usethis_3.1.0.zip'
package ‘credentials’ successfully unpacked and MD5 sums checked
package ‘openssl’ successfully unpacked and MD5 sums checked
package ‘zip’ successfully unpacked and MD5 sums checked
package ‘gitcreds’ successfully unpacked and MD5 sums checked
package ‘httr2’ successfully unpacked and MD5 sums checked
package ‘ini’ successfully unpacked and MD5 sums checked
package ‘desc’ successfully unpacked and MD5 sums checked
package ‘gert’ successfully unpacked and MD5 sums checked
package ‘gh’ successfully unpacked and MD5 sums checked
package ‘rprojroot’ successfully unpacked and MD5 sums checked
package ‘whisker’ successfully unpacked and MD5 sums checked
package ‘usethis’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\User\AppData\Local\Temp\RtmpcNQQRk\downloaded_packages
> install.packages("gert")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/gert_2.1.5.zip'
Content type 'application/zip' length 3712453 bytes (3.5 MB)
downloaded 3.5 MB

package ‘gert’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\User\AppData\Local\Temp\RtmpcNQQRk\downloaded_packages
> # ==========================================================
> # R Script: Automatic Git Push for Project Files
> # ==========================================================
> 
> # 1️⃣ Load required packages
> if(!require(usethis)) install.packages("usethis")
Loading required package: usethis
> if(!require(gert)) install.packages("gert")
Loading required package: gert
Linking to libgit2 v1.7.2, ssh support: YES
Global config: C:\Users\User\.gitconfig
System config: C:/Program Files/Git/etc/gitconfig
Default user: Kajal Roy <kajal.ponnu@gmail.com>
> 
> library(usethis)
> library(gert)
> 
> # 2️⃣ Set Git identity
> git_config_set("user.name", "Kajal Ponnu")
> git_config_set("user.email", "kajal.ponnu@gmail.com")
> 
> # 3️⃣ List of files to push
> files_to_push <- c(
+     ".gitignore",
+     "Correlation_Pairplot.png",
+     "DEM_variability_points_main90.csv",
+     "DEM_variability_points_main90_cleaned.csv",
+     "EDA_Boxplots.pdf",
+     "EDA_Descriptive_Statistics.csv",
+     "EDA_Histograms.pdf",
+     "Pearson_Correlation_Matrix.csv",
+     "datacleaning.Rproj",
+     "histogram 0-5.png",
+     "pearson.png",
+     "train_data.csv",
+     "valid_data.csv"
+ )
> 
> # 4️⃣ Initialize Git if not done already
> if(!git_info()$repo) {
+     usethis::use_git()
+ }
Error in !git_info()$repo : invalid argument type

> # 4️⃣ Initialize Git if not done already
> git_status <- try(git_info(), silent = TRUE)
> 
> if(inherits(git_status, "try-error") || is.null(git_status)) {
+     usethis::use_git()
+     cat("✅ Git initialized in this project.\n")
+ } else {
+     cat("✅ Git is already initialized.\n")
+ }
✅ Git is already initialized.
> 
> # ==========================================================
> # R Script: Automatic Git Push for Project Files
> # ==========================================================
> 
> # 1️⃣ Load required packages
> if(!require(usethis)) install.packages("usethis")
> if(!require(gert)) install.packages("gert")
> 
> library(usethis)
> library(gert)
> 
> # 2️⃣ Set Git identity
> git_config_set("user.name", "Kajal Ponnu")
> git_config_set("user.email", "kajal.ponnu@gmail.com")
> 
> # 3️⃣ List of files to push
> files_to_push <- c(
+     ".gitignore",
+     "Correlation_Pairplot.png",
+     "DEM_variability_points_main90.csv",
+     "DEM_variability_points_main90_cleaned.csv",
+     "EDA_Boxplots.pdf",
+     "EDA_Descriptive_Statistics.csv",
+     "EDA_Histograms.pdf",
+     "Pearson_Correlation_Matrix.csv",
+     "datacleaning.Rproj",
+     "histogram 0-5.png",
+     "pearson.png",
+     "train_data.csv",
+     "valid_data.csv"
+ )
> 
> # 4️⃣ Initialize Git if not done already
> if(!git_info()$repo) {
+     usethis::use_git()
+ }
Error in !git_info()$repo : invalid argument type

> install.packages("git2r")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/git2r_0.36.2.zip'
Content type 'application/zip' length 1983364 bytes (1.9 MB)
downloaded 1.9 MB

package ‘git2r’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\User\AppData\Local\Temp\RtmpcNQQRk\downloaded_packages
> # ==========================================================
> # R Script to Push Files to GitHub Automatically
> # ==========================================================
> 
> # Load required libraries
> if(!require(usethis)) install.packages("usethis", dependencies = TRUE)
> if(!require(git2r)) install.packages("git2r", dependencies = TRUE)
Loading required package: git2r

Attaching package: ‘git2r’

The following object is masked from ‘package:sp’:

    merge

The following object is masked from ‘package:psych’:

    lookup

The following objects are masked from ‘package:purrr’:

    is_empty, when

The following object is masked from ‘package:dplyr’:

    pull
> library(usethis)
> library(git2r)
> 
> # 1️⃣ Set Git user info
> git_config_global(user.name = "Kajal Roy")
Error in git_config_global(user.name = "Kajal Roy") : 
  unused argument (user.name = "Kajal Roy")

> library(git2r)
> 
> # Set global Git user info
> config(user.name = "Kajal Roy", user.email = "kajal.ponnu@gmail.com", global = TRUE)
> cat("✅ Git user configured.\n")
✅ Git user configured.
> 
> # ==========================================================
> # R Script to Push Files to GitHub Automatically
> # ==========================================================
> 
> # Load required libraries
> if(!require(usethis)) install.packages("usethis", dependencies = TRUE)
> if(!require(git2r)) install.packages("git2r", dependencies = TRUE)
> library(usethis)
> library(git2r)
> 
> # 1️⃣ Set Git user info
> git_config_global(user.name = "Kajal Roy")
Error in git_config_global(user.name = "Kajal Roy") : 
  unused argument (user.name = "Kajal Roy")

> # ==========================================================
> # R Script: Auto Git Commit & Push
> # ==========================================================
> 
> # Load required library
> library(git2r)
> 
> # 1️⃣ Set global Git user info (only needs to be done once)
> config(user.name = "Kajal Roy", 
+        user.email = "kajal.ponnu@gmail.com", 
+        global = TRUE)
> cat("✅ Git user configured.\n")
✅ Git user configured.
> 
> # 2️⃣ Define repository path (local folder containing your R project)
> repo_path <- "C:/Users/User/Documents/R script/datacleaning"  # change if needed
> 
> # 3️⃣ Initialize Git repository if not already done
> if(!file.exists(file.path(repo_path, ".git"))) {
+     repo <- init(repo_path)
+     cat("✅ Git repository initialized.\n")
+ } else {
+     repo <- repository(repo_path)
+     cat("✅ Existing Git repository found.\n")
+ }
✅ Existing Git repository found.
> 
> # 4️⃣ Add files to staging (all your project files)
> add(repo, c(".gitignore",
+             "Correlation_Pairplot.png",
+             "DEM_variability_points_main90.csv",
+             "DEM_variability_points_main90_cleaned.csv",
+             "EDA_Boxplots.pdf",
+             "EDA_Descriptive_Statistics.csv",
+             "EDA_Histograms.pdf",
+             "Pearson_Correlation_Matrix.csv",
+             "datacleaning.Rproj",
+             "histogram 0-5.png",
+             "pearson.png",
+             "train_data.csv",
+             "valid_data.csv"))
> cat("✅ Files added to staging.\n")
✅ Files added to staging.
> 
> # 5️⃣ Commit with a message
> commit(repo, "Updated data cleaning + EDA + correlation scripts and outputs")
[e5bf5ae] 2025-08-17: Updated data cleaning + EDA + correlation scripts and outputs
> cat("✅ Commit created.\n")
✅ Commit created.
> 
> # 6️⃣ Set remote (replace with your GitHub repo URL)
> remote_url <- "https://github.com/yourusername/your-repo.git"  # change this
> if(!"origin" %in% names(remotes(repo))) {
+     remote_add(repo, name = "origin", url = remote_url)
+     cat("✅ Remote 'origin' added.\n")
+ }
Error in remote_add(repo, name = "origin", url = remote_url) : 
  Error in 'git2r_remote_add': remote 'origin' already exists

> # ==========================================================
> # R Script: Auto Git Commit & Push
> # ==========================================================
> 
> # Load required library
> library(git2r)
> 
> # 1️⃣ Set global Git user info (only needs to be done once)
> config(user.name = "Kajal Roy", 
+        user.email = "kajal.ponnu@gmail.com", 
+        global = TRUE)
> cat("✅ Git user configured.\n")
✅ Git user configured.
> 
> # 2️⃣ Define repository path (local folder containing your R project)
> repo_path <- "C:/Users/User/Documents/R script/datacleaning"  # change if needed
> 
> # 3️⃣ Initialize Git repository if not already done
> if(!file.exists(file.path(repo_path, ".git"))) {
+     repo <- init(repo_path)
+     cat("✅ Git repository initialized.\n")
+ } else {
+     repo <- repository(repo_path)
+     cat("✅ Existing Git repository found.\n")
+ }
✅ Existing Git repository found.
> 
> # 4️⃣ Add files to staging (all your project files)
> add(repo, c(".gitignore",
+             "Correlation_Pairplot.png",
+             "DEM_variability_points_main90.csv",
+             "DEM_variability_points_main90_cleaned.csv",
+             "EDA_Boxplots.pdf",
+             "EDA_Descriptive_Statistics.csv",
+             "EDA_Histograms.pdf",
+             "Pearson_Correlation_Matrix.csv",
+             "datacleaning.Rproj",
+             "histogram 0-5.png",
+             "pearson.png",
+             "train_data.csv",
+             "valid_data.csv"))
> cat("✅ Files added to staging.\n")
✅ Files added to staging.
> 
> # 5️⃣ Commit with a message
> commit(repo, "Updated data cleaning + EDA + correlation scripts and outputs")
Error in commit(repo, "Updated data cleaning + EDA + correlation scripts and outputs") : 
  Error in 'git2r_commit': Nothing added to commit

> # Make sure all files are added to staging
> add(repo, "*")  # adds all files in the repository folder
> status(repo)     # check which files are staged
working directory clean
> 
> # Now commit
> commit(repo, "Updated data cleaning + EDA + correlation scripts and outputs")
Error in commit(repo, "Updated data cleaning + EDA + correlation scripts and outputs") : 
  Error in 'git2r_commit': Nothing added to commit

> library(git2r)
> 
> # 1️⃣ Open your repository
> repo <- repository("path/to/your/project")  # replace with your project folder path
Error in repository("path/to/your/project") : 
  The 'path' is not in a git repository
In addition: Warning message:
In normalizePath(path.expand(path), winslash, mustWork) :
  path[1]="path/to/your/project": The system cannot find the path specified

> library(git2r)
> 
> # Set global Git user info
> config(user.name = "Kajal Ponnu", user.email = "kajal.ponnu@gmail.com", global = TRUE)
> 
> # Initialize or open the repository
> repo_path <- "path/to/your/project"  # Replace with your actual project path
> repo <- if (file.exists(file.path(repo_path, ".git"))) {
+     repository(repo_path)
+ } else {
+     init(repo_path)
+ }
Error in normalizePath(path.expand(path), winslash, mustWork) : 
  path[1]="path/to/your/project": The system cannot find the path specified

> repo_path <- "C:/Users/User/Documents/RProjects/datacleaning"
> 
> # Load required libraries
> library(usethis)
> library(git2r)
> 
> # -----------------------------
> # 1️⃣ Define your project path
> repo_path <- "C:/Users/User/Documents/RProjects/datacleaning"
> 
> # -----------------------------
> # 2️⃣ Initialize or open Git repository
> repo <- if (file.exists(file.path(repo_path, ".git"))) {
+     repository(repo_path)
+ } else {
+     init(repo_path)
+ }
Error in normalizePath(path.expand(path), winslash, mustWork) : 
  path[1]="C:/Users/User/Documents/RProjects/datacleaning": The system cannot find the path specified

> # Load required libraries
> library(usethis)
> library(git2r)
> 
> # -----------------------------
> # 1️⃣ Define your project path
> repo_path <- "C:/Users/User/Documents/R script/datacleaning"
> 
> # -----------------------------
> # 2️⃣ Initialize or open Git repository
> repo <- if (file.exists(file.path(repo_path, ".git"))) {
+     repository(repo_path)
+ } else {
+     init(repo_path)
+ }
> 
> # -----------------------------
> # 3️⃣ Set global Git user info (only once)
> config(repo, user.name = "Kajal Roy", user.email = "kajal.ponnu@gmail.com")
> 
> # -----------------------------
> # 4️⃣ Add all files in the project folder to staging
> add(repo, "*")
> 
> # -----------------------------
> # 5️⃣ Commit the changes
> commit(repo, "Updated data cleaning + EDA + correlation scripts and outputs")
Error in commit(repo, "Updated data cleaning + EDA + correlation scripts and outputs") : 
  Error in 'git2r_commit': Nothing added to commit

> setwd("C:/Users/User/Documents/R script/datacleaning")
> getwd()  # just to confirm
[1] "C:/Users/User/Documents/R script/datacleaning"
> 
> library(git2r)
> 
> repo <- if (file.exists(".git")) {
+     repository(".")
+ } else {
+     init(".")
+ }
> 
> files_to_add <- c(
+     ".gitignore",
+     "Correlation_Pairplot.png",
+     "DEM_variability_points_main90.csv",
+     "DEM_variability_points_main90_cleaned.csv",
+     "EDA_Boxplots.pdf",
+     "EDA_Descriptive_Statistics.csv",
+     "EDA_Histograms.pdf",
+     "Pearson_Correlation_Matrix.csv",
+     "datacleaning.Rproj",
+     "histogram 0-5.png",
+     "pearson.png",
+     "train_data.csv",
+     "valid_data.csv"
+ )
> 
> add(repo, files_to_add)
> 
> commit(repo, "Updated data cleaning + EDA + correlation scripts and outputs")
Error in commit(repo, "Updated data cleaning + EDA + correlation scripts and outputs") : 
  Error in 'git2r_commit': Nothing added to commit

> # Load required libraries
> library(usethis)
> library(git2r)
> 
> # -----------------------------
> # 1️⃣ Define your project path
> repo_path <- "C:/Users/User/Documents/R script/datacleaning"  # change to your folder path
> 
> # -----------------------------
> # 2️⃣ Initialize or open Git repository
> repo <- if (file.exists(file.path(repo_path, ".git"))) {
+     repository(repo_path)
+ } else {
+     init(repo_path)
+ }
> 
> # -----------------------------
> # 3️⃣ Set global Git user info (only once)
> config(repo, user.name = "Kajal Roy", user.email = "kajal.ponnu@gmail.com")
> 
> # -----------------------------
> # 4️⃣ List files to add (R scripts + outputs)
> files_to_add <- c(
+     "datacleaning.Rproj",       # R project
+     "datacleaning.R",           # your R script file
+     ".gitignore",
+     "Correlation_Pairplot.png",
+     "DEM_variability_points_main90.csv",
+     "DEM_variability_points_main90_cleaned.csv",
+     "EDA_Boxplots.pdf",
+     "EDA_Descriptive_Statistics.csv",
+     "EDA_Histograms.pdf",
+     "Pearson_Correlation_Matrix.csv",
+     "histogram 0-5.png",
+     "pearson.png",
+     "train_data.csv",
+     "valid_data.csv"
+ )
> 
> # -----------------------------
> # 5️⃣ Add files to staging
> add(repo, files_to_add)
> 
> # -----------------------------
> # 6️⃣ Commit the changes
> commit_message <- "Updated data cleaning + EDA + correlation scripts and outputs"
> commit(repo, commit_message)
Error in commit(repo, commit_message) : 
  Error in 'git2r_commit': Nothing added to commit

> add(repo, "*")  # stages everything in the repo folder
> status(repo)     # check what’s staged
working directory clean
> commit(repo, "Force commit all files including unchanged ones")
Error in commit(repo, "Force commit all files including unchanged ones") : 
  Error in 'git2r_commit': Nothing added to commit

> # Path to your file
> file_path <- "C:/Users/User/Documents/R script/datacleaning/datacleaning.R"
> 
> # Append a comment line at the end
> write("\n# Minor update for Git commit", file = file_path, append = TRUE)
> 
> add(repo, "datacleaning.R")
> commit(repo, "Added minor comment for Git commit")
[947ba23] 2025-08-17: Added minor comment for Git commit
> 
> # ================================
> # Git Auto Commit Script for R
> # ================================
> 
> # Load libraries
> library(usethis)
> library(git2r)
> 
> # -------------------------------
> # 1️⃣ Define your project path
> repo_path <- "C:/Users/User/Documents/R script/datacleaning"  # CHANGE this
> 
> # -------------------------------
> # 2️⃣ Initialize or open Git repository
> repo <- if (file.exists(file.path(repo_path, ".git"))) {
+     repository(repo_path)
+ } else {
+     init(repo_path)
+ }
> 
> # -------------------------------
> # 3️⃣ Set global Git info (once)
> config(repo, user.name = "Kajal Roy", user.email = "kajal.ponnu@gmail.com")
> 
> # -------------------------------
> # 4️⃣ Append minor comment to all R scripts to ensure a commit
> r_scripts <- list.files(repo_path, pattern = "\\.R$", full.names = TRUE)
> for (f in r_scripts) {
+     write("\n# Minor update for Git commit", file = f, append = TRUE)
+ }
> 
> # -------------------------------
> # 5️⃣ List of files to add (scripts + outputs)
> files_to_add <- c(
+     "datacleaning.Rproj",
+     r_scripts,  # all R scripts
+     ".gitignore",
+     "Correlation_Pairplot.png",
+     "DEM_variability_points_main90.csv",
+     "DEM_variability_points_main90_cleaned.csv",
+     "EDA_Boxplots.pdf",
+     "EDA_Descriptive_Statistics.csv",
+     "EDA_Histograms.pdf",
+     "Pearson_Correlation_Matrix.csv",
+     "histogram 0-5.png",
+     "pearson.png",
+     "train_data.csv",
+     "valid_data.csv"
+ )
> 
> # -------------------------------
> # 6️⃣ Stage files
> add(repo, files_to_add)
> 
> # -------------------------------
> # 7️⃣ Commit changes
> commit_message <- "Updated data cleaning + EDA + correlation scripts and outputs"
> commit(repo, commit_message)
[80d4636] 2025-08-17: Updated data cleaning + EDA + correlation scripts and outputs
> 
> # -------------------------------
> # 8️⃣ Push to remote (change URL if needed)
> remote_add(repo, "origin", "https://github.com/kajalponnu-lgtm/datacleaning.git")
Error in remote_add(repo, "origin", "https://github.com/kajalponnu-lgtm/datacleaning.git") : 
  Error in 'git2r_remote_add': remote 'origin' already exists

> # Push to existing remote
> push(repo, name = "origin", refspec = "refs/heads/main")
Error in push(repo, name = "origin", refspec = "refs/heads/main") : 
  Error in 'git2r_push': no error

> library(git2r)
> 
> # Set up credentials
> cred <- cred_user_pass(username = "kajalponnu-lgtm", password = "YOUR_PERSONAL_ACCESS_TOKEN")
> 
> # Push with credentials
> push(repo, name = "origin", refspec = "refs/heads/main", credentials = cred)
Error in push(repo, name = "origin", refspec = "refs/heads/main", credentials = cred) : 
  Error in 'git2r_push': too many redirects or authentication replays

> library(git2r)
> 
> # 1️⃣ Open your local repository
> repo_path <- "C:/Users/User/Documents/R script/datacleaning"
> repo <- repository(repo_path)
> 
> # 2️⃣ Create credentials using your GitHub username and personal access token
> cred <- cred_user_pass(
+     username = "kajalponnu-lgtm",
+     password = "YOUR_PERSONAL_ACCESS_TOKEN"  # replace with your PAT
+ )
> 
> # 3️⃣ Check current branch (usually 'main')
> current_branch <- repository_head(repo)$name
> print(current_branch)
[1] "main"
> 
> # 4️⃣ Push to remote 'origin'
> push(repo, name = "origin", refspec = paste0("refs/heads/", current_branch), credentials = cred)
Error in push(repo, name = "origin", refspec = paste0("refs/heads/", current_branch),  : 
  Error in 'git2r_push': too many redirects or authentication replays

> library(usethis)
> 
> # Open your project
> proj_path <- "C:/Users/User/Documents/R script/datacleaning"
> setwd(proj_path)
> 
> # Push to GitHub
> git_push()
No remote set for this branch, using default remote 'origin'
Looking up https credentials for https://github.com/kajalponnu-lgtm/datacleaning.git
Error in libgit2::git_remote_push : SSL error: unknown error

> library(usethis)
> 
> # Open your project
> proj_path <- "C:/Users/User/Documents/R script/datacleaning"
> setwd(proj_path)
> 
> # Push to GitHub
> git_push()
No remote set for this branch, using default remote 'origin'
Looking up https credentials for https://github.com/kajalponnu-lgtm/datacleaning.git
Transferred 21 of 21 objects...done!
[status] refs/heads/main: unchanged
[new]     80d46363245f3710f894 refs/remotes/origin/main
> 
> 