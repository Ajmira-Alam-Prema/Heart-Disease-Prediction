library(dplyr)
library(rsample)
library(visdat)
library(ggplot2) 
library(dplyr)
library(ROSE)
library(readr)
library(caret)

data <- read_csv("C:/Users/laptop/Desktop/Introducing To Data Science/heart_disease_uci - modified.csv",show_col_types = FALSE)

data <- data %>% select(-dataset)

sapply(data, class)
sapply(data, typeof)
is.na(data)

colSums(is.na(data))
rowSums(is.na(data))

which(is.na(data$age))
which(is.na(data$ca))
which(is.na(data$thal))

get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data$age[is.na(data$age)] <- median(data$age, na.rm = TRUE)
data$ca[is.na(data$ca)] <- get_mode(data$ca)
data$thal[is.na(data$thal)] <- get_mode(data$thal)

colSums(is.na(data))

numeric_cols <- sapply(data, is.numeric)
categorical_cols <- names(data)[!numeric_cols]
data[categorical_cols] <- lapply(data[categorical_cols], as.factor)

cat("Numeric Columns:")
print(names(data)[numeric_cols])
cat("Categorical Columns:")
print(categorical_cols)

hist(data$age,
     col = "skyblue",       
     main = "Histogram of Age",  
     xlab = "Age",          
     ylab = "Frequency",    
     breaks = 5)

barplot(table(data$sex),
        main = "Count of Male and Female Patients",
        xlab = "Sex",
        ylab = "Count",
        col = c("skyblue", "pink"))

pie(table(data$cp),
    labels = c("Typical Angina", "Atypical Angina", "Non-Anginal", "Asymptomatic"),
    main = "Chest Pain Type Distribution",
    col = c("forestgreen", "gold", "dodgerblue", "tomato"))

hist(data$trestbps,
     main = "Histogram of Resting Blood Pressure",
     xlab = "Resting BP (mm Hg)",
     col = "skyblue",
     border = "white")

hist(data$chol,
     main = "Histogram of Cholesterol Levels",
     xlab = "Cholesterol (mg/dl)",
     col = "lightgreen",
     border = "white")

plot(data$chol,
     main = "Histogram of Cholesterol Levels",
     xlab = "Cholesterol (mg/dl)",
     ylab = "Frequency",
     col = "blue",
     pch = 20) 

pie(table(data$exang),
    labels = c("TRUE", "FALSE"),
    main = "Chest pain during exercise",
    col = c("skyblue", "pink"))

pie(table(data$fbs),
    labels = c("greater than 120 mg/dl", "less than 120 mg/dl"),
    main = "Fasting blood sugar",
    col = c("skyblue", "pink"))

ggplot(data, aes(x = factor(restecg))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Resting Electrocardiographic Results",
       x = "Resting Electrocardiogram",
       y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = thalch)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) 
  labs(title = "Distribution of Maximum Heart Rate Achieved",
     x = "Maximum Heart Rate Achieved",
     y = "Frequency") +
  theme_minimal()

  ggplot(data, aes(x = factor(exang))) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Exercise Induced Angina (exang)",
         x = "Exercise Induced Angina (exang)",
         y = "Frequency") +
    scale_x_discrete(labels = c("No Angina (0)", "Angina (1)")) 
  theme_minimal()

  ggplot(data, aes(x = oldpeak)) +
    geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) 
  labs(title = "Distribution of Depression Induced by Exercise (oldpeak)",
       x = "Oldpeak (Depression Induced by Exercise)",
       y = "Frequency") +
    theme_minimal()

  ggplot(data, aes(x = factor(slope))) +
    geom_boxplot(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Slope of Peak Exercise ST Segment (slope)",
         x = "Slope of ST Segment (slope)",
         y = "Frequency") +
    scale_x_discrete(labels = c("Downsloping (1)", "Flat (2)", "Upsloping (3)")) 
  theme_minimal()

  ggplot(data, aes(x = factor(slope))) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Slope of Peak Exercise ST Segment (slope)",
         x = "Slope of ST Segment (slope)",
         y = "Frequency") +
    scale_x_discrete(labels = c("Downsloping (1)", "Flat (2)", "Upsloping (3)")) 
  theme_minimal()

  ggplot(data, aes(x = factor(ca))) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Number of Major Vessels Colored by Fluoroscopy (ca)",
         x = "Number of Major Vessels (ca)",
         y = "Frequency") +
    scale_x_discrete(labels = c("0 Vessels", "1 Vessel", "2 Vessels", "3 Vessels")) 
  theme_minimal()

  ggplot(data, aes(x = factor(thal))) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Thalassemia (thal)",
         x = "Thalassemia (thal)",
         y = "Frequency") +
    scale_x_discrete(labels = c("Normal (3)", "Fixed Defect (6)", "Reversible Defect (7)")) 
  theme_minimal()
  
  ggplot(data, aes(x = factor(num))) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Heart Disease (num)",
         x = "Heart Disease (num)",
         y = "Frequency") +
    scale_x_discrete(labels = c("No Heart Disease (0)", "Heart Disease (1)")) 
  theme_minimal()
  
  ggplot(data, aes(x = "", y = age)) +
    geom_boxplot(fill = "skyblue", color = "black") +
    labs(title = "Boxplot for Age", y = "Age") +
    theme_minimal()
  
  ggplot(data, aes(x = "", y = trestbps)) +
    geom_boxplot(fill = "lightgreen", color = "black") +
    labs(title = "Boxplot for Resting Blood Pressure (trestbps)", y = "trestbps") +
    theme_minimal()

  ggplot(data, aes(x = "", y = chol)) +
    geom_boxplot(fill = "lightpink", color = "black") +
    labs(title = "Boxplot for Serum Cholesterol (chol)", y = "chol") +
    theme_minimal()
  
  ggplot(data, aes(x = "", y = thalch)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(title = "Boxplot for Maximum Heart Rate (thalach)", y = "thalach") +
    theme_minimal()
  
  ggplot(data, aes(x = "", y = oldpeak)) +
    geom_boxplot(fill = "orange", color = "black") +
    labs(title = "Boxplot for ST Depression (oldpeak)", y = "oldpeak") +
    theme_minimal()

  
  data$exang <- ifelse(data$exang == "TRUE", 1, 0)
  data$fbs <- ifelse(data$fbs == "TRUE", 1, 0)
  data$sex <- ifelse(data$sex == "Male", 1, 0)
  data$cp <- ifelse(data$cp == "asymptomatic", 0,
                    ifelse(data$cp == "atypical angina", 1,
                           ifelse(data$cp == "non-anginal", 2,
                                  ifelse(data$cp == "typical angina", 3, NA))))
  data$restecg <- ifelse(data$restecg == "lv hypertrophy", 1, 0)  
  data$slope <- ifelse(data$slope == "downsloping", 1,
                       ifelse(data$slope == "flat", 2,
                              ifelse(data$slope == "upsloping", 3, NA)))
  data$thal <- ifelse(data$thal == "normal", 1,
                      ifelse(data$thal == "fixed defect", 2,
                             ifelse(data$thal == "reversable defect", 3, NA)))
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  data[numeric_cols] <- lapply(data[numeric_cols], normalize)
  head(data[numeric_cols])
  summary(data[numeric_cols])
  
  dup_rows <- data[duplicated(data$id), ]
  cat("Number of duplicate rows found:", nrow(dup_rows), "\n")
  data <- data[!duplicated(data$id), ]
  cat("Number of rows after removing duplicates:", nrow(data), "\n")
  
  age_thresh <- quantile(data$age, .60)
  older_patients <- data %>% filter(age > age_thresh)
  male_angina <- data %>% filter(sex == 1, exang == 1)
  mid_chol <- data %>% filter(between(chol, 0.5, 0.8))
  sample_50 <- data %>% sample_n(50)
  
  head(older_patients)
  head(male_angina)
  head(mid_chol)
  head(sample_50)
  
  set.seed(123)
  data$num <- as.factor(data$num)
  balanced_data <- ROSE(num ~ ., data = data, seed = 123)$data
  cat("Original distribution of num:\n")
  print(table(data$num))
  cat("\nBalanced distribution of num:\n")
  print(table(balanced_data$num))
  
  data_split <- initial_split(balanced_data, prop = 0.8)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  cat("Training set dimensions:", dim(train_data)[1], "rows,", dim(train_data)[2], "columns\n")
  cat("Testing set dimensions:", dim(test_data)[1], "rows,", dim(test_data)[2], "columns\n")
  
  cat("\nClass distribution in training set:\n")
  print(table(train_data$num))
  cat("\nClass distribution in testing set:\n")
  print(table(test_data$num))

  data_original <- data
  numeric_vars <- c("age", "chol")
  for (var in numeric_vars) {
    x <- data_original[[var]]
    m <- mean(x, na.rm = TRUE)
    med <- median(x, na.rm = TRUE)
    mo <- get_mode(x)
    cat(sprintf("Variable %s:\n", var))
    cat(sprintf("  Mean   = %.2f\n", m))
    cat(sprintf("  Median = %.2f\n", med))
    cat(sprintf("  Mode   = %s\n\n", mo))
    
    # brief interpretation
    if (var == "age") {
      cat("  Interpretation: Patients are on average", round(m,1),
          "years old, with half younger and half older than", med,
          "and the most common age is", mo, "years.\n\n")
    } else if (var == "chol") {
      cat("  Interpretation: The average cholesterol level is", round(m,1),
          "mg/dl, median is", med,
          "mg/dl, and the most frequent value is", mo, "mg/dl.\n\n")
    }
  }
  
  sex_mo <- get_mode(data_original$sex)
  cat("Categorical Variable sex:\n")
  cat("  Mode =", sex_mo, "(", ifelse(sex_mo==1, "Male", "Female"), ")\n")
  cat("  Interpretation: Most patients are",
      ifelse(sex_mo==1, "male", "female"), ".\n\n")
  
  
  cp_labels <- c("asymptomatic", "atypical angina", "non-anginal", "typical angina")
  cp_mo <- get_mode(data_original$cp)
  cat("Categorical Variable cp (chest pain type):\n")
  cat("  Mode =", cp_mo, "(", cp_labels[cp_mo+1], ")\n")
  cat("  Interpretation: The most frequent chest pain type is",
      cp_labels[cp_mo+1], ".\n")

  spread_vars <- c("age", "thalch")
  
  for (var in spread_vars) {
    x <- data_original[[var]]
    
    range_val <- diff(range(x, na.rm = TRUE))
    iqr_val <- IQR(x, na.rm = TRUE)
    var_val <- var(x, na.rm = TRUE)
    sd_val <- sd(x, na.rm = TRUE)
    
    cat(sprintf("Spread Statistics for %s:\n", var))
    cat(sprintf("  Range               = %.2f\n", range_val))
    cat(sprintf("  Interquartile Range = %.2f\n", iqr_val))
    cat(sprintf("  Variance            = %.2f\n", var_val))
    cat(sprintf("  Standard Deviation  = %.2f\n\n", sd_val))
    
    if (var == "age") {
      cat("  Interpretation: The age of patients spans", range_val, "years,\n")
      cat("  with the middle 50% of patients within an age range of", iqr_val, "years.\n")
      cat("  The standard deviation of", round(sd_val, 1), "years indicates the typical\n")
      cat("  deviation from the mean age. A higher standard deviation suggests\n")
      cat("  greater variability in patient ages.\n\n")
    } else if (var == "thalch") {
      cat("  Interpretation: The maximum heart rate values span", range_val, "beats per minute,\n")
      cat("  with the middle 50% of values within a range of", iqr_val, "beats per minute.\n")
      cat("  The standard deviation of", round(sd_val, 1), "beats per minute indicates\n")
      cat("  the typical variation from the mean. This variability is important\n")
      cat("  clinically as both very high and very low maximum heart rates during\n")
      cat("  exercise can indicate cardiac issues.\n\n")
    }
  }
  
  
    
  
  