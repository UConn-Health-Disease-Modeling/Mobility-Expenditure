# 获取所有业务类别
biz_categories <- unique(USDdata_with_totalBIZDIST$BIZ)

# 用于存储每个业务类别的新模型结果
all_years_results_AR1 <- list()
coefficients_list_AR1 <- list()

for (biz in biz_categories) {
  yearly_sums <- list()
  
  for (year in 2019:2022) {
    # 过滤每年的数据并按DIST汇总支出
    yearly_data <- USDdata_with_totalBIZDIST %>%
      filter(YEAR == year, BIZ == biz) %>%
      group_by(DIST) %>%
      summarise(sumexp = sum(EXPENDITURE, na.rm = TRUE))
    
    colname <- paste0(year, "sumexp")
    names(yearly_data)[2] <- colname
    
    yearly_sums[[as.character(year)]] <- yearly_data
  }
  
  # 合并所有年份的数据
  sumexp_biz <- reduce(yearly_sums, full_join, by = "DIST")
  
  all_years_coefficients_AR1 <- list()
  
  for (year in 2019:2022) {
    data_year_biz <- subset(USDdata_with_totalBIZDIST, BIZ == biz & YEAR == as.character(year))
    coefficients_biz_year_AR1 <- data.frame(DIST = character(), slope = numeric(), std_error = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
    
    unique_dists <- unique(data_year_biz$DIST)
    
    for (dist in unique_dists) {
      subset_data <- data_year_biz[data_year_biz$DIST == dist, ]
      
      # 创建一个唯一的时间变量 time_index
      subset_data <- subset_data %>%
        arrange(YEARMONTH) %>%
        mutate(time_index = 1:n())
      
      # 标准化数据
      subset_data$EXPENDITURE <- scale(subset_data$EXPENDITURE)
      subset_data$mobility <- scale(subset_data$mobility)
      
      # 使用 gls() 添加 AR(1) 结构，删除 YEARMONTH
      model_AR1 <- gls(EXPENDITURE ~ mobility, data = subset_data, correlation = corAR1(form = ~ time_index))
      
      # 提取模型的系数信息
      model_summary_AR1 <- summary(model_AR1)
      slope_info_AR1 <- model_summary_AR1$tTable["mobility", ]
      
      # 将系数信息保存到数据框中
      coefficients_biz_year_AR1 <- rbind(coefficients_biz_year_AR1, 
                                         data.frame(DIST = dist,
                                                    slope = slope_info_AR1["Value"],
                                                    std_error = slope_info_AR1["Std.Error"],
                                                    p_value = slope_info_AR1["p-value"]))
    }
    
    all_years_coefficients_AR1[[as.character(year)]] <- coefficients_biz_year_AR1
  }
  
  names_list <- c("2019", "2020", "2021", "2022")
  for (year in names_list) {
    names(all_years_coefficients_AR1[[year]])[2:4] <- paste0(names(all_years_coefficients_AR1[[year]])[2:4], year)
  }
  
  coefficients_biz_AR1 <- Reduce(function(x, y) merge(x, y, by = "DIST", all = TRUE), all_years_coefficients_AR1)
  
  biz_slope_characteristic_AR1 <- coefficients_biz_AR1 %>%
    left_join(sumexp_biz, by = "DIST") %>%
    left_join(district_info, by = "DIST")
  
  long_df_biz_AR1 <- pivot_longer(biz_slope_characteristic_AR1,
                                  cols = starts_with("slope") | starts_with("std_error") | starts_with("p_value"),
                                  names_to = c("Measure", "Year"),
                                  names_pattern = "(.*)(2019|2020|2021|2022)",
                                  values_to = "Value")
  
  long_df_biz_AR1 <- long_df_biz_AR1 %>%
    pivot_wider(names_from = Measure, values_from = Value)
  
  long_df_biz_AR1$BIZ <- biz
  all_years_results_AR1[[biz]] <- long_df_biz_AR1
  coefficients_list_AR1[[biz]] <- coefficients_biz_AR1  # 保存每个业务类别的新模型系数表
}

# 将 coefficients_list_AR1 保存为新变量
coefficients_travel_AR1 <- coefficients_list_AR1[["travel"]]
coefficients_restaurant_AR1 <- coefficients_list_AR1[["restaurant"]]
coefficients_shopping_AR1 <- coefficients_list_AR1[["shopping"]]
coefficients_medical_AR1 <- coefficients_list_AR1[["medical"]]
coefficients_clothing_AR1 <- coefficients_list_AR1[["clothing"]]
coefficients_entertainment_AR1 <- coefficients_list_AR1[["entertainment"]]
coefficients_education_AR1 <- coefficients_list_AR1[["education"]]
coefficients_homelife_AR1 <- coefficients_list_AR1[["homelife"]]
coefficients_allBIZ_AR1 <- coefficients_list_AR1[["allBIZ"]]




###################plot figure
coefficients_restaurant_AR1$category <- 'restaurant'
coefficients_entertainment_AR1$category <- 'entertainment'
coefficients_clothing_AR1$category <- 'clothing'
coefficients_medical_AR1$category <- 'medical'
coefficients_travel_AR1$category <- 'travel'
coefficients_education_AR1$category <- 'education'
coefficients_shopping_AR1$category <- 'shopping'
coefficients_homelife_AR1$category <- 'home appliances'
newdata_AR1 <- bind_rows(coefficients_restaurant_AR1, coefficients_entertainment_AR1,coefficients_clothing_AR1,
                     coefficients_medical_AR1, coefficients_travel_AR1, coefficients_education_AR1,
                     coefficients_shopping_AR1, coefficients_homelife_AR1)

newdata_AR1 <- newdata_AR1 %>% 
  gather(key = "year", value = "slope", slope2019, slope2020, slope2021, slope2022) %>% 
  mutate(year = sub("slope", "", year)) # 移除 'slope' 文字，只保留年份
newdata_AR1$category <- paste0(toupper(substr(newdata_AR1$category, 1, 1)), tolower(substr(newdata_AR1$category, 2, nchar(newdata_AR1$category))))
newdata_AR1$category <- factor(newdata_AR1$category, levels = c("Travel", "Restaurant", "Shopping", "Medical", "Clothing", "Entertainment", "Education", "Home appliances"))
ggplot(newdata_AR1, aes(x = year, y = slope, fill = category)) + 
  geom_boxplot() +
  facet_wrap(~category, scales = 'fixed', ncol = 8) +  # 使用DIST变量创建分面，并允许每个分面有自己的y轴范围
  #theme_minimal() +
  labs(title = "Elasticity by year and business sectors in all districts", 
       x = "Year", 
       y = "Slope: consumption elasticity",
       fill = "Category") +
  scale_fill_manual(values = custom_colors) +
  geom_hline(yintercept = 0, color = "red",linetype = "dashed", size = 1.3)+
  ylim(-1,1.5)+
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90"),  
        text = element_text(size = 20, face = "bold"), # Default text size and style for the entire plot
        axis.title = element_text(size = 20, face = "bold"), # Axis titles
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Title
        axis.text = element_text(size = 16, face = "bold"), # Axis text
        legend.title = element_text(size = 18, face = "bold"), # Legend title
        legend.text = element_text(size = 16, face = "bold"), # Legend text
        legend.position = "bottom", # 图例位置设置为底部
        legend.direction = "horizontal", # 图例横向排列
        legend.box = "horizontal",
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

figure4


####################save table
# Ensure the necessary library is loaded
library(openxlsx)

# Round slope and standard error to 2 decimal places for all coefficient tables
round_coefficients <- function(df) {
  df %>%
    mutate(across(starts_with("slope"), round, 2),
           across(starts_with("std_error"), round, 2))
}

# Apply rounding to all tables
coefficients_travel_AR1 <- round_coefficients(coefficients_travel_AR1)
coefficients_restaurant_AR1 <- round_coefficients(coefficients_restaurant_AR1)
coefficients_shopping_AR1 <- round_coefficients(coefficients_shopping_AR1)
coefficients_medical_AR1 <- round_coefficients(coefficients_medical_AR1)
coefficients_clothing_AR1 <- round_coefficients(coefficients_clothing_AR1)
coefficients_entertainment_AR1 <- round_coefficients(coefficients_entertainment_AR1)
coefficients_education_AR1 <- round_coefficients(coefficients_education_AR1)
coefficients_homelife_AR1 <- round_coefficients(coefficients_homelife_AR1)
coefficients_allBIZ_AR1 <- round_coefficients(coefficients_allBIZ_AR1)

# Combine all coefficient tables into one with slope, std_error, and p_value
combined_table <- coefficients_travel_AR1 %>%
  dplyr::select(DIST, slope2019, std_error2019, p_value2019, slope2020, std_error2020, p_value2020,
                slope2021, std_error2021, p_value2021, slope2022, std_error2022, p_value2022) %>%
  rename(travel_slope2019 = slope2019, travel_std_error2019 = std_error2019, travel_p_value2019 = p_value2019,
         travel_slope2020 = slope2020, travel_std_error2020 = std_error2020, travel_p_value2020 = p_value2020,
         travel_slope2021 = slope2021, travel_std_error2021 = std_error2021, travel_p_value2021 = p_value2021,
         travel_slope2022 = slope2022, travel_std_error2022 = std_error2022, travel_p_value2022 = p_value2022)

# Function to add new coefficient data with slope, std_error, and p_value
add_coefficient_data <- function(main_table, new_df, biz_prefix) {
  main_table %>%
    left_join(new_df %>%
                dplyr::select(DIST, starts_with("slope"), starts_with("std_error"), starts_with("p_value")) %>%
                rename_with(~paste0(biz_prefix, "_", .), starts_with("slope")) %>%
                rename_with(~paste0(biz_prefix, "_", .), starts_with("std_error")) %>%
                rename_with(~paste0(biz_prefix, "_", .), starts_with("p_value")),
              by = "DIST")
}

# Add each business category to the combined table with p_value columns
combined_table <- add_coefficient_data(combined_table, coefficients_restaurant_AR1, "restaurant")
combined_table <- add_coefficient_data(combined_table, coefficients_shopping_AR1, "shopping")
combined_table <- add_coefficient_data(combined_table, coefficients_medical_AR1, "medical")
combined_table <- add_coefficient_data(combined_table, coefficients_clothing_AR1, "clothing")
combined_table <- add_coefficient_data(combined_table, coefficients_entertainment_AR1, "entertainment")
combined_table <- add_coefficient_data(combined_table, coefficients_education_AR1, "education")
combined_table <- add_coefficient_data(combined_table, coefficients_homelife_AR1, "homelife")
combined_table <- add_coefficient_data(combined_table, coefficients_allBIZ_AR1, "allBIZ")

# 定义函数，用于根据 p 值添加显著性标记
add_significance <- function(slope, std_error, p_value) {
  paste0(
    round(slope, 2), " (", round(std_error, 2), ")",
    case_when(
      p_value < 0.05 ~ " **",  # p < 0.05 添加两个星号
      p_value < 0.1 ~ " *",    # p < 0.1 添加一个星号
      TRUE ~ ""                # p >= 0.1 不添加星号
    )
  )
}

# 定义业务类别和年份
biz_categories <- c("travel", "restaurant", "shopping", "medical", "clothing", 
                    "entertainment", "education", "homelife", "allBIZ")
years <- c("2019", "2020", "2021", "2022")

# 使用循环为每个业务类别和年份生成新的列，并将其添加到 combined_table 中
for (biz in biz_categories) {
  for (year in years) {
    slope_col <- paste0(biz, "_slope", year)
    std_error_col <- paste0(biz, "_std_error", year)
    p_col <- paste0(biz, "_p_value", year)
    
    # 创建新的列名，例如 'restaurant2019'
    new_col <- paste0(biz, year)
    
    # 添加带有显著性标记的列到 combined_table
    combined_table[[new_col]] <- add_significance(
      combined_table[[slope_col]],
      combined_table[[std_error_col]],
      combined_table[[p_col]]
    )
  }
}

# 删除不需要的 slope、std_error 和 p_value 列
columns_to_keep <- c("DIST", grep("^.+\\d{4}$", names(combined_table), value = TRUE))
combined_table <- combined_table[, columns_to_keep]

# 按照 Daytime_pop_percent 排序并保存表格
sorted_district_info <- district_info %>%
  filter(DIST != "total") %>%
  arrange(desc(Daytime_pop_percent))

sorted_districts <- c("total", sorted_district_info$DIST)
combined_table <- combined_table %>%
  mutate(DIST = factor(DIST, levels = sorted_districts)) %>%
  arrange(DIST)

# 将结果保存为 Excel 文件
write.xlsx(combined_table, file = "~/Desktop/coefficients_summary_AR1.xlsx")



########################Granger test
# 加载必要的包
library(lmtest)

# 准备结果存储
results <- data.frame(DIST = character(), BIZ = character(), YEAR = numeric(), p_value = numeric())

# 对每个地区DIST，每种商业类型BIZ和每一年YEAR进行Granger检验
for (dist in unique(USDdata_with_totalBIZDIST$DIST)) {
  for (biz in unique(USDdata_with_totalBIZDIST$BIZ)) {
    for (year in unique(USDdata_with_totalBIZDIST$YEAR)) {
      
      # 子集数据
      subset_data <- USDdata_with_totalBIZDIST[USDdata_with_totalBIZDIST$DIST == dist &
                                                 USDdata_with_totalBIZDIST$BIZ == biz &
                                                 USDdata_with_totalBIZDIST$YEAR == year, ]
      
      # 确保子集数据足够用于Granger检验
      if (nrow(subset_data) > 1) {
        
        # 进行Granger因果检验 (假设y是因变量，x是自变量)
        test_result <- grangertest(EXPENDITURE ~ mobility, order = 1, data = subset_data)
        
        # 提取p值并存储
        p_value <- test_result$`Pr(>F)`[2]
        results <- rbind(results, data.frame(DIST = dist, BIZ = biz, YEAR = year, p_value = p_value))
      }
    }
  }
}

# 使用BH方法调整p值
results$p_value_adjusted <- p.adjust(results$p_value, method = "BH")

# 查看调整后的结果
print(results)
sum(results$p_value_adjusted > 0.1)
sum(results$p_value > 0.1)
# 保存路径为桌面
saveRDS(results, file = "~/Desktop/granger_results.rds")
result <- readRDS("~/Desktop/granger_results.rds")


library(dplyr)
library(tidyr)
library(openxlsx)

# 假设 result 和 district_info 都已存在
# 按照 district_info$Daytime_pop_percent 降序排列 DIST，并确保 'total' 在第一个
sorted_DIST <- district_info %>%
  arrange(desc(Daytime_pop_percent)) %>%
  pull(DIST)
sorted_DIST <- c('total', sorted_DIST[sorted_DIST != 'total'])

# 将 result 数据表 reshape 成 wide 格式
result_wide <- result %>%
  mutate(BIZ_YEAR = paste(BIZ, YEAR, sep = "_")) %>%
  dplyr::select(DIST, BIZ_YEAR, p_value_adjusted) %>%
  spread(key = BIZ_YEAR, value = p_value_adjusted)

# 按照 sorted_DIST 的顺序排列 DIST
result_sorted <- result_wide %>%
  mutate(DIST = factor(DIST, levels = sorted_DIST)) %>%
  arrange(DIST)

# 保存到 Excel 文件
write.xlsx(result_sorted, file = file.path("~/Desktop", "result_sorted.xlsx"), rowNames = FALSE)
