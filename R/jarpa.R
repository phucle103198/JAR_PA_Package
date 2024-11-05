library(FactoMineR)
library(SensoMineR)
library(dplyr)
library(tidyr)
library(readxl)
library(knitr)
library(FactoMineR)
library(ggplot2)
library(MASS)
library(corrplot)
library(factoextra)
library(ExPosition)
library(rstatix)
library(ggpubr)
library(car)
library(carData)
library(fmsb)
library(stringr)

JAR_Phuc <- function(data, col.l = NA, percent_consum = 20, meandrop_line=1){
    name_col_liking <- colnames(data)[col.l]
    df <- data[,-col.l]
    for (j in c(1:ncol(df))) df[,j] <- as.factor(df[,j])
    for (j in c(1:ncol(df))) levels(df[,j]) <- c("ne","ne","JAR","tm","tm")
    df2 <- cbind(df,data[,col.l])
    colnames(df2)[ncol(df2)] <- name_col_liking

    count_unique <- function(column) {
        df <- data.frame(value = column) %>%
            count(value) %>%
            mutate(percentage = n / sum(n) * 100)
        return(df)
    }
    df_percent <- map_dfr(names(df),
                          ~ count_unique(df[[.x]]) %>%
                              mutate(column = .x),
                          .id = "column_id")
    plot1 <- ggplot(df_percent, aes(x = column, y = percentage, fill = as.factor(value))) +
        geom_bar(stat = "identity") +
        labs(x = "Thuộc tính", y = "Percentage (%)",
             fill = "Mức", title="Phần trăm các mức JAR cho từng thuộc tính") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        geom_text(aes(label = n),
                  position = position_stack(vjust = 0.5),
                  size = 4, color = "black")
    plot(plot1)
    colnames_d <- colnames(df)
    df1 <- {}
    for (i in colnames_d) {
        res.aov <- aov(as.formula(paste(paste(name_col_liking), " ~ ", paste(i))), data = df2)
        a <- as.data.frame(tukey_hsd(res.aov))
        df1 <- rbind(df1,a)
    }

    df1$w1 <- paste(df1$group1, df1$group2)
    df1$w1 <- gsub("ne JAR", "ne",df1$w1)
    df1$w1 <- gsub("ne tm", "ne_tm",df1$w1)
    df1$w1 <- gsub("JAR tm", "tm",df1$w1)
    df1$w2 <- paste(df1$term, df1$w1, sep = "_")

    df_percent$w2 <- paste(df_percent$column, df_percent$value,sep = "_")
    cbind(df_percent,df1)

    df2 <- merge(df_percent, df1, by.x="w2", by.y="w2")
    df2$estimate2 <- abs(df2$estimate)

    df3 <- df2 %>%
        mutate(
            color = case_when(
                percentage > percent_consum & p.adj < 0.05 ~ "red",
                percentage > percent_consum & p.adj >= 0.05 ~ "green",
                TRUE ~ "gray"
            )
        )

    plot2 <- ggplot(df3, aes(x = w2, y = estimate2, fill = color)) +
        geom_bar(stat = "identity") +
        scale_fill_identity() +
        labs(x = "Thuộc tính", y = "Mean Drop") +
        ggtitle("Biểu đồ Mean Drop cho từng thuộc tính cảm quan") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        geom_text(aes(label = round(estimate2,2)),
                  position = position_stack(vjust = 0.5),
                  size = 3, color = "black")
    plot(plot2)
    df4 <- df3 %>% select(w2, estimate2, percentage)
    df4$category  <- str_sub(df4$w2, -2)

    plot3 <- ggplot(df4, aes(x = percentage, y = estimate2, color = category, label = w2)) +
        geom_point(aes(shape = category), size = 4) +
        geom_text(nudge_y = 0.1, nudge_x = 0.5, size = 4) +
        scale_color_manual(values = c("ne" = "blue", "tm" = "red")) +
        geom_vline(xintercept = percent_consum, linetype = "dashed", color = "black") +
        geom_hline(yintercept = meandrop_line, linetype = "dashed", color = "black") +
        labs(x = "%", y = "Mean drops", title = "Mean drops vs %") +
        theme_minimal() +
        scale_y_continuous(limits = c(0, max(df4$estimate2)+1), expand = c(0,0)) +
        scale_x_continuous(limits = c(0, max(df4$percentage)+10), expand = c(0,0))
    plot(plot3)
    results <- list()
    results$percent <- df_percent
    results$aov <- df2
    return(results)
}
