library(ggplot2)
library(readxl)

args_file <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = TRUE)
root <- normalizePath(file.path(dirname(args_file), ".."), mustWork = TRUE)
input <- file.path(root, "Processed data", "04 - TEWAMETER", "TEWL_processed_raw_window.xlsx")
output <- file.path(root, "Processed data", "04 - TEWAMETER", "TEWL_raw_window_mean_missing_heatmap_R.png")

df <- read_excel(input, sheet = "RawWindowSummaryClean")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

df$raw_window_tewl_mean_of_measurements_g_m2_h <- to_missing(df$raw_window_tewl_mean_of_measurements_g_m2_h)
df$subject <- as.character(df$subject)
df$group <- factor(df$group, levels = c("Control", "Ex", "ExAG"))
df$plot_stage <- as.character(df[[4]]) # Excel column D: stage

stage_order <- data.frame(plot_stage = unique(df$plot_stage), stringsAsFactors = FALSE)
stage_order$stage_id <- as.character(seq_len(nrow(stage_order)))
stage_order$stage_label <- stage_order$plot_stage

subjects <- do.call(
  rbind,
  lapply(levels(df$group), function(g) {
    data.frame(group = g, subject = sort(unique(df$subject[df$group == g])), stringsAsFactors = FALSE)
  })
)
subjects$subject_id <- ave(subjects$subject, subjects$group, FUN = seq_along)

grid <- merge(subjects, stage_order, by = NULL)
observed <- df[, c("group", "subject", "plot_stage", "raw_window_tewl_mean_of_measurements_g_m2_h", "Rik_score")]
names(observed) <- c("group", "subject", "plot_stage", "raw_window_tewl_mean_of_measurements_g_m2_h", "Rik_score")
observed$present_value <- !is.na(observed$raw_window_tewl_mean_of_measurements_g_m2_h)
present_lookup <- aggregate(
  present_value ~ group + subject + plot_stage,
  data = observed,
  FUN = any
)
score_lookup <- aggregate(
  Rik_score ~ group + subject + plot_stage,
  data = observed,
  FUN = function(x) {
    non_missing <- x[!is.na(x)]
    if (length(non_missing) == 0) NA else non_missing[1]
  },
  na.action = na.pass
)
observed <- merge(present_lookup, score_lookup, by = c("group", "subject", "plot_stage"), all = TRUE)
plot_df <- merge(grid, observed, by = c("group", "subject", "plot_stage"), all.x = TRUE)
plot_df$Rik_score <- suppressWarnings(as.numeric(plot_df$Rik_score))
plot_df$status <- ifelse(
  is.na(plot_df$present_value) | !plot_df$present_value,
  "No record",
  ifelse(
    !is.na(plot_df$Rik_score) & plot_df$Rik_score %in% c(1, 2),
    paste0("score ", as.integer(plot_df$Rik_score)),
    "Present"
  )
)
plot_df$status <- factor(
  plot_df$status,
  levels = c("No record", "Present", "score 1", "score 2")
)
plot_df$score_label <- ifelse(plot_df$Rik_score %in% c(1, 2), as.character(as.integer(plot_df$Rik_score)), NA)
plot_df$score_label <- factor(plot_df$score_label, levels = c("1", "2"))

missing_summary <- aggregate(
  missing ~ group,
  data = transform(plot_df, missing = status == "No record"),
  FUN = sum
)
total_summary <- aggregate(stage_id ~ group, data = plot_df, FUN = length)
names(total_summary)[2] <- "total"
group_labels <- merge(missing_summary, total_summary, by = "group")
group_labels$label <- paste0(group_labels$group, "\n", group_labels$missing, "/", group_labels$total)

plot_df$subject <- factor(plot_df$subject, levels = rev(unique(subjects$subject)))
plot_df$stage_id <- factor(plot_df$stage_id, levels = stage_order$stage_id)

p <- ggplot(plot_df, aes(x = stage_id, y = subject, fill = status)) +
  geom_tile(color = "white", linewidth = 0.35) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_discrete(labels = setNames(stage_order$stage_label, stage_order$stage_id)) +
  scale_fill_manual(
    values = c(
      "No record" = "#c9342f",
      "Present" = "#e8f3ff",
      "score 1" = "#fee08b",
      "score 2" = "#fdae61"
    ),
    drop = FALSE
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.text.y = element_text(size = 8),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 10),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.spacing.y = unit(0.6, "lines")
  )

ggsave(output, p, width = 9, height = 10.5, dpi = 300)
cat(output, "\n")
