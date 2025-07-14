##### Requirements #####
library(psych)
library(ggthemes)
library(yardstick)
library(irr)  # Added for Cohen's Kappa
library(tidyverse)


# Set colors for visualizations
viz_colors <- c("#efefefff","#8db3beff","#4a68a0ff")


# Combine LLM ratings w/ human ratings into long format
process_all_ratings <- function(humans, llms) {
  # Process human ratings
  human_ratings <- humans %>% 
    # Select relevant columns including ground_truth
    select(domain, item, response_id, human_1, human_2, human_3, human_avg) 
  
  llms %>% 
    # left join with human ratings
    left_join(
      human_ratings,
      by = c("domain", "item", "response_id")
    ) %>%
    # Make factors
    mutate(model_size = factor(str_to_sentence(model_size),
                               levels = c("Human", "Small", "Large", "Frontier","New"),
                               labels = c("Human", "Small", "Large", "Frontier","New")),
           rubric_type = factor(rubric_type, 
                                levels = c("Empty", "Criteria Only", "Full"),
                                labels = c("Empty", "Criteria Only", "Full"))
    ) %>% 
    mutate(model_rubric = paste0(model_name,": ", rubric_type)) %>%
    relocate(-starts_with("llm"), -starts_with("human"))
}





calc_irr_by_domain <- function(data, type = "k_alpha") {
  data <- data %>%
    # Get LLM data
    select(domain, item, response_id, model_size, model_name, rubric_type, model_rubric, llm_1, llm_2, llm_3) %>%
    rename(R1 = llm_1,
           R2 = llm_2,
           R3 = llm_3) %>%
    # Add Human data
    bind_rows(data %>%
                distinct(domain, item, response_id, human_1, human_2, human_3) %>%
                rename(R1 = human_1,
                       R2 = human_2,
                       R3 = human_3) %>%
                # Add additional info for humans
                mutate(model_size = factor("Human",
                                             levels = c("Human", "Small", "Large", "Frontier","New"),
                                             labels = c("Human", "Small", "Large", "Frontier","New")),
                       rubric_type = factor("Full",
                                            levels = c("Empty", "Criteria Only", "Full"),
                                            labels = c("Empty", "Criteria Only", "Full")),
                       model_name = "Human",
                       model_rubric = "Human: Full"))
  
  # Overall alpha or kappa
  overall_stat <- data %>%
    group_by(model_rubric) %>%
    dplyr::summarize(
      stat = safe_calculate_irr(pick(R1, R2, R3), type = type),
      .groups = "drop"
    ) %>%
    rename("Overall" = stat)
  
  # Join with domain-specific alpha or kappa
  overall_stat %>%
    left_join(
      data %>%
        group_by(model_rubric, domain) %>%
        dplyr::summarize(
          stat = safe_calculate_irr(pick(R1, R2, R3), type = type),
          .groups = "drop"
        ) %>%
        pivot_wider(names_from = domain, values_from = stat),
      by = "model_rubric"
    ) %>%
    left_join(
      data %>%
        distinct(model_rubric, rubric_type, model_size, model_name),
      by = "model_rubric"
    ) %>%
    select(-model_rubric) %>%
    relocate(model_size, model_name, rubric_type) %>%
    arrange(model_size, tolower(model_name), rubric_type)
}


#' Calculate Krippendorff's alpha or Fleiss' kappa by item for each model
calc_irr_by_item <- function(data, type = "k_alpha") {
  data <- data %>%
    select(domain, item, response_id, model_size, model_name, rubric_type, model_rubric, llm_1, llm_2, llm_3) %>%
    rename(R1 = llm_1,
           R2 = llm_2,
           R3 = llm_3) %>%
    # Add Humans
    bind_rows(data %>%
                distinct(domain, item, response_id, human_1, human_2, human_3) %>%
                rename(R1 = human_1,
                       R2 = human_2,
                       R3 = human_3) %>%
                # Add additional info for humans
                mutate(model_size = factor("Human",
                                           levels = c("Human", "Small", "Large", "Frontier","New"),
                                           labels = c("Human", "Small", "Large", "Frontier","New")),
                       rubric_type = factor("Full",
                                            levels = c("Empty", "Criteria Only", "Full"),
                                            labels = c("Empty", "Criteria Only", "Full")),
                       model_name = "Human",
                       model_rubric = "Human: Full"))
  data %>%
    group_by(model_rubric, domain, item) %>%
    dplyr::summarize(
      stat = safe_calculate_irr(pick(R1, R2, R3), type = type),
      n_cases = n(),
      .groups = "drop"
    ) %>%
    left_join(
      data %>%
        distinct(model_size, model_rubric, model_name, rubric_type),
      by = "model_rubric"
    ) %>%
    select(-model_rubric) %>%
    relocate(model_size, model_name, rubric_type) %>%
    arrange(model_size, tolower(model_name), rubric_type)
}


##### Analysis Functions #####

### Krippendorfs Alpha (note that irr::kripp.alpha doesn't work)
calculate_kalpha <- function(data) {
  data <- as.matrix(data)
  categories <- unique(as.vector(data))
  categories <- categories[!is.na(categories)]
  k <- length(categories)
  
  coincidence_matrix <- matrix(0, k, k)
  n_pairs <- 0
  
  for (unit in 1:nrow(data)) {
    unit_values <- data[unit, !is.na(data[unit,])]
    n_values <- length(unit_values)
    
    if (n_values >= 2) {
      for (i in 1:(n_values - 1)) {
        for (j in (i + 1):n_values) {
          v1 <- match(unit_values[i], categories)
          v2 <- match(unit_values[j], categories)
          coincidence_matrix[v1, v2] <- coincidence_matrix[v1, v2] + 1
          coincidence_matrix[v2, v1] <- coincidence_matrix[v2, v1] + 1
          n_pairs <- n_pairs + 2
        }
      }
    }
  }
  
  observed_disagreement <- 1 - sum(diag(coincidence_matrix)) / n_pairs
  
  n_values <- rowSums(coincidence_matrix)
  expected_disagreement <- 1 - sum((n_values / sum(n_values))^2)
  
  alpha <- 1 - (observed_disagreement / expected_disagreement)
  
  # Return results
  return(list(
    alpha = alpha,
    observed_disagreement = observed_disagreement,
    expected_disagreement = expected_disagreement,
    coincidence_matrix = coincidence_matrix,
    n_pairs = n_pairs
  ))
}

### Fleiss' Kappa
calculate_fleiss_kappa <- function(data) {
  # Ensure data is a matrix
  data <- as.matrix(data)
  
  # Get dimensions
  n_subjects <- nrow(data)
  n_raters_per_subject <- rowSums(!is.na(data))
  
  # Identify all possible categories
  categories <- sort(unique(as.vector(data)))
  categories <- categories[!is.na(categories)]
  n_categories <- length(categories)
  
  # Create count matrix
  # Each row is a subject, each column is a category
  count_matrix <- matrix(0, n_subjects, n_categories)
  
  # Fill count matrix
  for (i in 1:n_subjects) {
    for (j in 1:n_categories) {
      count_matrix[i, j] <- sum(data[i, ] == categories[j], na.rm = TRUE)
    }
  }
  
  # Calculate observed agreement for each subject
  p_i <- numeric(n_subjects)
  for (i in 1:n_subjects) {
    n <- n_raters_per_subject[i]
    if (n >= 2) {
      sum_squares <- sum(count_matrix[i, ]^2)
      p_i[i] <- (sum_squares - n) / (n * (n - 1))
    } else {
      p_i[i] <- NA  # Not enough raters for this subject
    }
  }
  
  # Calculate mean observed agreement
  p_bar <- mean(p_i, na.rm = TRUE)
  
  # Calculate expected agreement
  n_total_ratings <- sum(n_raters_per_subject)
  p_j <- colSums(count_matrix) / n_total_ratings
  p_e <- sum(p_j^2)
  
  # Calculate Fleiss' Kappa
  kappa <- (p_bar - p_e) / (1 - p_e)
  
  # Return results
  return(list(
    kappa = kappa,
    observed_agreement = p_bar,
    expected_agreement = p_e,
    subject_agreement = p_i,
    category_proportions = p_j,
    count_matrix = count_matrix
  ))
}

### Cohen's Kappa
calculate_cohen_kappa <- function(truth, estimate) {
  # Convert factors to data frame for kappa2
  ratings <- data.frame(
    truth = truth,
    estimate = estimate
  )
  
  # Calculate kappa using irr::kappa2
  result <- tryCatch({
    kappa_result <- irr::kappa2(ratings)
    return(kappa_result$value)
  }, error = function(e) {
    return(NA)
  })
  
  return(result)
}

### Percent Agreement (raw agreement)
calculate_pairwise_agreement <- function(data) {
  n_raters <- ncol(data)
  agreements <- matrix(NA, n_raters, n_raters)
  
  for(i in 1:n_raters) {
    for(j in 1:n_raters) {
      if(i < j) {
        agreement <- mean(data[,i] == data[,j], na.rm = TRUE)
        agreements[i,j] <- agreement * 100
      }
    }
  }
  return(agreements)
}

### Combine multiple Interrater Agreement Calculations
interrater_stats <- function(data) {
  # Convert to matrix if not already
  data_matrix <- as.matrix(data)
  
  # Calculate various statistics
  results <- list(
    # Krippendorff's Alpha
    kripp_alpha = calculate_kalpha(data_matrix)$alpha,
    
    # Fleiss' Kappa
    fleiss_kappa = calculate_fleiss_kappa(data_matrix)$kappa,
    
    # ICC (type 1,1) - Random raters
    icc_11 = ICC(data_matrix)$results[1,2] %>% suppressWarnings(),
    
    # Average pairwise percent agreement
    pairwise_agreement = mean(calculate_pairwise_agreement(data_matrix)[upper.tri(matrix(NA, ncol(data_matrix), ncol(data_matrix)))], na.rm = TRUE)
  )
  
  # Return results as is (without renaming)
  results_df <- data.frame(
    Statistic = names(results),
    Value = unlist(results)
  )
  
  return(results_df)
}

#' Helper to safely calculate interrater stats
#' @param ratings Matrix of ratings
#' @param type Type of interrater reliability to calculate: "k_alpha" or "f_kappa"
safe_calculate_irr <- function(ratings, type = "k_alpha") {
  tryCatch(
    {
      if (type == "k_alpha") {
        ratings %>%
          as.matrix() %>%
          calculate_kalpha() %>%
          {.$alpha}
      } else if (type == "f_kappa") {
        ratings %>%
          as.matrix() %>%
          calculate_fleiss_kappa() %>%
          {.$kappa}
      } else {
        stop("Unknown IRR type: ", type)
      }
    },
    error = function(e) NA_real_
  )
}


#' Get detailed performance metrics
#' @param data Input data frame with target and pred columns
calc_perf_metrics <- function(data, by_domain = FALSE, by_item = FALSE) {
  # Input validation  
  if(!is.logical(by_domain) || !is.logical(by_item)) {
    stop("'by_domain' and 'by_item' parameters must be logical (TRUE or FALSE)")
  }
  
  # Create grouping based on parameters
  if(by_domain && by_item) {
    data <- data %>%
      group_by(model_rubric, domain, item)
  } else if(by_domain) {
    data <- data %>%
      group_by(model_rubric, domain) 
  } else if(by_item) {
    data <- data %>%
      group_by(model_rubric, item)
  } else {
    data <- data %>%
      group_by(model_rubric)
  }
  
  data <- data %>%
    rename(pred = llm_avg,
           target = human_avg)
  
  data %>%
    dplyr::summarize(
      n_cases = n(),
      mcc = mcc_vec(truth = factor(target, levels = c("0", "1")),
                    estimate = factor(pred, levels = c("0", "1"))),
      # Add Cohen's Kappa
      cohen_kappa = calculate_cohen_kappa(truth = factor(target, levels = c("0", "1")),
                                          estimate = factor(pred, levels = c("0", "1"))),
      true_negatives = sum(pred == "0" & target == "0"),
      false_positives = sum(pred == "1" & target == "0"),
      false_negatives = sum(pred == "0" & target == "1"),
      true_positives = sum(pred == "1" & target == "1"),
      total_cases = n(),
      fpr = false_positives / (false_positives + true_negatives),
      fnr = false_negatives / (false_negatives + true_positives),
      precision = true_positives / (true_positives + false_positives),
      recall = true_positives / (true_positives + false_negatives),
      f1_score = 2 * (precision * recall) / (precision + recall),
      .groups = "drop"
    ) %>%
    left_join(
      data %>%
        ungroup %>%
        distinct(model_size, model_rubric, model_name, rubric_type, .keep_all = F),
      by = "model_rubric"
    ) %>%
    select(-model_rubric) %>%
    relocate(model_size, model_name, rubric_type) %>%
    arrange(model_size, tolower(model_name), rubric_type)
}

format_tidy_t <- function(t_test) {
  tidy_df <- broom::tidy(t_test)
  est_names <- names(t_test$estimate) %>% gsub(".*group ","",.)
  test_name <- t_test$data.name
  
  return(tidy_df %>% 
           select(-method) %>%
           mutate(test = test_name) %>%
           relocate(test, alternative, estimate1, estimate2, estimate, statistic, parameter) %>%
           rename(!!est_names[1] := estimate1, 
                  !!est_names[2] := estimate2,
                  difference = estimate,
                  t = statistic, 
                  df = parameter,
                  t.test.type = alternative) %>%
           janitor::adorn_rounding(4))
}

##### Visualizations #####


plot_rubric_boxplot <- function(data, value_col = "stat", val_min = 0.4, 
                                val_max = 1, val_max_bar = NULL, title = NULL,
                                h_bar = NULL, y_label = NULL) {
  # Input validation
  required_cols <- c("rubric_type")
  if ("model_name" %in% names(data)) {
    required_cols <- c(required_cols, "model_name")
  }
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain columns: ", paste(required_cols, collapse = ", "))
  }
  if (!value_col %in% names(data)) {
    stop("Value column '", value_col, "' not found in data")
  }
  
  # Set default title and y-label
  if(is.null(title)) {
    plot_title <- paste("Distribution of", toupper(value_col), "Values by Rubric Type")
  } else {
    plot_title <- title
  }
  
  if(is.null(y_label)) {
    plot_y_label <- toupper(value_col)
  } else {
    plot_y_label <- y_label
  }
  
  # Extract human baseline value (if exists)
  human_value <- NULL
  if ("Human" %in% data$model_name) {
    human_data <- data %>% filter(model_name == "Human")
    if (nrow(human_data) > 0) {
      human_value <- mean(human_data[[value_col]])  # Take first value if multiple
    }
  }
  
  # Prepare data - aggregate across all models, items, and domains
  plot_data <- data %>%
    # Remove Human from the chart data
    filter(model_name != "Human") %>%
    # Ensure rubric_type is properly factored
    mutate(rubric_type = factor(rubric_type, levels = c("Empty", "Criteria Only", "Full")))
  
  # Color palette matching your original plot
  colors <- c(
    "Empty" = viz_colors[1],
    "Criteria Only" = viz_colors[2], 
    "Full" = viz_colors[3]
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = rubric_type, y = .data[[value_col]], fill = rubric_type)) +
    # Reference lines
    {if(!is.null(val_max_bar)) geom_hline(yintercept = val_max, color = "grey", linewidth = 0.6)} +
    {if(!is.null(h_bar)) geom_hline(yintercept = h_bar, color = "grey", linewidth = 0.8)} +
    {if(!is.null(human_value)) geom_hline(yintercept = human_value, color = "red", linewidth = 0.8, linetype = "dotted")} +
    {if(!is.null(human_value)) geom_line(aes(linetype = "Human"), color = "red", alpha = 0)} +
    
    # Box and whisker plots
    geom_boxplot(
      width = 0.6,
      outlier.shape = 21,
      outlier.fill = "white",
      outlier.color = "black",
      outlier.size = 1.5
    ) +
    
    # Add h_bar label if h_bar exists
    {if(!is.null(h_bar)) geom_label(data = data.frame(x = 1, y = h_bar, label = sprintf("%.2f", h_bar)),
                                    aes(x = x, y = y, label = label),
                                    inherit.aes = FALSE,
                                    hjust = 5,
                                    vjust = 0.5,
                                    size = 3.5,
                                    color = "black",
                                    # fill = alpha("white", 0.5),
                                    label.padding = unit(0.15, "lines"),
                                    label.r = unit(0.1, "lines"),
                                    label.size = 0)} +
    
    # Theme and styling
    theme_clean() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",  # Show legend for human line
      legend.box = "horizontal",
      legend.margin = margin(t = 5, b = 5, l = 5, r = 5),
      legend.spacing.x = unit(0.2, "cm"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 14, face = "bold"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    
    # Labels
    labs(
      title = plot_title,
      x = "Rubric Type",
      y = plot_y_label,
      fill = "Rubric",
      linetype = ""
    ) +
    
    # Scales
    scale_y_continuous(limits = c(val_min, val_max)) +
    scale_fill_manual(values = colors) +
    {if(!is.null(human_value)) scale_linetype_manual(
      values = c("Human" = "dotted"),
      labels = paste0("Human (", round(human_value, 3), ")"),
      guide = guide_legend(override.aes = list(color = "red", alpha = 1))
    )} +
    
    # Ensure proper x-axis scaling
    scale_x_discrete()
  
  return(p)
}



plot_items_barchart <- function(data, value_col = "stat", val_min = 0.4, 
                                val_max = 1, val_max_bar = NULL, title = NULL,
                                h_bar = NULL, y_label = NULL) {
  # Input validation
  required_cols <- c("model_name", "model_size", "rubric_type")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain columns: ", paste(required_cols, collapse = ", "))
  }
  if (!value_col %in% names(data)) {
    stop("Value column '", value_col, "' not found in data")
  }
  
  # Set default title and y-label
  if(is.null(title)) {
    plot_title <- paste("Distribution of", toupper(value_col), "Values by Model and Rubric Type")
  } else {
    plot_title <- title
  }
  
  if(is.null(y_label)) {
    plot_y_label <- toupper(value_col)
  } else {
    plot_y_label <- y_label
  }
  
  # Extract human baseline value (if exists)
  human_value <- NULL
  if ("Human" %in% data$model_name) {
    human_data <- data %>% filter(model_name == "Human")
    if (nrow(human_data) > 0) {
      human_value <- human_data[[value_col]][1]  # Take first value if multiple
    }
  }
  
  # Prepare data - ensure proper factor ordering and exclude Human
  plot_data <- data %>%
    # Remove Human from the chart data
    filter(model_name != "Human") %>%
    # Ensure model_size is a factor with proper ordering
    mutate(
      model_size = factor(model_size, levels = c("Small", "Large", "Frontier","New")),
      rubric_type = factor(rubric_type, levels = c("Empty", "Criteria Only", "Full"))
    ) %>%
    # Create ordered model factor for x-axis within each size group
    arrange(model_size, model_name) %>%
    group_by(model_size) %>%
    mutate(
      model_ordered = factor(model_name, levels = unique(model_name))
    ) %>%
    ungroup()
  
  # Color palette matching your original plot
  colors <- c(
    "Empty" = viz_colors[1],
    "Criteria Only" = viz_colors[2], 
    "Full" = viz_colors[3]
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = model_ordered, y = .data[[value_col]], fill = rubric_type)) +
    # Reference lines
    {if(!is.null(val_max_bar)) geom_hline(yintercept = val_max, color = "grey", linewidth = 0.6)} +
    {if(!is.null(h_bar)) geom_hline(yintercept = h_bar, color = "grey", linewidth = 0.8)} +
    {if(!is.null(human_value)) geom_hline(yintercept = human_value, color = "red", linewidth = 0.8, linetype = "dotted")} +
    {if(!is.null(human_value)) geom_line(aes(linetype = "Human"), color = "red", alpha = 0)} +
    
    
    # Bar chart
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7,
      color = "black",
      linewidth = 0.3
    ) +
    
    # Facet by model_size with variable widths
    facet_grid(~ model_size, scales = "free_x", space = "free_x") +
    
    # Theme and styling
    theme_clean() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 5, b = 5, l = 5, r = 5),
      legend.spacing.x = unit(0.2, "cm"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 14, face = "bold"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
    ) +
    
    # Labels
    labs(
      title = plot_title,
      x = "Model",
      y = plot_y_label,
      fill = "Rubric",
      linetype = ""
    ) +
    
    # Scales
    scale_y_continuous(limits = c(val_min, val_max)) +
    scale_fill_manual(values = colors) +
    {if(!is.null(human_value)) scale_linetype_manual(
      values = c("Human" = "dotted"),
      labels = paste0("Human (", round(human_value, 3), ")"),
      guide = guide_legend(override.aes = list(color = "red", alpha = 1))
    )} +
    
    # Free x-axis scaling within each facet
    scale_x_discrete() 
  
  return(p)
}


plot_items <- function(data, value_col, model_name = NULL, val_min = 0.4, 
                       val_max = 1, val_max_bar = NULL, h_bar = NULL, 
                       title = NULL, y_label = NULL) {
  # Input validation
  required_cols <- c("item", "domain", "model_size", "model_name","rubric_type")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain columns: ", paste(required_cols, collapse = ", "))
  }
  if (!value_col %in% names(data)) {
    stop("Value column '", value_col, "' not found in data")
  }
  
  if(is.null(title)) {
    plot_title <- paste("Comparison of", toupper(value_col), "Values by Domain and Item")
  } else {
    plot_title <- title
  }
  
  if(is.null(y_label)) {
    plot_y_label <- toupper(value_col)
  } else {
    plot_y_label <- y_label
  }
  
  # Create plot data with cleaned scorer names
  plot_data <- data %>%
    filter(model_name == model_name)
  
  # Predefined color and fill palettes adjusted for new inputs levels
  line_colors <- c(
    "Empty" = viz_colors[1],
    "Criteria Only" = viz_colors[2],
    "Full" = viz_colors[3]
  )
  
  fill_colors <- c(
    "Empty" = "#f7f5f4ff",
    "Criteria Only" = viz_colors[2],
    "Full" = viz_colors[3]
  )
  
  # Create linetype vector
  scorer_linetypes <- c(
    "Empty" = "solid",
    "Criteria Only" = "solid",
    "Full" = "solid"
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = item, y = .data[[value_col]], group = rubric_type)) +
    
    # Reference lines
    {if(!is.null(val_max_bar)) geom_hline(yintercept = val_max, color = "grey", linewidth = 0.6)} +
    {if(!is.null(h_bar)) geom_hline(yintercept = h_bar, color = "grey", linewidth = 0.8)} +
    
    # Lines and points
    geom_line(aes(color = rubric_type), linewidth = 1) +
    geom_point(aes(fill = rubric_type), color = "black", size = 2, shape = 21) +
    
    # Faceting
    facet_wrap(~domain, scales = "free_x", ncol = 1) +
    
    # Theme and styling
    theme_clean() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      strip.text = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin(t = 5, b = 5, l = 5, r = 5),
      legend.spacing.x = unit(0.2, "cm"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) +
    
    # Labels
    labs(
      title = plot_title,
      x = "Item",
      y = y_label,
      color = "Rubric",
      fill = "Rubric"
    ) +
    
    # Scales
    scale_y_continuous(limits = c(val_min, val_max)) +
    
    # Define aesthetics with specified colors
    scale_color_manual(values = line_colors) +
    scale_fill_manual(values = fill_colors)
  
  # Use scale_x_continuous with breaks function that handles different domains
  p <- p + scale_x_continuous(breaks = function(limits) {
    seq(from = floor(limits[1]), to = ceiling(limits[2]))
  }, labels = function(breaks) {
    breaks  # This will use the actual item values
  })
  
  # Add subtitle if model is provided
  if (!is.null(model_name)) {
    p <- p + labs(subtitle = model_name)
  }
  
  return(p)
}


plot_items_boxplot <- function(data, value_col = "stat", val_min = 0.4, 
                               val_max = 1, val_max_bar = NULL, title = NULL,
                               h_bar = NULL, y_label = NULL) {
  # Input validation
  required_cols <- c("item", "domain", "model_name", "rubric_type")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain columns: ", paste(required_cols, collapse = ", "))
  }
  if (!value_col %in% names(data)) {
    stop("Value column '", value_col, "' not found in data")
  }
  
  # Set default title and y-label
  if(is.null(title)) {
    plot_title <- paste("Distribution of", toupper(value_col), "Values by Domain and Item")
  } else {
    plot_title <- title
  }
  
  if(is.null(y_label)) {
    plot_y_label <- toupper(value_col)
  } else {
    plot_y_label <- y_label
  }
  
  # Prepare data - aggregate across all models for each item and rubric type
  plot_data <- data %>%
    # Ensure item is treated as factor for proper ordering
    mutate(item = as.factor(item),
           rubric_type = factor(rubric_type, levels = c("Empty", "Criteria Only", "Full")))
  
  # Color palette matching your original plot
  colors <- c(
    "Empty" = viz_colors[1],
    "Criteria Only" = viz_colors[2], 
    "Full" = viz_colors[3]
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = item, y = .data[[value_col]], fill = rubric_type)) +
    # Reference lines
    {if(!is.null(val_max_bar)) geom_hline(yintercept = val_max, color = "grey", linewidth = 0.6)} +
    {if(!is.null(h_bar)) geom_hline(yintercept = h_bar, color = "grey", linewidth = 0.8)} +
    
    # Box and whisker plots
    geom_boxplot(
      aes(fill = rubric_type),
      position = position_dodge(width = 0.8),
      width = 0.6,
      outlier.shape = 21,
      outlier.fill = "white",
      outlier.color = "black",
      outlier.size = 1.5
    ) +
    
    # Faceting by domain
    facet_wrap(~domain, scales = "free_x", ncol = 1) +
    
    # Theme and styling
    theme_clean() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      strip.text = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 5, b = 5, l = 5, r = 5),
      legend.spacing.x = unit(0.2, "cm"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 14, face = "bold"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    
    # Labels
    labs(
      title = plot_title,
      x = "Item",
      y = plot_y_label,
      fill = "Rubric"
    ) +
    
    # Scales
    scale_y_continuous(limits = c(val_min, val_max)) +
    scale_fill_manual(values = colors) +
    
    # Ensure proper x-axis scaling
    scale_x_discrete()
  
  return(p)
}


## Boxplots for combined FPR and FNR
plot_dual_error_rates <- function(data, domain_filter, val_min = 0, 
                                  val_max = 1, val_max_bar = NULL, title = NULL,
                                  h_bar = NULL, fpr_label = "False Positive Rate",
                                  fnr_label = "False Negative Rate") {
  # Input validation
  required_cols <- c("item", "domain", "model_name", "rubric_type", "fpr", "fnr")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Filter data for specified domain
  if (!domain_filter %in% unique(data$domain)) {
    stop("Domain '", domain_filter, "' not found in data")
  }
  
  filtered_data <- data %>%
    filter(domain == domain_filter)
  
  # Set default title
  if(is.null(title)) {
    plot_title <- paste("Error Rates by Item -", domain_filter)
  } else {
    plot_title <- title
  }
  
  # Prepare separate datasets for FPR and FNR
  fpr_data <- filtered_data %>%
    select(item, domain, model_name, rubric_type, fpr) %>%
    mutate(
      item = as.factor(item),
      rubric_type = factor(rubric_type, levels = c("Empty", "Criteria Only", "Full"))
    )
  
  fnr_data <- filtered_data %>%
    select(item, domain, model_name, rubric_type, fnr) %>%
    mutate(
      item = as.factor(item),
      rubric_type = factor(rubric_type, levels = c("Empty", "Criteria Only", "Full")),
      # Make FNR negative for bottom plot
      fnr = -fnr
    )
  
  # Color palette
  colors <- c(
    "Empty" = viz_colors[1],
    "Criteria Only" = viz_colors[2], 
    "Full" = viz_colors[3]
  )
  
  # Create the plot
  p <- ggplot() +
    # Reference lines
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +  # Central axis
    {if(!is.null(h_bar)) geom_hline(yintercept = h_bar, color = "grey", linewidth = 0.6)} +
    {if(!is.null(h_bar)) geom_hline(yintercept = -h_bar, color = "grey", linewidth = 0.6)} +
    
    # FPR boxplots (above x-axis)
    geom_boxplot(
      data = fpr_data,
      aes(x = item, y = fpr, fill = rubric_type),
      position = position_dodge(width = 0.8),
      width = 0.6,
      outlier.shape = 21,
      outlier.fill = "white",
      outlier.color = "black",
      outlier.size = 1.5
    ) +
    
    # FNR boxplots (below x-axis, negative values)
    geom_boxplot(
      data = fnr_data,
      aes(x = item, y = fnr, fill = rubric_type),
      position = position_dodge(width = 0.8),
      width = 0.6,
      outlier.shape = 21,
      outlier.fill = "white",
      outlier.color = "black",
      outlier.size = 1.5
    ) +
    
    # Theme and styling
    theme_clean() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 5, b = 5, l = 5, r = 5),
      legend.spacing.x = unit(0.2, "cm"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 14, face = "bold"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    
    # Labels
    labs(
      title = plot_title,
      x = "Item",
      y = "Error Rate",
      fill = "Rubric"
    ) +
    
    # Custom y-axis with labels for both FPR and FNR
    scale_y_continuous(
      limits = c(-val_max, val_max),
      breaks = c(-val_max, -val_max/2, -h_bar %||% -0.1, 0, h_bar %||% 0.1, val_max/2, val_max),
      labels = function(x) {
        ifelse(x < 0, abs(x), x)
      }
    ) +
    scale_fill_manual(values = colors) +
    scale_x_discrete() +
    
    # Add text annotations for FPR/FNR regions
    annotate("text", x = Inf, y = val_max * 0.8, label = fpr_label, 
             hjust = 1.1, vjust = 0.5, size = 4, fontface = "bold") +
    annotate("text", x = Inf, y = -val_max * 0.8, label = fnr_label, 
             hjust = 1.1, vjust = 0.5, size = 4, fontface = "bold")
  
  return(p)
}

##### Save #####
save_ggplots_list <- function(image_list, output_dir = "../output/visualizations/", 
                              width = 10, height = 6, dpi = 300, prefix = "") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Add underscore to prefix if it's not empty
  if (prefix != "" && !grepl("_$", prefix)) {
    prefix <- paste0(prefix, "_")
  }
  
  # Iterate through main list
  for (main_name in names(image_list)) {
    main_object <- image_list[[main_name]]
    
    # Check if it's directly a ggplot
    if (inherits(main_object, "ggplot")) {
      filename <- paste0(prefix, main_name, ".png")
      filepath <- file.path(output_dir, filename)
      ggsave(filepath, plot = main_object, device = "png", 
             width = width, height = height, dpi = dpi, units = "in")
      cat("Saved:", filename, "\n")
      
      # Check if it's a list (containing more images)
    } else if (is.list(main_object)) {
      # Iterate through the sublist
      for (sub_name in names(main_object)) {
        sub_object <- main_object[[sub_name]]
        
        # Only save if it's a ggplot
        if (inherits(sub_object, "ggplot")) {
          filename <- paste0(prefix, main_name, "_", sub_name, ".png")
          filepath <- file.path(output_dir, filename)
          ggsave(filepath, plot = sub_object, device = "png", 
                 width = width, height = height, dpi = dpi, units = "in")
          cat("Saved:", filename, "\n")
        }
      }
    }
  }
  
  cat("All images saved to:", output_dir, "\n")
}

save_table_list <- function(table_list, output_dir = "../output/tables/", prefix = "") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Add underscore to prefix if it's not empty
  if (prefix != "" && !grepl("_$", prefix)) {
    prefix <- paste0(prefix, "_")
  }
  
  # Iterate through main list
  for (main_name in names(table_list)) {
    main_object <- table_list[[main_name]]
    
    # Check if it's directly a data.frame or tibble
    if (inherits(main_object, c("data.frame", "tbl_df", "tbl"))) {
      filename <- paste0(prefix, main_name, ".csv")
      filepath <- file.path(output_dir, filename)
      write.csv(main_object, filepath, row.names = FALSE)
      cat("Saved:", filename, "\n")
      
      # Check if it's a list (containing more tables)
    } else if (is.list(main_object)) {
      # Iterate through the sublist
      for (sub_name in names(main_object)) {
        sub_object <- main_object[[sub_name]]
        
        # Only save if it's a data.frame or tibble
        if (inherits(sub_object, c("data.frame", "tbl_df", "tbl"))) {
          filename <- paste0(prefix, main_name, "_", sub_name, ".csv")
          filepath <- file.path(output_dir, filename)
          write.csv(sub_object, filepath, row.names = FALSE)
          cat("Saved:", filename, "\n")
        }
      }
    }
  }
  
  cat("All tables saved to:", output_dir, "\n")
}

save_results_list <- function(results_list, output_dir = "../output/results/", prefix = "") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Add underscore to prefix if it's not empty
  if (prefix != "" && !grepl("_$", prefix)) {
    prefix <- paste0(prefix, "_")
  }
  
  # Iterate through main list
  for (main_name in names(results_list)) {
    main_object <- results_list[[main_name]]
    
    # Check if it's directly a character vector
    if (is.character(main_object)) {
      filename <- paste0(prefix, main_name, ".txt")
      filepath <- file.path(output_dir, filename)
      writeLines(main_object, filepath)
      cat("Saved:", filename, "\n")
      
      # Check if it's a list (containing more text objects)
    } else if (is.list(main_object)) {
      # Iterate through the sublist
      for (sub_name in names(main_object)) {
        sub_object <- main_object[[sub_name]]
        
        # Only save if it's a character vector
        if (is.character(sub_object)) {
          filename <- paste0(prefix, main_name, "_", sub_name, ".txt")
          filepath <- file.path(output_dir, filename)
          writeLines(sub_object, filepath)
          cat("Saved:", filename, "\n")
        }
      }
    }
  }
  
  cat("All text files saved to:", output_dir, "\n")
}