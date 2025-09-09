library(tidyverse)
library(MatchIt)
library(glue)

#' @model a model object
#' @outcome the outcome variable (unquoted)
#' @var the stratifying variable (unquoted)
#' @data the data frame
#' @method whether to use parametric or non-parametric decomposition of the model
#' @matching whether to use the decomposed model or all variables for matching
#' @seed seed for reproducibility

c4b <- function(model, outcome, var, data, method = c("parametric", "non-parametric"), matching = c("decompose", "mahalanobis"), seed = 123) {
  
  # Capture variable names using tidy evaluation
  outcome0 <- rlang::as_name(rlang::enquo(outcome))
  var0 <- rlang::as_name(rlang::enquo(var))
  
  method <- match.arg(method, c("parametric", "non-parametric"))
  matching <- match.arg(matching, c("decompose", "mahalanobis"))
  
  if (is.null(data) && method %in% c("non-parametric")) {
    stop("If method is non-parametric, data must be provided through the data argument")
  }
  
  if (method == "parametric") {
    dm <- decompose_model(model, {{var}}, newdata = data)
  } else if (method == "non-parametric") {
    dm <- decompose_model_non_parametric(model, {{var}}, {{outcome}}, data = data)
  }
  
  result_pre_match <- dm$decomposed_model %>%
    mutate(rn = row_number()) %>%
    cbind.data.frame(setNames(list(dm[["data"]][[outcome0]]), outcome0))
  
  if (matching == "mahalanobis") {
    result_pre_match <- result_pre_match %>% 
      cbind.data.frame(dm$data %>% 
                         dplyr::select(all_of(as.character(attr(terms(model), "variables"))[-1:-2])) %>% 
                         dplyr::select(-all_of(var0))
      )
  }
  
  set.seed(seed)
  
  # Find lower sample size across treatment groups
  min_size <- result_pre_match %>%
    group_by({{var}}) %>%
    summarise(n = n(), .groups = "drop") %>%
    pull(n) %>%
    min()
  
  # Balance dataset by sampling from the larger group
  result_balanced <- result_pre_match %>%
    group_by({{var}}) %>%
    sample_n(min_size, replace = FALSE) %>%
    ungroup() %>%
    arrange(rn)
  
  # Perform matching depending on method
  if (matching == "decompose") {
    match_formula <- as.formula(glue("{var0} ~ homo + hetro"))
    
  } else if (matching == "mahalanobis") {
    
    variables <- as.character(attr(terms(model), "variables"))[-1:-2]
    variables <- variables[variables != var0]
    match_formula <- as.formula(glue("{var0} ~ {paste(variables, collapse = '+')}"))
    
  }
  
  m.out <- matchit(
    formula = match_formula,
    data = result_balanced,
    method = "optimal",
    ratio = 1,
    replace = FALSE
  )
  matched_data <- match.data(m.out)
  
  result_pre_match %>%
    merge(matched_data %>% dplyr::select(rn, subclass), ., by = "rn") %>%
    mutate(mean_het = mean(hetro), .by = subclass) %>%
    dplyr::select(all_of(c(outcome0, var0)), subclass, mean_het) %>%
    pivot_wider(names_from = {{var}}, values_from = {{outcome}}) %>%
    mutate(diff = `1` - `0`) %>%
    lm(diff ~ mean_het, data = .) %>%
    survival::concordance() %>%
    return()
}