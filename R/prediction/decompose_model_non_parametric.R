library(tidyverse)
library(glue)

#' @model a glm to be decomposed
#' @var the variable to be decomposed over (unquoted)
#' @outcome the outcome variable (unquoted)
#' @data any new data to be provided, defaults to null
#' @link the link function if necessary
#' @leave_as_binary whether to undo the labeling

decompose_model_non_parametric <- function(model, var, outcome, data, link = identity, leave_var_as_binary = TRUE, ...) {
  
  model_data <- data
  var0 <- rlang::as_name(rlang::enquo(var))
  outcome0 <- rlang::as_name(rlang::enquo(outcome))
  levels <- levels(model_data[[var0]])
  var1 <- glue("{var0}{levels[2]}")
  
  pred_homo <- link(predict(model, data %>% mutate({{var}} := levels[1])))
  
  pred_hetro <- link(predict(model, data %>% mutate({{var}} := levels[2])))
  
  out <- cbind.data.frame(
    "homo" = pred_homo,
    "hetro" = pred_hetro - pred_homo,
    var1 = data[[var0]]
  ) %>%
    rename({{var}} := var1)
  
  if (leave_var_as_binary) {
    out[[var0]] <- as.numeric(out[[var0]]) - 1
  }
  
  structure(
    list(
      "decomposed_model" = out,
      "data" = model_data,
      "var" = var0,
      "outcome" = outcome0
    ),
    class = "decompose_model"
  )
}