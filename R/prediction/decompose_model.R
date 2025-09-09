library(tidyverse)
library(glue)

#' @model a glm to be decomposed
#' @var1 the variable to be decomposed over (unquoted)
#' @newdata any new data to be provided, defaults to null
#' @leave_as_binary whether to undo the labeling

decompose_model <- function(model, var, newdata = NULL, leave_var_as_binary = TRUE) {
  
  model_data <- model$model
  var0 <- rlang::as_name(rlang::enquo(var))
  levels <- levels(model_data[[var0]])
  var1 <- glue("{var0}{levels[2]}")
  
  coeffs <- coef(model)
  
  if (is.null(newdata)) {
    X <- model.matrix(model)
  } else {
    X <- model.matrix(model, data = newdata)
    model_data <- newdata
    X_names <- rownames(X)
    model_data <- model_data[rownames(model_data) %in% X_names, ]
  }
  coeff_names <- names(coeffs)
  
  # Identify components
  homo_terms <- coeff_names[!grepl(glue("{var1}:"), coeff_names) & coeff_names != var1]
  hetro_terms <- coeff_names[grepl(glue("{var1}:"), coeff_names)]
  
  # Calculate homo component (as before)
  homo_comp <- X[, homo_terms, drop = FALSE] %*% coeffs[homo_terms]
  
  # For hetro component: get the base terms without multiplying by var1
  # Extract the hetro coefficients and multiply by the base variables only
  hetro_comp <- rep(0, nrow(X))
  
  for (term in hetro_terms) {
    # Get the base variable name (remove "var1:" prefix)
    base_var <- gsub(glue("{var1}:"), "", term)
    # Multiply coefficient by base variable only (not by var1)
    hetro_comp <- hetro_comp + coeffs[term] * X[, base_var]
  }
  
  # Handle var1 main effect - add it to hetro component
  if (var1 %in% coeff_names) {
    hetro_comp <- hetro_comp + coeffs[var1]
  }
  
  # Extract the var1 data
  var_data <- X[, var1]
  
  out <- data.frame(
    homo = as.vector(homo_comp),
    hetro = as.vector(hetro_comp),
    var1 = as.vector(var_data)
  )
  colnames(out)[3] <- var0
  
  if (!leave_var_as_binary) {
    out[[var0]] <- ifelse(out[[var0]] == 1, levels[2], levels[1])
  }
  
  if (!is.null(attr(model$terms, "offset"))) {
    
    offset_name <- (model %>% formula %>% all.vars())[attr(model$terms, "offset")]
    
    if (is.null(newdata)) {
      offset <- model$offset
    } else {
      offset <- newdata[[offset_name]]
    }
    
    out[[offset_name]] <- offset
  }
  
  if (nrow(out) != nrow(model_data)) {
    stop("Row mismatch")
  }
  
  structure(
    list(
      "decomposed_model" = out,
      "data" = model_data,
      "var" = var0,
      "outcome" = as.character(model$terms)[2]
    ),
    class = "decompose_model"
  )
}
