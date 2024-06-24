setwd("C:/Users/Vero/Desktop/Machine Final")

# install.packages("h2o")
library(h2o)
h2o.init()

data <- h2o.importFile("datos_finales.csv")
y <- "deposit"
x <- setdiff(names(data), y)

# Parámetros de AutoML con validación cruzada repetida
aml <- h2o.automl(x = x, y = y,
                  training_frame = data,
                  nfolds = 5,  # Número de folds para la validación cruzada
                  keep_cross_validation_predictions = TRUE,
                  keep_cross_validation_models = TRUE,
                  max_runtime_secs = 3600, 
                  max_models = 15,  
                  seed = 1234,  
                  sort_metric = "AUC",  # Métrica para evaluar los modelos
                  project_name = "automl_model") 

# Detalles y resumen del mejor modelo
best_model <- aml@leader
summary(best_model)

# -----------------------------------
# Información adicional sobre los modelos y variables

# Lista de todos los modelos generados por AutoML y la importancia de las variables
models <- aml@leaderboard$model_id
models_list <- as.list(models)

variable_importance <- list()

for (model_id in models_list) {
  model <- h2o.getModel(model_id)
  var_imp <- h2o.varimp(model)
  variable_importance[[model_id]] <- var_imp
}
print(variable_importance)

# Modelos que ha utilizado en el ensamblado
best_ensemble_model <- aml@leader
base_models <- best_ensemble_model@model$base_models
print(base_models)

# Para ver los modelos de forma individual
# Lista de modelos generados por AutoML
models <- aml@leaderboard$model_id
models_list <- as.list(models)

# Cada modelo individualmente
evaluation_results <- list()

for (model_id in models_list) {
  model <- h2o.getModel(model_id)
  performance <- h2o.performance(model, newdata = data)
  evaluation_results[[model_id]] <- performance
}

# Métricas de rendimiento para cada modelo
for (model_id in names(evaluation_results)) {
  cat("\nModel ID:", model_id)
  print(evaluation_results[[model_id]])
}


# Detener el servidor H2O
h2o.shutdown()