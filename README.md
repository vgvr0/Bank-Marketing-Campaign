# Análisis Predictivo de Depósitos Bancarios

Este proyecto utiliza técnicas de aprendizaje automático para predecir si un cliente contratará un depósito a plazo fijo en un banco, basándose en datos de campañas de marketing anteriores.

## Descripción

El código implementa varios modelos de aprendizaje automático, incluyendo regresión logística, árboles de decisión, random forest, gradient boosting y redes neuronales, para predecir la probabilidad de que un cliente contrate un depósito a plazo fijo. Se utilizan técnicas de validación cruzada y ensamblado para mejorar y comparar el rendimiento de los modelos.

## Características principales

- Preprocesamiento de datos, incluyendo estandarización y creación de variables dummy
- Selección de variables utilizando diferentes métodos (Boruta, RFE, MXM)
- Implementación de múltiples modelos de aprendizaje automático
- Validación cruzada para evaluar el rendimiento de los modelos
- Técnicas de ensamblado para combinar predicciones de diferentes modelos
- Visualizaciones para interpretar los resultados

## Modelos implementados

- Regresión Logística
- Árboles de Decisión
- Random Forest
- Gradient Boosting (XGBoost)
- Redes Neuronales
- Support Vector Machines (SVM)
- CatBoost

## Métricas de evaluación

- Tasa de error
- AUC (Área bajo la curva ROC)

## Requisitos

- R (versión recomendada: 4.0.0 o superior)
- Paquetes de R: caret, randomForest, xgboost, gbm, nnet, kernlab, catboost, ggplot2, dplyr, pROC, entre otros.

## Uso

1. Asegúrate de tener instalados todos los paquetes necesarios.
2. Carga los datos en el formato adecuado.
3. Ejecuta las secciones del código en orden para realizar el análisis completo.
4. Revisa los resultados y visualizaciones generadas.

## Estructura del proyecto

- `datos_finales.csv`: Archivo de datos (no incluido en el repositorio)
- `script_principal.R`: Código principal con todo el análisis
- Archivos de funciones auxiliares:
  - `funcion steprepetido binaria.R`
  - `cruzadas avnnet y log binaria.R`
  - `cruzada arbolbin.R`
  - `cruzada rf binaria.R`
  - `cruzada gbm binaria.R`
  - `cruzada xgboost binaria.R`
  - `cruzada catboost binaria.R`
  - `cruzada SVM binaria lineal.R`
  - `cruzada SVM binaria polinomial.R`
  - `cruzada SVM binaria RBF.R`

## Contribuciones

Las contribuciones son bienvenidas. Por favor, abre un issue para discutir los cambios propuestos antes de realizar un pull request.
