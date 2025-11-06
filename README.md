# 🧠 K-Means Clustering — Comparativa Multilenguaje (Iris Dataset)

Este proyecto implementa el algoritmo **K-Means** desde cero en **Python** y **R**, compara sus resultados con las versiones disponibles en **scikit-learn** y **kmeans()**.  
Forma parte de una serie de prácticas orientadas al estudio comparativo de algoritmos de *Machine Learning* en distintos lenguajes de programación.

---

## 📊 Objetivos

- Implementar el algoritmo **K-Means** de forma manual en Python y R.  
- Aplicarlo al dataset clásico **Iris**.  
- Comparar los resultados con las versiones de librerías oficiales en Python y R.  
- Evaluar las métricas de **inercia** y **precisión** .
- Visualizar los resultados mediante reducción de dimensionalidad con **PCA**.  

## 📈 Resultados

| Implementación              | Inercia | Precisión aproximada |
|-----------------------------|----------|----------------------|
| Python (manual)             | 0.89     | 78.8% |
| Python (scikit-learn)       | 0.89     | 78.8% |
| R (manual)                  | 0.88     | 78.8% |
| R (`kmeans()`)              | 0.89     | 78.8% |

🧠 Conclusiones

La implementación manual reproduce de forma fiel los resultados de las librerías oficiales.

K-Means es un algoritmo eficiente y sencillo, aunque sensible a la inicialización de centroides.

La comparación entre lenguajes evidencia las diferencias sintácticas, pero no en el rendimiento final.

La visualización con PCA facilita la interpretación de los clústeres en el dataset Iris.
