# Aplicaciones de Datos Funcionales

Table of Contents
=================

* [Introducción](#introducción)
* [Definiciones](#definiciones)
* [Método de Suavizamiento](#método-de-suavizamiento)
* [Modelos de Regresión](#modelos-de-regresión)
* [Riesgo País](#riesgo-país)
* [Predicción de la Precipitación en Ecuador](#predicción-de-la-precipitación-en-ecuador)
* [Bibliografía](#bibliografía)

## Introducción

Los desarrollos tecnológicos han hecho posible que los investigadores de muchas áreas dispongan de grandes volúmenes de información contenidos en curvas, superficies o datos pertenecientes a un espacio continuo, por lo que, existe un gran interés por el aprendizaje y desarrollo de herramientas estadísticas para su análisis. En el análisis multivariante, la situación se torna compleja cuando se tratan variables correlacionadas por la gran cantidad de información que poseen, como suele ocurrir con las series temporales o con los procesos estocásticos en tiempo continuo en donde los datos no son numerables. 


El Análisis de Datos Funcionales \textit{(FDA)} resulta ser una herramienta estadística de alto alcance, novedosa y cuyo objetivo es simplificar el estudio de variables que dependen de parámetros continuos gracias a que los datos se representan a través de curvas o en general de funciones; pues el análisis multivariante es insuficiente al procesar datos estructurados como funciones. Generalmente el \textit{FDA} tiene una ventaja significativa que es la reducción apreciable de la dimensión que tienen este tipo de variables mediante un método de suavizamiento; ya que los datos observados inicialmente tienen una naturaleza discreta  y se desea utilizar un método para suavizar los datos iniciales con el fin de obtener una función como unidad básica de información \cite{CarrilloRamirez2017}.

En este libro se proveerá la introducción y la aplicación del Análisis de Datos Funcionales, desde cómo construir la estructura funcional de la información hasta la implementación de modelos estadísticos de regresión bajo el enfoque funcional; afortunadamente, la mayoría de las técnicas que se estudiarán en el presente libro tienen su tratamiento informático, siendo el software estadístico \texttt{R} uno de los más utilizados para la aplicación del \textit{FDA}, en particular, gracias a  las librerías \texttt{fda} y \texttt{fda.usc}. 

## Definiciones

**Variable Aleatoria Funcional**
Se dice que una variable  \(\{\mathcal{X}(t)\}_{t\in[0,T]}\) definida sobre un espacio de probabilidad $(\Omega,A,P)$ es una variable funcional si toma valores en un espacio infinito dimensional (espacio funcional), es decir, un espacio normado o semi-normado completo. 

A una observación de \(\chi_i\) de \(\mathcal{X}(t)\), se le llama dato funcional.

Sea \(\Omega\) un subconjunto compacto y medible de \(\mathbb{R}\) con medida de Lebesgue positiva. Se define el espacio de Hilbert \(\{\mathcal{L}^2[T]:\; T=[0,T]\in \Omega\}\) que es el espacio de funciones cuadrado integrable sobre el intervalo real \(T\) y se determina por:

\[\mathcal{L}^2[T]=\left\{f:T\rightarrow\mathbb{R}:\int_T f ^2(t)dt < \infty \right\}  \]

cuyo producto escalar usual esta definido por: 

\begin{equation*}
    \langle f,g \rangle = \int_T f(t)g(t)dt \hspace{0.4cm} \forall f,g \in \mathcal{L}^2(T)
\end{equation*}

Este producto es el equivalente al producto interno de vectores en $\mathbb{R}^n$, 


La notación del producto interno se enfatiza para considerar las conexiones y similitudes entre el contexto multivariante y funcional, pues siempre hay que tomar en cuenta la naturaleza de los datos, ya sean vectores o funciones; por lo cual, lo importante es cómo se define el producto interno para que se adapte a los tipos de datos. En el Apéndice \ref{apen:ProdInner}, se presentan conceptos básicos sobre el {\it producto interno}.

A lo largo del contenido se presentará la sintaxis en el programa \texttt{RStudio}, en particular, utilizando las librerías {\tt fda} \citep{Ramsay2009} y {\tt fda.usc} \citep{FebreroBande2012}, las cuales se complementan muy bien para desarrollar varias técnicas. Para empezar es la función \texttt{fdata} convierte un objeto de datos de clase: {\tt fd, fds, fts, sfts} o  formatos de tipo {\tt vector, matrix, data.frame} en un objeto de clase \texttt{fdata}.


### Métodos de Suavizamiento

El suavizamiento es el método por el cual se identifica cada observación de alta dimensión con una función. El objetivo de este método es construir los datos funcionales a partir de sus observaciones discretas. 

Por lo general, en la práctica se encuentran datos donde cada observación está dada por un vector real cuya dimensión es muy alta, como ejemplo, el precio de una acción por hora de todo el año ya que no se puede almacenar la información continua del precio. Es decir, es complicado contar con un conjunto de funciones de manera continua en el tiempo. 

Por lo cual, el problema es que en vez de contar con observaciones continuas \(\{\chi_1,..., \chi_n\}\), se  empezará trabajando con observaciones discretas \(\{x_1(t_{i0}),...,x_n(t_{im_{i}})\}\) donde \(t_{ik} \in [0,T]\) es el momento \(1\leq i\leq n\;  \text{y} \; 1\leq k\leq m_i\). Es decir, se contará con observaciones de tales funciones en diferentes momentos del tiempo \(\{t_{i0}...,t_{im_i}\}\)  y con un distinto número de observaciones para cada individuo. De este modo, se considera que la muestra está determinada por los vectores \(x_i=(x_i(t_{i0}),...,x_i(t_{im_{i}}))'\) donde \(x_{ik}\) es el valor observado de la trayectoria muestral en el instante \(t_{ik}\).

Frente a ello, se propone reconstruir la forma funcional de las trayectorias muestrales  \(\mathcal{S}_n=\{\chi_i\}_{i=1}^n\) que describen el comportamiento de los datos discretos a partir de una representación en términos de bases funcionales. Estas funciones recuperadas son consideradas datos funcionales. 

Existen diferentes tipos de bases:

-  Fourier

![01Fourier](https://user-images.githubusercontent.com/51028737/190863634-eaeb5b6f-3549-4ba6-8e72-fbcd3036070d.png)

-  B splines

![02Bspline](https://user-images.githubusercontent.com/51028737/190863666-3c4f92c9-2a90-4b37-8e63-dc8f03215b69.png)

-  Wavelets
-  Polinomiales y Expnenciales

## Modelos de Regresión

En general los modelos de regresión se utilizan para estudiar la relación entre una o más covariables explicativas (independientes) y una variable de respuesta (dependiente). En este trabajo, se tiene la finalidad de modelar una variable de respuesta escalar a partir de covariables explicativas funcionales.

Por estas razones, se analizará teóricamente los Modelo de Regresión Lineal Funcional \textit{(FLR)} con Respuesta Escalar tales como: el modelo \textit{FLR} con Representación en Bases detallado en \cite{Ramsay2009}, el modelo \textit{FLR} con Base por Componentes Principales  Funcionales \textit{(FPC)} propuesto por \cite{Cardot2006}, el modelo \textit{FLR} con Base por Mínimos Cuadrados Parciales Funcionales \textit{(FPLS)} planteado en \cite{Preda2005} y la adaptación del modelo \textit{FLR} con Representación en Bases usando dos covariables funcionales. 


## Riego Pais

## Predicción de la Precipitación


Para esta aplicación se analizan las variables meteorológicas con la finalidad de estimar la Precipitación en los cantones con más influyentes en la productividad agrícola del maíz en Ecuador mediante modelos \textit{FLR} de tal manera se pueda evaluar, validar y seleccionar el mejor ajuste. En este sentido, la precipitación, la temperatura y la velocidad del viento son las variables meteorológicas seleccionadas que tienen influencia directa en la productividad del cultivo y fundamentales para determinar el balance hídrico \citep{FAO1977}.


Para ello, se han tomado datos meteorológicos recolectados  desde la página de la \href{https://power.larc.nasa.gov/data-access-viewer/}{\underline{NASA}} obteniendo información diaria durante \(10\) años desde \((01/1/2010)\) hasta  \((31/12/2020)\) las cuales se muestran las variables meteorológicas descritas:

![tres_final](https://user-images.githubusercontent.com/51028737/190863581-2ce8c916-52f2-4302-a3fe-51b4a25273f8.png)


