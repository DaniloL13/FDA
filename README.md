# Aplicaciones de Datos Funcionales

Table of Contents
=================

* [Introducción](#introducción)
* [Definiciones](#definiciones)
* [Método de Suavizamiento](#método-de-suavizamiento)
* [Modelos de Regresión](#modelos-de-regresión)
* [Predicción de Riesgo País en Ecuador](#predicción-de-riesgo-país-en-Ecuador)
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

Uno de los indicadores catalogado como el termómetro de la salud de una economía es el {\it riesgo país}. De acuerdo a \cite{Lapitz} este indicador mide la perspectiva de los mercados internacionales frente al cumplimiento de las obligaciones del país, acarreando impactos políticos sociales y económicos, como por ejemplo el nivel de incertidumbre para otorgar un financiamiento al país. Por lo tanto, cuanto más bajo este indicador, los inversionistas  tendrán más confianza de invertir.

El riesgo país, visto de otra forma, determina la predisposición de un Estado frente a sus obligaciones, orientado a resguardar la rentabilidad de los inversionistas, \cite{Calahorrano}.

El \textit{EMBI}, introducido y calculado en 1990 por la consultora internacional JPMorgan Chase \& Co.\footnote{Empresa líder en servicios financieros para consumidores y pequeñas empresas, en banca de inversión, en gestión de activos, etc.}, se basa en la tendencia de la deuda externa de cada país. Por lo cual, mientras una baja certeza de que el país asumirá con sus obligaciones, más alto será el \textit{EMBI} del país y viceversa, \cite{Lazo}.

### Representación Funcional

Las puntuaciones diarias del indicador del riesgo país del Ecuador desde el año 2000 hasta el año 2020, se obtuvieron de la página oficial del Banco Central del Ecuador, \cite{BCE}. Toda esta información se la organizó en una base de datos con 365 filas (del 01 de enero al 31 de diciembre) y 21 columnas (del año 2000 al 2020).

![06DF](https://user-images.githubusercontent.com/51028737/190864482-81a8be86-abbd-4c6a-a83a-0c7afec6475d.png)

### Resultados de Predicción

Se muestra la curva pronosticada junto con bandas de confianza a un nivel del 90\%. La gráfica proporciona el comportamiento de las puntuaciones que podrían darse a lo largo del tiempo para este escenario, teniendo en cuenta que para ciertos periodos de tiempo puede alcanzar un valor mínimo de alrededor 500 pb y para otros periodos, alcanzar a casi los 1000 pb, pero no superarlo. 

#### Mejor Caso

![07BootMejor](https://user-images.githubusercontent.com/51028737/190864580-f4a66c81-e430-40fe-bfd6-a40e33c36d74.png)

### Peor Caso

![08BootPeor](https://user-images.githubusercontent.com/51028737/190864599-029ca1f3-7425-4cc5-a4af-82063463aba1.png)


## Predicción de la Precipitación


Para esta aplicación se analizan las variables meteorológicas con la finalidad de estimar la Precipitación en los cantones con más influyentes en la productividad agrícola del maíz en Ecuador mediante modelos \textit{FLR} de tal manera se pueda evaluar, validar y seleccionar el mejor ajuste. En este sentido, la precipitación, la temperatura y la velocidad del viento son las variables meteorológicas seleccionadas que tienen influencia directa en la productividad del cultivo y fundamentales para determinar el balance hídrico \citep{FAO1977}.


Para ello, se han tomado datos meteorológicos recolectados  desde la página de la \href{https://power.larc.nasa.gov/data-access-viewer/}{\underline{NASA}} obteniendo información diaria durante \(10\) años desde \((01/1/2010)\) hasta  \((31/12/2020)\) las cuales se muestran las variables meteorológicas descritas:

![tres_final](https://user-images.githubusercontent.com/51028737/190863581-2ce8c916-52f2-4302-a3fe-51b4a25273f8.png)

### Análisis Funcional para la Temperatura

El análisis se centra en el promedio diario de la Temperatura entre los años \(2010\) y \(2020\), de los \(25\) cantones de Ecuador estratégicamente ubicados por su alta productividad en maíz. La idea es utilizar la variable dada por \(365\) observaciones en \(25\) lugares para reconstruir la forma funcional de las muestras a partir de bases de Fourier. 

![temp_original_df](https://user-images.githubusercontent.com/51028737/190863899-2b1bf1dd-cfe7-4c03-84ba-474130fc7f81.png)

Ahora, el método bootstrap con \(1000\) remuestreos se refleja en la siguiente figura:

![BC_temp2](https://user-images.githubusercontent.com/51028737/190863964-7e36f6a2-21ec-42a8-a9f1-8e37334b3790.png)

Luego, los datos atípicos hallados en el caso de la Temperatura se ven a continuación:

![atipico_temp](https://user-images.githubusercontent.com/51028737/190863997-fc468d24-7322-49a6-9381-e62968af4b56.png)


Las funciones indicadas permiten determinar las curvas atípicas en la muestra. En la figura \eqref{atipicos_temp} se obtiene que las curvas atípicas son: la curva \(21\) correspondiente al cantón Loja y Loreto \((25)\). 

Las curvas encontradas intervienen en la estimación de los modelos, por lo que, se procederá a comparar los resultados obtenidos considerando y sin considerar dichas curvas para obtener el mejor ajuste en cada modelo.

### Análisis Funcional para la Velocidad del Viento

El análisis del promedio diario de la Velocidad del Viento entre los años \(2010\) y \(2020\) en los \(25\) cantones de Ecuador. 

![viento_original_df](https://user-images.githubusercontent.com/51028737/190864113-62a0b054-b756-4069-bd48-4c7b668a27b5.png)

El método bootstrap con \(1000\) remuestreos se muestra en la figura:

![BC_viento2](https://user-images.githubusercontent.com/51028737/190864142-efc77ff1-b292-42de-9fa0-589824428233.png)

Los datos atípicos se ve a continuación:

![atipicos_vientos](https://user-images.githubusercontent.com/51028737/190864169-5a37f5bc-dd4d-4424-b872-08f29db33e40.png)

Mediante la primera función se detecta cuatro curvas atípicas correspondientes a Manta \((7)\), Santa Elena \((23)\), Pedernales \((4)\) y Jipijapa \((9)\); mientras que la segunda función no encuentra datos atípicos. Por lo que, no se considera datos atípicos en este caso; sin embargo, existe una curva influyente correspondiente a Loja \((21)\).

### Resultados de la Regresión FLR

Una vez que la base de datos no contiene valores atípicos, se ajustan los modelos de regresión de \textit{FLR} presentados en el Capitulo \ref{Cap3}. Resumiendo, la respuesta escalar es la precipitación, mientras que las covariables explicativas funcionales son la velocidad del viento y la temperatura. El primer modelo ajustado es el que incluye solo una variable explicativa, la temperatura. El segundo modelo también es un modelo \textit{FLR} con una covariable, en este caso se usa la velocidad del viento. Posteriormente, también se ajusta el modelo que considera ambas variables funcionales, temperatura y velocidad del viento, como variables explicativas. Esta extensión de los modelos tradicionales de regresión funcional, consistente en la introducción de más de una covariable (concretamente la temperatura y la velocidad del viento), es otra aportación del presente trabajo.

![regFLR](https://user-images.githubusercontent.com/51028737/190864388-3b3ffbdd-86f3-452a-8a84-03495a82f086.png)


