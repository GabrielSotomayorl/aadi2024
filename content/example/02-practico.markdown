---
title: "2. Visualización de datos y correlación"
linktitle: "2. Visualización de datos y correlación"
date: "2024-08-19"
menu:
  example:
    parent: Ejemplos
    weight: 2
type: docs
toc: true
editor_options:
  chunk_output_type: console
---
<script src="/rmarkdown-libs/kePrint/kePrint.js"></script>
<link href="/rmarkdown-libs/lightable/lightable.css" rel="stylesheet" />
<script src="/rmarkdown-libs/kePrint/kePrint.js"></script>
<link href="/rmarkdown-libs/lightable/lightable.css" rel="stylesheet" />

# 0. Objetivo del práctico

El objetivo de este práctico es introducir el uso de ggplot para la construcción de visualizaciones de datos en el contexto de análisis bivariado, así como la obtención de estadísiticos descriptivos y de correlación con R.

Para esto usaremos datos comunales de registros administrativos y del Censo sobre salarios, educación y ruralidad para analizar las diferencias territoriales de la brecha salarial de género. Los datos de registros administrativos en diversos ámbitos pueden encontrarse en el siguiente enlace <https://observatorio.ministeriodesarrollosocial.gob.cl/rraa-2023>


```r
pacman::p_load(dplyr, ggplot2)

datos <- readRDS(url("https://github.com/GabrielSotomayorl/aadi2024/raw/main/content/example/input/data/datos.rds"))  %>% 
  select(comuna,ing_prom_hombre, ing_prom_mujer,promedio_anios_escolaridad25_2017, prop_rural_2020, )
```

Trabajaremos con las siguientes variables:

1.  Comuna
2.  Ingreso promedio de los hombres ocupados formales (RRAA MDSF)
3.  Ingreso promedio de las mujeres ocupados formales (RRAA MDSF)
4.  Promedio de años de escolaridad de la población mayor de 25 años hasta el año 2017 (Censo 2017).
5.  Porcentaje de población rural por comuna (Proyecciones de población INE).

# Cálculo de la brecha salarial

Utilizaremos la brecha salarial media de cada comuna como la variable de interés en nuestro análisis. Esta variable es crucial para entender las disparidades de ingresos entre hombres y mujeres a nivel comunal. La brecha salarial media se calcula utilizando la siguiente fórmula:

$$  
\frac{\text{Salario Promedio Hombres} - \text{Salario Promedio Mujeres}}{\text{Salario Promedio Hombres}} \times 100
$$

Este cálculo nos da un porcentaje que representa cuánto menos ganan, en promedio, las mujeres en comparación con los hombres en una comuna específica. Un valor positivo indica que, en promedio, los hombres ganan más que las mujeres, mientras que un valor negativo indicaría lo contrario (aunque en la práctica, lo común es encontrar valores positivos).

Por ejemplo si en una comuna el salario promedio de los hombres es de \$400.000 y el de las mujeres es 300.000

$$  
\text{Brecha Salarial de Género} = \frac{100.000}{400.000} \times 100 = 0.25 \times 100 = 25\%
$$

Esto significa que, en esta comuna, las mujeres ganan en promedio un 25% menos que los hombres.

Ahora calcularemos esta variable para cada una de las comunas en nuestra base de datos integrando la fórmula en el cálculo de una nueva variable con mutate.


```r
datos <- datos %>% 
  mutate(brecha = (ing_prom_hombre - ing_prom_mujer)/ing_prom_hombre*100)
```

# Exploración de datos y presentación de tablas

Para obtener una visión general completa de los datos, podemos utilizar la función dfSummary del paquete summarytools. Esta función genera un resumen detallado de cada variable en la base de datos, con estad´siticos descritpivos y una visualición simple.


```r
# Instalar y cargar summarytools si no está ya instalado
#install.packages("summarytools")
library(summarytools)

# Crear el resumen de los datos y renderizarlo en HTML
resumen <- dfSummary(datos)
print(resumen, headings = FALSE)
```

```
## 
## -------------------------------------------------------------------------------------------------------------------------------------------
## No   Variable                            Stats / Values                    Freqs (% of Valid)    Graph                 Valid      Missing  
## ---- ----------------------------------- --------------------------------- --------------------- --------------------- ---------- ---------
## 1    comuna                              1. aisen                            1 ( 0.3%)                                 326        0        
##      [character]                         2. algarrobo                        1 ( 0.3%)                                 (100.0%)   (0.0%)   
##                                          3. alhue                            1 ( 0.3%)                                                     
##                                          4. alto biobio                      1 ( 0.3%)                                                     
##                                          5. alto del carmen                  1 ( 0.3%)                                                     
##                                          6. alto hospicio                    1 ( 0.3%)                                                     
##                                          7. ancud                            1 ( 0.3%)                                                     
##                                          8. andacollo                        1 ( 0.3%)                                                     
##                                          9. angol                            1 ( 0.3%)                                                     
##                                          10. antofagasta                     1 ( 0.3%)                                                     
##                                          [ 316 others ]                    316 (96.9%)           IIIIIIIIIIIIIIIIIII                       
## 
## 2    ing_prom_hombre                     Mean (sd) : 845870.9 (244374.3)   326 distinct values   . :                   326        0        
##      [numeric]                           min < med < max:                                        : :                   (100.0%)   (0.0%)   
##                                          542650.4 < 788061.3 < 2545608                           : :                                       
##                                          IQR (CV) : 236214.6 (0.3)                               : : :                                     
##                                                                                                  : : : .                                   
## 
## 3    ing_prom_mujer                      Mean (sd) : 737962.5 (172516.7)   326 distinct values     :                   326        0        
##      [numeric]                           min < med < max:                                          :                   (100.0%)   (0.0%)   
##                                          450713.6 < 698137.8 < 1907486                             :                                       
##                                          IQR (CV) : 144336.5 (0.2)                                 : .                                     
##                                                                                                  : : :                                     
## 
## 4    promedio_anios_escolaridad25_2017   Mean (sd) : 9.8 (1.5)             326 distinct values       :                 326        0        
##      [numeric]                           min < med < max:                                          : : .               (100.0%)   (0.0%)   
##                                          6.8 < 9.5 < 15.6                                          : : : :                                 
##                                          IQR (CV) : 2 (0.2)                                        : : : : :                               
##                                                                                                  . : : : : : .                             
## 
## 5    prop_rural_2020                     Mean (sd) : 32.9 (26.5)           286 distinct values   :                     326        0        
##      [numeric]                           min < med < max:                                        :                     (100.0%)   (0.0%)   
##                                          0 < 29.4 < 100                                          : . . .                                   
##                                          IQR (CV) : 41.9 (0.8)                                   : : : : : : .                             
##                                                                                                  : : : : : : : .   :                       
## 
## 6    brecha                              Mean (sd) : 11.2 (10.4)           326 distinct values               :         326        0        
##      [numeric]                           min < med < max:                                                    :         (100.0%)   (0.0%)   
##                                          -54.1 < 10.8 < 44.1                                               . : :                           
##                                          IQR (CV) : 12.2 (0.9)                                             : : :                           
##                                                                                                          . : : : :                         
## -------------------------------------------------------------------------------------------------------------------------------------------
```

Para presentar estadísticas descriptivas de las variables numéricas de manera clara y mejorada visualmente, puedes utilizar kable junto con kableExtra.


```r
# Instalar y cargar knitr y kableExtra si no están ya instalados
# install.packages("knitr")
# install.packages("kableExtra")
library(knitr)
library(kableExtra)

# Calcular estadísticas descriptivas
descriptivas <- datos %>%
  summarise(
    Minimo = min(brecha, na.rm = TRUE),
    Q1 = quantile(brecha, 0.25, na.rm = TRUE),
    Media = mean(brecha, na.rm = TRUE),
    Mediana = median(brecha, na.rm = TRUE),
    Q3 = quantile(brecha, 0.75, na.rm = TRUE),
    Maximo = max(brecha, na.rm = TRUE),
    DesviacionEstandar = sd(brecha, na.rm = TRUE)
  )

# Mostrar la tabla con kable y kableExtra
descriptivas %>%
  kable("html", caption = "Estadísticas Descriptivas de la Brecha Salarial") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption><span id="tab:unnamed-chunk-4"></span>Table 1: Estadísticas Descriptivas de la Brecha Salarial</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> Minimo </th>
   <th style="text-align:right;"> Q1 </th>
   <th style="text-align:right;"> Media </th>
   <th style="text-align:right;"> Mediana </th>
   <th style="text-align:right;"> Q3 </th>
   <th style="text-align:right;"> Maximo </th>
   <th style="text-align:right;"> DesviacionEstandar </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> -54.10985 </td>
   <td style="text-align:right;"> 5.196901 </td>
   <td style="text-align:right;"> 11.22107 </td>
   <td style="text-align:right;"> 10.82206 </td>
   <td style="text-align:right;"> 17.44238 </td>
   <td style="text-align:right;"> 44.14797 </td>
   <td style="text-align:right;"> 10.44345 </td>
  </tr>
</tbody>
</table>

Ahora podemos ver como se agrupan las comunas en distintos niveles de brecha salarial. Para crear una tabla de frecuencias basada en la variable brecha, primero necesitamos recodificar la variable de manera categórica.


```r
datos <- datos %>%
  mutate(brecha_recodificada = cut(brecha, breaks = c(-12.8, 0, 12, 24, 36.5),
                                   labels = c("(-12.8, 0]", "(0, 12]", "(12, 24]", "(24, 36.5]")))

# Generar tabla de frecuencias
frecuencias <- datos %>%
  group_by(brecha_recodificada) %>%
  summarise(Frecuencia = n()) %>%
  mutate(Porcentaje = round((Frecuencia / sum(Frecuencia)) * 100, 2))

# Mostrar la tabla con kable y kableExtra
frecuencias %>%
  kable("html", caption = "Tabla de Frecuencias de la Brecha Salarial Recodificada") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) 
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption><span id="tab:unnamed-chunk-5"></span>Table 2: Tabla de Frecuencias de la Brecha Salarial Recodificada</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> brecha_recodificada </th>
   <th style="text-align:right;"> Frecuencia </th>
   <th style="text-align:right;"> Porcentaje </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (-12.8, 0] </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 10.74 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (0, 12] </td>
   <td style="text-align:right;"> 147 </td>
   <td style="text-align:right;"> 45.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (12, 24] </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 31.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (24, 36.5] </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 11.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.92 </td>
  </tr>
</tbody>
</table>

# Construcción de gráficos con ggplot2

La principal herramienta de visualización de datos en R es el paquete ggplot2, que forma parte de tidyverse. ggplot2 implementa la gramática de los gráficos, un sistema coherente para describir y construir gráficos.  
Su versatilidad y capacidad de obtener resultados visualmente atractivos lo hacen más pertinente para tareas de presentación de resultados, tanto a públicos especializados como no especializados.  
Veremos los elementos básicos para poder hacer uso del paquete más adelante en el contexto de las técnicas estadísticas a ver en el curso.

| **Capa**     | **Descripción** |
|--------------|-----------------|
| **Datos**    | Conjunto de información que se representará de manera gráfica. En nuestro caso se trata de una o más variables, o una base de datos. |
| **Estética** | Escala en la cual se posicionará la información de interés. Refiere al posicionamiento de la información a representar sobre los diferentes ejes y dimensiones del gráfico resultante. Hablamos del posicionamiento de variables en los ejes X e Y como también de la posibilidad de indicar variables que pueden ser posicionadas como color de relleno dentro de los diferentes ejes, como una función de transparencia, etc. |
| **Geometría**| Formas, elementos visuales, que se emplearán para representar visualmente la información ya consignada en los **datos** y ubicada en las diferentes posiciones del gráfico mencionadas en la **estética**. Cada especificación de geometría permite visualizar diferentes características de la(s) variable(s) y su distribución. |

En primer lugar tenemos que seleccionar un conjunto de datos sobre el que trabajaremos. Podemos hacer esto de manera integrada con el flujo de trabajo del pipeline que vimos para dplyr. 


```r
#install.packages("ggplot2")
library(ggplot2)

datos %>% 
  ggplot() 
```

<img src="/example/02-practico_files/figure-html/unnamed-chunk-6-1.png" width="672" />

El resultado de entregar solo una base de datos a ggplot será un gráfico vació, sin información en sus ejes, ni representaciones gráficas de los datos. El siguiente paso es definir la estética o aesthetics, mediante la función aes(), en la cual indicaremos cuales son las variables que constituirán los ejes de nuestro gráfico. En este caso construiremos un gráfico con una sola variable, por lo que pondremos la edad en el eje x. 

Cada capa adicional que vayamos agregando a nuestro gráfico en ggplot 2 debe agregarse mediante un "+". No confundir con el trabajo con el pipeline. 


```r
datos %>% 
  ggplot() + 
  aes(x = brecha)
```

<img src="/example/02-practico_files/figure-html/unnamed-chunk-7-1.png" width="672" />

Por último, necesitamos decirle a ggplot como representar nuestros datos en el eje señalado mediante un geom, es decir, el objeto geométrico que se utilizará para representar los datos. Para esto hay múltiple funciones que empiezan con "geom_", como geom_bar, geom_point o geom_line. En este caso queremos contruir un histograma así que usaremos geom_histogram. 


```r
datos %>% 
  ggplot() + 
  aes(x = brecha) +
  geom_histogram()
```

<img src="/example/02-practico_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Ya tenemos una visualización básica. Ahora introduciremos algunos elementos adicionales para mejorar la apariencia de nuestro gráfico.

- labs(): Se añaden un título, un subtítulo y una nota al pie (caption) para proporcionar contexto y citar la fuente de los datos. Esto mejora la comprensión del gráfico por parte del espectador.

theme_minimal(): Se aplica un tema minimalista al gráfico para un aspecto limpio y moderno.

theme(): Se personalizan varios elementos del gráfico, incluyendo el estilo del título, subtítulo, nota al pie, y el tamaño del texto de los ejes y las etiquetas de los ejes. Esto mejora la legibilidad y la presentación visual del gráfico.

- labs(): Se añaden un título, un subtítulo y una nota al pie (caption) para proporcionar contexto y citar la fuente de los datos. Esto mejora la comprensión del gráfico por parte del espectador.

- theme_minimal(): Se aplica un tema minimalista al gráfico para un aspecto limpio y moderno.

- theme(): Se personalizan varios elementos del gráfico, incluyendo el estilo del título, subtítulo, nota al pie, y el tamaño del texto de los ejes y las etiquetas de los ejes. Esto mejora la legibilidad y la presentación visual del gráfico.


```r
ggplot(datos, aes(x = brecha)) +
  geom_histogram(
    binwidth = 5,                    # Ajusta el ancho de las barras
    fill = "#42affa",                # Color de relleno de las barras
    color = "white",                 # Color de borde de las barras
    alpha = 0.8                      # Transparencia de las barras
  ) +
  geom_vline(
    aes(xintercept = mean(datos$brecha)),
    color = "red",                   # Color de la línea
    linetype = "dashed",             # Tipo de línea (discontinua)
    size = 1                         # Grosor de la línea
  ) +
  labs(
    x = "Brecha Salarial de Género (%)",
    y = "Frecuencia",
    title = "Distribución de la Brecha Salarial de Género por Comuna"
  )
```

<img src="/example/02-practico_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Ahora veamos el ejemplo de un gráfico bivariado. para esto debemos fijar una varaible en cada eje para las aesthetics. En este caso veremos la relación entre años de escolaridad y ingresos del trabajo. Para esto usaremos un scatterplot, construido a partir de geom_point().
Filtraremos la base de datos a la Región de Valparaíso para reducir la cantidad de datos a graficar, y limitaremos los datos hasta 5 millones de peso para evitar valores muy extremos que dificulten la visualización.  


```r
datos %>% 
ggplot(aes(x = brecha, y = promedio_anios_escolaridad25_2017)) +
  geom_point(color = "#0073C2", size = 3, alpha = 0.7)
```

<img src="/example/02-practico_files/figure-html/unnamed-chunk-10-1.png" width="672" />

Como vemos el gráfico tiene la escolaridad en el eje x, y los ingresos en el eje y. sin embargo, dada la gran cantidad de puntos, la relación entre ambas variables no resulta tan clara. Para mejorar lo anterior haremos dos cambios, aplicaremos una transparencia a cada punto apra poder observar la densidad de los puntos en cada parte del gráfico, y añadiremos una linea de tendencia lineal con la función geom_smooth().



Ahora podemos observar de mejor manera que hay una relación positiva entre ambas variables, es decir que, a más escolaridad, en promedio los ingresos del trabajo son más altos. 



Por último agregamos las etiquetas del gráfico y otros elementos estéticos para mejorar su presentación.


# Estadística descriptiva y bivariada
