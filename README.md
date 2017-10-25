# Encuesta Investigadores de Posgrado 2016

Este directorio contiene datos sobre la Primera Encuesta a Investigadores de Posgrado, realizada por ANIP entre Diciembre de 2016 y Enero de 2017.

## Qué contiene este repositorio

El directorio **docs/** contiene documentos que describen el trabajo realizado:

- [AstudilloOlivos2016.pdf](docs/AstudilloOlivos2016.pdf): Memoria de título de Cristian Olivos y Claudio Astudillo, del año 2016. Este trabajo realiza un primer análisis de los datos recogidos de la encuesta.
- [Informe1eraEncuestaInsercion-2017.pdf](docs/Informe1eraEncuestaInsercion-2017.pdf): Informe de la primera encuesta, publicada en el sitio de ANIP y repetido aquí sólo por completitud.

El directorio **reporte/** contiene un reporte de la encuesta en [LaTeX](https://www.latex-project.org/) (en proceso de compleción a octubre/2017). Para compilar el informe:

	% pdflatex reporte-2016.tex

Lo anterior generará el archivo **reporte-2016.pdf**.

El directorio **analisis/** contiene código en [R](https://www.r-project.org/) que permite leer los datos contenidos en **analisis/data/** y generar tablas y gráficos para el reporte anterior.

Para generar las tablas y gráficos (asumiendo un ambiente con R 3.4.0 o superior instalado, con la librería 'xtable'):

	% cd analisis
	% R --no-save < analisis.r
