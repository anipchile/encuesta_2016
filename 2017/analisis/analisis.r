#> -------------------------------------------------------------------------------------------
#> Proyecto: Primera Encuesta de Inserción de Doctores, ANIP 2016
#> Archivo: analisis.r
#> Autor: Cristian Bravo Lillo <cristian@bravolillo.xyz>
#
#> Este archivo lee los archivos data/personas.csv y data/trabajos.csv generados por cleaning.r
#> y genera tablas y gráficos en ../reporte/tablas/ y ../reporte/graficos/.
#> -------------------------------------------------------------------------------------------

library(xtable)

clio.root = './'
out.path = '../reporte/'
source('lib/functions.general.r')
source('lib/functions.graphs.r')

#> Leemos los datos
personas <- read.csv('data/personas.csv')
trabajos <- read.csv('data/trabajos.csv')

#> Al leer desde un CSV, se pierde toda la informacion relacionada con variables. Asi que tenemos que volver a crearla.
personas$ins.cuantostrabajos <- as.numeric(personas$ins.cuantostrabajos)
personas$ins.cuantostrabajos[ is.na(personas$ins.cuantostrabajos) ] <- 5 # Esta es la unica persona que respondio "5 o más de 5"
personas$ins.ingreso <- factor(personas$ins.ingreso,
															 levels=c(
															 	'Menos de 400.000',
															 	'Entre 400.001 y 700.000',
															 	'Entre 700.001 y 1.200.000',
															 	'Entre 1.200.001 y 2.000.000',
															 	'Entre 2.000.001 y 3.500.000',
															 	'Entre 3.500.001 y 6.000.000',
															 	'Más de 6.000.000'
															 ))

#> Creamos algunas variables necesarias para calculos intermedios
personas.enchile <- personas[
	personas$bio.resideenchile,
	c('Response.ID','bio.edad','bio.genero','bio.essostenedor', 'bio.cuantoshijos',
		'ins.ingreso','ins.tienetrabajo','ins.cuantostrabajos','ins.anhelo.academico','ins.prom.percepcion',
		'aporte.numarticulos','aporte.numlibros','aporte.numproyectos.gana',
		'sit.tiene.msc','sit.tiene.phd','sit.tiene.pos'
	)
]
row.names(personas.enchile) <- NULL

#> Tabla de doble entrada: número de personas con posgrado y/o postdoc
largetable <- as.matrix(
	ftable(
		personas[,c('sit.cursando.posgrado','sit.tiene.posgrado','sit.cursando.pos','sit.tiene.pos')],
		row.vars = c('sit.cursando.posgrado','sit.tiene.posgrado')
	)
)
rownames(largetable) <- c('No cursa posgrado, no tiene posgrado','No cursa posgrado, tiene posgrado',
													'Cursa posgrado, no tiene posgrado','cursa posgrado, tiene posgrado')
colnames(largetable) <- c('No cursa posdoc., no tiene posdoc.','No cursa posdoc., tiene posdoc.',
													'Cursa posdoc., no tiene posdoc.','Cursa posdoc., tiene posdoc.')
thistable <- xtable(cf.asPercentage(largetable, frequency = TRUE, keepfrequency = TRUE),
										caption='Total de encuestados, según si poseen posgrado y/o posdoctorado. Se entiende posgrado como máster o doctorado.',
										label='tab:count',
										align=c('p{3cm}|','p{2cm}|','p{2cm}|','p{2cm}|','p{2cm}'), digits=0)
print(thistable, file=paste(out.path,'tablas/tab-count1.tex', sep=''))

#> Tabla de doble entrada: número de mujeres/hombres con MsC y/o PhD
writeCountTable <- function(gender, thiscaption, thislabel, thisfile) {
	largetable <- as.matrix(
		ftable(
			list(
				personas$sit.tiene.msc[ personas$bio.genero==gender ],
				personas$sit.tiene.phd[ personas$bio.genero==gender ],
				personas$sit.tiene.pos[ personas$bio.genero==gender ]
			),
			row.vars = c(1,2)
		)
	)
#	largetable <- rbind(largetable, colSums(largetable))
#	largetable <- cbind(largetable, rowSums(largetable))
	rownames(largetable) <- c('No tiene máster ni doctorado', 'No tiene máster, sí tiene doct.',
														'Tiene máster, no tiene doct.', 'Tiene máster y doct.')#, 'Subtotal')
	colnames(largetable) <- c('No tiene posdoc.', 'Tiene posdoc.')#, 'Subtotal')
	thistable <- xtable(cf.asPercentage(largetable, frequency = TRUE, keepfrequency = TRUE),
											caption=thiscaption, label=thislabel, digits=0)
	print(thistable, file=paste(out.path, 'tablas/', thisfile, sep=''))
}

writeCountTable('Femenino',
								'Número de mujeres con máster, doctorado, y/o posdoctorado.',
								'tab:countwomen', 'tab-countwomen.tex')
writeCountTable('Masculino',
								'Número de hombres con máster, doctorado, y/o posdoctorado.',
								'tab:countmen', 'tab-countmen.tex')
rm(writeCountTable, largetable, thistable)


#> Analisis: Reg. Log. sobre personas$ins.tienetrabajo con variables sociodemograficas y de aporte. Que factores influyen
#> mas en que la persona tenga trabajo?
reg <- glm(formula = ins.tienetrabajo ~ bio.edad + bio.genero + aporte.numarticulos + aporte.numlibros + aporte.numproyectos.gana
					 + sit.tiene.msc + sit.tiene.phd + sit.tiene.pos,
					 data = personas.enchile, family = binomial)
thistable <- xtable(summary(reg),
										caption='¿Qué factores influyen más en que la persona tenga trabajo?',
										label='tab:reg1')
print(thistable, file=paste(out.path, 'tablas/tab-reg1.tex', sep=''))
rm(reg,thistable)

#..............................................................


#> Analisis: Alguna clase de regresion sobre personas$ins.cuantostrabajos con variables sociodemograficas de entrada. Que
#> factores influyen mas en la cantidad de trabajos?
reg <- lm(formula = ins.cuantostrabajos ~ bio.edad + bio.genero + aporte.numarticulos + aporte.numlibros + aporte.numproyectos.gana
					+ sit.tiene.msc + sit.tiene.phd + sit.tiene.pos,
					data = personas.enchile)
thistable <- xtable(summary(reg),
										caption='¿Qué factores influyen más en la cantidad de trabajos que tienen las personas?',
										label='tab:reg2')
print(thistable, file=paste(out.path, 'tablas/tab-reg2.tex', sep=''))
rm(reg,thistable)

#> Analisis: Agrupar los tipos en trabajos$trabajo.tipocontrato en variable "trabajo_estable?" (booleana), y realizar una
#> reg. log. sobre la variable, con variables SDs de entrada. Que factores influyen mas en que la persona obtenga un "trabajo
#> estable"?
trabajos2 <- trabajos
trabajos2$trabajo.estable <- FALSE
trabajos2$trabajo.estable[ trabajos2$trabajo.tipocontrato %in% c('Contrato a honorarios','Contrato de trabajo a plazo fijo','Contrato de trabajo a plazo indefinido') ] <- TRUE

continuo <- merge(
	personas.enchile,
	trabajos2[,c('Response.ID','trabajo.tipocontrato','trabajo.estable')],
	by='Response.ID'
)

reg <- glm(
	formula = trabajo.estable ~ bio.genero + bio.edad + sit.tiene.msc + sit.tiene.phd + sit.tiene.pos +
		bio.essostenedor + bio.cuantoshijos + ins.anhelo.academico,
	data = continuo, family = binomial
)
thistable <- xtable(summary(reg),
										caption='¿Qué factores influyen más en que la persona tenga un trabajo \\emph{estable}?',
										label='tab:reg3')
print(thistable, file=paste(out.path, 'tablas/tab-reg3.tex', sep=''))
rm(reg,thistable)

#> ¿Explica el salario recibido la percepción de la inserción?
reg <- lm(
	formula = ins.prom.percepcion ~ bio.edad + bio.genero + bio.essostenedor + bio.cuantoshijos
		+ ins.tienetrabajo + ins.anhelo.academico,
	data = personas.enchile)
thistable <- xtable(summary(reg),
										caption='¿Explica el salario recibido la percepción de inserción? Este análisis se realiza para todo el universo de personas.',
										label='tab:reg4')
print(thistable, file=paste(out.path, 'tablas/tab-reg4.tex', sep=''))
rm(reg,thistable)

#> Este análisis es el mismo anterior, pero solo para las personas que tienen trabajo
reg <- lm(
	formula = ins.prom.percepcion ~ bio.edad + bio.genero + bio.essostenedor + bio.cuantoshijos
		+ ins.anhelo.academico + as.numeric(ins.ingreso),
	data = personas.enchile[ personas.enchile$ins.tienetrabajo, ])
thistable <- xtable(summary(reg),
										caption='¿Explica el salario recibido la percepción de inserción? Este análisis se realiza sólo para las personas que poseen trabajo.',
										label='tab:reg5')
print(thistable, file=paste(out.path, 'tablas/tab-reg5.tex', sep=''))
rm(reg,thistable)

#> Relacion entre ingresos y promedio de percepcion de insercion.
# par(las=1, mar=c(2.5, 7.8, 2.5, 0.5), mgp=c(1.5,0.4,0))
# boxplot(
# 	personas$ins.prom.percepcion ~ as.numeric(personas$ins.ingreso),
# 	horizontal=T, cex.axis=0.7, cex.lab=0.9, cex.main=0.9,
# 	names = levels(personas$ins.ingreso),
# 	xlab = 'Promedio de percepción de inserción',
# 	main=paste('Gráfico ',numgrafico,': Promedio de percepción de inserción, por nivel de ingreso', sep='')
# )
