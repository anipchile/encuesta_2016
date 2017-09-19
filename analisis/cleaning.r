#> -------------------------------------------------------------------------------------------
#> Proyecto: Primera Encuesta de Inserción de Doctores, ANIP 2016
#> Archivo: cleaning.r
#> Autor: Cristian Bravo Lillo <cristian@bravolillo.xyz>
#
#> Este archivo lee un archivo de datos original (data/base-anip-201702011627.csv) y lo limpia
#> para poder analizarlo. Ese archivo no es distribuido libremente porque contiene información
#> que identifica de manera personal a las personas que contestaron la encuesta. El archivo
#> está contenido en data/original-data.zip.
#
#> Por favor notar que el archivo que está en data/base-anip-201702011627.csv *está anonimizado*,
#> y no será transformado adecuadamente por este script porque parte de lo que hace este script
#> depende de que el archivo no esté anonimizado (en particular, el encontrar y eliminar respuestas
#> duplicadas).
#> -------------------------------------------------------------------------------------------

library(stringr)
clio.root = './'
source('lib/functions.general.r')
source('lib/functions.graphs.r')

#> Leemos los datos
personas <- read.csv('data/base-anip-201702011627.csv')

#> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#> Eliminamos (de momento) las respuestas parciales y las completas que fueron hechas en inglés, pero hay que
#> reincorporar estas últimas en algún momento
personas <- personas[ personas$Status=='Complete', ]
personas <- personas[ personas$X.Está.cursando.actualmente.algún.programa.de.Postgrado.ya.sea.en.Chile.o.el.Extranjero.!='', ]
personas$X.En.qué.idioma.prefiere.responder.la.encuesta...What.language.do.you.prefer.to.take.the.survey. <- NULL
#> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#> Limpiamos un poco
for (i in c(
	'Status','Contact.ID','Legacy.Comments','Comments','Language','Referer','Extended.Referer','User.Agent','Tags',
	'SessionID', 'Extended.User.Agent', 'IP.Address', 'Longitude', 'Latitude'
))
	personas[,c(i)] <- NULL
for (i in 1009:607)
	personas[,i] <- NULL
for (i in c(
	'X.Le.interesaría.ser.contactado.por.la.Asociación.Nacional.de.Investigadores.de.Postgrado..ANIP..y.Redes.Chilenas..ReCh..',
	'X.Deseas.recibir.información.sobre.los.resultados.de.esta.encuesta.',
	'Country','City','State.Region','Postal'
))
	personas[,c(i)] <- NULL
rm(i)

#> Renombramos algunas columnas de bio
personas$bio.nombre <- str_trim(tolower(personas$Nombre.Nombre.y.Apellido))
personas$bio.apellido <- str_trim(tolower(personas$Apellido.Nombre.y.Apellido))
personas$bio.email <- str_trim(tolower(personas$Correo.electrónico))
personas$bio.genero <- personas$Género
personas$bio.genero[ personas$bio.genero=='Transgénero' ] <- 'Masculino' #>!!!!!!!!!!!!!!!!!!!!!!! Esto es para no exponer a la única persona transgénero.
personas$bio.genero <- factor(personas$bio.genero)
personas$bio.edad <- personas$X.Cuál.es.su.edad..Exprésela.en.años

personas$bio.nacionalidad <- factor(personas$X.Cuál.es.su.nacionalidad., levels=c('Chilena','Otra'), labels=c('Chilena','Extranjera'))
personas$bio.resideenchile <- personas$X.Reside.actualmente.en.Chile. == 'Sí'
personas$bio.region <- factor(personas$Region..Indique.la.Región.y.Comuna.donde.reside)
personas$bio.comuna <- factor(personas$Comuna..Indique.la.Región.y.Comuna.donde.reside)
personas$bio.pais <- factor(personas$País.Indique.el.País.y.Ciudad.donde.reside)
personas$bio.ciudad <- factor(personas$Ciudad.Indique.el.País.y.Ciudad.donde.reside)

personas$X.Cuál.es.su.estado.civil...Opcional.[ personas$Otro..Especifique...Cuál.es.su.estado.civil...Opcional.=='Con pareja' ] = 'Conviviente civil'
personas$X.Cuál.es.su.estado.civil...Opcional.[ personas$Otro..Especifique...Cuál.es.su.estado.civil...Opcional.=='vivo con mi pareja' ] = 'Conviviente civil'
personas$X.Cuál.es.su.estado.civil...Opcional.[ personas$Otro..Especifique...Cuál.es.su.estado.civil...Opcional.=='conviviente' ] = 'Conviviente civil'
personas$X.Cuál.es.su.estado.civil...Opcional.[ personas$Otro..Especifique...Cuál.es.su.estado.civil...Opcional.=='Conviviente' ] = 'Conviviente civil'
personas$X.Cuál.es.su.estado.civil...Opcional.[ personas$Otro..Especifique...Cuál.es.su.estado.civil...Opcional.=='Conviviente ' ] = 'Conviviente civil'
personas$X.Cuál.es.su.estado.civil...Opcional.[ personas$Otro..Especifique...Cuál.es.su.estado.civil...Opcional.=='separada' ] = 'Divorciado'
personas$X.Cuál.es.su.estado.civil...Opcional.[ personas$Otro..Especifique...Cuál.es.su.estado.civil...Opcional.=='Separado' ] = 'Divorciado'
personas$X.Cuál.es.su.estado.civil...Opcional.[ personas$X.Cuál.es.su.estado.civil...Opcional.=='Otro (Especifique)' ] = ''
personas$bio.estadocivil <- factor(personas$X.Cuál.es.su.estado.civil...Opcional.,
																	 levels=c('Soltero','Casado','Divorciado','Viudo','Conviviente civil',''),
																	 labels=c('Soltero','Casado','Divorciado','Viudo','Conviviente','NS/NR'))
personas$bio.numpersonasfamilia <- personas$Aparte.de.usted..Cuántas.personas.viven.en.su.casa.
personas$bio.essostenedor <- personas$X.Es.usted.el.la.principal.sostenedor.a.económico.del.hogar.=='Sí'
personas$bio.cuantoshijos <- personas$X.Cuántos.hijos.tiene...Opcional.
personas$bio.cuantoshijos[ personas$X.Tiene.hijos...Opcional.!='Sí' ] <- 0


#> Creamos un pseudoid para tratar de identificar nombres repetidos
personas$pseudoid <- paste(personas$bio.nombre, personas$bio.apellido, personas$bio.email,sep='/')
personas <- personas[ !duplicated(personas$pseudoid), ]

#> Limpieza de columnas. No es necesario pero es útil.
for (i in c(
	'Nombre.Nombre.y.Apellido', 'Apellido.Nombre.y.Apellido', 'Correo.electrónico', 'Género', 'X.Cuál.es.su.edad..Exprésela.en.años',
	'X.Cuál.es.su.nacionalidad.', 'X.Reside.actualmente.en.Chile.', 'Region..Indique.la.Región.y.Comuna.donde.reside',
	'Comuna..Indique.la.Región.y.Comuna.donde.reside', 'País.Indique.el.País.y.Ciudad.donde.reside', 'Ciudad.Indique.el.País.y.Ciudad.donde.reside',
	'X.Cuál.es.su.estado.civil...Opcional.', 'Aparte.de.usted..Cuántas.personas.viven.en.su.casa.',
	'X.Es.usted.el.la.principal.sostenedor.a.económico.del.hogar.', 'X.Cuántos.hijos.tiene...Opcional.', 'X.Tiene.hijos...Opcional.',
	'Otro..Especifique..Género','Otro..Especifique...Cuál.es.su.estado.civil...Opcional.', 'pseudoid'
))
	personas[,c(i)] <- NULL
rm(i)

#> --------------------------------------------- Situación ---------------------------------------------
personas$sit.cursando.posgrado <- personas$X.Está.cursando.actualmente.algún.programa.de.Postgrado.ya.sea.en.Chile.o.el.Extranjero. =='Sí'
personas$sit.tiene.posgrado <- personas$X.Tiene.ya.algún.grado.académico.de.Postgrado.obtenido.ya.sea.en.Chile.o.el.Extranjero. == 'Sí'

#> --------------------------------------------- sit.cursando.msc ---------------------------------------------
personas$sit.cursando.msc <- personas$Magíster..MSc..Elija.los.tipos.de.programas.de.Postgrado.que.está.cursando.actualmente != ''
personas$sit.cursando.msc.inicio <- personas$X.En.qué.año.ingresó.al.programa.de.Magíster..MSc..que.está.cursando.actualmente..Magíster..MSc.
personas$sit.cursando.msc.fin <- personas$X.En.qué.año.espera.obtener.el.grado.de.Magíster..MSc...Magíster..MSc.

personas$X.En.qué.país.está.realizando.su.programa..Magíster..MSc.[personas$X.Está.realizando.su.programa.en.Chile..Magíster..MSc.=='Sí'] <- 'Chile'
personas$sit.cursando.msc.pais <- NA
personas$sit.cursando.msc.pais[ personas$X.En.qué.país.está.realizando.su.programa..Magíster..MSc.!='' ] <- personas$X.En.qué.país.está.realizando.su.programa..Magíster..MSc.[personas$X.En.qué.país.está.realizando.su.programa..Magíster..MSc.!='']
personas$sit.cursando.msc.institucion <- NA
personas$sit.cursando.msc.institucion[personas$Seleccione.la.Institución.o.Empresa.donde.realiza.su.Magíster..MSc..Magíster..MSc.!=''] <- personas$Seleccione.la.Institución.o.Empresa.donde.realiza.su.Magíster..MSc..Magíster..MSc.[personas$Seleccione.la.Institución.o.Empresa.donde.realiza.su.Magíster..MSc..Magíster..MSc.!='']
personas$sit.cursando.msc.institucion[personas$Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realiza.su.Magíster..MSc..Magíster..MSc.!=''] <- personas$Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realiza.su.Magíster..MSc..Magíster..MSc.[personas$Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realiza.su.Magíster..MSc..Magíster..MSc.!='']

colnames(personas)[grep("financia\\.este\\.programa.+MSc", colnames(personas))] <- paste('sit.cursando.msc.financiamiento.', sapply(strsplit(grep("financia\\.este\\.programa.+MSc", colnames(personas), value=T), "\\.\\."), function(x) {return(x[1])}), sep='')
for (i in grep("^sit\\.cursando\\.msc\\.financiamiento\\.", colnames(personas), value=T))
{
	personas[,c(i)] <- personas[,c(i)]!=''
	personas[,c(i)] <- sapply(personas[,c(i)] , function(x) { if (is.na(x)) return(FALSE) else return(x); })
}
rm(i)

personas$sit.cursando.msc.financiamiento.Other.text <- personas$Other...Especifique..Cómo.financia.este.programa..Marque.todas.las.opciones.que.correspondan.a.su.caso.10778
personas$Other...Especifique..Cómo.financia.este.programa..Marque.todas.las.opciones.que.correspondan.a.su.caso.10778 <- NULL

personas$sit.cursando.msc.premio <- personas$X.Ha.ganado.algún.premio.o.reconocimiento.durante.el.curso.de.este.programa..Magíster..MSc.=='Sí'
personas$sit.cursando.msc.premio[ !personas$sit.cursando.msc ] <- NA
personas$sit.cursando.msc.premio.cual <- personas$Especifíque.Magíster..MSc.
personas$sit.cursando.msc.trabajo.relacionado <- personas$Durante.la.duración.del.programa..Ha.tenido.algún.tipo.de.empleo.relacionado.con.el.área.de.conocimiento.de.su.programa..Magíster..MSc.=='Sí'
personas$sit.cursando.msc.trabajo.relacionado.cual <- personas$Describa.brevemente.el.empleo.Magíster..MSc.

#> --------------------------------------------- sit.cursando.phd ---------------------------------------------
personas$sit.cursando.phd <- personas$Doctorado..PhD..Elija.los.tipos.de.programas.de.Postgrado.que.está.cursando.actualmente != ''
personas$sit.cursando.phd.inicio <- personas$X.En.qué.año.ingresó.al.programa.de.Doctorado..PhD..que.está.cursando.actualmente..Doctorado..PhD.
personas$sit.cursando.phd.fin <- personas$X.En.qué.año.espera.obtener.el.grado.de.Doctorado..PhD...Doctorado..PhD.

personas$X.En.qué.país.está.realizando.su.programa..Doctorado..PhD.[personas$X.Está.realizando.su.programa.en.Chile..Doctorado..PhD.=='Sí'] <- 'Chile'
personas$sit.cursando.phd.pais <- NA
personas$sit.cursando.phd.pais[ personas$X.En.qué.país.está.realizando.su.programa..Doctorado..PhD.!='' ] <- personas$X.En.qué.país.está.realizando.su.programa..Doctorado..PhD.[personas$X.En.qué.país.está.realizando.su.programa..Doctorado..PhD.!='']
personas$sit.cursando.phd.institucion <- NA
personas$sit.cursando.phd.institucion[personas$Seleccione.la.Institución.o.Empresa.donde.realiza.su.Doctorado..PhD..Doctorado..PhD.!=''] <- personas$Seleccione.la.Institución.o.Empresa.donde.realiza.su.Doctorado..PhD..Doctorado..PhD.[personas$Seleccione.la.Institución.o.Empresa.donde.realiza.su.Doctorado..PhD..Doctorado..PhD.!='']
personas$sit.cursando.phd.institucion[personas$Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realiza.su.Doctorado..PhD..Doctorado..PhD.!=''] <- personas$Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realiza.su.Doctorado..PhD..Doctorado..PhD.[personas$Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realiza.su.Doctorado..PhD..Doctorado..PhD.!='']

colnames(personas)[grep("financia\\.este\\.programa.+PhD", colnames(personas))] <- paste('sit.cursando.phd.financiamiento.', sapply(strsplit(grep("financia\\.este\\.programa.+PhD", colnames(personas), value=T), "\\.\\."), function(x) {return(x[1])}), sep='')
for (i in grep("^sit\\.cursando\\.phd\\.financiamiento\\.", colnames(personas), value=T))
{
	personas[,c(i)] <- personas[,c(i)]!=''
	personas[,c(i)] <- sapply(personas[,c(i)] , function(x) { if (is.na(x)) return(FALSE) else return(x); })
}
rm(i)
personas$sit.cursando.phd.financiamiento.Other.text <- personas$Other...Especifique..Cómo.financia.este.programa..Marque.todas.las.opciones.que.correspondan.a.su.caso.10779
personas$Other...Especifique..Cómo.financia.este.programa..Marque.todas.las.opciones.que.correspondan.a.su.caso.10779 <- NULL

personas$sit.cursando.phd.premio <- personas$X.Ha.ganado.algún.premio.o.reconocimiento.durante.el.curso.de.este.programa..Doctorado..PhD.=='Sí'
personas$sit.cursando.phd.premio[ !personas$sit.cursando.phd ] <- NA
personas$sit.cursando.phd.premio.cual <- personas$Especifíque.Doctorado..PhD.
personas$sit.cursando.phd.trabajo.relacionado <- personas$Durante.la.duración.del.programa..Ha.tenido.algún.tipo.de.empleo.relacionado.con.el.área.de.conocimiento.de.su.programa..Doctorado..PhD.=='Sí'
personas$sit.cursando.phd.trabajo.relacionado.cual <- personas$Describa.brevemente.el.empleo.Doctorado..PhD.

#> --------------------------------------------- sit.cursando.pos ---------------------------------------------
personas$sit.cursando.pos <- personas$X.Está.haciendo.actualmente.algún.Postdoctorado.ya.sea.en.Chile.o.el.Extranjero. == 'Sí'
personas$sit.cursando.pos.inicio <- personas$X.En.qué.año.comenzó.su.Postdoctorado.
personas$sit.cursando.pos.fin <- personas$X.En.qué.año.terminará.o.planea.terminar.su.Postdoctorado.
personas$sit.cursando.pos.pais <- NA
personas$sit.cursando.pos.pais[ personas$X.Está.realizando.su.Postdoctorado.en.Chile.=='Sí' ] <- 'Chile'
personas$sit.cursando.pos.pais[ personas$X.En.qué.país.está.realizando.su.Postdoctorado.!='' ] <- personas$X.En.qué.país.está.realizando.su.Postdoctorado.[ personas$X.En.qué.país.está.realizando.su.Postdoctorado.!='' ]
personas$sit.cursando.pos.institucion <- NA
personas$sit.cursando.pos.institucion[ personas$Seleccione.la.Institución.o.Empresa.donde.realiza.su.Postdoctorado!='' ] <- personas$Seleccione.la.Institución.o.Empresa.donde.realiza.su.Postdoctorado[ personas$Seleccione.la.Institución.o.Empresa.donde.realiza.su.Postdoctorado!='' ]
personas$sit.cursando.pos.institucion[ personas$Especifique.el.nombre.de.la.Institución.o.Empresa!='' ] <- personas$Especifique.el.nombre.de.la.Institución.o.Empresa[ personas$Especifique.el.nombre.de.la.Institución.o.Empresa!='' ]

colnames(personas)[grep("Cómo\\.financia\\.su\\.Postdoctorado", colnames(personas))[1:11]] <- paste('sit.cursando.pos.financiamiento.', sapply(strsplit(grep("Cómo\\.financia\\.su\\.Postdoctorado", colnames(personas), value=T)[1:11], "\\.\\."), function(x) {return(x[1])}), sep='')
for (i in grep("^sit\\.cursando\\.pos\\.financiamiento\\.", colnames(personas), value=T))
{
	personas[,c(i)] <- personas[,c(i)]!=''
	personas[,c(i)] <- sapply(personas[,c(i)] , function(x) { if (is.na(x)) return(FALSE) else return(x); })
}
rm(i)
personas$sit.cursando.pos.financiamiento.Otro <- personas$Otro...Especifique..Cómo.financia.su.Postdoctorado..Marque.todas.las.opciones.que.correspondan.a.su.caso8
personas$sit.cursando.pos.financiamiento.Otro[ personas$sit.cursando.pos.financiamiento.Otro=='' ] <- NA

personas$sit.cursando.pos.premio <- personas$X.Ha.ganado.algún.premio.o.reconocimiento.durante.la.ejecución.de.su.Postdoctorado.=='Sí'
personas$sit.cursando.pos.premio[ !personas$sit.cursando.pos ] <- NA
personas$sit.cursando.pos.premio.cual <- personas$Especifíque

#> --------------------------------------------- sit.tiene.msc ---------------------------------------------
personas$sit.tiene.msc <- personas$Magíster..MSc..Elija.los.grados.académicos.de.Postgrado.que.tiene.actualmente != ''
personas$sit.tiene.msc1.inicio <- personas$X.En.qué.año.ingresó.al.programa.de.Magíster..MSc...Magíster..MSc.
personas$sit.tiene.msc1.fin <- personas$X.En.qué.año.obtuvo.el.grado.de.Magíster..MSc...Magíster..MSc.
personas$sit.tiene.msc2.inicio <- personas$X.En.qué.año.ingresó.al.otro.programa.de.Magíster..MSc...Magíster..MSc.
personas$sit.tiene.msc2.fin <- personas$X.En.qué.año.obtuvo.el.otro.grado.de.Magíster..MSc...Magíster..MSc.

personas$sit.tiene.msc.pais <- NA
personas$sit.tiene.msc.pais[ personas$X.Realizó.su.programa.en.Chile..Magíster..MSc.=='Sí' ] <- 'Chile'
personas$sit.tiene.msc.pais[ personas$X.Realizó.su.programa.en.Chile..Magíster..MSc.=='No' ] <- personas$X.En.qué.país.realizó.su.programa..Magíster..MSc.[ personas$X.Realizó.su.programa.en.Chile..Magíster..MSc.=='No' ]

personas$sit.tiene.msc.institucion <- NA
personas$sit.tiene.msc.institucion[ personas$Seleccione.la.Institución.o.Empresa.donde.realizó.su.Magíster..MSc..Magíster..MSc.!='' ] <- personas$Seleccione.la.Institución.o.Empresa.donde.realizó.su.Magíster..MSc..Magíster..MSc.[ personas$Seleccione.la.Institución.o.Empresa.donde.realizó.su.Magíster..MSc..Magíster..MSc.!='' ]
personas$sit.tiene.msc.institucion[ personas$Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realizó.su.Magíster..MSc..Magíster..MSc.!='' ] <- personas$Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realizó.su.Magíster..MSc..Magíster..MSc.[ personas$Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realizó.su.Magíster..MSc..Magíster..MSc.!='' ]

colnames(personas)[grep("Cómo\\.financió\\.su\\.programa.+Magíster", colnames(personas))] <- paste('sit.tiene.msc.financiamiento.', sapply(strsplit(grep("Cómo\\.financió\\.su\\.programa.+Magíster", colnames(personas), value=T), "\\.\\."), function(x) {return(x[1])}), sep='')
for (i in grep("^sit\\.tiene\\.msc\\.financiamiento\\.", colnames(personas), value=T))
{
	personas[,c(i)] <- personas[,c(i)]!=''
	personas[,c(i)] <- sapply(personas[,c(i)] , function(x) { if (is.na(x)) return(FALSE) else return(x); })
}
rm(i)
personas$sit.tiene.msc.financiamiento.Other <- personas$Other...Especifique..Cómo.financió.su.programa..Marque.todas.las.opciones.que.correspondan.a.su.caso.10780
personas$sit.tiene.msc.financiamiento.Other[ personas$sit.tiene.msc.financiamiento.Other=='' ] <- NA

personas$sit.tiene.msc.premio <- personas$X.Ganó.algún.premio.o.reconocimiento.durante.el.curso.del.programa..Magíster..MSc.=='Sí'
personas$sit.tiene.msc.premio[ !personas$sit.tiene.msc ] <- NA
personas$sit.tiene.msc.premio.cual <- personas$Especifíque.Magíster..MSc.4
personas$sit.tiene.msc.premio.cual[ !personas$sit.tiene.msc ] <- NA

personas$sit.tiene.msc.trabajo.relacionado <- personas$Durante.la.duración.del.programa..Tuvo.algún.tipo.de.empleo.relacionado.con.el.área.de.conocimiento.de.su.programa..Magíster..MSc.
personas$sit.tiene.msc.trabajo.relacionado[ personas$sit.tiene.msc.trabajo.relacionado=='' ] <- NA
personas$sit.tiene.msc.trabajo.relacionado <- personas$sit.tiene.msc.trabajo.relacionado=='Sí'

#> --------------------------------------------- sit.tiene.phd ---------------------------------------------
personas$sit.tiene.phd <- personas$Doctorado..PhD..Elija.los.grados.académicos.de.Postgrado.que.tiene.actualmente != ''
personas$sit.tiene.phd1.inicio <- personas$X.En.qué.año.ingresó.al.programa.de.Doctorado..PhD...Doctorado..PhD.
personas$sit.tiene.phd1.fin <- personas$X.En.qué.año.obtuvo.el.grado.de.Doctorado..PhD...Doctorado..PhD.
personas$sit.tiene.phd2.inicio <- personas$X.En.qué.año.ingresó.al.otro.programa.de.Doctorado..PhD...Doctorado..PhD.
personas$sit.tiene.phd2.fin <- personas$X.En.qué.año.obtuvo.el.otro.grado.de.Doctorado..PhD...Doctorado..PhD.

personas$sit.tiene.phd.pais <- NA
personas$sit.tiene.phd.pais[ personas$X.Realizó.su.programa.en.Chile..Doctorado..PhD.=='Sí' ] <- 'Chile'
personas$sit.tiene.phd.pais[ personas$X.Realizó.su.programa.en.Chile..Doctorado..PhD.=='No' ] <- personas$X.En.qué.país.realizó.su.programa..Doctorado..PhD.[ personas$X.Realizó.su.programa.en.Chile..Doctorado..PhD.=='No' ]

personas$sit.tiene.phd.institucion <- NA
personas$sit.tiene.phd.institucion[ personas$Seleccione.la.Institución.o.Empresa.donde.realizó.su.Doctorado..PhD..Doctorado..PhD.!='' ] <- personas$Seleccione.la.Institución.o.Empresa.donde.realizó.su.Doctorado..PhD..Doctorado..PhD.[ personas$Seleccione.la.Institución.o.Empresa.donde.realizó.su.Doctorado..PhD..Doctorado..PhD.!='' ]
personas$sit.tiene.phd.institucion[ personas$Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realizó.su.Doctorado..PhD..Doctorado..PhD.!='' ] <- personas$Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realizó.su.Doctorado..PhD..Doctorado..PhD.[ personas$Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realizó.su.Doctorado..PhD..Doctorado..PhD.!='' ]

colnames(personas)[grep("Cómo\\.financió\\.su\\.programa.+Doctorado", colnames(personas))] <- paste('sit.tiene.phd.financiamiento.', sapply(strsplit(grep("Cómo\\.financió\\.su\\.programa.+Doctorado", colnames(personas), value=T), "\\.\\."), function(x) {return(x[1])}), sep='')
for (i in grep("^sit\\.tiene\\.phd\\.financiamiento\\.", colnames(personas), value=T))
{
	personas[,c(i)] <- personas[,c(i)]!=''
	personas[,c(i)] <- sapply(personas[,c(i)] , function(x) { if (is.na(x)) return(FALSE) else return(x); })
}
rm(i)
personas$sit.tiene.phd.financiamiento.Other <- personas$Other...Especifique..Cómo.financió.su.programa..Marque.todas.las.opciones.que.correspondan.a.su.caso.10781
personas$sit.tiene.phd.financiamiento.Other[ personas$sit.tiene.phd.financiamiento.Other=='' ] <- NA

personas$sit.tiene.phd.premio <- personas$X.Ganó.algún.premio.o.reconocimiento.durante.el.curso.del.programa..Doctorado..PhD.=='Sí'
personas$sit.tiene.phd.premio[ !personas$sit.tiene.phd ] <- NA
personas$sit.tiene.phd.premio.cual <- personas$Especifíque.Doctorado..PhD.5
personas$sit.tiene.phd.premio.cual[ !personas$sit.tiene.phd ] <- NA

personas$sit.tiene.phd.trabajo.relacionado <- personas$Durante.la.duración.del.programa..Tuvo.algún.tipo.de.empleo.relacionado.con.el.área.de.conocimiento.de.su.programa..Doctorado..PhD.
personas$sit.tiene.phd.trabajo.relacionado[ personas$sit.tiene.phd.trabajo.relacionado=='' ] <- NA
personas$sit.tiene.phd.trabajo.relacionado <- personas$sit.tiene.phd.trabajo.relacionado=='Sí'

#> --------------------------------------------- sit.tiene.pos ---------------------------------------------
personas$sit.tiene.pos <- personas$X.Terminó.ya.algún.Postdoctorado.ya.sea.en.Chile.o.el.Extranjero. == 'Sí'
personas$sit.tiene.pos1.inicio <- personas$X.En.qué.año.comenzó.su.Postdoctorado.9
personas$sit.tiene.pos1.fin <- personas$X.En.qué.año.terminó.su.Postdoctorado.
personas$sit.tiene.pos2.inicio <- personas$X.En.qué.año.comenzó.el.otro.Postdoctorado.
personas$sit.tiene.pos2.fin <- personas$X.En.qué.año.terminó.el.otro.Postdoctorado.

personas$sit.tiene.pos.pais <- NA
personas$sit.tiene.pos.pais[ personas$X.Realizó.su.Postdoctorado.en.Chile.=='Sí' ] <- 'Chile'
personas$sit.tiene.pos.pais[ personas$X.En.qué.país.realizó.su.Postdoctorado.!='' ] <- personas$X.En.qué.país.realizó.su.Postdoctorado.[ personas$X.En.qué.país.realizó.su.Postdoctorado.!='' ]

colnames(personas)[grep("Cómo\\.financió\\.su\\.Postdoctorado", colnames(personas))[1:11]] <- paste('sit.tiene.pos.financiamiento.', sapply(strsplit(grep("Cómo\\.financió\\.su\\.Postdoctorado", colnames(personas), value=T)[1:11], "\\.\\."), function(x) {return(x[1])}), sep='')
for (i in grep("^sit\\.tiene\\.pos\\.financiamiento\\.", colnames(personas), value=T))
{
	personas[,c(i)] <- personas[,c(i)]!=''
	personas[,c(i)] <- sapply(personas[,c(i)] , function(x) { if (is.na(x)) return(FALSE) else return(x); })
}
rm(i)

personas$sit.tiene.pos.financiamiento.Otro <- personas$Otro...Especifique..Cómo.financió.su.Postdoctorado..Marque.todas.las.opciones.que.correspondan.a.su.caso13
personas$sit.tiene.pos.financiamiento.Otro[ personas$sit.tiene.pos.financiamiento.Otro=='' ] <- NA

personas$sit.tiene.pos.institucion <- NA
personas$sit.tiene.pos.institucion[ personas$Seleccione.la.Institución.o.Empresa.donde.realizó.su.Postdoctorado!='' ] <- personas$Seleccione.la.Institución.o.Empresa.donde.realizó.su.Postdoctorado[ personas$Seleccione.la.Institución.o.Empresa.donde.realizó.su.Postdoctorado!='' ]
personas$sit.tiene.pos.institucion[ personas$Especifique.el.nombre.de.la.Institución.o.Empresa12!='' ] <- personas$Especifique.el.nombre.de.la.Institución.o.Empresa12[ personas$Especifique.el.nombre.de.la.Institución.o.Empresa12!='' ]

personas$sit.tiene.pos.premio <- personas$X.Ganó.algún.premio.o.reconocimiento.durante.la.ejecución.de.su.Postdoctorado.=='Sí'
personas$sit.tiene.pos.premio[ !personas$sit.tiene.pos ] <- NA
personas$sit.tiene.pos.premio.cual <- personas$Especifíque15


#> ......................................................................................................
personas <- personas[ personas$sit.cursando.msc | personas$sit.cursando.phd | personas$sit.cursando.pos |
												personas$sit.tiene.msc | personas$sit.tiene.phd | personas$sit.tiene.pos, ]
#> ......................................................................................................


for (i in c(
	'X.Está.cursando.actualmente.algún.programa.de.Postgrado.ya.sea.en.Chile.o.el.Extranjero.',
	'X.Tiene.ya.algún.grado.académico.de.Postgrado.obtenido.ya.sea.en.Chile.o.el.Extranjero.',
	'X.Está.haciendo.actualmente.algún.Postdoctorado.ya.sea.en.Chile.o.el.Extranjero.',
	'X.Terminó.ya.algún.Postdoctorado.ya.sea.en.Chile.o.el.Extranjero.',
	'Magíster..MSc..Elija.los.tipos.de.programas.de.Postgrado.que.está.cursando.actualmente',
	'X.En.qué.año.ingresó.al.programa.de.Magíster..MSc..que.está.cursando.actualmente..Magíster..MSc.',
	'X.En.qué.año.espera.obtener.el.grado.de.Magíster..MSc...Magíster..MSc.',
	'Doctorado..PhD..Elija.los.tipos.de.programas.de.Postgrado.que.está.cursando.actualmente',
	'X.En.qué.año.ingresó.al.programa.de.Doctorado..PhD..que.está.cursando.actualmente..Doctorado..PhD.',
	'X.En.qué.año.espera.obtener.el.grado.de.Doctorado..PhD...Doctorado..PhD.',
	'Magíster..MSc..Elija.los.grados.académicos.de.Postgrado.que.tiene.actualmente',
	'X.En.qué.año.ingresó.al.programa.de.Magíster..MSc...Magíster..MSc.',
	'X.En.qué.año.obtuvo.el.grado.de.Magíster..MSc...Magíster..MSc.',
	'Doctorado..PhD..Elija.los.grados.académicos.de.Postgrado.que.tiene.actualmente',
	'X.En.qué.año.ingresó.al.programa.de.Doctorado..PhD...Doctorado..PhD.',
	'X.En.qué.año.obtuvo.el.grado.de.Doctorado..PhD...Doctorado..PhD.',
	'X.En.qué.año.comenzó.su.Postdoctorado.',
	'X.En.qué.año.terminará.o.planea.terminar.su.Postdoctorado.',
	'X.En.qué.año.comenzó.su.Postdoctorado.9',
	'X.En.qué.año.terminó.su.Postdoctorado.',
	'X.En.qué.año.comenzó.el.otro.Postdoctorado.',
	'X.En.qué.año.terminó.el.otro.Postdoctorado.',
	'X.En.qué.año.ingresó.al.otro.programa.de.Magíster..MSc...Magíster..MSc.',
	'X.En.qué.año.obtuvo.el.otro.grado.de.Magíster..MSc...Magíster..MSc.',
	'X.En.qué.año.ingresó.al.otro.programa.de.Doctorado..PhD...Doctorado..PhD.',
	'X.En.qué.año.obtuvo.el.otro.grado.de.Doctorado..PhD...Doctorado..PhD.',

	'X.Está.realizando.su.programa.en.Chile..Magíster..MSc.',
	'X.Está.realizando.su.programa.en.Chile..Doctorado..PhD.',
	'X.En.qué.país.está.realizando.su.programa..Magíster..MSc.',
	'X.En.qué.país.está.realizando.su.programa..Doctorado..PhD.',
	'X.Realizó.su.programa.en.Chile..Magíster..MSc.',
	'X.En.qué.país.realizó.su.programa..Magíster..MSc.',
	'X.Realizó.su.programa.en.Chile..Doctorado..PhD.',
	'X.En.qué.país.realizó.su.programa..Doctorado..PhD.',

	'Seleccione.la.Institución.o.Empresa.donde.realiza.su.Magíster..MSc..Magíster..MSc.',
	'Seleccione.la.Institución.o.Empresa.donde.realizó.su.Magíster..MSc..Magíster..MSc.',
	'Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realiza.su.Magíster..MSc..Magíster..MSc.',
	'Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realizó.su.Magíster..MSc..Magíster..MSc.',
	'Seleccione.la.Institución.o.Empresa.donde.realiza.su.Doctorado..PhD..Doctorado..PhD.',
	'Seleccione.la.Institución.o.Empresa.donde.realizó.su.Doctorado..PhD..Doctorado..PhD.',
	'Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realiza.su.Doctorado..PhD..Doctorado..PhD.',
	'Especifique.el.nombre.de.la.Institución.o.Empresa.donde.realizó.su.Doctorado..PhD..Doctorado..PhD.',

	'X.Está.realizando.su.Postdoctorado.en.Chile.',
	'X.En.qué.país.está.realizando.su.Postdoctorado.',
	'Seleccione.la.Institución.o.Empresa.donde.realiza.su.Postdoctorado',
	'Especifique.el.nombre.de.la.Institución.o.Empresa',

	'X.Ha.ganado.algún.premio.o.reconocimiento.durante.el.curso.de.este.programa..Magíster..MSc.',
	'X.Ha.ganado.algún.premio.o.reconocimiento.durante.el.curso.de.este.programa..Doctorado..PhD.',
	'Especifíque.Magíster..MSc.',
	'Especifíque.Doctorado..PhD.',
	'X.Ganó.algún.premio.o.reconocimiento.durante.el.curso.del.programa..Magíster..MSc.',
	'Especifíque.Magíster..MSc.4',
	'X.Ganó.algún.premio.o.reconocimiento.durante.el.curso.del.programa..Doctorado..PhD.',
	'Especifíque.Doctorado..PhD.5',
	'Durante.la.duración.del.programa..Ha.tenido.algún.tipo.de.empleo.relacionado.con.el.área.de.conocimiento.de.su.programa..Magíster..MSc.',
	'Durante.la.duración.del.programa..Tuvo.algún.tipo.de.empleo.relacionado.con.el.área.de.conocimiento.de.su.programa..Magíster..MSc.',
	'Durante.la.duración.del.programa..Ha.tenido.algún.tipo.de.empleo.relacionado.con.el.área.de.conocimiento.de.su.programa..Doctorado..PhD.',
	'Durante.la.duración.del.programa..Tuvo.algún.tipo.de.empleo.relacionado.con.el.área.de.conocimiento.de.su.programa..Doctorado..PhD.',
	'Describa.brevemente.el.empleo.Magíster..MSc.',
	'Describa.brevemente.el.empleo.Doctorado..PhD.',
	'Describa.brevemente.el.empleo.Magíster..MSc.6',
	'Describa.brevemente.el.empleo.Doctorado..PhD.7',

	'Other...Especifique..Cómo.financió.su.programa..Marque.todas.las.opciones.que.correspondan.a.su.caso.10780',
	'Other...Especifique..Cómo.financió.su.programa..Marque.todas.las.opciones.que.correspondan.a.su.caso.10781',
	'Otro...Especifique..Cómo.financia.su.Postdoctorado..Marque.todas.las.opciones.que.correspondan.a.su.caso8',
	'Otro...Especifique..Cómo.financió.su.Postdoctorado..Marque.todas.las.opciones.que.correspondan.a.su.caso13',

	'X.Ha.ganado.algún.premio.o.reconocimiento.durante.la.ejecución.de.su.Postdoctorado.',
	'Especifíque',
	'X.Realizó.su.Postdoctorado.en.Chile.',
	'X.En.qué.país.realizó.su.Postdoctorado.',
	'Seleccione.la.Institución.o.Empresa.donde.realizó.su.Postdoctorado',
	'Especifique.el.nombre.de.la.Institución.o.Empresa12',
	'X.Ganó.algún.premio.o.reconocimiento.durante.la.ejecución.de.su.Postdoctorado.',
	'Especifíque15'
))
personas[,c(i)] <- NULL
rm(i)

#> Areas y disciplinas
personas$sit.pregrado.area <- factor(personas$Area.del.conocimiento...En.qué.área.y.disciplina.clasificaría.su.Pregrado.)
personas$sit.pregrado.disciplina <- factor(personas$Disciplina...En.qué.área.y.disciplina.clasificaría.su.Pregrado.)

personas$sit.cursando.msc.area <- factor(personas$Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.programa.de.Magíster..MSc...Magíster..MSc.)
personas$sit.cursando.msc.disciplina <- factor(personas$Disciplina...En.qué.disciplina.y.área.clasificaría.su.programa.de.Magíster..MSc...Magíster..MSc.)
personas$sit.tiene.msc.area <- factor(personas$Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.grado.de.Magíster..MSc...Magíster..MSc.)
personas$sit.tiene.msc.disciplina <- factor(personas$Disciplina...En.qué.disciplina.y.área.clasificaría.su.grado.de.Magíster..MSc...Magíster..MSc.)

personas$sit.cursando.phd.area <- factor(personas$Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.programa.de.Doctorado..PhD...Doctorado..PhD.)
personas$sit.cursando.phd.disciplina <- factor(personas$Disciplina...En.qué.disciplina.y.área.clasificaría.su.programa.de.Doctorado..PhD...Doctorado..PhD.)
personas$sit.tiene.phd.area <- factor(personas$Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.grado.de.Doctorado..PhD...Doctorado..PhD.)
personas$sit.tiene.phd.disciplina <- factor(personas$Disciplina...En.qué.disciplina.y.área.clasificaría.su.grado.de.Doctorado..PhD...Doctorado..PhD.)

personas$sit.cursando.pos.area <- factor(personas$Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.Postdoctorado.)
personas$sit.cursando.pos.disciplina <- factor(personas$Disciplina...En.qué.disciplina.y.área.clasificaría.su.Postdoctorado.)
personas$sit.tiene.pos.area <- factor(personas$Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.Postdoctorado.11)
personas$sit.tiene.pos.disciplina <- factor(personas$Disciplina...En.qué.disciplina.y.área.clasificaría.su.Postdoctorado.10)

for (i in c(
	'Area.del.conocimiento...En.qué.área.y.disciplina.clasificaría.su.Pregrado.',
	'Disciplina...En.qué.área.y.disciplina.clasificaría.su.Pregrado.',

	'Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.programa.de.Magíster..MSc...Magíster..MSc.',
	'Disciplina...En.qué.disciplina.y.área.clasificaría.su.programa.de.Magíster..MSc...Magíster..MSc.',
	'Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.grado.de.Magíster..MSc...Magíster..MSc.',
	'Disciplina...En.qué.disciplina.y.área.clasificaría.su.grado.de.Magíster..MSc...Magíster..MSc.',

	'Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.programa.de.Doctorado..PhD...Doctorado..PhD.',
	'Disciplina...En.qué.disciplina.y.área.clasificaría.su.programa.de.Doctorado..PhD...Doctorado..PhD.',
	'Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.grado.de.Doctorado..PhD...Doctorado..PhD.',
	'Disciplina...En.qué.disciplina.y.área.clasificaría.su.grado.de.Doctorado..PhD...Doctorado..PhD.',

	'Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.Postdoctorado.',
	'Disciplina...En.qué.disciplina.y.área.clasificaría.su.Postdoctorado.',
	'Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.Postdoctorado.11',
	'Disciplina...En.qué.disciplina.y.área.clasificaría.su.Postdoctorado.10'
))
personas[,c(i)] <- NULL
rm(i)

#> Trabajo
personas$ins.tienetrabajo <- personas$X.Tiene.algún.trabajo.en.la.actualidad..Considere.todos.sus.trabajos..incluidos.los.Postgrados.y.Postdocs..independientemente.de.si.están.o.no.vinculados.a.su.área.de.formación. == 'Sí'
personas$ins.cuantostrabajos <- factor(personas$X.Cuántos.empleos.tiene.en.la.actualidad..Incluya.los.empleos.formales.e.informales.,
																			 levels=c('', '1', '2', '3', '4', '5 o más'),
																			 labels=c('0', '1', '2', '3', '4', '5+'))
#> Retorno a Chile
personas$ins.yaretorno <- NA
personas$ins.yaretorno[ personas$sit.tiene.msc & personas$X.Ya.retornó.a.Chile..Magíster..MSc.=='Sí' ] <- TRUE
personas$ins.yaretorno[ personas$sit.tiene.phd & personas$X.Ya.retornó.a.Chile..Doctorado..PhD.=='Sí' ] <- TRUE
personas$ins.yaretorno[ personas$X.Ya.retornó.a.Chile.=='Sí' ] <- TRUE
personas$ins.yaretorno[ personas$sit.tiene.msc & personas$X.Ya.retornó.a.Chile..Magíster..MSc.=='No' ] <- FALSE
personas$ins.yaretorno[ personas$sit.tiene.phd & personas$X.Ya.retornó.a.Chile..Doctorado..PhD.=='No' ] <- FALSE
personas$ins.yaretorno[ personas$X.Ya.retornó.a.Chile.=='No' ] <- FALSE
personas$ins.retorno.meses.msc <-
	as.numeric(personas$Años.Indique.hace.cuántos.años.y.meses.retornó.a.Chile.Magíster..MSc.)*12 +
	as.numeric(personas$Meses.Indique.hace.cuántos.años.y.meses.retornó.a.Chile.Magíster..MSc.)
personas$ins.retorno.meses.phd <-
	as.numeric(personas$Años.Indique.hace.cuántos.años.y.meses.retornó.a.Chile.Doctorado..PhD.)*12 +
	as.numeric(personas$Meses.Indique.hace.cuántos.años.y.meses.retornó.a.Chile.Doctorado..PhD.)
personas$ins.retorno.meses.pos <-
	as.numeric(personas$Años.Indique.hace.cuántos.años.y.meses.retornó.a.Chile)*12 +
	as.numeric(personas$Meses.Indique.hace.cuántos.años.y.meses.retornó.a.Chile)

#> Tiempo que tardo en encontrar trabajo
theselevels <- c('Menos de 6 meses','Entre 6 meses y 12 meses','Entre 13 meses y 24 meses','Entre 25 meses y 36 meses',
								 'Más de 36 meses','No encontré empleo','No busqué empleo','')
theselabels <- c('Menos de 6 meses','Entre 6 meses y 12 meses','Entre 13 meses y 24 meses','Entre 25 meses y 36 meses',
								 'Más de 36 meses','No encontré empleo','No busqué empleo','N/A')
personas$ins.tiempotrabajo.msc <- factor(personas$Una.vez.terminado.el.Magíster..MSc..y.estando.físicamente.en.Chile..Cuánto.tiempo.tardó.en.encontrar.empleo.vinculado.a.su.especialización..Entiéndase.que.hacer.un.nuevo.Postgrado.o.trabajar.como.Postdoctorado.son.considerados.empleos..Magíster..MSc., levels=theselevels, labels=theselabels)
personas$ins.tiempotrabajo.phd <- factor(personas$Una.vez.terminado.el.Doctorado..PhD..y.estando.físicamente.en.Chile..Cuánto.tiempo.tardó.en.encontrar.empleo.vinculado.a.su.especialización..Entiéndase.que.hacer.un.nuevo.Postgrado.o.trabajar.como.Postdoctorado.son.considerados.empleos..Doctorado..PhD., levels=theselevels, labels=theselabels)
personas$ins.tiempotrabajo.pos <- factor(personas$Una.vez.terminado.el.Postdoctorado.y.estando.físicamente.en.Chile..Cuánto.tiempo.tardó.en.encontrar.empleo.vinculado.a.su.especialización..Entiéndase.que.hacer.un.nuevo.Postgrado.o.trabajar.como.Postdoctorado.son.considerados.empleos., levels=theselevels, labels=theselabels)
rm(theselevels,theselabels)

#> Limpieza
for (i in c(
	'X.Tiene.algún.trabajo.en.la.actualidad..Considere.todos.sus.trabajos..incluidos.los.Postgrados.y.Postdocs..independientemente.de.si.están.o.no.vinculados.a.su.área.de.formación.',
	'X.Cuántos.empleos.tiene.en.la.actualidad..Incluya.los.empleos.formales.e.informales.',
	'X.Ya.retornó.a.Chile.',
	'X.Ya.retornó.a.Chile..Magíster..MSc.',
	'X.Ya.retornó.a.Chile..Doctorado..PhD.',
	'Años.Indique.hace.cuántos.años.y.meses.retornó.a.Chile.Magíster..MSc.',
	'Meses.Indique.hace.cuántos.años.y.meses.retornó.a.Chile.Magíster..MSc.',
	'Años.Indique.hace.cuántos.años.y.meses.retornó.a.Chile.Doctorado..PhD.',
	'Meses.Indique.hace.cuántos.años.y.meses.retornó.a.Chile.Doctorado..PhD.',
	'Años.Indique.hace.cuántos.años.y.meses.retornó.a.Chile',
	'Meses.Indique.hace.cuántos.años.y.meses.retornó.a.Chile',
	'Una.vez.terminado.el.Magíster..MSc..y.estando.físicamente.en.Chile..Cuánto.tiempo.tardó.en.encontrar.empleo.vinculado.a.su.especialización..Entiéndase.que.hacer.un.nuevo.Postgrado.o.trabajar.como.Postdoctorado.son.considerados.empleos..Magíster..MSc.',
	'Una.vez.terminado.el.Doctorado..PhD..y.estando.físicamente.en.Chile..Cuánto.tiempo.tardó.en.encontrar.empleo.vinculado.a.su.especialización..Entiéndase.que.hacer.un.nuevo.Postgrado.o.trabajar.como.Postdoctorado.son.considerados.empleos..Doctorado..PhD.',
	'Una.vez.terminado.el.Postdoctorado.y.estando.físicamente.en.Chile..Cuánto.tiempo.tardó.en.encontrar.empleo.vinculado.a.su.especialización..Entiéndase.que.hacer.un.nuevo.Postgrado.o.trabajar.como.Postdoctorado.son.considerados.empleos.'
))
personas[,c(i)] <- NULL
rm(i)

#> .........................................................................................
malaLikert <- function(vec, low = 'Muy<br />mala<br />1', high = 'Muy<br />buena<br />7') {
	vec[ vec==low ] <- 1
	vec[ vec==high ] <- 7
	vec[ vec=='' ] <- NA
	return(factor(as.numeric(vec), levels = 1:7))
}
#> .........................................................................................

#> Cómo calificaría su inserción...
personas$ins.tiene.msc.percepcion <- malaLikert(personas$X.Cómo.calificaría.su.experiencia.de.inserción.laboral.en.Chile..Magíster..MSc.)
personas$ins.tiene.phd.percepcion <- malaLikert(personas$X.Cómo.calificaría.su.experiencia.de.inserción.laboral.en.Chile..Doctorado..PhD.)
personas$ins.tiene.pos.percepcion <- malaLikert(personas$X.Cómo.calificaría.su.experiencia.de.inserción.laboral.en.Chile.)
personas$sit.cursando.msc.satisfecho <- malaLikert(personas$X.Qué.tan.satisfecho.se.encuentra.con.el.programa.que.cursa..Magíster..MSc.,
																									 low = 'Muy<br /> insatisfecho<br /> 1',
																									 high = 'Muy<br /> satisfecho<br /> 7')
personas$sit.cursando.phd.satisfecho <- malaLikert(personas$X.Qué.tan.satisfecho.se.encuentra.con.el.programa.que.cursa..Doctorado..PhD.,
																									 low = 'Muy<br /> insatisfecho<br /> 1',
																									 high = 'Muy<br /> satisfecho<br /> 7')
personas$ins.cursando.pos.satisfecho <- malaLikert(personas$X.Qué.tan.satisfecho.se.encuentra.con.la.Institución.donde.realiza.su.Postdoctorado.,
																									 low = 'Muy<br /> insatisfecho<br /> 1',
																									 high = 'Muy<br /> satisfecho<br /> 7')
personas$sit.tiene.msc.satisfecho <- malaLikert(personas$X.Qué.tan.satisfecho.se.encuentra.con.el.programa.que.cursó..Magíster..MSc.,
																								low = 'Muy<br /> insatisfecho<br /> 1',
																								high = 'Muy<br /> satisfecho<br /> 7')
personas$sit.tiene.phd.satisfecho <- malaLikert(personas$X.Qué.tan.satisfecho.se.encuentra.con.el.programa.que.cursó..Doctorado..PhD.,
																								low = 'Muy<br /> insatisfecho<br /> 1',
																								high = 'Muy<br /> satisfecho<br /> 7')
personas$sit.tiene.pos.satisfecho <- malaLikert(personas$X.Qué.tan.satisfecho.se.encuentra.con.la.Institución.donde.realizó.su.Postdoctorado.,
																								low = 'Muy<br /> insatisfecho<br /> 1',
																								high = 'Muy<br /> satisfecho<br /> 7')

#> Obligación de volver a chile y si perdió alguna afiliación
personas$ins.retornoobligado <- NA
personas$ins.retornoobligado[ personas$X.Su.financiamiento.le.exige.retornar.a.Chile.luego.de.un.periodo..Magíster..MSc.=='No' ] <- FALSE
personas$ins.retornoobligado[ personas$X.Su.financiamiento.le.exige.o.exigió.retornar.a.Chile.luego.de.un.periodo..Magíster..MSc.=='No' ] <- FALSE
personas$ins.retornoobligado[ personas$X.Su.financiamiento.le.exige.retornar.a.Chile.luego.de.un.periodo..Doctorado..PhD.=='No' ] <- FALSE
personas$ins.retornoobligado[ personas$X.Su.financiamiento.le.exige.o.exigió.retornar.a.Chile.luego.de.un.periodo..Doctorado..PhD.=='No' ] <- FALSE
personas$ins.retornoobligado[ personas$X.Su.financiamiento.le.exige.retornar.a.Chile.luego.de.un.periodo.=='No' ] <- FALSE
personas$ins.retornoobligado[ personas$X.Su.financiamiento.le.exigió.retornar.a.Chile.luego.de.un.periodo.=='No' ] <- FALSE
personas$ins.retornoobligado[ personas$X.Su.financiamiento.le.exige.retornar.a.Chile.luego.de.un.periodo..Magíster..MSc.=='Sí' ] <- TRUE
personas$ins.retornoobligado[ personas$X.Su.financiamiento.le.exige.o.exigió.retornar.a.Chile.luego.de.un.periodo..Magíster..MSc.=='Sí' ] <- TRUE
personas$ins.retornoobligado[ personas$X.Su.financiamiento.le.exige.retornar.a.Chile.luego.de.un.periodo..Doctorado..PhD.=='Sí' ] <- TRUE
personas$ins.retornoobligado[ personas$X.Su.financiamiento.le.exige.o.exigió.retornar.a.Chile.luego.de.un.periodo..Doctorado..PhD.=='Sí' ] <- TRUE
personas$ins.retornoobligado[ personas$X.Su.financiamiento.le.exige.retornar.a.Chile.luego.de.un.periodo.=='Sí' ] <- TRUE
personas$ins.retornoobligado[ personas$X.Su.financiamiento.le.exigió.retornar.a.Chile.luego.de.un.periodo.=='Sí' ] <- TRUE

personas$ins.perdioafiliacion <- NA
personas$ins.perdioafiliacion[ personas$X.Perdió.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile..Magíster..MSc.=='No' ] <- FALSE
personas$ins.perdioafiliacion[ personas$X.Perdió.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile..Doctorado..PhD.=='No' ] <- FALSE
personas$ins.perdioafiliacion[ personas$X.Perdió.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile.=='No' ] <- FALSE
personas$ins.perdioafiliacion[ personas$X.Perderá.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile..Magíster..MSc.=='No' ] <- FALSE
personas$ins.perdioafiliacion[ personas$X.Perderá.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile..Doctorado..PhD.=='No' ] <- FALSE
personas$ins.perdioafiliacion[ personas$X.Perderá.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile.=='No' ] <- FALSE
personas$ins.perdioafiliacion[ personas$X.Perdió.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile..Magíster..MSc.=='Sí' ] <- TRUE
personas$ins.perdioafiliacion[ personas$X.Perdió.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile..Doctorado..PhD.=='Sí' ] <- TRUE
personas$ins.perdioafiliacion[ personas$X.Perdió.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile.=='Sí' ] <- TRUE
personas$ins.perdioafiliacion[ personas$X.Perderá.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile..Magíster..MSc.=='Sí' ] <- TRUE
personas$ins.perdioafiliacion[ personas$X.Perderá.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile..Doctorado..PhD.=='Sí' ] <- TRUE
personas$ins.perdioafiliacion[ personas$X.Perderá.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile.=='Sí' ] <- TRUE

tmp <- personas[,grep("escrib.+situac", colnames(personas))]
tmp2 <- NULL
for (i in 1:nrow(tmp))
	tmp2[i] <- paste(tmp[i,], collapse='/')
personas$ins.perdioafiliacion.explique <- tmp2
rm(tmp,tmp2,i)

#> Limpieza
for (i in c(
	'X.Cómo.calificaría.su.experiencia.de.inserción.laboral.en.Chile..Magíster..MSc.',
	'X.Cómo.calificaría.su.experiencia.de.inserción.laboral.en.Chile..Doctorado..PhD.',
	'X.Cómo.calificaría.su.experiencia.de.inserción.laboral.en.Chile.',
	'X.Su.financiamiento.le.exige.retornar.a.Chile.luego.de.un.periodo..Magíster..MSc.',
	'X.Su.financiamiento.le.exige.o.exigió.retornar.a.Chile.luego.de.un.periodo..Magíster..MSc.',
	'X.Su.financiamiento.le.exige.retornar.a.Chile.luego.de.un.periodo..Doctorado..PhD.',
	'X.Su.financiamiento.le.exige.o.exigió.retornar.a.Chile.luego.de.un.periodo..Doctorado..PhD.',
	'X.Su.financiamiento.le.exige.retornar.a.Chile.luego.de.un.periodo.',
	'X.Su.financiamiento.le.exigió.retornar.a.Chile.luego.de.un.periodo.',
	'X.Perdió.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile..Magíster..MSc.',
	'X.Perdió.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile..Doctorado..PhD.',
	'X.Perdió.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile.',
	'X.Perderá.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile..Magíster..MSc.',
	'X.Perderá.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile..Doctorado..PhD.',
	'X.Perderá.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile.',
	'X.Perderá.algún.tipo.de.afiliación.profesional.por.retornar.a.Chile.',

	'X.Qué.tan.satisfecho.se.encuentra.con.el.programa.que.cursa..Magíster..MSc.',
	'X.Qué.tan.satisfecho.se.encuentra.con.el.programa.que.cursa..Doctorado..PhD.',
	'X.Qué.tan.satisfecho.se.encuentra.con.la.Institución.donde.realiza.su.Postdoctorado.',
	'X.Qué.tan.satisfecho.se.encuentra.con.el.programa.que.cursó..Magíster..MSc.',
	'X.Qué.tan.satisfecho.se.encuentra.con.el.programa.que.cursó..Doctorado..PhD.',
	'X.Qué.tan.satisfecho.se.encuentra.con.la.Institución.donde.realizó.su.Postdoctorado.',

	grep("escrib.+situac", colnames(personas), value=T)
))
personas[,c(i)] <- NULL

#> Vamos a eliminar los grados extra. No mucha gente tiene grados extra, y no tiene importancia para nuestro estudio.
for (i in c(
	'X.Tiene.otro.grado.de.Magíster..MSc...Entiéndase.distinto.al.que.acaba.de.responder..Magíster..MSc.',
	'X.Tiene.otro.grado.de.Doctorado..PhD...Entiéndase.distinto.al.que.acaba.de.responder..Doctorado..PhD.',
	'X.Hizo.algún.otro.Postdoctorado..Entiéndase.distinto.al.que.acaba.de.responder.',
	'Disciplina...En.qué.disciplina.y.área.clasificaría.su.otro.grado.de.Magíster..MSc...Magíster..MSc.',
	'Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.otro.grado.de.Magíster..MSc...Magíster..MSc.',
	'Disciplina...En.qué.disciplina.y.área.clasificaría.su.otro.grado.de.Doctorado..PhD...Doctorado..PhD.',
	'Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.su.otro.grado.de.Doctorado..PhD...Doctorado..PhD.',
	'Disciplina...En.qué.disciplina.y.área.clasificaría.el.otro.Postdoctorado.',
	'Area.del.conocimiento...En.qué.disciplina.y.área.clasificaría.el.otro.Postdoctorado.'
))
	personas[,c(i)] <- NULL
rm(i)


#> .......................................................................................................................
#> ....................................................... Trabajo .......................................................
#> .......................................................................................................................
#> Las siguientes preguntas fueron omitidas en la encuesta por error. Las incluimos porque sino genera un problema.
personas$X.Quién.se.encarga.de.pagar.las.cotizaciones.legales.de.salud..5 <- NA
personas$X.Qué.tipo.de.seguro.o.previsión.de.salud.tiene..5 <- NA
personas$X.En.qué.grado.su.seguro.o.previsión.de.salud.cubre.los.riesgos.que.implica.su.trabajo..5 <- NA
personas$País.4.Indique.el.País.y.Ciudad.donde.trabaja <- NA
personas$País.5.Indique.el.País.y.Ciudad.donde.trabaja <- NA
personas$Otro..Quién.se.encarga.de.pagar.las.cotizaciones.legales.de.salud..5 <- NA
personas$Otra..Qué.tipo.de.seguro.o.previsión.de.salud.tiene..5 <- NA

for (i in 4:5) {
	thesecolnames <- c(
		paste('Acuerdo.con.la.institución.en.que.trabajaba..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Corfo..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Finaciamiento.Basal..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Financiamiento.de.privados.nacionales..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Financiamiento.de.privados.internacionales..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Fondart..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Fondecyt..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Fondecyt..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i,38+(i-1)*2, sep=''),
		paste('Fondos.Milenio..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Fonis..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Fosis..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Postdoctorado..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Postulación.a.concurso..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Otro..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Otro..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i,39+(i-1)*2, sep='')
	)
	for (j in thesecolnames)
		personas[,c(j)] <- NA
}
rm(i,j)

trabajos <- NULL
for (i in 1:5) {
	thesecolnames <- c(
		'Response.ID',
		paste('X.Cuántas.horas.a.la.semana.dedica.a.este.trabajo..',i,sep=''),
		paste('X.Cuánto.se.relaciona.este.trabajo.con.su.formación.y.grado.académico..',i,sep=''),
		paste('Considerando.las.responsabilidades.y.habilidades.que.requieren.el.trabajo.que.desempeña...Qué.tipo.de.formación.mínima.cree.usted.que.debería.tener.una.persona.para.realizar.este.trabajo...',i,sep=''),
		paste('Otra.Considerando.las.responsabilidades.y.habilidades.que.requieren.el.trabajo.que.desempeña...Qué.tipo.de.formación.mínima.cree.usted.que.debería.tener.una.persona.para.realizar.este.trabajo...',i, sep=''),
		paste('Beneficios.',i,'.Evalúe.en.qué.grado.su.empleo.satisface.los.siguientes.aspectos.de.su.vida', sep=''),
		paste('Condiciones.de.trabajo.',i,'.Evalúe.en.qué.grado.su.empleo.satisface.los.siguientes.aspectos.de.su.vida', sep=''),
		paste('Estabilidad.Laboral.',i,'.Evalúe.en.qué.grado.su.empleo.satisface.los.siguientes.aspectos.de.su.vida', sep=''),
		paste('Estatus.social.',i,'.Evalúe.en.qué.grado.su.empleo.satisface.los.siguientes.aspectos.de.su.vida', sep=''),
		paste('Grado.de.independencia.',i,'.Evalúe.en.qué.grado.su.empleo.satisface.los.siguientes.aspectos.de.su.vida', sep=''),
		paste('Nivel.de.responsabilidad.',i,'.Evalúe.en.qué.grado.su.empleo.satisface.los.siguientes.aspectos.de.su.vida', sep=''),
		paste('Posibilidad.de.progreso.',i,'.Evalúe.en.qué.grado.su.empleo.satisface.los.siguientes.aspectos.de.su.vida', sep=''),
		paste('Salario.',i,'.Evalúe.en.qué.grado.su.empleo.satisface.los.siguientes.aspectos.de.su.vida', sep=''),
		paste('Integridad.Física.',i,'.Evalúe.en.qué.grado.su.empleo.pone.en.riesgo.los.siguientes.aspectos.de.su.vida', sep=''),
		paste('Integridad.Psicológica.',i,'.Evalúe.en.qué.grado.su.empleo.pone.en.riesgo.los.siguientes.aspectos.de.su.vida', sep=''),
		paste('Salud.en.general.',i,'.Evalúe.en.qué.grado.su.empleo.pone.en.riesgo.los.siguientes.aspectos.de.su.vida', sep=''),
		paste('Seguridad.personal.o.familiar.',i,'.Evalúe.en.qué.grado.su.empleo.pone.en.riesgo.los.siguientes.aspectos.de.su.vida', sep=''),
		paste('X.Cuenta.con.un.seguro.o.previsión.de.salud..',i, sep=''),
		paste('X.Quién.se.encarga.de.pagar.las.cotizaciones.legales.de.salud..',i, sep=''),
		paste('Otro..Quién.se.encarga.de.pagar.las.cotizaciones.legales.de.salud..',i, sep=''),
		paste('X.Qué.tipo.de.seguro.o.previsión.de.salud.tiene..',i, sep=''),
		paste('Otra..Qué.tipo.de.seguro.o.previsión.de.salud.tiene..',i, sep=''),
		paste('X.En.qué.grado.su.seguro.o.previsión.de.salud.cubre.los.riesgos.que.implica.su.trabajo..',i, sep=''),
		paste('X.Cómo.consiguió.este.trabajo..',i, sep=''),
		paste('Otro..Cómo.consiguió.este.trabajo..',i, sep=''),
		paste('X.Estableció.su.propia.oficina.o.laboratorio..',i, sep=''),
		paste('Acuerdo.con.la.institución.en.que.trabajaba..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Corfo..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Finaciamiento.Basal..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Financiamiento.de.privados.nacionales..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Financiamiento.de.privados.internacionales..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Fondart..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Fondecyt..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Fondecyt..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i,38+(i-1)*2, sep=''),
		paste('Fondos.Milenio..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Fonis..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Fosis..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Postdoctorado..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Postulación.a.concurso..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Otro..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i, sep=''),
		paste('Otro..Con.qué.fondos.estableció.esta.oficina.o.laboratorio..Marque.todas.las.opciones.que.estime.pertinentes.',i,39+(i-1)*2, sep=''),

		paste('X.Qué.tipo.de.relación.laboral.tiene.en.este.trabajo..',i, sep=''),
		paste('Otra..Qué.tipo.de.relación.laboral.tiene.en.este.trabajo..',i, sep=''),
		paste('X.Trabaja.físicamente.en.Chile..',i, sep=''),
		paste('Region..Indique.la.Región.y.Comuna.donde.trabaja.',i, sep=''),
		paste('Comuna..Indique.la.Región.y.Comuna.donde.trabaja.',i, sep=''),
		paste('País.',i,'.Indique.el.País.y.Ciudad.donde.trabaja', sep=''),
		paste('Ciudad.',i,'.Indique.el.País.y.Ciudad.donde.trabaja', sep=''),
		paste('X.Cuál.es.el.nombre.de.la.organización..institución..empresa.o.iniciativa.privada.donde.trabaja..',i, sep=''),
		paste('Administración.pública..En.qué.contexto.se.enmarca.su.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Centro.de.investigación..En.qué.contexto.se.enmarca.su.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Institución.sin.fines.de.lucro..En.qué.contexto.se.enmarca.su.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Instituto.Profesional..En.qué.contexto.se.enmarca.su.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Sector.Empresarial..En.qué.contexto.se.enmarca.su.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Trabajador.independiente..En.qué.contexto.se.enmarca.su.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Universidad.Privada..En.qué.contexto.se.enmarca.su.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Universidad.pública.tradicional..En.qué.contexto.se.enmarca.su.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Otra..En.qué.contexto.se.enmarca.su.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Otra..En.qué.contexto.se.enmarca.su.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i,i+27,sep=''),
		paste('Académico.docencia..Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Administrativo.Gestión..Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Asistente.de.investigación..Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Consultoría..Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Director.de.proyectos..Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Docencia..ayudantía...Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Edición.de.textos.publicaciones..Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Investigador.asociado..Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Investigador.colaborador.en.proyectos.de.terceros..Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Investigador.principal..Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Trabajo.de.campo..producción.de.información.o.datos.para.investigación...trabajo.de.escritorio...Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Traducciones..Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Otra..Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i, sep=''),
		paste('Otra..Qué.tipo.de.labores.desempeña.en.este.trabajo..Marque.todas.las.opciones.que.correspondan.a.su.caso.',i,i+32, sep='')
	)
	tmp  <- personas[,thesecolnames]
	colnames(tmp) <- c(
		'Response.ID',
		'trabajo.cuantashoras',
		'trabajo.relacionconformacion',
		'trabajo.formacionminima',
		'trabajo.formacionminima.otra',
		'satisfaccion.beneficios',
		'satisfaccion.condicionestrabajo',
		'satisfaccion.estabilidad',
		'satisfaccion.statussocial',
		'satisfaccion.independencia',
		'satisfaccion.responsabilidad',
		'satisfaccion.posibilidadprogreso',
		'satisfaccion.salario',
		'riesgo.integridadfisica',
		'riesgo.integridadsicologica',
		'riesgo.saludgeneral',
		'riesgo.seguridadpersonalfamiliar',
		'seguro.tiene',
		'seguro.quienpaga',
		'seguro.quienpaga.otro',
		'seguro.quetipo',
		'seguro.quetipo.otro',
		'seguro.cubreriesgos',
		'trabajo.comoconsiguio',
		'trabajo.comoconsiguio.otro',
		'trabajo.oficinaolabpropio',
		'trabajo.oficinaolabpropio.acuerdo.con.institucion',
		'trabajo.oficinaolabpropio.corfo',
		'trabajo.oficinaolabpropio.financiamiento.basal',
		'trabajo.oficinaolabpropio.financiamiento.privado.nacional',
		'trabajo.oficinaolabpropio.financiamiento.privado.internacional',
		'trabajo.oficinaolabpropio.fondart',
		'trabajo.oficinaolabpropio.fondecyt',
		'trabajo.oficinaolabpropio.fondecyt2',
		'trabajo.oficinaolabpropio.fondosmilenio',
		'trabajo.oficinaolabpropio.fonis',
		'trabajo.oficinaolabpropio.fosis',
		'trabajo.oficinaolabpropio.posdoctorado',
		'trabajo.oficinaolabpropio.concurso',
		'trabajo.oficinaolabpropio.otro',
		'trabajo.oficinaolabpropio.otro.cual',
		'trabajo.tipocontrato',
		'trabajo.tipocontrato.otro',
		'trabajo.donde.enchile',
		'trabajo.donde.region',
		'trabajo.donde.comuna',
		'trabajo.donde.pais',
		'trabajo.donde.ciudad',
		'trabajo.institucion',
		'trabajo.institucion.adm.publica',
		'trabajo.institucion.centro.inv',
		'trabajo.institucion.inst.no.lucro',
		'trabajo.institucion.inst.profesional',
		'trabajo.institucion.empresa',
		'trabajo.institucion.trabajador.indep',
		'trabajo.institucion.univ.privada',
		'trabajo.institucion.univ.publica',
		'trabajo.institucion.otra',
		'trabajo.institucion.otra.cual',
		'trabajo.labor.academico.principal',
		'trabajo.labor.administrativo',
		'trabajo.labor.invest.asistente',
		'trabajo.labor.consultor',
		'trabajo.labor.directorproyecto',
		'trabajo.labor.academico.ayudante',
		'trabajo.labor.editortextos',
		'trabajo.labor.invest.asociado',
		'trabajo.labor.invest.colaborador',
		'trabajo.labor.invest.principal',
		'trabajo.labor.trabajocampo',
		'trabajo.labor.traductor',
		'trabajo.labor.otra',
		'trabajo.labor.otra.cual'
	)
	trabajos <- rbind(trabajos, tmp)
	for (j in thesecolnames[2:length(thesecolnames)])
		personas[,c(j)] <- NULL
}

trabajos2 <- NULL
for (i in 1:nrow(trabajos)) {
	if (nchar(str_trim(paste0(trabajos[i,2:ncol(trabajos)], collapse='')))>0) {
		if (regexpr("^(NA)+$", str_trim(paste0(trabajos[i,2:ncol(trabajos)], collapse='')))==-1) {
			trabajos2 <- rbind(trabajos2, trabajos[i,])
		}
	}
}
trabajos <- trabajos2
rm(i, j, tmp, trabajos2, thesecolnames)

#> Limpieza de campos
trabajos$trabajo.cuantashoras <- as.numeric(trabajos$trabajo.cuantashoras)
trabajos$trabajo.relacionconformacion <- malaLikert(trabajos$trabajo.relacionconformacion,
																										low='Totalmente no relacionado<br />1', high='Totalmente relacionado<br />7')
for (i in grep("^satisfaccion", colnames(trabajos), value=T))
	trabajos[,c(i)] <- malaLikert(trabajos[,c(i)], low='Totalmente<br />insatisfecho<br />1', high='Totalmente<br />satisfecho<br />7')
rm(i)

trabajos$trabajo.formacionminima <- factor(trabajos$trabajo.formacionminima,
																					 levels=c('Enseñanza media completa',
																					 				 'Formación técnica',
																					 				 'Licenciatura/Título Universitario',
																					 				 'Licenciatura/Título Universitario  con especialización de Postítulo',
																					 				 'Magíster/MSc',
																					 				 'Doctorado/PhD',
																					 				 'Otra'))

for (i in grep("^riesgo", colnames(trabajos), value=T))
	trabajos[,c(i)] <- malaLikert(trabajos[,c(i)], low='No existe<br />riesgo<br />1', high='Existe mucho<br />riesgo<br />7')
rm(i)

#> Hay que transformar las columnas trabajos$trabajo.institucion.* en booleans.
#> Hay que transformar las columnas trabajos$trabajo.labor.* en booleans.
#> Idem con trabajos$trabajo.
for (i in c(
	grep("^trabajo\\.institucion\\.", colnames(trabajos), value=T)[1:9],
	grep("^trabajo\\.labor\\.", colnames(trabajos), value=T)[1:13],
	grep("^trabajo\\.oficinaolabpropio\\.", colnames(trabajos), value=T)[1:14]
))
{
	trabajos[,c(i)] <- trabajos[,c(i)]!=''
	trabajos[ is.na(trabajos[,c(i)]) , c(i) ] <- FALSE
}
rm(i)

trabajos$seguro.tiene <- trabajos$seguro.tiene=='Sí'
trabajos$seguro.quienpaga[ trabajos$seguro.quienpaga=='' ] <- NA
trabajos$seguro.quienpaga <- factor(trabajos$seguro.quienpaga,
																		levels=c('Yo','Mi empleador','Otro'))
trabajos$seguro.quetipo[ trabajos$seguro.quetipo=='' ] <- NA
trabajos$seguro.quetipo <- factor(trabajos$seguro.quetipo,
																	levels=c('Fonasa','Isapre','Seguro complementario','Otra'))
trabajos$seguro.cubreriesgos <- malaLikert(trabajos$seguro.cubreriesgos,
																					 low='No cubre para nada<br />los riesgos a los que<br />estoy expuesto/a<br />1',
																					 high='Cubre totalmente<br />los riesgos a los que<br />estoy expuesto/a<br />7')
trabajos$trabajo.comoconsiguio <- factor(trabajos$trabajo.comoconsiguio)
trabajos$trabajo.oficinaolabpropio <- trabajos$trabajo.oficinaolabpropio=='Sí'
trabajos$trabajo.tipocontrato[ trabajos$trabajo.tipocontrato=='<a class=\"tip\" href=\"javascript:void(0)\">Contrato a honorarios<span>No es el caso de Fondecyt de Pos' ] <- 'Contrato a honorarios'
trabajos$trabajo.tipocontrato[ trabajos$trabajo.tipocontrato=='<a class=\"tip\" href=\"javascript:void(0)\">Contrato a honorarios<span>No es el caso de Fondecyt de Postdoctorado. El contrato a honorarios es una convención en virtud de la cual una parte se encuentra obligada a prestar servicios específicos, por un tiempo determinado a favor de otro, el que a su vez se obliga a pagar una cierta cantidad de dinero por dichos servicios.</span></a>' ] <- 'Contrato a honorarios'
trabajos$trabajo.tipocontrato[ trabajos$trabajo.tipocontrato=='<a href=\"javascript:void(0)\" class=\"tip\">Convenio de beca<span>Esto incluye a Becas Nacionales y Becas Chile de Doctorado</span></a>' ] <- 'Convenio de beca'
trabajos$trabajo.tipocontrato[ trabajos$trabajo.tipocontrato=='<a href=\"javascript:void(0)\" class=\"tip\">Convenio de financiamiento<span>Esto incluye a Fondecyt de Postdoctorado y Fondecyt de Iniciación</span></a>' ] <- 'Convenio de financiamiento'
trabajos$trabajo.tipocontrato <- factor(trabajos$trabajo.tipocontrato,
																				levels=c(
																					'Acuerdo de palabra',
																					'Ad honorem',
																					'Convenio de beca',
																					'Convenio de financiamiento',
																					'Contrato a honorarios',
																					'Contrato de trabajo a plazo fijo',
																					'Contrato de trabajo a plazo indefinido',
																					'Contrato de trabajo por obra',
																					'Otra',
																					'Sin contrato')
)
trabajos$trabajo.donde.enchile <- trabajos$trabajo.donde.enchile=='Sí'

#> Otros
personas$aporte.hapublicado <- personas$X.Ha.publicado.algún.artículo.científico.o.libro..ya.sea.como.autor.principal.o.coautor.=='Sí'
personas$aporte.mastiempo.investigar <- NA
personas$aporte.mastiempo.investigar[ personas$Considerando.todos.sus.trabajos...cree.que.debería.disponer.de.más.tiempo.para.investigar.=='Sí' ] <- TRUE
personas$aporte.mastiempo.investigar[ personas$Considerando.todos.sus.trabajos...cree.que.debería.disponer.de.más.tiempo.para.investigar.=='No' ] <- FALSE

personas$aporte.mastiempo.investigar.cuanto <- personas$En.promedio...cuántas.horas.a.la.semana.debería.aumentar.su.tiempo.dedicado.a.la.investigación.
personas$aporte.numarticulos <- personas$X.Cuántos.artículos.científicos.ha.publicado.en.los.últimos.5.años..Considere.todos.los.artículos.publicados.ya.sea.como.autor.principal.o.coautor.
personas$aporte.numarticulos.ISI <- personas$X.Cuántos.artículos.científicos.ha.publicado.en.los.últimos.5.años.en.revistas.ISI..Considere.todos.los.artículos.publicados.ya.sea.como.autor.principal.o.coautor...Qué.es.ISI.ISI.Web.of.Knowledge..WoK..es.un.servicio.en.línea.de.información.científica..su
personas$aporte.numarticulos.1erautor <- personas$X.Cuántos.artículos.como.primer.autor..senior.o.autor.corresponsal.ha.publicado.en.los.últimos.5.años..Considere.todos.los.artículos.ya.sean.ISI.o.no.
personas$aporte.numlibros <- personas$X.Cuántos.libros.ha.publicado.
personas$aporte.numcapitulos <- personas$X.Cuántos.capítulos.de.libros.ha.publicado.
personas$aporte.numpatentes <- factor(personas$X.Cuántas.patentes.tiene.inscritas.,
																			levels=c('Ninguna','1','2','3','4','5 o más'))
personas$aporte.numobraspropintelectual <- factor(personas$X.Cuántos.trabajos.u.obras.tiene.inscritos.con.propiedad.intelectual.,
																									levels=c('Ninguna','1','2','3','4','5 o más'))
personas$aporte.numproyectos.postula <- personas$A.lo.largo.de.su.trayectoria.profesional..A.cuántos.proyectos.de.investigación.ha.postulado..Considere.los.proyectos..tanto.adjudicados.como.no.adjudicados.
personas$aporte.numproyectos.gana <- personas$X.Cuántos.de.estos.proyectos.se.adjudicó.
personas$aporte.redes.tiene <- personas$En.la.actualidad..Mantiene.relaciones.de.colaboración.profesional.con.investigadores.que.trabajan.fuera.de.Chile.=='Sí'
personas$aporte.redes.paises <- personas$Países.Especifique.los.países..instituciones.y.tipos.de.colaboración.que.mantiene
personas$aporte.redes.instituciones <- personas$Instituciones.y.Organizaciones.Especifique.los.países..instituciones.y.tipos.de.colaboración.que.mantiene
personas$aporte.redes.tipocolab <- personas$Tipos.de.Colaboración.Especifique.los.países..instituciones.y.tipos.de.colaboración.que.mantiene

personas$ins.ingreso <- personas$X.En.que.rango.sitúa.su.ingreso.líquido.mensual..Considere.todos.sus.ingresos.y.expréselo.en.pesos.chilenos
personas$ins.ingreso[ personas$ins.ingreso=='' ] <- NA
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
personas$ins.ingreso.satisfecho <- malaLikert(personas$Considerando.todos.sus.ingresos.mensuales...qué.tan.satisfecho.se.encuentra.con.sus.ingresos.líquidos., low='Totalmente<br />insatisfecho<br />1', high='Totalmente<br />satisfecho<br />7')
personas$ins.anhelo.academico <- personas$A.lo.largo.de.su.formación.y.trayectoria.profesional...ha.sido.o.fue.uno.de.sus.anhelos.trabajar.en.el.ámbito.académico.=='Sí'
personas$ins.pertenece.asociacion <- personas$X.Pertenece.a.alguna.Asociación.de.Investigadores..Profesionales.y.o.Científicos.=='Sí'
personas$ins.pertenece.asociacion.cual <- personas$Especifique.cuáles

personas$aporte.numarticulos[ is.na(personas$aporte.numarticulos) ] <- 0
personas$aporte.numlibros[ is.na(personas$aporte.numlibros) ] <- 0
personas$aporte.numproyectos.gana[ is.na(personas$aporte.numproyectos.gana) ] <- 0

#> Limpieza
for (i in c(
	'X.Ha.publicado.algún.artículo.científico.o.libro..ya.sea.como.autor.principal.o.coautor.',
	'Considerando.todos.sus.trabajos...cree.que.debería.disponer.de.más.tiempo.para.investigar.',
	'En.promedio...cuántas.horas.a.la.semana.debería.aumentar.su.tiempo.dedicado.a.la.investigación.',
	'X.Cuántos.artículos.científicos.ha.publicado.en.los.últimos.5.años..Considere.todos.los.artículos.publicados.ya.sea.como.autor.principal.o.coautor.',
	'X.Cuántos.artículos.científicos.ha.publicado.en.los.últimos.5.años.en.revistas.ISI..Considere.todos.los.artículos.publicados.ya.sea.como.autor.principal.o.coautor...Qué.es.ISI.ISI.Web.of.Knowledge..WoK..es.un.servicio.en.línea.de.información.científica..su',
	'X.Cuántos.artículos.como.primer.autor..senior.o.autor.corresponsal.ha.publicado.en.los.últimos.5.años..Considere.todos.los.artículos.ya.sean.ISI.o.no.',
	'X.Cuántos.libros.ha.publicado.',
	'X.Cuántos.capítulos.de.libros.ha.publicado.',
	'X.Cuántas.patentes.tiene.inscritas.',
	'X.Cuántos.trabajos.u.obras.tiene.inscritos.con.propiedad.intelectual.',
	'A.lo.largo.de.su.trayectoria.profesional..A.cuántos.proyectos.de.investigación.ha.postulado..Considere.los.proyectos..tanto.adjudicados.como.no.adjudicados.',
	'X.Cuántos.de.estos.proyectos.se.adjudicó.',
	'En.la.actualidad..Mantiene.relaciones.de.colaboración.profesional.con.investigadores.que.trabajan.fuera.de.Chile.',
	'Países.Especifique.los.países..instituciones.y.tipos.de.colaboración.que.mantiene',
	'Instituciones.y.Organizaciones.Especifique.los.países..instituciones.y.tipos.de.colaboración.que.mantiene',
	'Tipos.de.Colaboración.Especifique.los.países..instituciones.y.tipos.de.colaboración.que.mantiene',

	'X.En.que.rango.sitúa.su.ingreso.líquido.mensual..Considere.todos.sus.ingresos.y.expréselo.en.pesos.chilenos',
	'Considerando.todos.sus.ingresos.mensuales...qué.tan.satisfecho.se.encuentra.con.sus.ingresos.líquidos.',
	'A.lo.largo.de.su.formación.y.trayectoria.profesional...ha.sido.o.fue.uno.de.sus.anhelos.trabajar.en.el.ámbito.académico.',
	'X.Pertenece.a.alguna.Asociación.de.Investigadores..Profesionales.y.o.Científicos.',
	'Especifique.cuáles',
	grep("En\\.qué\\.sector\\.económico", colnames(personas), value=T)
))
personas[,c(i)] <- NULL
rm(i)

#> Creamos una ultima variable necesaria
personas$ins.prom.percepcion <- rowMeans(
	sapply(
		personas[,grep('percep', colnames(personas))],
		function (x) {return(as.numeric(x))}
	),
	na.rm = T
)

#> Anonimizamos
for (i in c('bio.nombre','bio.apellido','bio.email'))
	personas[,c(i)] <- NULL
rm(i)

#> Hay una persona que dice haber escrito mas de 100 libros. Esto es probablemente un error. La eliminamos.
personas <- personas[ personas$aporte.numlibros<100, ]

#> Escribimos el resultado y terminamos.
write.csv(personas, 'data/personas.csv', row.names = FALSE)
write.csv(trabajos, 'data/trabajos.csv', row.names = FALSE)
