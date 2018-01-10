# Datos en Primera Encuesta de Inserción de Doctores

Este directorio contiene los datos recogidos a través de la Primera Encuesta de Inserción Laboral de Doctores, realizada por ANIP entre diciembre de 2016 y enero de 2017 (https://github.com/anipchile/encuesta_2016).

El archivo ''original-data.zip'' contiene las fuentes originales de la encuesta.  Este archivo está protegido con un password para proteger la privacidad de las personas que respondieron la encuesta. Para obtener acceso a este archivo, por favor contacta a Cristian Bravo Lillo (cristian@bravolillo.xyz). Para obtener la información no anonimizada, por favor contacta a Roberto Muñoz (rmunoz@uc.cl) o a Karla Henríquez (karla.henriquez@usach.cl).

El archivo ''base-anip-201702011627.csv'' contiene la base de datos original, luego de anonimizar los datos por persona.  Cada fila de este archivo corresponde a la respuesta de una persona.  Los datos que identifican a las personas han sido eliminados.

La base anterior fue dividida para su análisis en otras dos:

* personas.csv: Contiene las respuestas de cada persona.  La persona es identificada a través de un identificador único (Response.ID).
* trabajos.csv: Dentro de la encuesta se preguntó por los trabajos que cada persona pudiera tener.  Esta base contiene la información de cada uno de los trabajos reportados.  Cada fila dentro de esta base contiene el identificador de la persona que reportó el trabajo.
