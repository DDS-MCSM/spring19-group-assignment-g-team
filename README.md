# Group Assignment - Data Driven Security
##spring19-group-assignment-g-team
## Xavi Figueras
## Santi Vilaró

Group Assignment base repository for the Data Driven Security subject of the [CyberSecurity Management Msc](https://www.talent.upc.edu/ing/professionals/presentacio/codi/221101/cybersecurity-management/).

## Practica de Mòdul - Data Driven Security

Análisis globlal d'atacs per denegació de servei i la seva geolocalització.

### Requirements Entrega 1

  - Perfil Developer
    - A partir del repository que conté el package dds.base, crear funcions
      que incloguin el contingut de la sessió 2.
      Cal incloure almenys les funcions:
        * download.data()     #per descarregar dades en local
        * generate.df(nrows)  #retorna un data frame amb les n primeres files
        * clean.df()          #funció interna, elimina files inválides
    - Alternativament, es pot parsejar un altre set de dades dels que s'han
      vist en les sessions anteriors
  - Perfil Analista
    - Documentar utilitzant roxygen el package resultant. Incloure també             documentació del data frame resultant.
    - Omplir aquest fitxer README.md del repositori
  
  
### Project Description

 
Utilització del llenguatge de programació per analitzar conjunts de dades
relacionades amb la cyberseguretat.
En el present projecte es vol analitzar un conjunt de dades relacionades amb
atacs de denegació de servei, DoS, i relacionar-les amb la localització GPS on
esdevé l'atac.

### Goals


### Data acquisition

Es descarregaran dos datasets diferents:
- Dades de tráfic a internet
Descarreguem una base de dades de la web del Centre Australià per a la Cyberseguretat.
- Registres que relacionen una IP determinada amb la seva ubicació física per GPS.
Descarreguem un dataset de la web de maxmind.com:
https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip
La idea és relacionar els atacs llistats al dataset anterior amb la seva geolocalització.


### Cleansing and transformations
Es realitzen els següents processos de neteja i transformació:
- Durant la descàrrega del fitxer de dades d'atacs, hem de modificar el nom de la primera columna pq no es copia bé. No entenem el per què.
- El fitxer que descarreguem de maxmind està comprimit. Després de descomprimir-lo eliminem el fixer descarregat.
- El data frame de destí conté inicialment les dates dels atacs, i posteriorment hi afegim les IP geolocalitzades i les dades de geolocalització que ens interessen.

### Data analysis
No es pot fer un primer análisis per no poder finalitzar l'execució del programa main.

### Results / Conclusions.
