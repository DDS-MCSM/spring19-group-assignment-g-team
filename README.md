##       CyberSecurity Management Msc. Spring 19 - Data Driven Security
##                         Group Assignment - G-Team

Group Assignment base repository for the Data Driven Security subject of the [CyberSecurity Management Msc](https://www.talent.upc.edu/ing/professionals/presentacio/codi/221101/cybersecurity-management/).

### Xavi Figueras
### Santi Vilaró


## Practica de Mòdul - Data Driven Security

Análisis globlal d'atacs per denegació de servei, DoS, i la seva geolocalització.

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
    - Documentar utilitzant roxygen el package resultant. Incloure també documentació del data frame resultant.
    - Omplir aquest fitxer README.md del repositori
  
  
### Project Description

Utilització del llenguatge de programació R per analitzar conjunts de dades relacionades amb la cyberseguretat.

### Goals

En el present projecte es vol analitzar un conjunt de dades relacionades amb atacs de denegació de servei, DoS, 
i relacionar-les amb la localització GPS on esdevé l'atac per tal de fer-ne una representació gràfica sobre un mapa
mundial.

### Data acquisition

Es descarregaran dos datasets diferents:
- Dades de tráfic a internet relacionat amb cyberatacs de tipus DoS
Descarreguem un dataset[https://data.mendeley.com/datasets/psjxnzsxyx/2] de la web de la fundació per a la recerca Mendeley.

- Un conjunt de registres que relacionen una IP determinada amb la seva localització GPS.
Concretament descarreguem un dataset[https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip] de la web de maxmind.com:

### Cleansing and transformations
Es realitzen els següents processos de neteja i transformació:
- Durant la descàrrega del fitxer de dades d'atacs, hem de modificar el nom de la primera columna pq no es copia bé. No entenem el per què.
- El fitxer que descarreguem de maxmind està comprimit. Després de descomprimir-lo eliminem el fixer descarregat.
- El data frame de destí conté inicialment les dates dels atacs, i posteriorment hi afegim les IP geolocalitzades i les dades de geolocalització que ens interessen.

### Data analysis
El dataframe resultat té 1390 línies, cadascuna corresponent a un atac, on els camps principals són:
- Timestamp
- IPs d'origen i destí
- Ports d'origen i destí
- La posició GPS
Addicionalment, la localització GPS incorpora també un camp de precisió.

Amb la matriu de resultats, volem fer els següents estudis o anàlisis:
- posicionar en un mapamundi tots el atacs, indicant el punt d'origen i el de destí
- observar, en un histograma, quines són les IPs atacants més actives i quines IPs de destí són les que reben més atacs
- observar, segurament també en un histograma, quins són els ports més usats per fer aquests tipus d'atacs.

### Results / Conclusions.
