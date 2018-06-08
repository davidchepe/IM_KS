Trabajo Fin de Máster de David Sánchez Martín y Eduardo Hernanz Moreno

El objetivo es diseñar un plan de competición basado en potencia para el segmento ciclista de un Triatlón.
 
Para ello, mediante técnicas de Machine Learning y Deep Learning se desarrollará un regresor que permita estimar la velocidad de cada tramo, basándonos en las características del mismo

Se añadirán factores externos como la velocidad y rumbo del viento

El diseño del plan de competición se hará simulando potencias a lo largo de todo el recorrido de forma que se minimice el tiempo total invertido, alcanzando una potencia media objetivo y cumpliendo con ciertas restricciones

La estructura del Repositorio es la siguiente

-Memoria: Explicación y guía de todo lo desarrollado durante el Proyecto
-Entrenamientos: Carga, limpieza y procesamiento de los entrenamientos disponibles
-RNN: Código de la Red Neuronal usada como regresor
-Simulador: Código del simulador de potencias sobre el recorrido ciclista
-Machine Learning: Pruebas desarrolladas durante la fase de regresión
-Desarrollo: Contiene información de los paquetes instalados en nuestros PC´s, así como el notebook de Weather importado en los entrenamientos
-Weather: Contiene la librería usada para la descarga del tiempo

Dejamos a continuación la lista de requisitos aunque también la mostramos en el info.txt de la carpeta Memoria

Requisitos: 
Entornos:

    Python 3.6
    Anaconda 5.2

Lista de paquetes y orden necesarios (pip install):

    msgpack
    geopy
    ipynb
    python-metar
    tensorflow
    keras
    plotly

Otro Software

    Tableau Reader o Desktop 10.5
   
Conexión a internet para ejecutar la parte de Análisis Track puesto que se conecta para recuperar la información climática