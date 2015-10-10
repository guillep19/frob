frob
====

Proyecto de grado - Guillermo Pacheco - Facultad de Ingeniería - UdelaR
2014

Noticias
--------
* Sábado 10-10-2015 - :
  - Correcciones de Informe.
* Domingo 05-10-2015:
  - Finalizo implementación de compilador en haskell.
* Sábado 23-05-2015 a Domingo 24-05-2015:
  - Cambio implementación a version final con frp.
  - Implemento ejemplo en bytecode a mano, falta poder compilar.
  - Pruebas varias con sensor de distancia y leds. Procesar con
    lift y contar 'casas' con folds.
  - Refactor general de vm.
* Domingo 18-05-2015:
  - Actualizo informe, diseño, caso de estudio.
* Lunes 27-04-2015 a Sábado 02-05-2015:
  - Creo documentación en latex.
  - Agrego secciones: Resumen, Introducción, Programación funcional.
    reactiva, diseño, implementación, casos de estudio y conclusiones.
  - Redacto resumen e introduccion, frp, algo de diseño e implementación.
  - Comienzo a redactar caso de estudio.
  - Planifico mejoras de diseño e implementación.
  - Agrego algunas referencias y bibliografia.
  - Documentación accesible en: https://github.com/guillep19/frob/blob/master/report/Informe.pdf
* Sábado 18-04-2015 a Domingo 19-04-2015:
  - Funciona ejemplo básico example3 que vincula sensor de distancia
    filtra de acuerdo a la distancia y enciende un led cuando la misma
    es menor a una distancia dada.
  - El mismo es compilado a bytecode y lo corre la VM en el mbed. Se 
    agregó la abstracción de "Sensor de distancia" a la MBEDInterface
    que implementa IOInterface.
  - Armo sensores de luz y color.
  - Video: https://www.youtube.com/watch?v=q8zfUJGZRxE
* 15-04-2015:
  - Organizo implementación de VM. Creo IOInterface para abstraer kits.
* Domingo 22-03-2015:
  - Implemento 90% del compilador. Faltan detalles.
  - Probado con ejemplo2.frob.
* Jueves 19-03-2015:
  - Implemento 50% del compilador, genero bytecode a partir de alto nivel.
    Hay que actualizar la
    implementacion de la vm con cambios que surgieron en el bytecode.
* Miércoles 18-03-2015:
  - Creé una pequeña muestra de como sería un "editor" web de grafos que
    pueden representar los programas en alto nivel (graficamente).
    Está hecho en javascript, podría ser una buena idea para un frontend.
* Martes 17-03-2015:
  - Comienzo la implementación del compilador con python.ast.
  - Creo ejemplos en el lenguaje de alto nivel para compilar. Implementación
    del desafío escolar completa.
* Domingo 15-03-2015:
  - Tengo probado el ejemplo hecho anteriormente en bytecode, en la placa mbed.
  - Terminé el diseño a papel de toda la solución.
    * Diagrama del compilador, los componentes necesarios y los pasos para compilar un programa en alto nivel hasta subirlo a un kit.
    * Diagrama de máquina virtual. Mostrando como se portaría a diferentes arquitecturas (IOInterface) y el código en común (VM).
    * Diagrama en bajo nivel del funcionamiento de la vm. Mostrando sus partes, interacciones, y su disposición en la memoria ram o rom. (
      ip_buffer, input_buffer, output_buffer, stack, sp, ip, code)
    * Diagrama conceptual de la vm, con sus tres etapas IPO (Input Process Output).
  - Encontré un mecanismo para facilitar la generación de bytecode desde un lenguaje de alto nivel.
    * Utilizando python.ast, se puede obtener el AST de un programa en python, con el AST ya definido, es muy fácil generar
      el código final. La sintaxis de alto nivel, la estoy definiendo con esa idea en mente.
  - Encontré una idea que puede ser interesante. Pythonect:Dataflow programming language based on python. 
    http://docs.pythonect.org/en/latest/tutorial/helloworld.html
    Dataflow programming y FRP se parecen bastante y lo rescaté al ser parte de la idea inicial del proyecto.
  - Idea de editor web simple.
  - Falta terminar:
    * Definición del bytecode a bajo nivel 95%.
    * Definición del lenguaje de alto nivel 50%.

* Sábado 14-03-2015:
  - Logré configurar un entorno y cross compilar la máquina para la placa mbed.
  - Probé con el ejemplo en bytecode, debuggear desde mi pc la ejecución por
    el puerto serial, y que encienda un led.

* Viernes 13-03-2015:
  - Implementación de threading y ceder control al pedir input.
  - Tengo listo un ejemplo escrito en bytecode funcionando, que enciende un led al apretar un botón.
  - Diseño en papel de portabilidad de vm.
  - Diseño en papel de lo implementado hasta ahora.
  - Avance de implementación de vm: 70%.
  - Falta:
    * Separar/Implementar Lib de I/O con configuración (definición y cómo hacer mapeo).
    * Pasar a .tex diseño en papel.
    * Parser, generación de código.
    * Armar prueba con prototipo, implementar desafío escolar en placa *mbed*.
    * Por ahora la máquina simple, necesita solo ~ 512 bytes de RAM, de acuerdo al resultado de las pruebas puede aumentar/disminuír.

* Jueves 12-03-2015:
  Implementación de operaciones y pruebas de vm usando ejemplo. (Avance: 60%).

* Miércoles 11-03-2015:
  Implementación de operaciones y pruebas de vm (Avance: 40%).

* Martes 10-03-2015:
  Implementación de operaciones (Avance: 20%).

* Lunes 9-03-2015:
  Implementación de máquina virtual en ANSI C.

* Viernes 6-03-2015:
  Termino definición de vm y comienzo implementación de máquina virtual.

* Martes 10-06:
  Armar manuales para programar en C en cada plataforma (y MV).
  Evaluar pblua (lego), elua (varios micro), leJos, mbedlogo. Armar benchmark.
  Lenguajes de programación funcional reactiva.

* Martes 03-06:
  Estado del arte - versión inicial.

* Martes 27-05: 
  Reunión.

* Martes 29-04:
  Reunión para evaluar el documento con los datos de los kits relevados.

* Viernes 11-04:
  Comienzo de relevamiento de kits de robótica. Incluir kits
  arduino, fischertechnik, lego mindstorms, mbed, robotis, usb4butia.
  Plazo 2 semanas.
