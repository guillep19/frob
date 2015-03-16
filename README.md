frob
====

Proyecto de grado - Guillermo Pacheco - Facultad de Ingeniería - UdelaR
2014

Noticias
--------

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
