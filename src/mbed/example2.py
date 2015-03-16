#!/usr/bin/env python

#Inputs
sensor_distancia = 1
sensor_color_izq = 2
sensor_color_der = 3

#Outputs
velocidad_motor_izq = 1
velocidad_motor_der = 2

#Constantes
DISTANCIA_CASA = 100

def hay_casa(distancia):
  return distancia < DISTANCIA_CASA


def task_contar_casas()
  cuenta = 0
  viendo = False

  while cuenta < 2:
    input_sd = read(sensor_distancia)
    if not hay_casa(input_sd):
      viendo = False
    else:
      viendo = True
      cuenta = cuenta + 1


button = 0
led = 1
i = 0
while i <= 5:
  i = i + 1
  value = read(button)
  if value == 1:
    write(led, 1)
  else:
    write(led, 0)
