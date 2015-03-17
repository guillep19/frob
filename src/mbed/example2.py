#!/usr/bin/env python

#Inputs
sensor_distancia = 1
sensor_color_izq = 2
sensor_color_der = 3

#Outputs
velocidad_motor_izq = 1
velocidad_motor_der = 2

#
# Constantes
#
# Al recibir un valor de distancia menor a DISTANCIA_CASA, se considera
# que se esta frente a una.
DISTANCIA_CASA = 100
#
# Los valores del sensor de color menores e este valor se consideran BLANCO
UMBRAL_DE_BLANCO = 50 

def hay_casa(distancia):
  return distancia < DISTANCIA_CASA

def task_contar_casas():
  cuenta = 0
  viendo = False

  while cuenta < 2:
    distancia = read(sensor_distancia)
    if not hay_casa(distancia):
      viendo = False
    else:
      viendo = True
      cuenta = cuenta + 1

def es_blanco(color):
    return color < UMBRAL_DE_BLANCO

## El camino es negro y el fondo es blanco,  
## el robot debe tratar de seguir la linea sin
## salirse.
def task_seguir_linea():
  while True:
    color_izq = read(sensor_color_izq)
    color_der = read(sensor_color_der)
    vel_izq = 1
    vel_der = 1
    if es_blanco(color_izq):
        vel_der = 0.5
    if es_blanco(color_der):
        vel_izq = 0.5
    write(velocidad_motor_izq, vel_izq)
    write(velocidad_motor_der, vel_der)


def task_stop_all():
    vel_izq = 0
    vel_der = 0
    write(velocidad_motor_izq, 0)
    write(velocidad_motor_der, 0)

def main():
    start(task_contar_casas)
    start(task_seguir_linea)

    on_finish(task_contar_casas):
        stop(task_seguir_linea)
        start(task_stop_all)