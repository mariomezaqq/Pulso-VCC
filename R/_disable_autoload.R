# Este archivo desactiva la carga automatica de R/ que hace Shiny (>= 1.5).
# app.R ya hace source() de cada archivo en el orden correcto y en el entorno
# global. Con la carga automatica, Shiny ademas los cargaba en OTRO entorno al
# arrancar, y el dashboard usaba esa copia de FONDOS/CATEGORIAS: los cambios de
# la lista de fondos (refrescar_fondos_globales) no se veian hasta reiniciar.
