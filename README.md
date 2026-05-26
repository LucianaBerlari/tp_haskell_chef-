## Preguntas:

### ¿Qué sucede si aplicamos cada uno de los trucos de la Parte A al Platinum?
Se podrían aplicar todas porque ninguna requiere saber la totalidad de la lista. En "endulzar", "salar", "darSabor" y "duplicarPorcion" no nos interesa saber la longitud de la lista, solo se aplica cuando se requiera. Pero, si generaría problemas si en consola ponemos:

> endulzar platinum \
> salar platinum \
> sarSabor platinum \
> duplicarPorcion platinum \

Esto sucede porque todas estas funciones devuelven un plato, y en este caso los ingrendientes del plato son infinitos.
Y por último, "simplificar" no se puede aplicar porque utilicé length en el desarrollo de la función, lo que requiere recorrer sí o sí la lista y produciría un error.

### ¿Cuáles de las preguntas de la parte B se pueden hacer sobre Platinum?
Se pueden hacer "esComplejo" y "noAptoHipertensión" porque no requieren recorrer toda la lista  como con "esVegano" y "esSinTacc".
(misma aclaración para "esComplejo" y "noAptoHipertensión" que con las funciones de la parte A respecto a como llamar por consola)

### ¿Se puede saber si el Platinum es mejor que otro plato?
No, porque no se puede saber la suma total de los infinitos componentes.

