# Proyecto: Intérprete de Lenguaje de Programación :computer:

Este proyecto consiste en el diseño de un intérprete para un lenguaje de programación específico que permite realizar operaciones con notación infija. El intérprete está basado en la gramática que se detalla a continuación.

## Gramática

### Valores Denotados
- Texto
- Número
- Booleano
- ProcVal

### Valores Expresados
- Texto
- Número
- Booleano
- ProcVal

### Reglas de la Gramática
- `<programa> := <expresion> un-programa (exp)`
- `<expresion> := <numero> numero-lit (num)`
- `:= """<texto> """`
- `:= <identificador> var-exp (id)`
- `:= (<expresion> <primitiva-binaria> <expresion>) primapp-bin-exp (exp1 prim-binaria exp2)`
- `:= <primitiva-unaria> (<expresion>) primapp-un-exp (prim-unaria exp)`

### Operadores
- `<primitiva-binaria> := + (primitiva-suma)`
- `:= ~ (primitiva-resta)`
- `:= / (primitiva-div)`
- `:= * (primitiva-multi)`
- `:= concat (primitiva-concat)`
- `:= > (primitiva-mayor)`
- `:= < (primitiva-menor)`
- `:= >= (primitiva-mayor-igual)`
- `:= <= (primitiva-menor-igual)`
- `:= != (primitiva-diferente)`
- `:= == (primitiva-comparador-igual)`

### Primitivas Unarias
- `<primitiva-unaria>:= longitud (primitiva-longitud)`
- `:= add1 (primitiva-add1)`
- `:= sub1 (primitiva-sub1)`
- `:= neg (primitiva-negacion-booleana)`

## Funciones Implementadas
1. **Ambiente Inicial**: Se define un ambiente con variables iniciales `(@a @b @c @d @e)` y valores correspondientes.
2. **Buscar Variable**: Se implementa una función `buscar-variable` para retornar el valor de una variable o un mensaje de error si no existe.
3. **Booleanos**: Se implementa la verificación de valores booleanos con la función `valor-verdad?`.
4. **Condicionales**: Se extiende la gramática para incluir condicionales con la regla `Si <expresion> "{ <expresion> }" sino "{ <expresion> }"`.
5. **Declaración de Variables Locales**: Se permite la declaración de variables locales con la regla `declarar`.
6. **Procedimientos**: Se permite la creación y evaluación de procedimientos con la gramática extendida.
7. **Llamados Recursivos**: Se implementa la capacidad para realizar llamados recursivos en procedimientos.
8. **Ejercicios Prácticos**: Se desarrollan varios procedimientos para operaciones matemáticas y decoradores en Python.

## Instalación
1. Clonar el repositorio.
   
    ```bash
   git clone https://github.com/Jpdv9/interpretador.git
   ```
3. Ejecutar el intérprete con el archivo de entrada deseado.


## Institución :mortar_board:

- Universidad: Universidad del Valle, Cali, Colombia.
- Curso: Fundamentos De Interpretación y Compilación de Lenguages de Programación.
- Semestre: 2024-02

## Participantes :busts_in_silhouette:

- [Mosquera Zapata, Wilson Andres](https://github.com/andresengineer) - 202182116
- [Davalos Valencia, Jean Paul](https://github.com/Jpdv9) - 201832375

## Contribuciones
Las contribuciones son bienvenidas. Por favor, abre un issue o un pull request si deseas colaborar.

## Licencia
Este proyecto está bajo la Licencia MIT.

