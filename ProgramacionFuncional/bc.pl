% Hechos sobre animales y sus características
animal(gato).
animal(perro).
animal(pajaro).
animal(tigre).

color(gato, negro).
color(perro, marron).
color(pajaro, azul).
color(tigre, naranja).

tiene_pelo(gato).
tiene_pelo(perro).
tiene_pelo(tigre).

vuela(pajaro).

% Hechos sobre propiedades de los animales representadas como listas
propiedades(gato, [cuatro_patas, ronronea, domestico]).
propiedades(perro, [cuatro_patas, ladra, domestico]).
propiedades(pajaro, [dos_patas, vuela, salvaje]).
propiedades(tigre, [cuatro_patas, rugir, salvaje]).

% Reglas con consultas anidadas
es_domestico(Animal) :- propiedades(Animal, Props), member(domestico, Props).
es_salvaje(Animal) :- propiedades(Animal, Props), member(salvaje, Props).

