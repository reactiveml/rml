# ReactiveML

A programming language for implementing interactive systems.
ReactiveML combines the temporal expressiveness of synchronous languages with the power of functional programming. 

## What is ReactiveML?

ReactiveML is a general purpose programming language extended with concurrency constructs to ease the programming of interactive systems.
The concurrency model of the language originates from the synchronous model that is used to program real-time applications.
Therefore, it takes advantages of the expressiveness and determinism of the model and removes the real-time constraints.

## What are the uses of ReactiveML?

ReactiveML is dedicated to the implementation of interactive systems as found in video games and simulation problems.
For example, it has been used to simulate power consumption in sensor networks ([Glonemo](http://reactiveml.org/glonemo/)) or to implement a sequencer for mixed music ([ReactiveAsco](http://rml.lri.fr/reactive_asco/index.html)).
More examples are given in the [examples](./examples) directory.

## How to learn ReactiveML?

Start learning ReactiveML with the interactive tutorial available [online](http://reactiveml.org/tryrml/tryrml.html).
A [documentation](http://reactiveml.org/documentation.html) explains how to compile ReactiveML programs and gives the syntax and intuitive semantics of the reactive constructs.
Finally, academic [publications](http://reactiveml.org/publications.html) describes precisely the language, its static analysis and implementation.


## How to install ReactiveML?

ReactiveML is distributed through [opam](http://opam.ocaml.org/) and can thus be installed with the following command:

```
opam install rml rmlbuild
```

To install ReactiveML from the source, the instructions are given in the [INSTALL](./INSTALL) file. This can be suummurized by the following commandes:

```
./configure
make
make install
```
