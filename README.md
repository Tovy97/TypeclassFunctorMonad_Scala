# Type class, Functor, Monad and Monad Transformer in Scala 2

This repository contains an implementation (IntelliJ project) in scala 2 of type class, functor, monad and monad transformer for a seminary. 
There are also LaTeX slides inside. 

## Code structure
The code consists of three main parts.
+ Under the `seminar_codes` package there are the implementations of type classes, functors, monads and monad transformers with some running examples.
+ Under the `wadler_example` package there are the implementations of monad presented in Wadler's article "Monads for functional programming".
+ Under the `tree_example` package there is an implementation of RedBlackTree and BinarySearchTree as type classes. There is also an implementation of functor on tree.
+ Under the `test` folder there are tests made in ScalaCheck of the laws for functors, monads and monad transformers.

# Location

Slides are available in

> `Seminar/`

Source codes in

> `src/main/scala/`

ScalaCheck test in

> `src/test/scala/`
