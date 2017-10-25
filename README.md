# The Spooky Programming Language 👻

At the bleeding edge of programming language research, theoreticians structure problems intractable for the retention of the mental contex of a single human mind into explicit semantics, their efforts weighted across the axis of expressiveness.

Among these computations determined too onerous for more affable turing machines, memory management has thrived in the zeitgeist, with such implementations as rust and ATS holding court upon many a Hacker News thread. Distributed systems are no less favored, with Erlang and Ponylang side projects blooming increasingly lush in Github's harvest season.

But there is one clade of computational semantics that remains too labyrinthine, too daring for the eye of even our most maverick programming language academics: writing spooky programs.

Until now.

Introducing:

_🎃 The Spooky Programming Language 🎃_

![Screenshot of some honestly pretty scary emojis](https://i.imgur.com/Ne7LEl6.png "boo!")

# The Programming Language of Fear 🌚

Spookylang stands behind but one goal: to be the scariest programming language. Yes, I know, "What about Java 7?!" as you wheel around to high-five your coworker. Enough merriment.

In order to write Spookylang, our primary researcher first had to truly understand the meaning of fear. Failing to do that, our researcher instead proposed these language features:



### Spooky-Completeness 😈

Simply put, Spookylang is a procedural, mostly dynamically typed machine that can be programmed to execute any computable functions that take "scary" input and output "spooky" output. This property (henceforth referred to as Spooky completeness) is isomorphic to Turing completeness by way of the following reversible morphisms:

```
let n be any string 
let 🤡 be any spooky string
let 🎃 be a scary word

N + 🎃 -> 🤡
🤡 - 🎃 -> N
```

To enforce this invariant, the Spookylang compiler provides I/O routines that crash upon being passed "non-scary" strings.



### Determining Scariness of Strings 💀

The problem of binary categorization of arbitrary input is well known to the artificial intelligence community. For our own purposes, we employed an "AI hardcoded list of regular expressions" that clusters strings according to the following policy:

![Artifical Intelligence Hardcoded List of Regexs](https://i.imgur.com/Jde71bT.png "SVM - Support Very Mhardcoded lists of regexs)")



### Vari(scary)ables 👹

In a good program, variable names must be terse, but expressive.

In Spookylang they must also be scary, or your program won't compile.



### The Spookiest Emojis 😱

There's nothing less scary than a keyword.

.. Except in Spookylang where all keywords are spooky emojis!



### Jump Scares 🤡

Surprise is a key element of being scared.

It will come as no surprise to you, then, that Spookyland employs the element of surprise in its mandate to spook. Not unlike MongoDB, the spookylang runtime will occasionally and unpredictably produce terrifying output.


# Building It 🍬

The Spookylang compiler is written in WOAH!Caml, a dialect of OCaml wherein the programmer must be

1. Sweaty
2. Relatively new to OCaml
3. Screaming

In order to protect our practitioners from the rank OCaml-building amateurity of our researchers we've provided a docker-based workflow. Provided you have a relatively modern version of docker installed, you may brace yourself against the overwhelming senselessness of fear and run `./docker-compile.sh examples/hell_world.spooky` to test out an example (the first time will run a bit slow due to building the docker image and the aforementioned OCaml-building amateurism - over 200 compiler warnings, sorry Jane Street!).

`docker-compile.sh` will build and persist a docker image dubbed `spookybox` (don't go in there!), and can be run on any spooky file you desire. This very repo helpfully contains a decent amount of source files in the `examples` subdirectory.
