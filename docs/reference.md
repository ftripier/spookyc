# Spookylang Reference

## Functions

Our language researchers are lazy, and Spookylang is a procedural language. This is no coincidence. A function is defined by an identifier and a list of parameters next to a spooky ghost. A spookylang program must contain a function named "boo!", and begins program execution by calling that function.

```
👻  my_spooky_function 👿  😈 💀
  🍬 ⚰️🎃
☠️
```

Ghosts indicate function definitions, devils delimit parameters, and skulls delimit statement blocks. All three scare you.

Spookylang does not implement closure over functions in its set of morphisms. Again, lazy researchers.

Varargs technically works, but only by accident. Please don't do it.

## Variables

Variables are declared following a scary clown. Variables cannot be declared and assigned to in the same statement, because ~~of our lazy worthless researchers~~ they're so scared of the declaration clown. Ah! Variable names have to be scary or we don't compile your program. Look, there are plenty of non-meme programming langauges out there if you want.

```
🤡 my_terrifying_variable_declaration🎃
```

Assignment is a screaming genderless anthromorph betwixt a binding and an expression, delimited
by a jack o' lantern, our statement terminator and a fun emoji.

```
assignment_can_be_scary 😱  ⚰️🎃
```

## Expressions

### Values

Spookylang values fear and Halloween festivity, but its actual data values are what you're used to.


*Numbers*
```
13
13.666 + 420
```


*Booleans*


```
🌝 - true
🌚 - false
```


*Strings*


```
"ahhhhhhhhhhh! I'm so scared of this programming language!"
"eek! strings can be cocatenated with the plus operator!" + 69 + "nice"
```


*Void*


```
⚰️
```


*Lists*


```
🍫 1 🍬 2.5 🍬 "uh oh skeletons" 🍬 🌝 🍬 🌚 🍬 ⚰️ 🍭
```


*Dictionary*


```
🍫
"top_level" 😱 🍫
  "scary_key" 😱 "spooky!"🍬
  "list_key" 😱 🍫 8 🍬 "bones" 🍭🍬
🍭
```


### Operators


```
4 + 5
4 + "boo!"
"spooky " + "skeletons!"
🍫 1 🍬 2 🍬 3 🍭 + 4
🍫 1 🍬 2 🍬 3 🍭 + 🍫 4 🍬 5 🍭

4.0 / 2
2 * 3
4 - 1
🌝 == 🌝
🌝 ⚡= 🌚
🌝 == ⚡🌚
4 > 3
3 < 4
4 >= 4
4 <= 4
```

## Control Flow

### Conditionals


There are but two branching structures in spookylang - the humble if, and the mighty if/else.

If is delimited by a perturbed emoji-thing beside a statement block (the skulls remember) and else is delimited by a rather concerned emoji expression beside a statement block.

```
😨 👿 skeletons_are_scary 😈 💀
  print_and_then_scream👿 "scary if statement!" 😈🎃
☠️ 😰 💀
  print_and_then_scream👿 "ahhhhhhhh it's an else statement!" 😈🎃
☠️
```

### Loops


Here at Spooky Labs, we have a lot of respect for your time and energy, so we only made one kind of loop. You gotta put a bat near an expression and a statement block.


```
  🤡 scariest_accumulator🎃 
  scariest_accumulator 😱 0🎃
  🦇 👿 scariest_accumulator < 10 😈 💀
    scariest_accumulator 😱 scariest_accumulator + 1🎃
  ☠️
```

## Standard Library


Spookylang's commitment to scares n' thrills is rivaled only by the laziness of its researchers. But then, what is scarier than a minimalist standard library? Ah, now you understand.

### `print_and_then_scream`


`print_and_then_scream` takes an expression, coerces it to a string, and then screams it into the nether of the standard output string. To preserve Spookylang's spooky complete invariant, `print_and_then_scream` will crash your program if passed an expression that our Artificial Intelligence Hardcoded List of Regular Expressions fails to recognize.

```
print_and_then_scream👿"i'm screaming!"😈🎃 -> "i'm screaming! Ahhhhhhhhhh!"
print_and_then_scream👿"this is not scary"😈🎃 -> nice try, we just crashed your program.
```

### `spooky_input`


`spooky_input` blocks program execution until the user inputs in a line. It returns the value of the user input. Guess what - if your user doesn't input in something spooky... We crash it! We crash your program! Spooky complete!


```
scariest_accumulator 😱 spooky_input🎃

user: *oh I guess I'll just input in my favorite bird!* Albatross

your program: ☠️

```

### `scary_length`


`scary_length` is nothing special. It takes a list or string and returns its length. Just good and honest work. Man, god bless `scary_length`.


```
  🤡 my_length_is_three_and_im_scary🎃 
  my_length_is_three_and_im_scary 😱 "123"🎃
  scary_length👿 my_length_is_three_and_im_scary😈🎃 -> 3, I love you scary-length
```

### `skeleton_keys`


`skeleton_keys` returns a list of all the keys in the passed-in dictionary. In a world where we had more than one kind of loop, `skeleton_keys` could hang up its oars and take a well-deserved rest but these researchers are something else man.


```
  sample_haunted_object 😱 🍫
    "aaaaah!" 😱 ⚰️🍬
    "blood scream!" 😱 ⚰️🍬
    "guts" 😱 ⚰️🍬
    "im scared!" 😱 ⚰️🍬
  🍭🎃
  scariest_keys 😱 skeleton_keys👿sample_haunted_object😈🎃 -> ['aaaaah!, 'blood scream!', 'guts', 'im scared!']
```

