# Battleship Cobol

A Cobol implementation of the classical [Battleship](https://en.wikipedia.org/wiki/Battleship_(game)) game.

### How to install
You can also install it if you wish.

```bash
    git clone github.com/gsteixeira/battleship-cobol
    cd battleship-cobol
    make install
```

### How to run
```bash
    # If you just installed
    battleship
    # if you haven't installed
    make run
```

### How to play

    - Type your guess:
        - A Letter representing a Column.
        - A Number representing a Line.
            - Eg: a 3, b 5, c 7,....
    - The Computer player will make his move.
    - Repeat until someone loose all the ships.
    
### Installing Dependencies
    Battleship-cobol uses only the basic **gnucobol**. You need to install it in your distribuition packagin manager, or:
    
```bash
    make dep
    make
```
    
Have fun!