# Battleship Cobol

A Cobol implementation of the classical [Battleship](https://en.wikipedia.org/wiki/Battleship_(game)) game.

![Battleship Cobol - Game board](/images/board.png)


### How to install

Build:

```bash
    git clone github.com/gsteixeira/battleship-cobol
    cd battleship-cobol
    make
    make install
```
Or just download from Releases..

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

There is no dependency to play the game. You can download a compiled game from the [Releases](https://github.com/gsteixeira/battleship-cobol/releases) section, and play right away.

I you are want to build it, Battleship-cobol uses only the basic **gnucobol**. You need to install it in your distribuition packaging manager, or:
    
```bash
    make dep
    make
```

![Battleship Cobol - Main menu](/images/menu.png)


![Battleship Cobol - Victory](/images/victory.png)

Have fun!
