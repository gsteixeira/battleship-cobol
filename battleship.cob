        IDENTIFICATION DIVISION.
            PROGRAM-ID. Battleship.
            AUTHOR. Gustavo Selbach Teixeira (gsteixei@gmail.com).
            DATE-WRITTEN. 2021-09-19.
            *> The classical game Battleship in Cobol
        DATA DIVISION.
            WORKING-STORAGE SECTION.
                *> Program parameters
                01 GAME_MODE    PIC 9 VALUE 0. *> 0 game, 1 test, 2 demo
                01 NOFOG_MODE   PIC 9 VALUE 0.
                01 FIRE_ON_HIT  PIC 9 VALUE 0.
                *> constants
                01 WATER        CONSTANT GLOBAL as '.'.
                01 SHIP_SPRITE  CONSTANT GLOBAL as "#".
                01 EXPLOSION    CONSTANT GLOBAL as "*".
                01 MISSED       CONSTANT GLOBAL as "0".
                01 BOARD_WIDTH  CONSTANT GLOBAL as 10.
                01 BOARD_HEIGTH CONSTANT GLOBAL as 9.
                01 SHIP_NUMBER  CONSTANT GLOBAL as 5.
                01 TILE_WITH    CONSTANT as 2.
                01 HUMAN_PLAYER CONSTANT as 1.
                01 CPU_PLAYER   CONSTANT as 2.
                *> score
                01 SAFE_SHIP_BONUS CONSTANT as 175.
                01 SCORE_HIT    CONSTANT as 100.
                01 SCORE_MISS   CONSTANT as 5.
                *> colors
                01 black        CONSTANT AS 0.
                01 blue         CONSTANT AS 1.
                01 green        CONSTANT AS 2.
                01 cyan         CONSTANT AS 3.
                01 red          CONSTANT AS 4.
                01 magenta      CONSTANT AS 5.
                01 brown        CONSTANT AS 6.
                01 white        CONSTANT AS 7.
                *> Game
                01 GAME_OVER    PIC 9 VALUE 0.
                01 player_name  PIC X(10) VALUE SPACES.
                *> auxiliary vars
                01 auxiliary_vars.
                    05 datetime         PIC X(21).
                    05 seed             PIC S9(9) BINARY.
                    05 menu_opt         PIC X VALUE ZERO.
                    *> working registers
                    05 x  PIC 99 VALUE ZERO.
                    05 y  PIC 99 VALUE ZERO.
                    05 y_screen  PIC 99 VALUE ZERO.
                    05 i  PIC 99 VALUE ZERO.
                    05 j  PIC 99 VALUE ZERO.
                *> map letters to numbers a1, b2, c3...
                01 letters              PIC X(54)
                    VALUE "abcdefghijklmnopqrstuvwxyz".
                01 letter_numbers OCCURS 32 TIMES.
                    05 letter_number    PIC X VALUE SPACES.
                *> offset to show user / enemy boards
                01 screen_offset OCCURS 2 TIMES.
                    02 x_offset         PIC 99 VALUE 0.
                    02 y_offset         PIC 99 VALUE 0.
                *> auxiliary vars to helps put print at the right place
                01 screen_line          PIC 99 VALUE 1.
                01 screen_column        PIC 99 VALUE 1.
                *> used for players move
                01 guess_x              PIC 9 VALUE 1.
                01 guess_y              PIC 99 VALUE 1.
                01 guess_y_tx           PIC X VALUE SPACE.
                *> player references
                01 player               PIC 9 VALUE 1.
                01 enemy                PIC 9 VALUE 2.
                *> ship construction yard
                01 build_counter        PIC 99 VALUE ZERO.
                01 start_at_x           PIC 99.
                01 start_at_y           PIC 99.
                01 build_direction      PIC 9 VALUE ZERO.
                01 can_build            PIC 9 VALUE ZERO.
                *> struct used to instantiate a "ship class"
                01 build_ship_parameters.
                    05 ship_lenght          PIC 9.
                    05 ship_color           PIC 9.
                    05 ship_name            PIC X(10).
                *> struct that represent a "ship class"
                01 Ship_classes.
                    05 carrier.
                        10 class_lenght   PIC 9 VALUE 5.
                        10 class_color    PIC 9 VALUE green.
                        10 class_name     PIC X(10) VALUE "carrier".
                    05 battleship.
                        10 class_lenght   PIC 9 VALUE 4.
                        10 class_color    PIC 9 VALUE cyan.
                        10 class_name     PIC X(10) VALUE "battleship".
                    05 cruiser.
                        10 class_lenght   PIC 9 VALUE 3.
                        10 class_color    PIC 9 VALUE magenta.
                        10 class_name     PIC X(10) VALUE "cruiser".
                    05 submarine.
                        10 class_lenght   PIC 9 VALUE 3.
                        10 class_color    PIC 9 VALUE brown.
                        10 class_name     PIC X(10) VALUE "submarine".
                    05 destroyer.
                        10 class_lenght   PIC 9 VALUE 2.
                        10 class_color    PIC 9 VALUE white.
                        10 class_name     PIC X(10) VALUE "destroyer".
                *> struct used to instantiate a Tile
                01 current_tile.
                    05 curr_tile_sprite  PIC X VALUE SHIP_SPRITE.
                    05 curr_tile_color   PIC 9 VALUE 1.
                    05 curr_tile_class   PIC X(10) VALUE SPACES.
                *> game vars
                01 score_bonnus                 PIC 9999 VALUE ZERO.
                01 ship_idx                     PIC 9 VALUE ZERO.
                *> info about ships of the game, for each player
                01 players_ship_list GLOBAL     OCCURS 2 TIMES.
                    *> p_ship_count is life. when zero, the player dies
                    05 p_ship_count             PIC 9 VALUE ZERO.
                    05 p_score_data.
                        10 p_score              PIC 9999 VALUE ZERO.
                        10 p_score_hit_count    PIC 999 VALUE ZERO.
                        10 p_score_rounds_count PIC 999 VALUE ZERO.
                        10 p_score_since_last   PIC 999 VALUE ZERO.
                    05 p_ship_idx               PIC 9 VALUE ZERO.
                    05 p_ships OCCURS SHIP_NUMBER TIMES.
                        10 p_ship_damage        PIC 9 VALUE ZERO.
                        10 p_ship_class.
                            15 p_ship_class_lenght   PIC 9.
                            15 p_ship_class_color    PIC 9.
                            15 p_ship_class_name     PIC X(10).
                *> the map, the board where the game runs
                01 board GLOBAL.
                    05 player_board OCCURS 2 TIMES.
                        10 x_axis OCCURS 99 TIMES.
                            15 y_axis OCCURS 10 TIMES.
                                20 tile_data.
                                    25 tile PIC X VALUE '.'.
                                    25 tile_color PIC 9 VALUE blue.
                                    25 tile_class PIC X(10)
                                                        VALUE SPACES.
                    
            SCREEN SECTION.
                01 main_menu_screen.
                    05 BLANK SCREEN BACKGROUND-COLOR black
                                    FOREGROUND-COLOR white.
                    05 LINE 3 COLUMN 10 VALUE      "Battleship Cobol"
                                            FOREGROUND-COLOR green.
                    05 LINE PLUS 1 COLUMN 10 VALUE "================"
                                            FOREGROUND-COLOR cyan.
                    05 LINE PLUS 1 COLUMN 10 VALUE "1- New Game     ".
                    05 LINE PLUS 1 COLUMN 10 VALUE "2- Hall of Fame ".
                    05 LINE PLUS 1 COLUMN 10 VALUE "3- Demo ".
                    05 LINE PLUS 1 COLUMN 10 VALUE "4- Options      ".
                    05 LINE PLUS 1 COLUMN 10 VALUE "q- Quit         ".
                    05 LINE PLUS 1 COLUMN 10 VALUE "----------------".
                    *>
                    05 LINE PLUS 2 COLUMN 10 VALUE "___i_###===______".
                    05 LINE PLUS 1 COLUMN 10 VALUE "\##############/".
                    05 COLUMN 11 VALUE "--------------"
                                        FOREGROUND-COLOR blue.
                    05 COLUMN 1 VALUE "~~~~~~~~~"
                                        FOREGROUND-COLOR blue.
                    05 COLUMN 26 VALUE "~~~~~~~~~"
                                        FOREGROUND-COLOR blue.
                    05 LINE PLUS 3 COLUMN 25 USING menu_opt  AUTO-SKIP.

                01 options_menu_screen.
                    05 BLANK SCREEN BACKGROUND-COLOR black
                                    FOREGROUND-COLOR white.
                    05 LINE 3 COLUMN 10 VALUE      "Options"
                                        FOREGROUND-COLOR green.
                    05 LINE PLUS 1 COLUMN 10 VALUE "================"
                                        FOREGROUND-COLOR cyan.
                    05 LINE PLUS 1 COLUMN 10
                                            VALUE "1- Set player name:".
                    05 COLUMN PLUS 2 USING player_name
                                    FOREGROUND-COLOR brown.
                    05 LINE PLUS 1 COLUMN 10 VALUE "2- Hits turn red".
                    05 COLUMN PLUS 2 USING FIRE_ON_HIT
                                    FOREGROUND-COLOR blue.
                    05 LINE PLUS 1 COLUMN 10 VALUE "3- Run tests".
                    05 COLUMN PLUS 2 USING GAME_MODE
                                    FOREGROUND-COLOR blue.
                    05 LINE PLUS 1 COLUMN 10 VALUE "4- Go back".
                    05 LINE PLUS 1 COLUMN 10 VALUE "----------------".

                01 screen_game_over.
                    05 LINE 12 COLUMN 16 VALUE "  *** GAME OVER ***".
                    05 LINE 21 COLUMN 5
                                    VALUE " Press key to continue...".
                    *> 05 COLUMN PLUS 2 VALUE "<Enter>" BLINK.
                    05 COLUMN PLUS 2 USING guess_y_tx.

                01 score_screen.
                    05 LINE 1 COLUMN  1 VALUE "Player 1".
                    05 LINE 1 COLUMN 30 VALUE "Computer".
                    05 LINE 14 COLUMN 2 VALUE "ships:".
                    05 COLUMN PLUS 2 USING p_ship_count(HUMAN_PLAYER).
                    05 COLUMN 30 VALUE "ships:".
                    05 COLUMN PLUS 2 USING p_ship_count(CPU_PLAYER).
                    *>
                    05 LINE PLUS 1 COLUMN 2 VALUE "score:".
                    05 COLUMN PLUS 2 USING p_score(HUMAN_PLAYER).
                    05 COLUMN 30 VALUE "score:".
                    05 COLUMN PLUS 2 USING p_score(CPU_PLAYER).

                01 set_player_name_screen.
                    05 BLANK SCREEN BACKGROUND-COLOR black
                                    FOREGROUND-COLOR white.
                    05 LINE PLUS 5 COLUMN 5
                            VALUE "Please write your name".
                    05 COLUMN PLUS 2 USING player_name.

                01 user_input_screen.
                    05 LINE 21 COLUMN 5 VALUE "Your turn: " BLINK.
                    05 COLUMN PLUS 2 USING guess_y_tx AUTO-SKIP.
                    05 COLUMN PLUS 2 USING guess_x AUTO-SKIP.

            PROCEDURE DIVISION.
            main_menu.
                ACCEPT main_menu_screen.
                EVALUATE menu_opt
                    WHEN 1
                        MOVE 0 TO NOFOG_MODE
                        MOVE 0 TO GAME_MODE
                        PERFORM game_start
                    WHEN 2
                        CALL "Hall_of_fame" USING 0
                    WHEN 3
                        MOVE 1 TO NOFOG_MODE
                        MOVE 2 TO GAME_MODE
                        PERFORM game_start
                    WHEN 4
                        PERFORM menu_options
                    WHEN "q"
                        STOP RUN
                    WHEN OTHER
                        MOVE 0 TO NOFOG_MODE
                        MOVE 0 TO GAME_MODE
                        PERFORM game_start
                END-EVALUATE.
                MOVE 0 TO menu_opt.
                GO TO main_menu.
                *> EXIT.
            STOP RUN.

            menu_options.
                DISPLAY options_menu_screen.
                ACCEPT menu_opt AUTO-SKIP
                EVALUATE menu_opt
                    WHEN 1
                        ACCEPT set_player_name_screen
                    WHEN 2
                        IF FIRE_ON_HIT = 0 THEN
                            MOVE 1 TO FIRE_ON_HIT
                        ELSE
                            MOVE 0 TO FIRE_ON_HIT
                        END-IF
                    WHEN 3
                        MOVE 1 TO NOFOG_MODE
                        MOVE 1 TO GAME_MODE
                        PERFORM game_start
                    WHEN 4
                        PERFORM main_menu
                END-EVALUATE.
                MOVE 0 TO menu_opt.
                GO TO menu_options.
                EXIT.

            game_start.
                PERFORM init_parameters.
                DISPLAY " " BLANK SCREEN BACKGROUND-COLOR black
                                        FOREGROUND-COLOR white.
                *> reset enemy's AI state.
                CALL "bot_ai" USING 0.
                *> screen offset for players
                MOVE 2 TO x_offset(HUMAN_PLAYER)
                MOVE 2 TO y_offset(HUMAN_PLAYER)
                MOVE 2 TO x_offset(CPU_PLAYER)  *> vertical
                MOVE 30 TO y_offset(CPU_PLAYER) *> horizontal
                *> create the player's ships
                PERFORM VARYING player FROM 1 BY 1 UNTIL player > 2
                    *> MOVE 0 TO p_
                    PERFORM zero_player_scores
                    MOVE 1 TO p_ship_idx(player)
                    MOVE 1 TO ship_idx
                    MOVE submarine TO build_ship_parameters
                    PERFORM build_ship
                    
                    MOVE destroyer TO build_ship_parameters
                    PERFORM build_ship
                
                    MOVE cruiser TO build_ship_parameters
                    PERFORM build_ship
                    
                    MOVE battleship TO build_ship_parameters
                    PERFORM build_ship
                    
                    MOVE carrier TO build_ship_parameters
                    PERFORM build_ship
                END-PERFORM.
                *> game main loop
                PERFORM game_loop.
                EXIT.

            game_loop.
                PERFORM show_board.
                PERFORM get_user_move.
                PERFORM get_enemy_move.
                IF GAME_OVER <> 1 THEN
                    *> never miss a chance to use GOTO :)
                    GO TO game_loop.
                PERFORM do_game_over.
                EXIT.

            *> show screen
            show_board.
                PERFORM VARYING player FROM 1 BY 1 UNTIL player > 2
                    *> DISPLAY player AT LINE 1 COLUMN y_offset(player)
                    PERFORM VARYING x FROM 1 BY 1 UNTIL x > BOARD_HEIGTH
                        ADD x TO x_offset(player) GIVING screen_line
                        DISPLAY x AT LINE screen_line COLUMN 1
                        DISPLAY x AT LINE screen_line COLUMN 29
                        
                        PERFORM VARYING y FROM 1 BY 1
                                            UNTIL y > BOARD_WIDTH
                            MULTIPLY y BY TILE_WITH GIVING y_screen
                            ADD y_screen TO y_offset(player)
                                                GIVING screen_column
                            DISPLAY letter_numbers(y) AT LINE 2
                                                COLUMN screen_column
                            MOVE tile_data(player, x, y) TO current_tile
                            IF player = 2
                                    AND curr_tile_sprite = SHIP_SPRITE
                                    AND NOFOG_MODE = 0 THEN
                                DISPLAY WATER
                                    AT LINE screen_line
                                                COLUMN screen_column
                                    WITH FOREGROUND-COLOR blue
                                END-DISPLAY
                            ELSE
                                DISPLAY curr_tile_sprite
                                    AT LINE screen_line
                                                COLUMN screen_column
                                    WITH FOREGROUND-COLOR
                                                curr_tile_color
                                END-DISPLAY
                            END-IF
                        END-PERFORM
                    END-PERFORM
                END-PERFORM.
                PERFORM show_score.
                EXIT.

            *> Show score information
            show_score.
                DISPLAY score_screen.
                MOVE 16 TO screen_line.
                PERFORM VARYING ship_idx FROM 1 BY 1
                                            UNTIL ship_idx > SHIP_NUMBER
                    PERFORM VARYING player FROM 1 BY 1 UNTIL player > 2
                        MOVE p_ship_class(player, ship_idx)
                                    TO build_ship_parameters
                        MOVE y_offset(player) TO y
                        MOVE p_ship_damage(player, ship_idx)
                            TO build_counter
                        PERFORM VARYING i FROM 1 BY 1
                                                UNTIL i > ship_lenght
                            IF build_counter > 0 THEN
                                MOVE EXPLOSION TO curr_tile_sprite
                                MOVE red TO curr_tile_color
                                SUBTRACT 1 FROM build_counter
                            ELSE
                                MOVE SHIP_SPRITE TO curr_tile_sprite
                                MOVE ship_color TO curr_tile_color
                            END-IF
                            DISPLAY curr_tile_sprite AT LINE screen_line
                                COLUMN y
                                WITH FOREGROUND-COLOR curr_tile_color
                            ADD 1 TO y
                        END-PERFORM
                        *> ADD 12 to y_offset(player) giving y
                        *> DISPLAY p_ship_damage(player, ship_idx)
                        *>      AT LINE screen_line COLUMN y
                        *>      WITH FOREGROUND-COLOR red
                    END-PERFORM
                    DISPLAY ship_idx AT LINE screen_line COLUMN 45
                            WITH FOREGROUND-COLOR ship_color
                    DISPLAY ship_name AT LINE screen_line COLUMN 47
                            WITH FOREGROUND-COLOR ship_color
                    ADD 1 TO screen_line
                END-PERFORM.
                EXIT.

            *> get human player input
            get_user_move.
                MOVE HUMAN_PLAYER TO player.
                MOVE CPU_PLAYER TO enemy.
                IF GAME_MODE = 0 THEN
                    ACCEPT user_input_screen END-ACCEPT
                    IF guess_y_tx = "q" THEN
                        DISPLAY "Player quit."
                        MOVE 1 TO GAME_OVER
                    END-IF
                    *> z is a cheat to remove the fog from enemy board
                    IF guess_y_tx = "z" THEN
                        IF NOFOG_MODE = 1 THEN
                            MOVE 0 TO NOFOG_MODE
                        ELSE
                            MOVE 1 TO NOFOG_MODE
                        END-IF
                    END-IF
                    CALL "letters_to_numbers" USING guess_y_tx
                                                GIVING guess_y
                ELSE
                    *> CALL "bot_ai" USING enemy, guess_x, guess_y
                    COMPUTE guess_x = (FUNCTION RANDOM
                                        * (BOARD_HEIGTH - 1 + 1) + 1)
                    COMPUTE guess_y = (FUNCTION RANDOM
                                        * (BOARD_WIDTH - 1 + 1) + 1)
                    IF GAME_MODE = 2 THEN
                        CALL "CBL_OC_NANOSLEEP" USING 500000000
                    END-IF
                END-IF.
                PERFORM resolve_move.
                EXIT.

            *> Get AI move for Enemy
            get_enemy_move.
                MOVE CPU_PLAYER TO player.
                MOVE HUMAN_PLAYER TO enemy.
                CALL "bot_ai" USING enemy, guess_x, guess_y.
                *> prevent repeated shots
                IF tile(enemy, guess_x, guess_y) = MISSED
                    OR tile(enemy, guess_x, guess_y) = EXPLOSION
                    *> never, ever miss a chance to use GOTO :)
                    GO TO get_enemy_move
                END-IF.

                PERFORM resolve_move.
                MOVE 1 TO guess_x.
                MOVE 1 TO guess_y.
                EXIT.

            *> Check if player move resulted in a hit
            resolve_move.
                *> fix bad input
                IF guess_y < 1
                    OR guess_y > BOARD_WIDTH
                    OR guess_x < 1
                    OR guess_x > BOARD_HEIGTH
                    THEN
                        MOVE 1 TO guess_x
                        MOVE 1 TO guess_y
                END-IF
                MOVE tile_data(enemy, guess_x, guess_y) TO current_tile.
                IF curr_tile_sprite = SHIP_SPRITE THEN
                    *> HIT!!
                    MOVE EXPLOSION TO curr_tile_sprite
                    PERFORM damage_ship
                    IF FIRE_ON_HIT = 1 THEN
                        MOVE red TO curr_tile_color
                    END-IF
                    ADD 1 TO p_score_hit_count(player)
                    COMPUTE p_score(player) = (
                            SCORE_HIT + p_score(player)
                            - (p_score_since_last(player) * SCORE_MISS))
                    MOVE 0 TO p_score_since_last(player)
                ELSE
                    *> MISS
                    MOVE MISSED TO curr_tile_sprite
                    ADD 1 TO p_score_since_last(player)
                END-IF
                ADD 1 TO p_score_rounds_count(player)
                MOVE current_tile TO tile_data(enemy, guess_x, guess_y).
                EXIT.

            *> store damage information
            damage_ship.
                EVALUATE curr_tile_class
                    WHEN "submarine"
                        MOVE 1 TO ship_idx
                        MOVE 3 TO ship_lenght
                    WHEN "destroyer"
                        MOVE 2 TO ship_idx
                        MOVE 2 TO ship_lenght
                    WHEN "cruiser"
                        MOVE 3 TO ship_idx
                        MOVE 3 TO ship_lenght
                    WHEN "battleship"
                        MOVE 4 TO ship_idx
                        MOVE 4 TO ship_lenght
                    WHEN "carrier"
                        MOVE 5 TO ship_idx
                        MOVE 5 TO ship_lenght
                END-EVALUATE.
                ADD 1 TO p_ship_damage(enemy, ship_idx).
                IF p_ship_damage(enemy, ship_idx) >= ship_lenght THEN
                    SUBTRACT 1 FROM p_ship_count(enemy)
                END-IF.
                *> check for game over
                IF p_ship_count(HUMAN_PLAYER) <= 0
                    OR p_ship_count(CPU_PLAYER) <= 0 THEN
                    MOVE 1 TO GAME_OVER
                END-IF.
                EXIT.

            *> get random parameters to build a new ship
            get_build_parameters.
                COMPUTE start_at_x = FUNCTION RANDOM * (10 - 1 + 1) + 1
                COMPUTE start_at_y = FUNCTION RANDOM * (10 - 1 + 1) + 1
                COMPUTE build_direction = (FUNCTION RANDOM
                                            * (1 - 0 + 1) + 0)
                EXIT.

            *> look for a random place until it finds room for a ship
            find_place_to_build_ship.
                PERFORM get_build_parameters.
                PERFORM check_space.
                IF can_build <> 1 THEN
                    *> Never miss a chance to use GOTO! :)
                    GO TO find_place_to_build_ship
                END-IF.
                EXIT.

            *> build a ship at a given place
            build_ship.
                PERFORM find_place_to_build_ship.
                PERFORM draw_ship.
                *> store ship information
                MOVE p_ship_idx(player) TO ship_idx.
                MOVE build_ship_parameters
                    TO p_ship_class(player, ship_idx).
                ADD 1 TO p_ship_idx(player).
                ADD 1 TO p_ship_count(player).
                EXIT.

            *> draw a ship on the map
            draw_ship.
                MOVE 0 TO build_counter.
                MOVE start_at_x TO x.
                MOVE start_at_y TO y.
                PERFORM VARYING i FROM 1 BY 1
                                UNTIL i > ship_lenght
                    MOVE SHIP_SPRITE TO curr_tile_sprite
                    MOVE ship_color TO curr_tile_color
                    MOVE ship_name TO curr_tile_class
                    MOVE current_tile TO tile_data(player, x, y)
                    IF build_direction = 1 THEN
                        ADD 1 TO x
                    ELSE
                        ADD 1 TO y
                    END-IF
                    ADD 1 TO build_counter
                END-PERFORM.
                EXIT.

            *> check if there is room to build a ship at a given place
            check_space.
                MOVE 0 TO build_counter.
                MOVE start_at_x TO x.
                MOVE start_at_y TO y.
                MOVE 1 TO can_build.
                PERFORM VARYING i FROM 1 BY 1
                                UNTIL i > ship_lenght
                                    OR can_build = 0
                    IF x >= 10 OR y >= 10 THEN
                        MOVE 0 TO can_build
                    ELSE
                        IF tile(player, x, y) NOT EQUAL WATER THEN
                            MOVE 0 TO can_build
                        ELSE
                            IF build_direction = 1 THEN
                                ADD 1 TO x
                            ELSE
                                ADD 1 TO y
                            END-IF
                        END-IF
                    END-IF
                END-PERFORM.
                EXIT.

            zero_player_scores.
                *> zero scoress
                MOVE 0 TO GAME_OVER.
                MOVE 0 TO p_score(player).
                MOVE 0 TO p_score_hit_count(player).
                MOVE 0 TO p_score_rounds_count(player).
                MOVE 0 TO p_score_since_last(player).
                MOVE 0 TO p_ship_count(player).
                MOVE 1 TO p_ship_idx(player).
                *> zero map
                PERFORM VARYING x FROM 1 BY 1 UNTIL x > BOARD_HEIGTH
                    PERFORM VARYING y FROM 1 BY 1 UNTIL y > BOARD_WIDTH
                        MOVE WATER TO curr_tile_sprite
                        MOVE blue TO curr_tile_color
                        MOVE SPACES TO curr_tile_class
                        MOVE current_tile TO tile_data(player, x, y)
                    END-PERFORM
                END-PERFORM.
                *> zero ships
                PERFORM VARYING ship_idx FROM 1 BY 1
                                            UNTIL ship_idx > SHIP_NUMBER
                    MOVE 0 TO ship_lenght
                    MOVE black TO ship_color
                    MOVE SPACES TO ship_name
                    MOVE build_ship_parameters
                        TO p_ship_class(player, ship_idx)
                    MOVE 0 TO p_ship_damage(player, ship_idx)
                END-PERFORM.
                EXIT.

            *> initialize some game parameters
            init_parameters.
                *> seed random generator
                MOVE FUNCTION CURRENT-DATE TO datetime.
                MOVE datetime(8:9) TO seed.
                COMPUTE x = FUNCTION RANDOM(seed).
                *> map letters to numbers a1, b2, c3...
                PERFORM VARYING i FROM 1 BY 1 UNTIL i > 26
                    COMPUTE j = i + 1
                    MOVE letters(i:j) TO letter_numbers(i)
                END-PERFORM.
                EXIT.

            *> Ends the game and show winners
            do_game_over.
                MOVE 1 TO NOFOG_MODE.
                PERFORM show_board.
                *> show scores
                PERFORM show_score.
                IF p_ship_count(HUMAN_PLAYER) > p_ship_count(CPU_PLAYER)
                    MOVE HUMAN_PLAYER TO player
                    MOVE 16 TO start_at_y
                ELSE
                    MOVE CPU_PLAYER TO player
                    MOVE 45 TO start_at_y
                END-IF.
                *> calculate extra bonus
                IF p_score(player) > 800 THEN
                    COMPUTE score_bonnus = (
                                p_ship_count(player) * SAFE_SHIP_BONUS)
                    ADD score_bonnus TO p_score(player)
                ELSE
                    MOVE 0 TO score_bonnus
                END-IF.
                *> show winner congratulations
                DISPLAY "WINNER!!" AT LINE 13 COLUMN y_offset(player)
                        FOREGROUND-COLOR cyan BLINK.
                ADD 10 TO y_offset(player) GIVING screen_column.
                DISPLAY "bonnus +" AT LINE 13 COLUMN screen_column.
                ADD 8 TO screen_column.
                DISPLAY score_bonnus AT LINE 13 COLUMN screen_column
                        FOREGROUND-COLOR green.
                ACCEPT screen_game_over.
                *> go to the hall of fame
                IF GAME_MODE = 0 THEN
                    IF player_name = SPACES THEN
                        ACCEPT set_player_name_screen
                    END-IF
                    CALL "Hall_of_fame" USING p_score(HUMAN_PLAYER),
                                              p_score(CPU_PLAYER),
                                              player_name
                        END-CALL
                END-IF.
                EXIT.

            *> =======================================================
        
            IDENTIFICATION DIVISION.
                PROGRAM-ID. bot_ai.
                *> Controls enemy's "pseudo" AI.
                *>   There are 4 strategies, and a criteria to choose
                *>   the right strategy for the situation.
                *>   1- Shoots randomly.
                *>   2- Try shots at a paced interval
                *>   3- Shoots methodically around known hit points
                *>   4- Shoots randomly around known hit points.
                *>   When we don't know where the ships are, do 1 and 2.
                *>   After that, and while we don't know where EVERY
                *>   some ships are known but not all, do any strategy
                *>   When we know where ALL the ships are, do 2 or 3.
                *> This is Cobol OO in action!
            DATA DIVISION.
                WORKING-STORAGE SECTION.
                    01 GIVE_UP constant as 9.
                    01 general_vars.
                        05 x  PIC 99 VALUE ZERO.
                        05 y  PIC 99 VALUE ZERO.
                        05 i  PIC 99 VALUE ZERO.
                        05 j  PIC 99 VALUE ZERO.
                    01 narrow_search.
                        05 start_x PIC 99 VALUE ZERO.
                        05 start_y PIC 99 VALUE ZERO.
                        05 end_x PIC 99 VALUE ZERO.
                        05 end_y PIC 99 VALUE ZERO.
                        01 step_y PIC 99 VALUE 1.
                    01 strategy_data.
                        05 strategy       PIC 9 VALUE ZERO.
                        05 counter        PIC 99 VALUE ZERO.
                        05 is_valid_guess PIC 9 VALUE ZERO.
                        *> used to define the right strategy
                        05 min_s   PIC 9 VALUE 1.
                        05 max_s   PIC 9 VALUE 4.
                        *> intelligence
                        05 know_one PIC 9 VALUE 0.
                        05 know_all PIC 9 VALUE 0.
                LINKAGE SECTION.
                    01 enemy PIC 9 VALUE 1.
                    01 guess_x PIC 9 VALUE 1.
                    01 guess_y PIC 99 VALUE 1.
                SCREEN SECTION.
                    01 debug_ai.
                        05 LINE 12 COLUMN 2 USING strategy.
                        05 COLUMN PLUS 5 USING know_one.
                        05 COLUMN PLUS 5 USING know_all.
                        05 LINE PLUS 1 COLUMN 2 USING x.
                        05 COLUMN PLUS 5 USING y.
                        05 LINE 13 COLUMN 15 USING guess_x.
                        05 COLUMN 20 USING guess_y.

            PROCEDURE DIVISION USING enemy, guess_x, guess_y.
                start_ai_process.
                    *> if called with enemy = 0, reset state
                    IF enemy = 0 THEN
                        PERFORM reset_state
                        GOBACK
                    END-IF.
                    MOVE 0 TO counter.
                    PERFORM gather_intelligence.
                    MOVE 0 TO is_valid_guess.
                    PERFORM UNTIL is_valid_guess = 1
                        COMPUTE strategy = (FUNCTION RANDOM
                                        * (max_s - min_s + 1) + min_s)
                        EVALUATE strategy
                            WHEN 1
                                PERFORM do_random_guess
                            WHEN 2
                                PERFORM step_search
                            WHEN 3
                                PERFORM shoot_near_known_ships
                            WHEN 4
                                PERFORM shoot_near_known_ships
                            WHEN OTHER
                                PERFORM do_random_guess
                        END-EVALUATE
                        PERFORM check_valid_guess
                        *> DISPLAY debug_ai
                    END-PERFORM.
                GOBACK.

                *> check game data to make decisions on strategy
                gather_intelligence.
                    *> check if we know location of at least one damaged ship
                    IF know_one = 0 THEN
                        PERFORM VARYING i FROM 1 BY 1
                                                UNTIL i > SHIP_NUMBER
                                                      OR know_one = 1
                            IF p_ship_damage(enemy, i) > 0 THEN
                                MOVE 1 TO know_one
                            END-IF
                        END-PERFORM
                    END-IF
                    *> check if we know the location of all of them
                    IF know_all = 0 AND know_one = 1 THEN
                        MOVE 1 TO know_all
                        PERFORM VARYING i FROM 1 BY 1
                                                UNTIL i > SHIP_NUMBER
                            IF p_ship_damage(enemy, i) <= 0 THEN
                                MOVE 0 TO know_all
                            END-IF
                        END-PERFORM
                    END-IF
                    EVALUATE TRUE
                        *> if we know where all are, we dont search randomly
                        WHEN know_all = 1
                            MOVE 3 TO min_s
                            MOVE 4 TO max_s
                        *> all strategies while we dont know where they are
                        WHEN know_one = 1 AND know_all = 0
                            MOVE 1 TO min_s
                            MOVE 4 TO max_s
                        *> if we havent hit anything, keep trying randomly
                        WHEN know_one = 0
                            MOVE 1 TO min_s
                            MOVE 2 TO max_s
                    END-EVALUATE.
                    EXIT.

                *> Prevents shooting same place twice
                check_valid_guess.
                    IF tile(enemy, guess_x, guess_y) = MISSED
                        OR tile(enemy, guess_x, guess_y) = EXPLOSION
                        MOVE 0 TO is_valid_guess
                    ELSE
                        MOVE 1 TO is_valid_guess
                    END-IF.
                    EXIT.

                *> try a shot anywhere on map. Keep trying until GIVE_UP
                do_random_guess.
                    COMPUTE guess_x = (FUNCTION RANDOM
                                        * (BOARD_HEIGTH - 1 + 1) + 1)
                    COMPUTE guess_y = (FUNCTION RANDOM
                                        * (BOARD_WIDTH - 1 + 1) + 1)
                    PERFORM check_valid_guess.
                    ADD 1 TO counter.
                    IF is_valid_guess = 0 AND counter < GIVE_UP THEN
                        GO TO do_random_guess
                    END-IF.
                    EXIT.

                *> try shots at a steped interval
                step_search.
                    MOVE 0 TO x.
                    MOVE step_y TO y.
                    PERFORM UNTIL x > BOARD_HEIGTH
                                    OR is_valid_guess = 1
                        MOVE step_y TO y
                        ADD 1 TO x
                        PERFORM UNTIL y > BOARD_WIDTH
                                        OR is_valid_guess = 1
                            IF tile(enemy, x, y) <> MISSED
                                AND tile(enemy, x, y) <> EXPLOSION THEN
                                    MOVE x TO guess_x
                                    MOVE y TO guess_y
                                    MOVE 1 TO is_valid_guess
                            END-IF
                            ADD 3 TO y
                        END-PERFORM
                    END-PERFORM.
                    IF x > BOARD_HEIGTH THEN
                        ADD 1 TO step_y
                    END-IF.
                    EXIT.

                shoot_near_known_ships.
                    *> look around, if find a HIT, we search around it
                    PERFORM VARYING x FROM 1 BY 1 UNTIL x > BOARD_HEIGTH
                                                        OR is_valid_guess = 1
                        PERFORM VARYING y FROM 1 BY 1 UNTIL y > BOARD_WIDTH
                                                        OR is_valid_guess = 1
                            IF tile(enemy, x, y) = EXPLOSION THEN
                                PERFORM get_narrow_search_params
                                IF strategy = 2 THEN
                                    PERFORM do_narrow_guess
                                ELSE
                                    PERFORM do_narrow_search
                                END-IF
                            END-IF
                        END-PERFORM
                    END-PERFORM.
                    EXIT.

                *> try a shot in a narrow area. Keep trying until GIVE_UP
                do_narrow_guess.
                    COMPUTE guess_x = (FUNCTION RANDOM
                                        * (end_x - start_x + 1)
                                            + start_x)
                    COMPUTE guess_y = (FUNCTION RANDOM
                                        * (end_x - start_x + 1)
                                            + start_x)
                    PERFORM check_valid_guess.
                    ADD 1 TO counter.
                    IF is_valid_guess = 0 AND counter < GIVE_UP THEN
                        GO TO do_narrow_guess
                    END-IF.
                    EXIT.

                do_narrow_search.
                    PERFORM VARYING i FROM start_x
                                        BY 1 UNTIL i > end_x
                                                OR is_valid_guess = 1
                        PERFORM VARYING j FROM start_y
                                    BY 1 UNTIL j > end_y
                                            OR is_valid_guess = 1
                            IF tile(enemy, i, j) = WATER
                                OR tile(enemy, i, j) = SHIP_SPRITE THEN
                                    MOVE i TO guess_x
                                    MOVE j TO guess_y
                                    *> MOVE 1 TO is_valid_guess
                                    PERFORM check_valid_guess
                                    *> GOBACK
                            END-IF
                        END-PERFORM
                    END-PERFORM.
                    EXIT.

                *> set parameters to search around a known hit.
                get_narrow_search_params.
                    *> where search starts
                    COMPUTE start_x = x - 1
                    IF start_x < 1 THEN
                        MOVE 1 TO start_x
                    END-IF
                    COMPUTE start_y = y - 1
                    IF start_y < 1 THEN
                        MOVE 1 TO start_y
                    END-IF
                    *> where search ends
                    COMPUTE end_x = x + 1
                    IF end_x > BOARD_HEIGTH THEN
                        MOVE BOARD_HEIGTH TO end_x
                    END-IF
                    COMPUTE end_y = y + 1
                    IF end_y > BOARD_WIDTH THEN
                        MOVE BOARD_WIDTH TO end_y
                    END-IF
                    EXIT.

                *> reset state of AI for the next round
                reset_state.
                    MOVE 0 TO know_one.
                    MOVE 0 TO know_all.
                    MOVE 1 TO min_s.
                    MOVE 4 TO max_s.
                    MOVE 0 TO counter.
                    MOVE 1 TO step_y.
                    EXIT.
            END PROGRAM bot_ai.
        END PROGRAM Battleship.
        *> ========================================================

        IDENTIFICATION DIVISION.
            PROGRAM-ID. letters_to_numbers.
            *> converts a char letter to the correspondent number
            *>     a 1, b 2, c 3,...
        DATA DIVISION.
            WORKING-STORAGE SECTION.
                01 cached           PIC 9 VALUE 0.
                01 letters          PIC X(54)
                    VALUE "abcdefghijklmnopqrstuvwxyz".
                01 letter_list   OCCURS 32 TIMES.
                    05 the_letter   PIC X VALUE SPACES.
            LOCAL-STORAGE SECTION.
                01 i                PIC 99 VALUE ZEROS.
                01 j                PIC 99 VALUE ZEROS.
            LINKAGE SECTION.
                01 char_arg PIC X.
        PROCEDURE DIVISION USING char_arg.
            IF cached = 0 THEN
                PERFORM initialize_cache
            END-IF.
            PERFORM VARYING i FROM 1 BY 1 UNTIL i > 26
                IF the_letter(i) = char_arg THEN
                    GOBACK RETURNING i
                END-IF
            END-PERFORM.
            GOBACK.
            initialize_cache.
                PERFORM VARYING i FROM 1 BY 1 UNTIL i > 26
                    COMPUTE j = i + 1
                    MOVE letters(i:j) TO letter_list(i)
                END-PERFORM.
                MOVE 1 TO cached.
                EXIT.
        END PROGRAM letters_to_numbers.
        *> ========================================================

        IDENTIFICATION DIVISION.
            PROGRAM-ID. Hall_of_fame.
            AUTHOR. Gustavo Selbach Teixeira.
            *> a persisted rank of top players and their scores
            ENVIRONMENT DIVISION.
                INPUT-OUTPUT SECTION.
                    FILE-CONTROL.
                        SELECT csv_file ASSIGN TO csv_file_name
                            ORGANIZATION IS LINE SEQUENTIAL
                            FILE STATUS IS file_status.
            DATA DIVISION.
                FILE SECTION.
                    FD  csv_file.
                        01 line_record  PIC X(32) VALUE SPACES.
                WORKING-STORAGE SECTION.
                    01 MAX_RECORDS  CONSTANT AS 10.
                    01 ALIGN_COLUMN CONSTANT AS 12.
                    *> colors
                    01 black        CONSTANT AS 0.
                    01 blue         CONSTANT AS 1.
                    01 green        CONSTANT AS 2.
                    01 cyan         CONSTANT AS 3.
                    01 red          CONSTANT AS 4.
                    01 magenta      CONSTANT AS 5.
                    01 brown        CONSTANT AS 6.
                    01 white        CONSTANT AS 7.
                    *> date manipulation
                    01 date_time.
                        02 dt_date.
                            03 year     PIC 9999 VALUE 0.
                            03 month    PIC 99 VALUE 0.
                            03 dday     PIC 99 VALUE 0.
                        02 dt_time.
                            03 hour     PIC 99 VALUE 0.
                            03 minute   PIC 99 VALUE 0.
                            03 second   PIC 99 VALUE 0.
                            03 milis    PIC 999 VALUE 0.
                    01 tx_date_time     PIC X(10) VALUE SPACES.
                    *> file manipulation
                    01 csv_data.
                        02 csv_file_name   PIC X(4096)
                                        VALUE 'hall_of_fame.csv'.
                        02 end_of_file     PIC 9 VALUE ZERO.
                    01 data_loaded PIC 9 VALUE 0.
                    01 file_status  PIC XX.
                    *> auxiliary
                    01 i PIC 99 VALUE 0.
                    01 j PIC 99 VALUE 0.
                    01 x PIC 99 VALUE 0.
                    01 y PIC 99 VALUE 0.
                    01 done PIC 9 VALUE 0.
                    01 insert_at PIC 99 VALUE 0.
                    01 score_record.
                        05 record_name PIC X(10) VALUE SPACES.
                        05 record_score PIC 9999 VALUE ZEROS.
                        05 record_date  PIC X(10) VALUE SPACES.

                    01 fame_counter     PIC 99 VALUE ZERO.
                    01 famous OCCURS 99 TIMES.
                        10 famous_name  PIC X(10) VALUE SPACES.
                        10 famous_score PIC 9999 VALUE ZEROS.
                        10 famous_date  PIC X(10) VALUE SPACES.

                LINKAGE SECTION.
                    01 arg_player_score     PIC 9999.
                    01 arg_player_name      PIC X(16).
                    01 arg_cpu_score        PIC 9999.

                SCREEN SECTION.
                    01 the_hall_of_fame_screen.
                        05 BLANK SCREEN.
                        05 LINE 2 COLUMN ALIGN_COLUMN
                                        VALUE "Hall of Fame".
                        05 LINE PLUS 1 COLUMN ALIGN_COLUMN
                            VALUE "===================================".

            PROCEDURE DIVISION USING arg_player_score,
                                     arg_cpu_score,
                                     arg_player_name.
                MOVE FUNCTION CURRENT-DATE TO date_time.
                STRING year"-"month"-"dday INTO tx_date_time.
                IF data_loaded = 0 THEN
                    PERFORM load_from_storage
                END-IF.
                IF arg_player_score > 0 THEN
                    MOVE arg_player_name TO record_name
                    MOVE arg_player_score TO record_score
                    MOVE tx_date_time TO record_date
                    PERFORM insert_record

                    MOVE "Computer" TO record_name
                    MOVE arg_cpu_score TO record_score
                    MOVE tx_date_time TO record_date
                    PERFORM insert_record
                    PERFORM dump_to_storage
                END-IF.
                PERFORM print_hall_of_fame.
            GOBACK.

                *> insert a record ordely
                insert_record.
                    MOVE 0 TO done.
                    PERFORM VARYING i FROM 1 BY 1 UNTIL i > MAX_RECORDS
                                                        OR done = 1
                        IF record_score = famous_score(i)
                            AND record_name = famous_name(i) THEN
                            MOVE 1 TO done
                        END-IF
                        IF record_score > famous_score(i) THEN
                            MOVE i TO insert_at
                            PERFORM move_list_behind
                            MOVE record_score TO famous_score(i)
                            MOVE record_name TO famous_name(i)
                            MOVE record_date TO famous_date(i)
                            ADD 1 TO fame_counter
                            MOVE 1 TO done
                        END-IF
                    END-PERFORM.
                    EXIT.

                *> make room for a new record
                move_list_behind.
                    PERFORM VARYING x FROM fame_counter BY -1
                                                    UNTIL x < insert_at
                        ADD 1 TO x GIVING j
                        MOVE famous(x) TO famous(j)
                    END-PERFORM.

                *> show the famed list
                print_hall_of_fame.
                    DISPLAY the_hall_of_fame_screen.
                    MOVE 4 TO j.
                    PERFORM VARYING i FROM 1 BY 1 UNTIL i > MAX_RECORDS
                                                OR famous_score(i) = 0
                        DISPLAY i AT LINE j COLUMN ALIGN_COLUMN
                                    WITH FOREGROUND-COLOR cyan
                        ADD 3 TO ALIGN_COLUMN GIVING x
                        DISPLAY famous_name(i) AT LINE j COLUMN x
                        ADD 19 TO ALIGN_COLUMN GIVING x
                        DISPLAY famous_score(i) AT LINE j COLUMN x
                                    WITH FOREGROUND-COLOR green
                        ADD 25 TO ALIGN_COLUMN GIVING x
                        DISPLAY famous_date(i) AT LINE j COLUMN x
                                    WITH FOREGROUND-COLOR brown
                        ADD 1 TO j
                    END-PERFORM.
                    ADD 1 TO j.
                    DISPLAY "Press any key"
                        AT LINE j COLUMN ALIGN_COLUMN BLINK.
                    ACCEPT OMITTED AT LINE 30.
                    EXIT.

                *> Dump hall of fame data to storage file
                dump_to_storage.
                    DISPLAY "Saving data to file..."
                    OPEN OUTPUT csv_file.
                    PERFORM VARYING i FROM 1 BY 1 UNTIL i > fame_counter
                                                OR famous_score(i) = 0
                                                OR i > MAX_RECORDS
                        STRING famous_name(i)";"
                               famous_score(i)";"
                               famous_date(i)
                            INTO line_record
                            END-STRING
                        WRITE line_record
                    END-PERFORM.
                    CLOSE csv_file.
                    EXIT.

                *> Load hall of fame data from storage file
                load_from_storage.
                    DISPLAY "Loading data from file..."
                    OPEN INPUT csv_file.
                    IF file_status = "00" THEN
                        MOVE 0 TO end_of_file
                        PERFORM UNTIL end_of_file = 1
                            READ csv_file
                                AT END MOVE 1 TO end_of_file
                            END-READ
                            IF end_of_file = 0 THEN
                                UNSTRING line_record DELIMITED BY ';'
                                    INTO record_name
                                         record_score
                                         record_date
                                    END-UNSTRING
                                PERFORM insert_record
                            END-IF
                        END-PERFORM
                        CLOSE csv_file
                        MOVE 1 TO data_loaded
                    END-IF.
                    EXIT.
        END PROGRAM Hall_of_fame.
