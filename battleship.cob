        IDENTIFICATION DIVISION.
            PROGRAM-ID. Battleship.
            AUTHOR. Gustavo Selbach Teixeira (gsteixei@gmail.com).
            DATE-WRITTEN. 2021-09-19.
            *> The classical game Battleship in Cobol
        DATA DIVISION.
            WORKING-STORAGE SECTION.
                *> Program parameters
                01 TEST_MODE    PIC 9 VALUE 0.
                01 CHEAT_MODE   PIC 9 VALUE 0.
                *> constants
                01 WATER        CONSTANT GLOBAL as '.'.
                01 SHIP_SPRITE  CONSTANT GLOBAL as "#".
                01 EXPLOSION    CONSTANT GLOBAL as "*".
                01 MISSED       CONSTANT GLOBAL as "0".
                01 BOARD_WIDTH  CONSTANT GLOBAL as 10.
                01 BOARD_HEIGTH CONSTANT GLOBAL as 9.
                01 SHIP_NUMBER  CONSTANT as 5.
                01 TILE_WITH    CONSTANT as 2.
                01 HUMAN_PLAYER CONSTANT as 1.
                01 CPU_PLAYER   CONSTANT as 2.
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
                *> auxiliary vars
                01 auxiliary_vars.
                    05 datetime         PIC X(21).
                    05 seed             PIC S9(9) BINARY.
                    *> working registers
                    05 x  PIC 99 VALUE ZERO.
                    05 y  PIC 99 VALUE ZERO.
                    05 y_screen  PIC 99 VALUE ZERO.
                    05 i  PIC 99 VALUE ZERO.
                    05 j  PIC 99 VALUE ZERO.
                *> map letters to numbers a1, b2, c3...
                01 letters              PIC X(26) 
                                    VALUE "abcdefghijklmnopqrstuvwxyz".
                01 letter_numbers OCCURS 26 TIMES.
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
                *> info about ships of the game, for each player
                01 ship_idx                     PIC 9 VALUE ZERO.
                01 players_ship_list OCCURS 2 TIMES. *> 2 players
                    *> p_ship_count is life. when zero, the player dies
                    05 p_ship_count             PIC 9 VALUE ZERO.
                    05 p_score_data.
                        10 p_score              PIC 999 VALUE ZERO.
                        10 p_score_hit_count    PIC 99 VALUE ZERO.
                        10 p_score_rounds_count PIC 99 VALUE ZERO.
                        10 p_score_since_last   PIC 99 VALUE ZERO.
                    05 p_ship_idx               PIC 9 VALUE ZERO.
                    05 p_ships OCCURS SHIP_NUMBER TIMES.
                        10 p_ship_damage             PIC 9 VALUE ZERO.
                        10 p_ship_class.
                            15 p_ship_class_lenght   PIC 9.
                            15 p_ship_class_color    PIC 9.
                            15 p_ship_class_name     PIC X(10).
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
                *> struct ised to instantiate a Tile
                01 current_tile.
                    05 curr_tile_sprite  PIC X VALUE SHIP_SPRITE.
                    05 curr_tile_color   PIC 9 VALUE 1.
                    05 curr_tile_class   PIC X(10) VALUE SPACES.
                *> the map, the board where the game runs
                01 board GLOBAL.
                    05 player_board OCCURS 2 TIMES.
                        10 x_axis OCCURS 99 TIMES.
                            15 y_axis OCCURS 10 TIMES.
                                20 tile_data.
                                    25 tile PIC X VALUE '.'.
                                    25 tile_color PIC 9 VALUE 1.
                                    05 tile_class PIC X(10) VALUE SPACES.
                    
            SCREEN SECTION.
                01 screen_game_over.
                    05 LINE 12 COLUMN 12 VALUE "*********************".
                    05 LINE 13 COLUMN 12 VALUE "*     GAME OVER     *".
                    05 LINE 14 COLUMN 12 VALUE "* the winner is:    *".
                    05 LINE 14 COLUMN 32 USING player.
                    05 LINE 15 COLUMN 12 VALUE "*********************".
                    05 LINE PLUS 1 COLUMN 10 VALUE " Press key to continue ".
                    05 COLUMN PLUS 2 VALUE "<Enter>" BLINK.
                    05 COLUMN PLUS 2 USING guess_y_tx.

                01 score_screen.
                    05 LINE 1 COLUMN  1 VALUE "Player 1".
                    05 LINE 1 COLUMN 30 VALUE "Player 2".
                    05 LINE 14 COLUMN 2 VALUE "ships:".
                    05 COLUMN PLUS 2 USING p_ship_count(HUMAN_PLAYER).
                    05 COLUMN 30 VALUE "ships:".
                    05 COLUMN PLUS 2 USING p_ship_count(CPU_PLAYER).
                    *>
                    05 LINE PLUS 1 COLUMN 2 VALUE "score:".
                    05 COLUMN PLUS 2 USING p_score(HUMAN_PLAYER).
                    05 COLUMN 30 VALUE "score:".
                    05 COLUMN PLUS 2 USING p_score(CPU_PLAYER).
                
                01 user_input_screen.
                    05 LINE 22 COLUMN 5 VALUE "Your turn: " BLINK.
                    05 column plus 2 using guess_y_tx AUTO-SKIP.
                    05 column plus 2 using guess_x AUTO-SKIP.

            PROCEDURE DIVISION.
            game_start.
                PERFORM initialize_game.
                *> screen offset for players
                MOVE 2 TO x_offset(HUMAN_PLAYER)
                MOVE 2 TO y_offset(HUMAN_PLAYER)
                MOVE 2 TO x_offset(CPU_PLAYER)  *> vertical
                MOVE 30 TO y_offset(CPU_PLAYER) *> horizontal
                *> create the player's ships
                PERFORM VARYING player FROM 1 BY 1 UNTIL player > 2
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
                game_loop.
                    PERFORM show_board.
                    PERFORM get_user_move.
                    PERFORM get_enemy_move.
                    IF GAME_OVER <> 1 THEN
                        *> never miss a chance to use GOTO :)
                        GO TO game_loop.
                    PERFORM do_game_over.
            STOP RUN.

            *> show screen
            show_board.
                PERFORM VARYING player FROM 1 BY 1 UNTIL player > 2
                    *> DISPLAY player AT LINE 1 COLUMN y_offset(player)
                    PERFORM VARYING x FROM 1 BY 1 UNTIL x > BOARD_HEIGTH
                        ADD x TO x_offset(player) GIVING screen_line
                        DISPLAY x AT LINE screen_line COLUMN 1
                        DISPLAY x AT LINE screen_line COLUMN 29
                        
                        PERFORM VARYING y FROM 1 BY 1 UNTIL y > BOARD_WIDTH
                            MULTIPLY y BY TILE_WITH GIVING y_screen
                            ADD y_screen TO y_offset(player) GIVING screen_column
                            DISPLAY letter_numbers(y) AT LINE 2 COLUMN screen_column
                            MOVE tile_data(player, x, y) TO current_tile
                            IF player = 2
                                    AND curr_tile_sprite = SHIP_SPRITE
                                    AND CHEAT_MODE = 0 THEN
                                DISPLAY WATER
                                    AT LINE screen_line COLUMN screen_column
                                    WITH FOREGROUND-COLOR blue
                                END-DISPLAY
                            ELSE
                                DISPLAY curr_tile_sprite
                                    AT LINE screen_line COLUMN screen_column
                                    WITH FOREGROUND-COLOR curr_tile_color
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
                        MOVE p_ship_damage(player, ship_idx) TO build_counter
                        PERFORM VARYING i FROM 1 BY 1 UNTIL i > ship_lenght
                            IF build_counter > 0 THEN
                                MOVE EXPLOSION TO curr_tile_sprite
                                MOVE red TO curr_tile_color
                                SUBTRACT 1 FROM build_counter
                            ELSE
                                MOVE SHIP_SPRITE TO curr_tile_sprite
                                MOVE ship_color TO curr_tile_color
                            END-IF
                            DISPLAY curr_tile_sprite AT LINE screen_line COLUMN y
                                WITH FOREGROUND-COLOR curr_tile_color
                            ADD 1 TO y
                        END-PERFORM
                        ADD 12 to y_offset(player) giving y
                        DISPLAY p_ship_damage(player, ship_idx)
                                AT LINE screen_line COLUMN y
                                WITH FOREGROUND-COLOR red
                    END-PERFORM
                    DISPLAY ship_idx AT LINE screen_line COLUMN 45
                            WITH FOREGROUND-COLOR curr_tile_color
                    DISPLAY ship_name AT LINE screen_line COLUMN 47
                            WITH FOREGROUND-COLOR curr_tile_color
                    ADD 1 TO screen_line
                END-PERFORM.
                EXIT.

            *> get human player input
            get_user_move.
                MOVE HUMAN_PLAYER TO player.
                MOVE CPU_PLAYER TO enemy.
                IF TEST_MODE = 0 THEN
                    ACCEPT user_input_screen END-ACCEPT
                    IF guess_y_tx = "q" THEN
                        DISPLAY "Player quit"
                        STOP RUN
                    END-IF
                    CALL "letters_to_numbers" USING guess_y_tx
                                                GIVING guess_y
                ELSE
                    PERFORM testing_guess
                END-IF.
                PERFORM resolve_move.
                EXIT.

            *> Get AI move from Enemy
            get_enemy_move.
                MOVE CPU_PLAYER TO player.
                MOVE HUMAN_PLAYER TO enemy.
                CALL "enemy_ai" USING enemy, guess_x, guess_y.
                *> prevent repeated shots
                IF tile(enemy, guess_x, guess_y) = '0'
                    OR tile(enemy, guess_x, guess_y) = '*'
                    *> never, ever miss a change for a GOTO :)
                    GO TO get_enemy_move
                END-IF.
                PERFORM resolve_move.
                MOVE 0 TO guess_x.
                MOVE 0 TO guess_y.
                EXIT.
            
            *> Check if player move resulted in a hit
            resolve_move.
                MOVE tile_data(enemy, guess_x, guess_y) TO current_tile.
                IF curr_tile_sprite = SHIP_SPRITE THEN
                    *> HIT!!
                    MOVE EXPLOSION TO curr_tile_sprite
                    PERFORM damage_ship
                    MOVE red TO curr_tile_color
                    ADD 1 TO p_score_hit_count(player)
                    COMPUTE p_score(player) = (100 + p_score(player) 
                                               - p_score_since_last(player))
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
                EVALUATE curr_tile_color
                    *> WHEN "submarine"
                    WHEN brown
                        MOVE 1 TO ship_idx
                        MOVE 3 TO ship_lenght
                    *> WHEN "destroyer"
                    WHEN white
                        MOVE 2 TO ship_idx
                        MOVE 2 TO ship_lenght
                    *> WHEN "cruiser"
                    WHEN magenta
                        MOVE 3 TO ship_idx
                        MOVE 3 TO ship_lenght
                        
                    *> WHEN "battleship"
                    WHEN cyan
                        MOVE 4 TO ship_idx
                        MOVE 4 TO ship_lenght
                    *> WHEN "carrier"
                    WHEN green
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
                COMPUTE build_direction = FUNCTION RANDOM * (1 - 0 + 1) + 0
                EXIT.

            *> look for a random place for the ship until it finds room for it
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
                MOVE build_ship_parameters TO p_ship_class(player, ship_idx).
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

            *> initialize some game parameters
            initialize_game.
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
                MOVE 1 TO CHEAT_MODE.
                PERFORM show_board.
                PERFORM show_score.
                IF p_ship_count(HUMAN_PLAYER) > p_ship_count(CPU_PLAYER)
                    MOVE HUMAN_PLAYER TO player
                ELSE
                    MOVE CPU_PLAYER TO player
                END-IF.
                ACCEPT screen_game_over.
                EXIT.

            *> simulate a user guess for testing purposes
            testing_guess.
                COMPUTE guess_x = FUNCTION RANDOM * (10 - 1 + 1) + 1
                COMPUTE guess_y = FUNCTION RANDOM * (10 - 1 + 1) + 1
                EXIT.
            *> =======================================================
        
            IDENTIFICATION DIVISION.
                PROGRAM-ID. enemy_ai.
                *> controls enemy "pseudo" AI
                *>   at every move, it looks for all tiles, when 
                *>   it finds a known hit, try to shoot around it
            DATA DIVISION.
                WORKING-STORAGE SECTION.
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
                    01 add_some_bias.
                        *> the less emotional, less methodic
                        05 emotional   USAGE COMP-1 VALUE 0.2 .
                        05 vibes   USAGE COMP-1 VALUE ZERO.
                LINKAGE SECTION.
                    01 enemy PIC 9 VALUE 1.
                    01 guess_x PIC 9 VALUE 1.
                    01 guess_y PIC 99 VALUE 1.
            PROCEDURE DIVISION USING enemy, guess_x, guess_y.
                *> add some uncertainity because our
                *> robot was too methodic
                COMPUTE vibes = FUNCTION RANDOM
                IF vibes < emotional THEN
                    COMPUTE guess_x = FUNCTION RANDOM * (10 - 1 + 1) + 1
                    COMPUTE guess_y = FUNCTION RANDOM * (10 - 1 + 1) + 1
                    GOBACK
                END-IF.

                PERFORM VARYING x FROM 1 BY 1 UNTIL x > BOARD_HEIGTH
                    PERFORM VARYING y FROM 1 BY 1 UNTIL y > BOARD_WIDTH
                        *> if we find a HIT, then we search around it
                        IF tile(enemy, x, y) = EXPLOSION THEN
                            PERFORM get_narrow_search_params
                            PERFORM VARYING i FROM start_x 
                                        BY 1 UNTIL i > end_x
                                PERFORM VARYING j FROM start_y 
                                            BY 1 UNTIL j > end_y
                                    IF tile(enemy, i, j) = WATER
                                        OR tile(enemy, i, j) = SHIP_SPRITE THEN
                                            MOVE i TO guess_x
                                            MOVE j TO guess_y
                                            GOBACK
                                    END-IF
                                END-PERFORM
                            END-PERFORM
                        END-IF
                    END-PERFORM
                END-PERFORM.
                *> if we cant find a smart guess, make dumb one.
                COMPUTE guess_x = FUNCTION RANDOM * (10 - 1 + 1) + 1
                COMPUTE guess_y = FUNCTION RANDOM * (10 - 1 + 1) + 1
            GOBACK.
            
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
                    IF end_x >= BOARD_HEIGTH THEN
                        MOVE BOARD_HEIGTH TO end_x
                    END-IF
                    COMPUTE end_y = y + 1
                    IF end_y > BOARD_WIDTH THEN
                        MOVE BOARD_WIDTH TO end_y
                    END-IF
                    EXIT.
            END PROGRAM enemy_ai.
            
        END PROGRAM Battleship.
        *> ========================================================
        
        IDENTIFICATION DIVISION.
            PROGRAM-ID. letters_to_numbers.
            *> converts a char letter to the correspondent number
            *>     a 1, b 2, c 3,...
        DATA DIVISION.
            WORKING-STORAGE SECTION.
                01 cached           PIC 9 VALUE 0.
                01 letters          PIC X(26) 
                    VALUE "abcdefghijklmnopqrstuvwxyz".
                01 letter_list   OCCURS 26 TIMES.
                    05 the_letter   PIC X VALUE SPACES.
                01 returning_number PIC 99 VALUE ZEROS.
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
        
