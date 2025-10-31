# Nanoshogi - The Exceedingly Okay Shogi Engine

A Universal Shogi Interface compliant shogi bot. This is part of a personal project I took on to build a shogi GUI in Godot 3d, which lead to me learning chess programming and making this engine. It supports the basic USI commands for positioning and making moves, and it seems to be better than me at the game (though I couldn't tell you what it's actual rating might be). In the future I intend to program it to play western chess as well.

This project is written in rust so if you want to build it you will need to do so with cargo. Shogi engines are performance-intensive so you will have to build it for release mode (`cargo build --release`) or it won't be nearly as powerful.

Credit to the authors of the chess programming wiki (https://chessprogramming.org/), as almost all of the info I used to build this engine I learned from there.
