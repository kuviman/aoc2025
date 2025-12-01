module:
use std.prelude.*;
use std.String;

let part1 = true;
let input_path = "example.txt";
(
    let argc = std.sys.argc ();
    let i = 1;
    loop (
        if i >= argc then break;
        let arg = std.sys.argv_at i;
        if arg == "--part2" then (
            part1 = false;
        ) else if arg == "--part1" then (
            part1 = true;
        ) else (
            input_path = arg;
        );
        i = i + 1;
    );
);
let not = x => if x then false else true;
let part2 = not part1;