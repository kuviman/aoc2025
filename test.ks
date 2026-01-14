#!/usr/bin/env kast
use std.prelude.*;

std.sys.chdir(std.path.dirname(__FILE__));
let mut only_day = :None;
let mut only_example = false;
let mut with_javascript = true;
for i in 1..std.sys.argc() do (
    let arg = std.sys.argv_at(i);
    if arg == "--only-example" then (
        only_example = true;
    ) else if arg == "--no-js" then (
        with_javascript = false;
    ) else (
        only_day = :Some(String.parse(arg))
    )
);
for day in 1..13 do (
    match only_day with (
        | :Some(only_day) => (
            if day != only_day then continue;
        )
        | :None => ()
    );
    let mut path = "./src/day";
    if day < 10 then (
        path += "0";
    );
    
    path += to_string(day);
    path += "/main.ks";
    let test = (part :: Int32, mut file) => with_return (
        if day == 12 then (
            if not (part == 1 and file == "input.txt") then return;
        );
        if day == 11 and part == 2 and file == "example.txt" then (
            file = "example.part2.txt";
        );
        let with_javascript = if with_javascript then "--target javascript " else "";
        let command = "kast run " + with_javascript + path + " --part" + to_string(part) + " " + file;
        print("executing " + command);
        let exit_code = std.sys.exec(command);
        if exit_code != 0 then (
            print(
                command
                + " failed with exit code = "
                + to_string(exit_code)
            );
            
            std.sys.exit(-1);
        );
    );
    
    test(1, "example.txt");
    test(2, "example.txt");
    if not only_example then (
        test(1, "input.txt");
        test(2, "input.txt");
    );
);
