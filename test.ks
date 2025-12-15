#!/usr/bin/env kast
use std.prelude.*;

std.sys.chdir (std.path.dirname __FILE__);
let only = if std.sys.argc () >= 2 then (
    :Some (std.sys.argv_at 1 |> String.parse)
) else (
    :None
);

for day in 1..13 do (
    match only with (
        | :Some only => (
            if day != only then continue;
        )
        | :None => ()
    );
    let path = "./src/day";
    if day < 10 then (
        path += "0";
    );
    path += to_string day;
    path += "/main.ks";
    let test = (part, file) => with_return (
        if day == 12 then (
            if not (part == 1 and file == "input.txt") then return;
        );
        if day == 11 and part == 2 and file == "example.txt" then (
            file = "example.part2.txt";
        );
        let command = path + " --part" + to_string part + " " + file;
        print ("executing " + command);
        let exit_code = std.sys.exec command;
        if exit_code != 0 then (
            print (
                command
                + " failed with exit code = "
                + to_string exit_code
            );
            std.sys.exit (-1);
        );
    );
    test (1, "example.txt");
    test (2, "example.txt");
    test (1, "input.txt");
    test (2, "input.txt");
);
