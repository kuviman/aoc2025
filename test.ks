#!/usr/bin/env kast
use std.StringBuilder;
std.sys.chdir(std.path.dirname(__FILE__));

const Problem = newtype (
    | :Day(Int32)
    | :liquidcake1
);

impl Problem as std.cmp.Ord = {
    .compare = (&a, &b) => match { a, b } with (
        | { :Day ref a, :Day ref b } => std.cmp.default_compare(a, b)
        | { :Day _, :liquidcake1 } => :Less
        | { :liquidcake1, :Day _ } => :Greater
        | { :liquidcake1, :liquidcake1 } => :Equal
    ),
};

let mut only :: Option.t[Problem] = :None;
let mut only_example = false;
let mut only_input = false;
let mut only_part :: Option.t[Int32] = :None;
for i in 1..std.sys.argc() do (
    let arg = std.sys.argv_at(i);
    if arg == "--part1" then (
        only_part = :Some 1;
    ) else if arg == "--part2" then (
        only_part = :Some 2;
    ) else if arg == "--only-input" then (
        only_input = true;
    ) else if arg == "--only-example" then (
        only_example = true;
    ) else if arg == "liquidcake1" then (
        only = :Some(:liquidcake1);
    ) else (
        only = :Some(:Day(String.parse(arg)));
    );
);
let test = (problem :: Problem) => with_return (
    if only is :Some(only) then (
        if problem != only then return;
    );
    let name = match problem with (
        | :Day(day) => (
            let mut name = StringBuilder.new();
            &mut name |> StringBuilder.add_str("day");
            if day < 10 then (
                &mut name |> StringBuilder.add_str("0");
            );
            &mut name |> StringBuilder.add_String(to_string(day));
            name |> StringBuilder.into_string
        )
        | :liquidcake1 => String.from_str("liquidcake1")
    );
    println!("Testing \(name)");
    let path = format!("src/\(name)/main.ks");
    let test = (part :: Int32, mut file) => with_return (
        if only_part is :Some only_part then (
            if part != only_part then (
                return;
            );
        );
        if problem == :Day(12) then (
            if not (part == 1 and file == "input.txt") then return;
        );
        if problem == :Day(11) and part == 2 and file == "example.txt" then (
            file = "example.part2.txt";
        );
        let extra_args = std.sys.get_env("KASTC_ARGS")
            |> Option.unwrap_or_else(() => String.from_str(""));
        let command = format!("kast run \(extra_args) \(path) --part\(part) \(file)");
        println!("executing \(command)");
        let exit_code = std.sys.exec(&command |> as_str);
        if exit_code != 0 then (
            println!("\(command) failed with exit_code \(exit_code)");
            std.sys.exit(-1);
        );
    );
    
    if not only_input then (
        test(1, "example.txt");
        test(2, "example.txt");
    );
    if not only_example then (
        test(1, "input.txt");
        test(2, "input.txt");
    );
);

for day in 1..13 do (
    test(:Day(day))
);
test(:liquidcake1);
