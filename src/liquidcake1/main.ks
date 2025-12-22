#!/usr/bin/env kast
use std.prelude.*;
use std.collections.Map;

use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

@syntax "if_is" 7.5 wrap never = "if" " " value " " "is" " " pattern " " "then" " " body;
impl syntax (if value is pattern then body) = `(
    match $value with (
        | $pattern => $body
        | _ => ()
    )
);
@syntax "if_is_else" 7.5 wrap never = "if" " " value " " "is" " " pattern " " "then" " " body " " "else" " " else_body ->;
impl syntax (if value is pattern then body else else_body) = `(
    match $value with (
        | $pattern => $body
        | _ => $else_body
    )
);

const strip_prefix = (s :: String, .prefix :: String) -> Option.t[String] => (
    let prefix_len = String.length prefix;
    if (
        String.length s >= prefix_len
        and String.substring (s, 0, String.length prefix) == prefix
    ) then (
        :Some (String.substring (s, prefix_len, String.length s - prefix_len))
    ) else :None
);

const index_of_substr = (s :: String, .sub :: String) -> Option.t[Int32] => with_return (
    let len = String.length sub;
    for i in 0..String.length s - String.length sub + 1 do (
        if String.substring (s, i, len) == sub then return :Some i;
    );
    :None
);

const string_split_once = (s :: String, .sep :: String) -> (String, String) => (
    match index_of_substr (s, .sub = sep) with (
        | :Some i => (
            let j = i + String.length sep;
            (
                String.substring (s, 0, i),
                String.substring (s, j, String.length s - j)
            )
        )
        | :None => panic "no substr"
    )
);

const ParseState = newtype (
    | :LetterMap
    | :Rules
    | :DigitMap
);

let mut parse_state :: ParseState = :LetterMap;

const LetterRule = newtype (
    | :ToPowerOf Int32
    | :PowerOf Int32
    | :Palindrome
    | :MultipleOf Int32
);

let mut letter_map = List.create ();
let mut letter_rules :: Map.t[Char, LetterRule] = Map.create ();
let mut digit_map = List.create ();

let parse_letter_rule = line => (
    let c, rule = string_split_once (line, .sep = " is a ");
    if String.length c != 1 then panic "AAAAAA";
    let c = String.at (c, 0);
    let rule = if rule == "cube" then (
        :ToPowerOf 3
    ) else if rule == "square" then (
        :ToPowerOf 2
    ) else if rule == "palindrome" then (
        :Palindrome
    ) else if strip_prefix (rule, .prefix = "power of ") is :Some x then (
        :PowerOf (x |> parse)
    ) else if strip_prefix (rule, .prefix = "multiple of ") is :Some x then (
        :MultipleOf (x |> parse)
    ) else (
        panic <| "how to parse rule: " + rule
    );
    # dbg.print (.c, .rule);
    Map.add (&mut letter_rules, c, rule);
);

String.lines (
    input,
    line => with_return (
        if String.length line == 0 then (
            parse_state = match parse_state with (
                | :LetterMap => :Rules
                | :Rules => :DigitMap
                | :DigitMap => return
            );
            return;
        );
        match parse_state with (
            | :LetterMap => (
                List.push_back (&mut letter_map, line);
            )
            | :Rules => (
                parse_letter_rule line;
            )
            | :DigitMap => (
                let mut digits = List.create ();
                String.iter (
                    line,
                    c => (
                        List.push_back (
                            &mut digits,
                            Char.to_digit c,
                        );
                    ),
                );
                List.push_back (&mut digit_map, digits);
            )
        );
    ),
);

let n = List.length &letter_map;
let m = String.length (List.at (&letter_map, 0))^;

let mut answer = 0;

let read_number = (si, sj, di, dj) -> Option.t[Int32] => with_return (
    let mut i, mut j = si, sj;
    let mut result = 0;
    loop (
        let digit = (List.at (List.at (&digit_map, i), j))^;
        if result == 0 and digit == 0 then return :None;
        result = result * 10 + digit;
        i = (i + di) % n;
        j = (j + dj) % m;
        let c = String.at ((List.at (&letter_map, i))^, j);
        if c != '.' then break;
    );
    :Some result
);

let check = (c :: Char, number :: Option.t[Int32]) => with_return (
    let number = match number with (
        | :None => return
        | :Some number => number
    );
    let rule = match Map.get (&letter_rules, c) with (
        | :None => panic "no rule found"
        | :Some (&rule) => rule
    );
    let rule_passes = match rule with (
        | :Palindrome => (
            let reversed = (
                let mut a = number;
                let mut b = 0;
                while a > 0 do (
                    b = b * 10 + a % 10;
                    a /= 10;
                );
                b
            );
            reversed == number
        )
        | :PowerOf x => with_return (
            let mut number = number;
            while number > 1 do (
                if number % x != 0 then return false;
                number /= x;
            );
            true
        )
        | :ToPowerOf x => with_return (
            let mut test = 1;
            loop (
                let mut test_to_power = 1;
                for _ in 0..x do (
                    test_to_power *= test;
                );
                if test_to_power == number then return true;
                if test_to_power > number then break;
                test += 1;
            );
            false
        )
        | :MultipleOf x => number % x == 0
    );
    if not rule_passes then (
        answer += number;
    );
);

for i in 0..n do (
    for j in 0..m do (
        let c = String.at ((List.at (&letter_map, i))^, j);
        if c == '.' then continue;
        check (c, read_number (i, j, 0, 1));
        let C = Char.from_code (Char.code c + (Char.code 'A' - Char.code 'a'));
        check (C, read_number (i, j, 1, 0));
    );
);

dbg.print answer;
