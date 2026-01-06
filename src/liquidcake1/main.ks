#!/usr/bin/env kast
use std.prelude.*;
use std.collections.Map;

include "../common.ks";
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

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

for line in String.lines input do (
    if String.length line == 0 then (
        parse_state = match parse_state with (
            | :LetterMap => :Rules
            | :Rules => :DigitMap
            | :DigitMap => continue
        );
        continue;
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
            for c in String.iter line do (
                List.push_back (
                    &mut digits,
                    Char.to_digit c,
                );
            );
            List.push_back (&mut digit_map, digits);
        )
    );
);

let n = List.length &letter_map;
let m = String.length (List.at (&letter_map, 0))^;

const SudokuMap = List.t[List.t[Option.t[Int32]]];
let mut sudoku_map :: SudokuMap = (
    let mut map = List.create ();
    for i in 0..n do (
        let mut row = List.create ();
        for j in 0..m do (
            List.push_back (&mut row, :None);
        );
        List.push_back (&mut map, row);
    );
    map
);

let print_sudoku_map = (sudoku_map :: &SudokuMap) => (
    for row in List.iter sudoku_map do (
        let mut line = "";
        for &cell in List.iter row do (
            match cell with (
                | :None => (line += ".")
                | :Some x => (line += to_string x)
            );
        );
        print line;
    );
);

let is_sudoku_solved = (sudoku_map) -> Bool => with_return (
    for row in List.iter sudoku_map do (
        for &cell in List.iter row do (
            match cell with (
                | :None => return false
                | :Some _ => ()
            );
        );
    );
    true
);

let mut answer = 0;

let read_number_from_digit_map = (si, sj, di, dj) -> Option.t[Int32] => with_return (
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

let read_number_from_sudoku_map = (sudoku_map :: &SudokuMap, si, sj, di, dj) -> List.t[Option.t[Int32]] => with_return (
    let mut i, mut j = si, sj;
    let mut result = List.create ();
    loop (
        let digit = (List.at (List.at (sudoku_map, i), j))^;
        List.push_back (&mut result, digit);
        i = (i + di) % n;
        j = (j + dj) % m;
        let c = String.at ((List.at (&letter_map, i))^, j);
        if c != '.' then break;
    );
    result
);

let write_number_from_digit_map = (si, sj, di, dj) => with_return (
    let mut i, mut j = si, sj;
    loop (
        let digit = (List.at (List.at (&digit_map, i), j))^;
        (List.at_mut (List.at_mut (&mut sudoku_map, i), j))^ = :Some digit;
        i = (i + di) % n;
        j = (j + dj) % m;
        let c = String.at ((List.at (&letter_map, i))^, j);
        if c != '.' then break;
    );
);

let check = (c :: Char, number :: Option.t[Int32]) -> Bool => with_return (
    let number = match number with (
        | :None => return false
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
    if part1 and not rule_passes then (
        answer += number;
    );
    rule_passes
);

let check_full = (map :: &SudokuMap) -> Bool => with_return (
    # print_sudoku_map map;
    let to_number = (list :: List.t[Option.t[Int32]]) -> Option.t[Int32] => with_return (
        let mut result = 0;
        for &digit in List.iter &list do (
            match digit with (
                | :None => return :None
                | :Some digit => (
                    result = result * 10 + digit
                )
            );
        );
        :Some result
    );
    for i in 0..n do (
        for j in 0..m do (
            let c = String.at ((List.at (&letter_map, i))^, j);
            if c == '.' then continue;
            let number = read_number_from_sudoku_map (map, i, j, 0, 1) |> to_number;
            if number is :Some _ then (
                if not check (c, number) then (
                    # dbg.print (.c, .number, .i, .j);
                    return false;
                );
            );
            let C = Char.from_code (Char.code c + (Char.code 'A' - Char.code 'a'));
            let Number = read_number_from_sudoku_map (map, i, j, 1, 0) |> to_number;
            if Number is :Some _ then (
                if not check (C, Number) then (
                    # dbg.print (.C, .Number);
                    return false;
                );
            );
        );
    );
    true
);

for i in 0..n do (
    for j in 0..m do (
        let c = String.at ((List.at (&letter_map, i))^, j);
        if c == '.' then continue;
        if check (c, read_number_from_digit_map (i, j, 0, 1)) and part2 then (
            write_number_from_digit_map (i, j, 0, 1);
        );
        let C = Char.from_code (Char.code c + (Char.code 'A' - Char.code 'a'));
        if check (C, read_number_from_digit_map (i, j, 1, 0)) and part2 then (
            write_number_from_digit_map (i, j, 1, 0);
        );
    );
);

const Part2 = (
    module:
    
    let fill = (sudoku_map :: &SudokuMap, c, number :: List.t[Int32], si, sj, di, dj) => (
        (
            let number = (
                let mut x = 0;
                for &digit in List.iter &number do (
                    x = x * 10 + digit;
                );
                x
            );
            (# dbg.print (
                .rule,
                .si,
                .sj,
                .di,
                .dj,
                .filled = filled_number
            ); #)
            print ("Try filling " + (@native "to_string") c + " with " + to_string number);
        );
        let mut sudoku_map = (
            let mut clone = List.create ();
            for row in List.iter sudoku_map do (
                let mut clone_row = List.create ();
                for &cell in List.iter row do (
                     List.push_back (&mut clone_row, cell);
                );
                List.push_back (&mut clone, clone_row);
            );
            clone
        );
        let mut i, mut j = si, sj;
        for &digit in List.iter &number do (
            (List.at_mut (List.at_mut (&mut sudoku_map, i), j))^ = :Some digit;
            i = (i + di) % n;
            j = (j + dj) % m;
        );
        try_solve &sudoku_map
    );
    let try_fill = (sudoku_map, c :: Char, si, sj, di, dj, .fill) => with_return (
        let number = read_number_from_sudoku_map (sudoku_map, si, sj, di, dj);
        let already_filled = with_return (
            for digit in List.iter &number do (
                if digit^ is :None then return false;
            );
            true
        );
        if already_filled then return;
        let min, max = (
            let mut min = 1;
            for _ in 1..List.length &number do (
                min *= 10;
            );
            min, min * 10 - 1
        );
        let rule = match Map.get (&letter_rules, c) with (
            | :None => panic "no rule found"
            | :Some (&rule) => rule
        );
        let try = filled => with_return (
            if List.length &filled != List.length &number then (
                panic "different length";
            );
            for i in 0..List.length &number do (
                if (List.at (&number, i))^ is :Some expected then (
                    let actual = (List.at (&filled, i))^;
                    if expected != actual then return;
                );
            );
            fill (sudoku_map, c, filled, si, sj, di, dj);
        );
        match rule with (
            | :Palindrome => with_return (
                let mut filled = List.create ();
                for _ in 0..List.length &number do (
                    List.push_back (&mut filled, -1);
                );
                for i in 0..(List.length &number + 1) / 2 do (
                    let j = List.length &number - i - 1;
                    let a = (List.at (&number, i))^;
                    let b = (List.at (&number, j))^;
                    let digit = match (a, b) with (
                        | (:None, :None) => return
                        | (:Some digit, :None) => digit
                        | (:None, :Some digit) => digit
                        | (:Some digit_a, :Some digit_b) => (
                            if digit_a != digit_b then panic "palindrome invalid";
                            digit_a
                        )
                    );
                    (List.at_mut (&mut filled, i))^ = digit;
                    (List.at_mut (&mut filled, j))^ = digit;
                );
                try filled;
            )
            | _ => (
                let try = (x :: Int32) => with_return (
                    if x < min or x > max then (
                        dbg.print (.rule, .x, .min, .max);
                        panic "out of acceptable range";
                    );
                    let filled = (
                        let mut reversed = List.create ();
                        let mut x = x;
                        while x != 0 do (
                            List.push_back (&mut reversed, x % 10);
                            x /= 10;
                        );
                        let mut list = List.create ();
                        for i in (0..List.length &reversed).rev () do (
                            List.push_back (&mut list, (List.at (&reversed, i))^);
                        );
                        list
                    );
                    try filled;
                );
                match rule with (
                    | :ToPowerOf power => with_return (
                        let pow = x => (
                            let mut result = 1;
                            for _ in 0..power do (
                                result *= x;
                            );
                            result
                        );
                        let mut x = 1;
                        while pow x < min do (
                            x += 1;
                        );
                        loop (
                            let p = pow x;
                            if p > max then break;
                            try p;
                            x += 1;
                        );
                    )
                    | :MultipleOf mult => with_return (
                        # x >= min / mult
                        let mut x = ((min + mult - 1) / mult) * mult;
                        while x <= max do (
                            try x;
                            x += mult;
                        );
                    )
                    | :PowerOf base => with_return (
                        let mut x = 1;
                        while x < min do (
                            x *= base;
                        );
                        while x <= max do (
                            try x;
                            x *= base;
                        );
                    )
                );
            )
        );
    );
    
    const current_solvable = @context type (.found_solution :: SudokuMap -> ());
    
    const Visited = (
        module:
        
        use std.collections.Treap;
        
        const t = newtype (.inner :: Treap.t[type (&SudokuMap)]);
        
        const create = () -> t => (.inner = Treap.create ());
        
        const Ord = newtype (
            | :Less
            | :Equal
            | :Greater
        );
        
        const cmp_list = [T] (a :: &List.t[T], b :: &List.t[T], .cmp_elem :: (&T, &T) -> Ord) -> Ord => with_return (
            for i in 0..List.length a do (
                let elem_a = List.at (a, i);
                let elem_b = List.at (b, i);
                match cmp_elem (elem_a, elem_b) with (
                    | :Equal => ()
                    | ord => return ord
                );
            );
            :Equal
        );
        
        const cmp_int32 = (a, b) => (
            if a == b then :Equal else if a < b then :Less else :Greater
        );
        
        const cmp_cell = (a :: &Option.t[Int32], b :: &Option.t[Int32]) -> Ord => (
            match (a^, b^) with (
                | (:None, :None) => :Equal
                | (:None, :Some _) => :Less
                | (:Some _, :None) => :Greater
                | (:Some a, :Some b) => cmp_int32 (a, b)
            )
        );
        
        const cmp = (a :: &SudokuMap, b :: &SudokuMap) -> Ord => (
            cmp_list (a, b, .cmp_elem = (a, b) => cmp_list (a, b, .cmp_elem = cmp_cell))
        );
        
        const was_visited = (map :: &mut t, state :: &SudokuMap) -> Bool => (
            let less, greater_or_equal = Treap.split (
                map^.inner,
                data => (
                    if cmp (data^.value, state) is :Less then (
                        :LeftSubtree
                    ) else (
                        :RightSubtree
                    )
                ),
            );
            let mut equal, greater = Treap.split (
                greater_or_equal,
                data => (
                    match cmp (data^.value, state) with (
                        | :Less => :LeftSubtree
                        | :Equal => :LeftSubtree
                        | :Greater => :RightSubtree
                    )
                ),
            );
            if Treap.length &equal == 0 then (
                equal = Treap.singleton state;
                map^.inner = Treap.join (less, Treap.join (equal, greater));
                false
            ) else (
                true
            )
        );
    );
    
    let mut visited_states = Visited.create ();
    
    let fill_obvious = sudoku_map -> Bool => with_return (
        let try_at = (c, i, j, di, dj) => (
            unwindable check (
                let mut called = :None;
                let f = args => (
                    if called is :Some _ then unwind check ();
                    called = :Some args;
                );
                try_fill (sudoku_map, c, i, j, di, dj, .fill = f);
                if called is :Some args then (
                    # print "obvious";
                    fill args;
                    return true;
                );
            );
        );
        for i in 0..n do (
            for j in 0..m do (
                let c = String.at ((List.at (&letter_map, i))^, j);
                if c == '.' then continue;
                try_at (c, i, j, 0, 1);
                let C = Char.from_code (Char.code c + (Char.code 'A' - Char.code 'a'));
                try_at (C, i, j, 1, 0);
            );
        );
        false
    );
    
    let try_solve = sudoku_map => with_return (
        if not check_full sudoku_map then return;
        if is_sudoku_solved sudoku_map then (
            (@current current_solvable).found_solution sudoku_map^;
        );
        if Visited.was_visited (&mut visited_states, sudoku_map) then return;
        print "trying to solve";
        print_sudoku_map sudoku_map;
        
        if not fill_obvious sudoku_map then (
            # print "Trying brute";
            for i in 0..n do (
                for j in 0..m do (
                    let c = String.at ((List.at (&letter_map, i))^, j);
                    if c == '.' then continue;
                    try_fill (sudoku_map, c, i, j, 0, 1, .fill);
                    let C = Char.from_code (Char.code c + (Char.code 'A' - Char.code 'a'));
                    try_fill (sudoku_map, C, i, j, 1, 0, .fill);
                );
            );
        );
    );
);

if part1 then (
    dbg.print answer;
) else (
    let solved = unwindable solving (
        with Part2.current_solvable = (
            .found_solution = solution => unwind solving solution,
        );
        Part2.try_solve &sudoku_map;
        panic "could not solve"
    );
    print "Solved!!!";
    print_sudoku_map &solved;
    
    for line in List.iter &solved do (
        let mut odd_digits = 0;
        for &(:Some digit) in List.iter line do (
            if digit % 2 == 0 then (
                answer += odd_digits;
                odd_digits = 0;
            ) else (
                odd_digits = odd_digits * 10 + digit;
            );
        );
        answer += odd_digits;
    );
    
    dbg.print (.answer);
);
