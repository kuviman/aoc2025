#!/usr/bin/env node
// Transpiled from kast -> JavaScript (best-effort)

const fs = require('fs');
const path = require('path');

const args = process.argv.slice(2);
if (args.length === 0) {
  console.error('usage: node solution.js <input-file> [--part1]');
  process.exit(1);
}
const input_path = args[0];
const part1 = args.includes('--part1');

const verbose = false;

function readFile(p) {
  return fs.readFileSync(p, 'utf8');
}

const input = readFile(input_path);

// helpers
const abs = x => (x < 0 ? -x : x);
const max = (a, b) => (a > b ? a : b);
const min = (a, b) => (a < b ? a : b);
const pow = (x, n) => {
  let result = 1;
  for (let i = 0; i < n; i++) result *= x;
  return result;
};

// simple queue
class Queue {
  constructor() {
    this.arr = [];
    this.head = 0;
  }
  push(x) { this.arr.push(x); }
  pop() { if (this.head >= this.arr.length) return undefined; const v = this.arr[this.head++]; return v; }
  get length() { return this.arr.length - this.head; }
}

// Matrix class
class Matrix {
  constructor(n, m) {
    this.rows = Array.from({ length: n }, () => Array(m).fill(0));
  }
  static create(n, m) { return new Matrix(n, m); }
  at(i, j) { return this.rows[i][j]; }
  set(i, j, v) { this.rows[i][j] = v; }
  row(i) { return this.rows[i]; }
  size() { return [this.rows.length, this.rows[0].length]; }
  print() {
    this.rows.forEach(r => {
      console.log(r.map(x => String(x)).join(' '));
    });
  }
}

// parse machine representation
// returns { lights, target_state, buttons:[], joltages:[] }
function parse_machine(s) {
  let lights = 0;
  let target_state = 0;
  let buttons = [];
  let joltages = [];

  const parts = s.split(' ');
  for (const part of parts) {
    if (!part) continue;
    const parens = part[0];
    const inside = part.substring(1, part.length - 1);
    if (parens === '[') {
      lights = inside.length;
      target_state = 0;
      // kast loops from high index downwards to build bits MSB..LSB
      for (let i = inside.length - 1; i >= 0; i--) {
        const cchar = inside[i];
        const c = (cchar === '#') ? 1 : 0;
        target_state = (target_state << 1) + c;
      }
      if (verbose) console.log(`[INFO] read target_state=${target_state}`);
    } else if (parens === '(') {
      let affected_lights = 0;
      if (inside.length > 0) {
        const idxs = inside.split(',');
        for (let idx of idxs) {
          idx = idx.trim();
          if (idx.length === 0) continue;
          const num = parseInt(idx, 10);
          affected_lights = affected_lights | (1 << num);
        }
      }
      buttons.push(affected_lights);
    } else if (parens === '{') {
      if (inside.length > 0) {
        const xs = inside.split(',');
        for (let x of xs) {
          x = x.trim();
          if (!x) continue;
          joltages.push(parseInt(x, 10));
        }
      }
    } else {
      throw new Error('unexpected char while parsing machine: ' + parens);
    }
  }

  return { lights, target_state, buttons, joltages };
}

// Part 1 BFS
function solve_part1(machine) {
  const q = new Queue();
  const totalStates = 1 << machine.lights;
  const d = new Array(totalStates).fill(0);

  if (verbose) console.log('[INFO] starting bfs');
  d[0] = 1;
  q.push(0);
  d[machine.target_state] = -1;
  q.push(machine.target_state);

  let machine_answer = -1;

  while (q.length !== 0) {
    const state = q.pop();
    const state_d = d[state];
    const dir = state_d < 0 ? -1 : +1;
    for (const changes of machine.buttons) {
      const new_state = state ^ changes;
      const new_state_d = d[new_state];
      if (new_state_d === 0) {
        d[new_state] = state_d + dir;
        q.push(new_state);
      } else if ((new_state_d < 0) !== (state_d < 0)) {
        machine_answer = Math.abs(state_d) + Math.abs(new_state_d) - 1;
        break;
      }
    }
    if (machine_answer >= 0) break;
  }

  if (machine_answer < 0) {
    throw new Error("couldn't solve the machine (part1)");
  }
  return machine_answer;
}

// Gaussian elimination (integer, best-effort port of kast gauss_elimination)
function gauss_elimination(aMatrix) {
  if (verbose) {
    console.log('[INFO] before gauss');
    aMatrix.print();
  }
  const [n, m] = aMatrix.size();
  let i = 0, j = 0;
  const free = [];
  const pivot = [];

  while (i < n && j < m) {
    // find non-zero row
    let non_zero_row = -1;
    for (let row = i; row < n; row++) {
      if (aMatrix.at(row, j) !== 0) non_zero_row = row;
    }
    if (non_zero_row === -1) {
      if (j + 1 < m) free.push(j);
      j += 1;
      continue;
    }
    if (j + 1 < m) pivot.push({ row: i, var: j });
    // swap rows i and non_zero_row
    if (non_zero_row !== i) {
      const ri = aMatrix.row(i);
      const rnn = aMatrix.row(non_zero_row);
      aMatrix.rows[i] = rnn;
      aMatrix.rows[non_zero_row] = ri;
    }
    const at_row = aMatrix.at(i, j);
    // eliminate rows below
    for (let lower = i + 1; lower < n; lower++) {
      // if lower[j] % at_row != 0 in kast they scaled; here we do integer elimination via gcd-multiplying to avoid fractions
      let lowerVal = aMatrix.at(lower, j);
      if (lowerVal === 0) continue;
      // scale rows to make division exact: multiply lower row by at_row, and subtract lower_j * row
      if (lowerVal % at_row !== 0) {
        // scale entire lower row by at_row
        for (let kk = 0; kk < m; kk++) {
          aMatrix.rows[lower][kk] *= at_row;
        }
        lowerVal = aMatrix.at(lower, j);
      }
      const mult = Math.floor(lowerVal / at_row);
      for (let k = j; k < m; k++) {
        aMatrix.rows[lower][k] -= aMatrix.at(i, k) * mult;
      }
    }
    i += 1;
    j += 1;
  }

  for (let jj = j; jj < m - 1; jj++) {
    free.push(jj);
  }

  if (verbose) {
    console.log('[INFO] after gauss');
    aMatrix.print();
    console.log('====');
  }

  console.log('free vars =', JSON.stringify(free));
  return { free, pivot };
}

// brute module (search over free variables)
const brute = {
  machine_answer: 0,
  n: 0,
  m: 0,
  a: null,
  vars: null,
  free: null,
  pivot: null,
  current_answer: 0,
  check() {
    // evaluate pivots to compute dependent vars, ensure integer >=0
    let saved = this.current_answer;
    // copy of vars will be used directly
    // iterate pivots in reverse order (highest vars first)
    // In kast they used treap.iter_rev; here pivot is array in order of discovery -> evaluate reversed
    for (let idx = this.pivot.length - 1; idx >= 0; idx--) {
      const p = this.pivot[idx];
      const varIndex = p.var;
      const rowIndex = p.row;
      const row = this.a.row(rowIndex);
      let result = row[this.m - 1];
      for (let j = varIndex + 1; j < this.m - 1; j++) {
        result -= this.vars[j] * row[j];
      }
      const k = row[varIndex];
      if (k === 0) return; // shouldn't happen
      if (result % k !== 0) return;
      result = Math.floor(result / k);
      if (result < 0) return;
      this.current_answer += result;
      if (this.current_answer >= this.machine_answer) {
        // prune
        this.current_answer = saved;
        return;
      }
      this.vars[varIndex] = result;
    }
    if (this.current_answer < this.machine_answer) {
      this.machine_answer = this.current_answer;
    }
    this.current_answer = saved;
  },

  force(i, max) {
    if (i < 0) {
      const save = this.current_answer;
      this.check();
      this.current_answer = save;
      return;
    } else {
      const varIdx = this.free[i];
      for (let x = 0; x <= max + 1; x++) {
        this.vars[varIdx] = x;
        this.current_answer += x;
        if (this.current_answer < this.machine_answer) {
          this.force(i - 1, max - x);
        }
        this.current_answer -= x;
      }
    }
  }
};

let max_combinations = 0;

function solve_part2(machine) {
  const n = machine.joltages.length;
  const m = machine.buttons.length + 1;
  const a = Matrix.create(n, m);

  // fill columns with button effects
  for (let i = 0; i < machine.buttons.length; i++) {
    let button = machine.buttons[i];
    for (let j = 0; j < n; j++) {
      if ((button & 1) !== 0) {
        a.set(j, i, 1);
      }
      button = button >>> 1;
    }
  }
  // last column = joltages
  for (let i = 0; i < machine.joltages.length; i++) {
    a.set(i, m - 1, machine.joltages[i]);
  }

  const { free, pivot } = gauss_elimination(a);

  let max_joltage = 0;
  for (const x of machine.joltages) max_joltage = max(max_joltage, x);

  console.log('max_joltage =', max_joltage);
  const combinations = pow(max_joltage, free.length);
  max_combinations = max(max_combinations, combinations);
  console.log('combinations(max^free) =', combinations);

  const vars = Array(m - 1).fill(0);

  brute.machine_answer = 1000000000;
  brute.a = a;
  brute.n = n;
  brute.m = m;
  brute.vars = vars;
  brute.free = free;
  brute.pivot = pivot;

  brute.force(free.length - 1, max_joltage);

  console.log('answer =', brute.machine_answer);
  return brute.machine_answer;
}

// main loop: read lines and solve machines
let max_buttons = 0;
let max_joltages = 0;
let max_joltage = 0;
let idx = 0;
let answer = 0;

const lines = input.split(/\r?\n/);
for (const line of lines) {
  if (!line || line.trim().length === 0) continue;
  const machine = parse_machine(line.trim());

  max_buttons = Math.max(max_buttons, machine.buttons.length);
  max_joltages = Math.max(max_joltages, machine.joltages.length);
  for (const x of machine.joltages) max_joltage = Math.max(max_joltage, x);

  idx += 1;
  console.log(`[INFO] read machine #${idx}`);

  if (part1) {
    answer += solve_part1(machine);
  } else {
    answer += solve_part2(machine);
  }
}

console.log('dbg:', { max_buttons, max_joltages, max_joltage, max_combinations });
console.log('answer:', answer);
