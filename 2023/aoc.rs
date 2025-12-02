// Advent of Code 2023 : Rust
// vim: ft=rust:nosi:nocin

#![allow(unused)]

use std::fs::File;
use std::io::Read;

fn input_all (path : &str) -> std::io::Result<String> {
 let mut file = File::open(path)?;
 let mut input = String::new();
 file.read_to_string(&mut input)?;
 Ok(input)
}

fn problem_01a () -> u32 {
 let input = input_all("01.txt").expect("file op error");
 let lines = input.lines();
 let mut acc = 0u32;
 for s in lines {
  let mut first = None;
  let mut last  = None;
  for c in s.chars() {
   last = c.to_digit(10).or(last);
   first = first.or(last);
  }
  acc += first.unwrap_or(0) * 10 + last.unwrap_or(0);
 }
 acc
}

fn problem_01b () -> u32 {
 let input = input_all("01.txt").expect("file op error");
 let numbers = [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ];
 let lines = input.lines();
 let mut acc = 0u32;
 for s in lines {
  let (mut first, mut last) = (None, None);
  for i in 0..s.as_bytes().len() {
   let c = s.as_bytes().get(i).unwrap();
   if c.is_ascii_digit() { last = Some(*c as u32 - 0x30); }
   else { for j in 0..9 { if numbers[j] == &s[i..(numbers[j].len()+i).min(s.len())] { last = Some(j as u32 + 1) }}}
   first = first.or(last);
  }
  acc += first.unwrap_or(0) * 10 + last.unwrap_or(0);
 }
 acc
}

fn problem_02 () -> (u32, u32) {
 let input = input_all("02.txt").expect("file op error");
 let mut acc = Vec::<(u32,u32,u32,u32)>::new();
 for s in input.lines() {
  let res = match s.split(':').collect::<Vec<_>>()[..] {
   [id_s, data] => {
    let id = id_s.trim().split(' ').nth(1).unwrap().parse::<u32>()
     .expect("invalid data shape: id parsing error");
    let (mut r, mut g, mut b) = (0,0,0);
    for s in data.split(&[';',','][..]) {
     // expected data shape: "# [color]"
     let mut it = s.trim().split(' ');
     let num   = it.next().unwrap().parse::<u32>()
      .expect("invalid data fragment shape: expected \"# [color]\"");
     let color = it.next().unwrap();
     match color {
      "red" => r = r.max(num),
      "green" => g = g.max(num),
      "blue" => b = b.max(num),
      _ => panic!("invalid color: expected (red|green|blue)"),
     };
    }; (id, r, g, b)},
   _ => panic!("invalid data shape: expected \"Game #: ...\"")
  }; acc.push(res);
 };
 acc.into_iter().fold((0,0),|(a0,a1),(id,r,g,b)| (if r <= 12 && g <= 13 && b <= 14 { a0+id } else {a0}, a1 + r * g * b))
}

// read "gear box"
fn problem_03a () -> u32 {
 fn addr_of_number (input : &Vec<&[u8]>, y : usize, x : usize) -> Option<(usize,usize)> {
  let mut x_prime = None;
  for x in (0..=x).rev(){
   if input[y][x].is_ascii_digit() { x_prime = Some(x) }
   else { break }
  }
  x_prime.map(|xp|(y, xp))
 }
 fn number_of_addr (input : &Vec<&[u8]>, (y,x) : (usize,usize)) -> u32 {
  String::from_utf8(
    input[y][x..].iter()
    .map_while(|c| c.is_ascii_digit().then_some(*c))
    .collect::<Vec<u8>>()
   ).unwrap().parse::<u32>().unwrap()
 }
 let input_raw = input_all("03.txt").expect("file op error");
 let input = input_raw.lines().map(|s| s.as_bytes()).collect::<Vec<&[u8]>>();
 let (h,w) = (input.len(), input[0].len()); 
 let mut addrs = std::collections::BTreeSet::new();
 for y in 0..h { for x in 0..w {
   let c = input[y][x];
   if !c.is_ascii_digit() && c != b'.' {
    // println!("{}",char::from_u32(c as u32).unwrap());
    // "safe" way of doing y-1..=y+1 x x-1..=x+1
    for i in (y.max(1)-1)..(y+2).min(h) { for j in (x.max(1)-1)..(x+2).min(w) {
     addr_of_number(&input,i,j).map(|addr| addrs.insert(addr)); }}
   }
 }}
 // println!("{:?}",&addrs);
 addrs.into_iter().fold(0,|a,addr| a + number_of_addr(&input,addr))
}

// read "gear box"
fn problem_03b () -> u32 {
 fn addr_of_number (input : &Vec<&[u8]>, y : usize, x : usize) -> Option<(usize,usize)> {
  let mut x_prime = None;
  for x in (0..=x).rev(){
   if input[y][x].is_ascii_digit() { x_prime = Some(x) }
   else { break }
  }
  x_prime.map(|xp|(y, xp))
 }
 // different function version
 fn number_of_addr (input : &Vec<&[u8]>, (y,x) : (usize,usize)) -> u32 {
  let len = input[y].len();
  let mut end = x;
  loop { if end >= len || !input[y][end].is_ascii_digit() {break}; end += 1 };
  std::str::from_utf8(&input[y][x..end]).unwrap().parse::<u32>().unwrap()
 }
 let input_raw = input_all("03.txt").expect("file op error");
 let input = input_raw.lines().map(|s| s.as_bytes()).collect::<Vec<&[u8]>>();
 let (h,w) = (input.len(), input[0].len()); 
 let mut acc = 0;
 let mut addrs = std::collections::BTreeSet::new();
 for y in 0..h { for x in 0..w {
   let c = input[y][x];
   if !c.is_ascii_digit() && c == b'*' {
    // "safe" way of doing y-1..=y+1 x x-1..=x+1
    for i in (y.max(1)-1)..(y+2).min(h) { for j in (x.max(1)-1)..(x+2).min(w) {
     addr_of_number(&input,i,j).map(|addr| addrs.insert(addr)); }}
    if addrs.len()==2 {acc += addrs.iter().fold(1,|a,addr| a * number_of_addr(&input,*addr))};
    addrs.clear()
   }
 }}
 // println!("{:?}",&addrs);
 acc
}

fn problem_04 () -> (u32,u32) {
 fn parse_line(input: &str) -> u32 {
  let mut raw_it = input.split(':').skip(1).next().expect("invalid input: expected ':'").split('|');
  let raw_wins = raw_it.next().expect("invalid input: expected '|'");
  let raw_picks = raw_it.next().unwrap();

  let mut wins = std::collections::BTreeSet::new();
  let mut picks = std::collections::BTreeSet::new();

  for s in raw_wins.split(' ') { if !s.is_empty() { wins.insert(s.parse::<u32>().unwrap()); } }
  for s in raw_picks.split(' ') { if !s.is_empty() { picks.insert(s.parse::<u32>().unwrap()); } }

  wins.intersection(&picks).count() as u32
 }
 let input_raw = input_all("04.txt").expect("file op error");
 let wins = input_raw.lines().map(parse_line).collect::<Vec<_>>();
 // for x in wins.iter() { println!(":{}",x) };
 let mut cards = vec![1; wins.len()];
 for (i,w) in wins.iter().enumerate() {
  let card_at_i = cards[i]; // capture before mutable borrow
  for card in cards[i+1..(i+*w as usize+1).min(wins.len())].iter_mut() { *card += card_at_i }
 }
 (wins.iter().map(|x| (1 << x) >> 1).sum(),cards.iter().sum())
}

// first problem that fails with u32!
fn problem_05 () -> (u64,u64) {

 fn parse_input(mut iterator: std::str::Lines<'_>) -> (Vec<u64>, Vec<Vec<(u64,u64,u64)>>){
  let seeds =
   iterator.next().unwrap()
   .split_once(':').unwrap().1
   .trim().split(' ')
   .map(|s| s.parse::<u64>().unwrap())
   .collect();
  let mut book = Vec::new();
  let mut almanac = Vec::new();
  loop {
   match iterator.next() {
    None => break,
    Some(s) if s.is_empty() => continue,
    Some(s) if s.contains(':') => {
     almanac.push(book);
     book = Vec::new();
    },
    Some(s) => {
     let mut split = s.split(' ').take(3).map(|s| s.parse::<u64>().unwrap());
     book.push((split.next().expect("invalid input: missing dst"),
                split.next().expect("invalid input: missing src"),
                split.next().expect("invalid input: missing len")));
    },
   };
  };
  if !book.is_empty() { almanac.push(book); }
  (seeds,almanac)
 }

 fn map_with_almanac(input: u64, almanac: &Vec<Vec<(u64,u64,u64)>>) -> u64 {
  let mut acc = input;
  for book in almanac { for &(dst,src,len) in book {
    if acc >= src && acc < src+len {acc = acc-src+dst; break}
  }}
  acc
 }

 fn map_with_almanac_inverted(input: u64, almanac: &Vec<Vec<(u64,u64,u64)>>) -> u64 {
  let mut acc = input;
  for book in almanac.iter().rev() { for &(src,dst,len) in book {
    if acc >= src && acc < src+len {acc = acc-src+dst; break}
  }}
  acc
 }

 let input_raw = input_all("05.txt").expect("file op error");
 let (seeds, almanac) = parse_input(input_raw.lines());

 // for s in seeds.iter().map(|s| map_with_almanac(*s,&almanac)) { println!("{}",s); }

 let seed_ranges =
  {let mut acc = Vec::new(); let _ = seeds.iter().fold(None,|a,&n| match a { None => Some(n), Some(a) => {acc.push((a,n)); None}}); acc};

 let mut found = 0;
 for i in (0u64..) {
  let s = map_with_almanac_inverted(i,&almanac);
  if seed_ranges.iter().any(|&(src,len)| s >= src && s < src+len) { found = i; break; }
 }

 (seeds.iter().map(|s| map_with_almanac(*s,&almanac)).min().unwrap(),found)
}

fn problem_06 () -> (u64, u64) {
 // fn test_win (t : i64, d: i64, x: i64) -> bool { x * x - x * t + d < 0 }
 fn wins (t : u64, d : u64) -> u64 {
  // Float Cast
  let tf : f64 = t as f64;
  let df : f64 = d as f64;
  let det = (tf * tf * 0.25 - df).sqrt();
  // floor / ceil +- 1 required to cover the case where det is a whole number!
  // test_win can be used for extra safety, but it is not necessary
  let low =  (0.5 * tf - det).floor() as u64 + 1;
  let high = (0.5 * tf + det).ceil() as u64 - 1;
  // println!("{}",high-low+1);
  (high-low+1)
 }
 let input_raw = input_all("06.txt").expect("file op error");
 let (t_1,d_1,t_2,d_2) = {
  let mut it = input_raw.lines().map(|s|s.split_once(':').unwrap().1.split(' ').filter(|s|!s.is_empty()));
  let t_raw = it.next().unwrap().collect::<Vec<_>>();
  let d_raw = it.next().unwrap().collect::<Vec<_>>();
  let t_1 = t_raw.iter().map(|s|s.parse::<u64>().unwrap()).collect::<Vec<_>>();
  let d_1 = d_raw.iter().map(|s|s.parse::<u64>().unwrap()).collect::<Vec<_>>();
  let t_2 = t_raw.into_iter().fold(String::new(),move |mut a,s| {a.push_str(s); a}).parse::<u64>().unwrap();
  let d_2 = d_raw.into_iter().fold(String::new(),move |mut a,s| {a.push_str(s); a}).parse::<u64>().unwrap();
  (t_1,d_1,t_2,d_2)
 };
 (t_1.into_iter().zip(d_1.into_iter()).fold(1,|a,(t,d)| a * wins(t,d)),wins(t_2,d_2))
}

fn problem_07a () -> u64 {
 fn card_to_number (c: char) -> usize {
  match c { '2' ..= '9' => u32::from(c) as usize - 0x32,
   'T' => 8, 'J' => 9, 'Q' => 10, 'K' => 11, 'A' => 12, _ => panic!("Invalid Card!")}
 }
 fn hand_rank (hand : &str) -> u8 {
  let mut set = vec![0;13];
  for idx in hand.chars().map(card_to_number) { set[idx] += 1; }
  set.sort_by(|x,y|y.cmp(x));
  if set[0] == 5 { return 6 }
  if set[0] == 4 { return 5 }
  if set[0] == 3 && set[1] == 2 { return 4 }
  if set[0] == 3 { return 3 }
  if set[0] == 2 && set[1] == 2 { return 2 }
  if set[0] == 2 { return 1 }
  0
 }
 fn cmp_hand(hand1 : &str, hand2 :&str) -> std::cmp::Ordering {
  match hand_rank(hand1).cmp(&hand_rank(hand2)) {
   std::cmp::Ordering::Equal =>
    hand1.chars().map(card_to_number).cmp(hand2.chars().map(card_to_number)),
   x => x
  }
 }
 let input_raw = input_all("07.txt").expect("file op error");
 let mut input =
  input_raw.lines()
  .map(|s|{let x = s.trim().split_once(' ').unwrap(); (x.0,x.1.parse::<u64>().unwrap())})
  .collect::<Vec<_>>();
 input.sort_by(|(hand1,_),(hand2,_)| cmp_hand(hand1,hand2));
 input.into_iter().fold((0u64,1),|(a,i),(_,b)| (a+i*b,i+1)).0
}

fn problem_07b () -> u64 {
 fn card_to_number (c: char) -> usize {
  match c { '2' ..= '9' => u32::from(c) as usize - 0x31,
   'T' => 9, 'J' => 0, 'Q' => 10, 'K' => 11, 'A' => 12, _ => panic!("Invalid Card!")}
 }
 fn hand_rank (hand : &str) -> u8 {
  let mut set = vec![0;13];
  for idx in hand.chars().map(card_to_number) { set[idx] += 1; }
  let jokers = set[0]; set[0] = 0;
  set.sort_by(|x,y|y.cmp(x));
  if set[0] + jokers == 5 { return 6 }
  if set[0] + jokers == 4 { return 5 }
  if set[0] + jokers == 3 && set[1] == 2 { return 4 }
  if set[0] + jokers == 3 { return 3 }
  if set[0] + jokers == 2 && set[1] == 2 { return 2 }
  if set[0] + jokers == 2 { return 1 }
  0
 }
 fn cmp_hand(hand1 : &str, hand2 :&str) -> std::cmp::Ordering {
  match hand_rank(hand1).cmp(&hand_rank(hand2)) {
   std::cmp::Ordering::Equal =>
    hand1.chars().map(card_to_number).cmp(hand2.chars().map(card_to_number)),
   x => x
  }
 }
 let input_raw = input_all("07.txt").expect("file op error");
 let mut input =
  input_raw.lines()
  .map(|s|{let x = s.trim().split_once(' ').unwrap(); (x.0,x.1.parse::<u64>().unwrap())})
  .collect::<Vec<_>>();
 input.sort_by(|(hand1,_),(hand2,_)| cmp_hand(hand1,hand2));
 input.into_iter().fold((0u64,1),|(a,i),(_,b)| (a+i*b,i+1)).0
}

fn problem_09 () -> (i64,i64) {
 fn next_value(line : &Vec<i64>) -> i64 {
  let mut mat = Vec::new();
  let len = line.len();
  mat.push({let mut new = line.clone(); new.push(0); new});
  for i in 1..=len { mat.push(vec![0;len+1]) }
  for i in 1..=len { for j in (0..len-i) { mat[i][j] = mat[i-1][j+1] - mat[i-1][j]}}
  let mut lowest_level=0;
  for i in 1..=len { if mat[i].iter().fold(true,|a,&x| a && (x==0)) { lowest_level = i; break }}
  for i in (1..=lowest_level).rev() { mat[i-1][len-i+1] = mat[i][len-i] + mat[i-1][len-i] }
  // for x in mat[0].iter() {print!("{} ",x);}
  // println!("");
  mat[0][len]
 }
 let input_raw = input_all("09.txt").expect("file op error");
 let input =
  input_raw
  .lines() 
  .map(|s|s.split(' ').map(|x|x.parse::<i64>().unwrap()).collect::<Vec<i64>>())
  .collect::<Vec<Vec<i64>>>();
 // for x in input.iter().map(next_value) {println!("{}",x);}
 (input.iter().map(next_value).sum()
 ,input.into_iter().map(|mut data|{data.reverse(); next_value(&data)}).sum())
 // ^ reverse in place reduces memory usage / removes collect call
 // ,input.into_iter().map(|data| next_value(&data.into_iter().rev().collect())).sum())
}

fn problem_08a () -> u64 {
 fn idx_of_str (cs : &str) -> u16 {
  let mut idx = 0;
  for c in cs.chars() {
   idx *= 36;
   idx += match c {
    'A'..='Z' => (c as u32 - 'A' as u32).max(0).min(35) as u16,
    '0'..='9' => (c as u32 - '0' as u32 + 26).max(0).min(35) as u16,
    _ => panic!("Invalid Index!")
   };
  }
  idx
 }
 let input_raw = input_all("08.txt").expect("file op error");
 let (dir, map) = {
  let mut input_it = input_raw.lines().filter(|s|!s.is_empty());
  let dir = input_it.next().unwrap().chars().map(|c| c == 'R').collect::<Vec<_>>();
  let mut map = [(36*36*36u16,36*36*36u16); 36*36*36];
  for s in input_it {
   map[idx_of_str(s.get(0..3).unwrap()) as usize] =
   (idx_of_str(s.get(7..10).unwrap()),idx_of_str(s.get(12..15).unwrap()));
  }
  (dir,map)
 };
 let mut ptr = 0usize;
 let end = idx_of_str("ZZZ") as usize;
 let mut acc = 0;
 let mut dir_idx = 0;
 while ptr != end {
  if dir[dir_idx] {ptr = map[ptr].1 as usize} else {ptr = map[ptr].0 as usize}
  dir_idx = (dir_idx + 1) % dir.len();
  acc += 1;
  // println!("{}, {}, {}",ptr,dir_idx,acc);
 }
 acc as u64
}

fn problem_08b () -> u64 {
 fn idx_of_str (cs : &str) -> u16 {
  let mut idx = 0;
  for c in cs.chars() {
   idx *= 36;
   idx += match c {
    'A'..='Z' => (c as u32 - 'A' as u32).max(0).min(35) as u16,
    '0'..='9' => (c as u32 - '0' as u32 + 26).max(0).min(35) as u16,
    _ => panic!("Invalid Index!")
   };
  }
  idx
 }
 let input_raw = input_all("08.txt").expect("file op error");
 // let mut a_s = Vec::<u16>::new();
 let mut z_s = Vec::<u16>::new();
 let (dir, map) = {
  let mut input_it = input_raw.lines().filter(|s|!s.is_empty());
  let dir = input_it.next().unwrap().chars().map(|c| c == 'R').collect::<Vec<_>>();
  let mut map = [(36*36*36u16,36*36*36u16); 36*36*36];
  for s in input_it {
   // map[idx_of_str(&(*s)[0..3]) as usize] =
   // map[idx_of_str(s.get(0..3).unwrap()) as usize] =
   map[idx_of_str(&s[0..3]) as usize] = (idx_of_str(&s[7..10]),idx_of_str(&s[12..15]));
   // if s[2..3].chars().next() == Some('A') {a_s.push(idx_of_str(s.get(0..3).unwrap()))} else
   if s[2..3].chars().next() == Some('Z') {z_s.push(idx_of_str(s.get(0..3).unwrap()))}
  }
  (dir,map)
 };
 /* cheating based on undocumented input design */
 let mut loops = Vec::<u64>::new();
 for start in z_s {
  let mut ptr = start as usize;
  let end = start as usize;
  let mut acc = 0u64;
  let mut dir_idx = 0;
  loop {
   if dir[dir_idx] {ptr = map[ptr].1 as usize} else {ptr = map[ptr].0 as usize}
   dir_idx = (dir_idx + 1) % dir.len();
   acc += 1;
   if ptr == end && dir_idx == 0 {break}
  }
  loops.push(acc/(dir.len() as u64));
 }
 dir.len() as u64 * loops.iter().fold(1u64,|a,&x|a * x as u64)
}

fn problem_10a () -> u64 {
 fn a_da (a : usize, da : isize) -> usize { ((a as isize) + da) as usize }
 fn print_ptr(ptr : &(usize,usize,usize,usize,u8)) {
  println!("ptr: {}, {}, {}, {}, {}",ptr.0, ptr.1, ptr.2, ptr.3, char::from_u32(ptr.4 as u32).unwrap());
 }
 fn step (map : &Vec<Vec<u8>>, ptr : &mut (usize,usize,usize,usize,u8)) {
  let (dy, dx) = (ptr.2 as isize - ptr.0 as isize, ptr.3 as isize - ptr.1 as isize);
  if ptr.4 == b'|' || ptr.4 == b'-' {*ptr = (ptr.2, ptr.3, a_da(ptr.2,0+dy), a_da(ptr.3,0+dx), map[a_da(ptr.2,0+dy)][a_da(ptr.3,0+dx)]); return}
  if ptr.4 == b'7' || ptr.4 == b'L' {*ptr = (ptr.2, ptr.3, a_da(ptr.2,0+dx), a_da(ptr.3,0+dy), map[a_da(ptr.2,0+dx)][a_da(ptr.3,0+dy)]); return}
  if ptr.4 == b'J' || ptr.4 == b'F' {*ptr = (ptr.2, ptr.3, a_da(ptr.2,0-dx), a_da(ptr.3,0-dy), map[a_da(ptr.2,0-dx)][a_da(ptr.3,0-dy)]); return}
  if ptr.4 == b'S' {*ptr = (ptr.2, ptr.3, ptr.2, ptr.3, b'S'); return}
 }
 let input_raw = input_all("10.txt").expect("file op error");
 let input = input_raw.lines().map(|s|s.bytes().collect()).collect::<Vec<Vec<u8>>>();
 let (h, w) = (input.len(), input[0].len());
 let mut ptr = (0,0,0,0,0);
 for i in 0..h { for j in 0..w { if input[i][j] == b'S' { ptr.0 = i; ptr.1 = j; break } }}

 // find a valid initial point (any will do)
 if      ptr.0 >= 1 &&
       ( input[ptr.0-1][ptr.1] == b'|'
      || input[ptr.0-1][ptr.1] == b'F'
      || input[ptr.0-1][ptr.1] == b'7') { ptr.2 = ptr.0-1; ptr.3 = ptr.1 }
 else if ptr.0 < h &&
       ( input[ptr.0+1][ptr.1] == b'|'
      || input[ptr.0+1][ptr.1] == b'L'
      || input[ptr.0+1][ptr.1] == b'J') { ptr.2 = ptr.0+1; ptr.3 = ptr.1 }
 else if ptr.1 >= 1 &&
       ( input[ptr.0][ptr.1-1] == b'-'
      || input[ptr.0][ptr.1-1] == b'L'
      || input[ptr.0][ptr.1-1] == b'F') { ptr.2 = ptr.0; ptr.3 = ptr.1-1 }
 else if ptr.1 < w &&
       ( input[ptr.0][ptr.1+1] == b'-'
      || input[ptr.0][ptr.1+1] == b'J'
      || input[ptr.0][ptr.1+1] == b'7') { ptr.2 = ptr.0; ptr.3 = ptr.1+1 }
 ptr.4 = input[ptr.2][ptr.3];

 // for i in 0..h { for j in 0..w { print!("{}",char::from_u32(input[i][j] as u32).unwrap());} println!("");}
 // print_ptr(&ptr);
 
 let mut acc = 0u64;
 while ptr.4 != b'S' {
  acc += 1;
  step(&input,&mut ptr);
 }
 (acc+1)/2
}

fn problem_10b () -> u64 {
 fn a_da (a : usize, da : isize) -> usize { ((a as isize) + da) as usize }
 fn print_ptr(ptr : &(usize,usize,usize,usize,u8)) {
  println!("ptr: {}, {}, {}, {}, {}",ptr.0, ptr.1, ptr.2, ptr.3, char::from_u32(ptr.4 as u32).unwrap());
 }
 fn step (map : &Vec<Vec<u8>>, ptr : &mut (usize,usize,usize,usize,u8)) {
  let (dy, dx) = (ptr.2 as isize - ptr.0 as isize, ptr.3 as isize - ptr.1 as isize);
  if ptr.4 == b'|' || ptr.4 == b'-' {*ptr = (ptr.2, ptr.3, a_da(ptr.2,0+dy), a_da(ptr.3,0+dx), map[a_da(ptr.2,0+dy)][a_da(ptr.3,0+dx)]); return}
  if ptr.4 == b'7' || ptr.4 == b'L' {*ptr = (ptr.2, ptr.3, a_da(ptr.2,0+dx), a_da(ptr.3,0+dy), map[a_da(ptr.2,0+dx)][a_da(ptr.3,0+dy)]); return}
  if ptr.4 == b'J' || ptr.4 == b'F' {*ptr = (ptr.2, ptr.3, a_da(ptr.2,0-dx), a_da(ptr.3,0-dy), map[a_da(ptr.2,0-dx)][a_da(ptr.3,0-dy)]); return}
  if ptr.4 == b'S' {*ptr = (ptr.2, ptr.3, ptr.2, ptr.3, b'S'); return}
 }
 let input_raw = input_all("10.txt").expect("file op error");
 let map = input_raw.lines().map(|s|s.bytes().collect()).collect::<Vec<Vec<u8>>>();
 let (h, w) = (map.len(), map[0].len());
 
 let mut ptr = (0,0,0,0,0);
 for i in 0..h { for j in 0..w { if map[i][j] == b'S' { ptr.0 = i; ptr.1 = j; break } }}

 // find a valid initial point (any will do)
 if      ptr.0 >= 1 &&
       ( map[ptr.0-1][ptr.1] == b'|'
      || map[ptr.0-1][ptr.1] == b'F'
      || map[ptr.0-1][ptr.1] == b'7') { ptr.2 = ptr.0-1; ptr.3 = ptr.1 }
 else if ptr.0 < h &&
       ( map[ptr.0+1][ptr.1] == b'|'
      || map[ptr.0+1][ptr.1] == b'L'
      || map[ptr.0+1][ptr.1] == b'J') { ptr.2 = ptr.0+1; ptr.3 = ptr.1 }
 else if ptr.1 >= 1 &&
       ( map[ptr.0][ptr.1-1] == b'-'
      || map[ptr.0][ptr.1-1] == b'L'
      || map[ptr.0][ptr.1-1] == b'F') { ptr.2 = ptr.0; ptr.3 = ptr.1-1 }
 else if ptr.1 < w &&
       ( map[ptr.0][ptr.1+1] == b'-'
      || map[ptr.0][ptr.1+1] == b'J'
      || map[ptr.0][ptr.1+1] == b'7') { ptr.2 = ptr.0; ptr.3 = ptr.1+1 }
 ptr.4 = map[ptr.2][ptr.3];

 // for i in 0..h { for j in 0..w { print!("{}",char::from_u32(map[i][j] as u32).unwrap());} println!("");}
 // print_ptr(&ptr);
 
 let mut subpixels = vec![vec![b'.';w*2+1];h*2+1];
 let mut is_path = vec![vec![false;w];h];
 is_path[ptr.0][ptr.1] = true;
 let (sy,sx) = (ptr.0,ptr.1);

 while ptr.4 != b'S' {
  is_path[ptr.2][ptr.3] = true;
  step(&map,&mut ptr);
 }

 for i in 1..(h*2) { for j in 1..(w*2) {
  if i % 2 == 1 && j % 2 == 1 && is_path[i/2][j/2] {subpixels[i][j] = map[i/2][j/2]}
  else if i % 2 == 1 && is_path[i/2][(j-1)/2] && is_path[i/2][(j+1)/2]
          && ( map[i/2][(j-1)/2] == b'-'
            || map[i/2][(j-1)/2] == b'F'
            || map[i/2][(j-1)/2] == b'L')
          && ( map[i/2][(j+1)/2] == b'-'
            || map[i/2][(j+1)/2] == b'7'
            || map[i/2][(j+1)/2] == b'J') { subpixels[i][j] = b'-' }
  else if j % 2 == 1 && is_path[(i-1)/2][j/2] && is_path[(i+1)/2][j/2]
          && ( map[(i-1)/2][j/2] == b'|'
            || map[(i-1)/2][j/2] == b'F'
            || map[(i-1)/2][j/2] == b'7')
          && ( map[(i+1)/2][j/2] == b'|'
            || map[(i+1)/2][j/2] == b'L'
            || map[(i+1)/2][j/2] == b'J') { subpixels[i][j] = b'|' }
 }}

 let shape_of_s =
  {
   let (n,e,s,w) = (map[sy-1][sx],map[sy][sx+1],map[sy+1][sx],map[sy][sx-1]);
   let (vn,ve,vs,vw) = 
    ((n == b'|' || n == b'7' || n == b'F')
    ,(e == b'-' || e == b'7' || e == b'J')
    ,(s == b'|' || s == b'L' || s == b'J')
    ,(w == b'-' || w == b'L' || w == b'F'));
   if      vn && ve { b'L' }
   else if vn && vs { b'|' }
   else if vn && vw { b'J' }
   else if ve && vw { b'-' }
   else if vs && vw { b'7' }
   else if vs && ve { b'F' }
   else { panic!("at the disco!") }
  };

 subpixels[sy][sx] = shape_of_s;

 match shape_of_s {
  b'|' => {subpixels[sy*2+0][sx*2+1] = b'|'; subpixels[sy*2+2][sx*2+1] = b'|'}, 
  b'-' => {subpixels[sy*2+1][sx*2+0] = b'-'; subpixels[sy*2+1][sx*2+2] = b'-'}, 
  b'L' => {subpixels[sy*2+1][sx*2+2] = b'-'; subpixels[sy*2+0][sx*2+1] = b'|'}, 
  b'F' => {subpixels[sy*2+1][sx*2+2] = b'-'; subpixels[sy*2+2][sx*2+1] = b'|'}, 
  b'J' => {subpixels[sy*2+0][sx*2+1] = b'|'; subpixels[sy*2+1][sx*2+0] = b'-'}, 
  b'7' => {subpixels[sy*2+2][sx*2+1] = b'|'; subpixels[sy*2+1][sx*2+0] = b'-'}, 
  _ => panic!("at the disco!")
 }

 // for i in 0..2*h+1 { for j in 0..2*w+1 { print!("{}",char::from_u32(subpixels[i][j] as u32).unwrap());} println!("");}

 fn mark_sub (subpixels : &mut Vec<Vec<u8>>, y: usize, x: usize) {
  if y < subpixels.len() && x < subpixels[0].len() && subpixels[y][x] == b'.' {
   subpixels[y][x] = b'I';
   mark_sub(subpixels, y-1, x);
   mark_sub(subpixels, y, x+1);
   mark_sub(subpixels, y+1, x);
   mark_sub(subpixels, y, x-1);
  }
 }

 // Iterate, you fool!
 fn mark_sub_it (subpixels : &mut Vec<Vec<u8>>) {
  let mut changed = false;
  let h = subpixels.len();
  let w = subpixels[0].len();
  loop {
   for i in 0..h { for j in 0..w {
    if subpixels[i][j] == b'.' &&
     (  (i > 0 && subpixels[i-1][j] == b'I')
     || (j < w-1 && subpixels[i][j+1] == b'I')
     || (i < h-1 && subpixels[i+1][j] == b'I')
     || (j > 0 && subpixels[i][j-1] == b'I')) {changed=true; subpixels[i][j] = b'I'}
   }}
   for i in (0..h).rev() { for j in (0..w).rev() {
    if subpixels[i][j] == b'.' &&
     (  (i > 0 && subpixels[i-1][j] == b'I')
     || (j < w-1 && subpixels[i][j+1] == b'I')
     || (i < h-1 && subpixels[i+1][j] == b'I')
     || (j > 0 && subpixels[i][j-1] == b'I')) {changed=true; subpixels[i][j] = b'I'}
   }}
   if !changed { break }
   changed = false;
  }
 }

 // inverted, use b'I' at boundary and check for b'.'
 subpixels[0][0] = b'I';
 // direct, based on guess (will work even if guess is wrong)
 // subpixels[sy*2+2][sx*2+0] = b'I';
 mark_sub_it(&mut subpixels);
 // may overflow if starting from the boundaries
 // mark_sub(&mut subpixels, sy*2+2, sx*2+0);
 let mut acc = 0;
 let marker = if subpixels[0][0] == b'I' { b'.' } else { b'I' };
 for i in 0..h { for j in 0..w { if subpixels[2*i+1][2*j+1] == marker { acc += 1 } }}
 acc
}

fn problem_11 () -> (usize, usize) {
 fn manhattan_distance(p1 : (isize,isize), p2 : (isize,isize)) -> usize {
  (p2.0-p1.0).abs() as usize + (p2.1-p1.1).abs() as usize
 }
 let space_scalar_1 = 2;
 let space_scalar_2 = 1000000;
 let input_raw = input_all("11.txt").expect("file op error");
 let mut map = input_raw.lines().map(|s|s.bytes().collect()).collect::<Vec<Vec<u8>>>();

 {
  let mut g_in_row;
  let mut g_in_col;
  // input is square, so you can check both rows and cols in one pass
  // label empty spaces with increments
  for y in 0..map.len() {
   g_in_row = false;
   g_in_col = false;
   for x in 0..map.len() {
    if map[y][x] == b'#' { g_in_row = true };
    if map[x][y] == b'#' { g_in_col = true };
   }
   if !g_in_row {for x in 0..map.len() {map[y][x] += 1}};
   if !g_in_col {for x in 0..map.len() {map[x][y] += 1}};
  }
 }

 let mut galaxy_list1 = Vec::new();
 let mut galaxy_list2 = Vec::new();
 {
  let mut real_pos_pt1 = (0,0); // y,x tuple
  let mut real_pos_pt2 = (0,0);
  for y in 0..map.len() {
   for x in 0..map.len() {
    if map[y][x] == b'#' { galaxy_list1.push(real_pos_pt1); galaxy_list2.push(real_pos_pt2)}
    if map[y][x] > b'.' { real_pos_pt1.1 += space_scalar_1 ; real_pos_pt2.1 += space_scalar_2 } else { real_pos_pt1.1 += 1; real_pos_pt2.1 += 1 }
   }
   if map[y][0] > b'.' { real_pos_pt1.0 += space_scalar_1 ; real_pos_pt2.0 += space_scalar_2 } else { real_pos_pt1.0 += 1; real_pos_pt2.0 += 1 }
   // don't forget to reset!
   real_pos_pt1.1 = 0; real_pos_pt2.1 = 0;
  }
 }
 let mut sum_pt1 = 0;
 let mut sum_pt2 = 0;
 for i in 0..(galaxy_list1.len() - 1) {
  for j in (i+1)..galaxy_list1.len() {
   sum_pt1 += manhattan_distance(galaxy_list1[i],galaxy_list1[j]);
   sum_pt2 += manhattan_distance(galaxy_list2[i],galaxy_list2[j]);
  }
 }
 (sum_pt1, sum_pt2)
}

// Prob12: Dynamic Programming
fn problem_12 () -> (usize, usize) {
 struct Vec3D<T> { dim1 : usize, dim2 : usize, dim3 : usize, data : Vec<T> }
 impl<T: Copy> Vec3D<T> {
  fn new(dim1 : usize, dim2 : usize, dim3 : usize, init : T) -> Vec3D<T> { Vec3D {dim1, dim2, dim3, data : vec![init;dim1*dim2*dim3]}}
  fn get(&self, z: usize, y : usize, x : usize) -> T { self.data[z * self.dim3 * self.dim2 + y * self.dim3 + x] }
  fn set(&mut self, z : usize, y : usize, x : usize, val : T) { self.data[z * self.dim3 * self.dim2 + y * self.dim3 + x] = val }
 }

 fn dp_solve(mut fmt1 : Vec<u8>, mut fmt2 : Vec<usize>) -> usize {
  fmt1.push(b'.'); fmt2.push(0);
  let dim1 = fmt1.len();
  let dim2 = fmt2.len();
  let dim3 = fmt2.iter().fold(0usize,|acc,x| acc.max(*x as usize)) + 1;
  // build dynamic programming table
  let mut dp = Vec3D::new(dim1,dim2,dim3,0usize);
  // set base case (1 = "valid")
  if fmt1[0] != b'.' {dp.set(0,0,1,1);} // "increment" k [0,0,1]
  if fmt1[0] != b'#' {dp.set(0,0,0,1);} // do not "increment" k [0,0,0]
  for i in 1..dim1 { for j in 0..dim2 { for k in 0..dim3 {
   // encounter dot: #. -> reset k, increase j ; .. -> k = 0, keep j
   // add values of possible previous paths: 
   //  [i-1,j-1,fmt2[j-1]] (#.) && [i-1,j,k] (..)
   let if_dot =
    if k == 0 { (if j > 0 {dp.get(i-1,j-1,fmt2[j-1])} else {0}) + dp.get(i-1,j,k) } else {0};
   // adding a hash "increments" k (extending valid paths)
   let if_hash = if k > 0 {dp.get(i-1,j,k-1)} else {0};
   match fmt1[i] {
    b'.' => {dp.set(i,j,k,if_dot)},
    b'#' => {dp.set(i,j,k,if_hash)},
    b'?' => {dp.set(i,j,k,if_dot + if_hash)},
    _ => panic!("invalid input for fmt1!"),
   };
  }}}
 dp.get(dim1-1,dim2-1,0)
 }

 let input = input_all("12.txt").expect("file op error");
 let rows_a = input.lines().filter(|s| s.len() > 2)
  .map(|s| {
   let mut it = s.split_ascii_whitespace();
   let fmt1 = it.next().unwrap();
   let fmt2 = it.next().unwrap().split(',').map(|s| s.parse::<usize>().unwrap()).collect();
   dp_solve(fmt1.bytes().collect(),fmt2)});
 let rows_b = input.lines().filter(|s| s.len() > 2)
  .map(|s| {
   let mut it = s.split_ascii_whitespace();
   let fmt1 = it.next().unwrap();
   let fmt1ext = fmt1.to_owned() // repeat 5 times interspersed with ?
                 + "?" + fmt1
                 + "?" + fmt1
                 + "?" + fmt1
                 + "?" + fmt1;
   let fmt2 = it.next().unwrap().split(',').map(|s| s.parse::<usize>().unwrap());
   let fmt2ext = fmt2.clone() // repeat 5 times
                 .chain(fmt2.clone())
                 .chain(fmt2.clone())
                 .chain(fmt2.clone())
                 .chain(fmt2).collect();
   dp_solve(fmt1ext.bytes().collect(), fmt2ext)});
 (rows_a.sum(),rows_b.sum())
}

fn problem_13() -> (usize, usize) {
 fn test_href(mat : &Vec<Vec<char>>, n : usize) -> bool {
  (0..n)
   .rev()
   .zip(n..mat.len())
   // test equality of horizontal slices
   .all(|(i,j)| (mat[i] == mat[j]))
 }
 fn test_vref(mat : &Vec<Vec<char>>, n : usize) -> bool {
  (0..n)
   .rev()
   .zip(n..mat[0].len())
   .all(|(i,j)| 
    // test equality of vertical slice iterators
        (0..mat.len()).map(|idx| mat[idx][i])
    .eq((0..mat.len()).map(|idx| mat[idx][j]))
   )
 }
 fn test2_href(mat : &Vec<Vec<char>>, n : usize) -> bool {
  (0..n)
   .rev()
   .zip(n..mat.len())
   // test levenshtein distance = 1
   .map(|(i,j)|  (0..mat[i].len()).map(|idx| mat[i][idx])
            .zip((0..mat[j].len()).map(|idx| mat[j][idx]))
            .map(|(x,y)| if x == y {0} else {1})
            .sum::<usize>())
   .sum::<usize>() == 1
 }
 fn test2_vref(mat : &Vec<Vec<char>>, n : usize) -> bool {
  (0..n)
   .rev()
   .zip(n..mat[0].len())
   // test levenshtein distance = 1
   .map(|(i,j)|  (0..mat.len()).map(|idx| mat[idx][i])
            .zip((0..mat.len()).map(|idx| mat[idx][j]))
            .map(|(x,y)| if x == y {0} else {1})
            .sum::<usize>())
   .sum::<usize>() == 1
 }

 // input processing "function"
 let input_raw = input_all("13.txt").expect("file op error");
 let mut input = Vec::new();
 {
  let mut it = input_raw.lines();
  loop {
   let mut mat : Vec<Vec<char>> = Vec::new();
   loop {
    let next = it.next();
    if next.is_none() || next.unwrap().len() == 0 { break }
    mat.push(next.unwrap().chars().collect());
   }
   if mat.len() > 1 {input.push(mat);} else {break};
  }
 }

 let mut res1 = 0;
 let mut res2 = 0;
 for mat in input {
  for i in 1..mat.len() {
   if test_href(&mat,i) { res1 += 100*i }
   if test2_href(&mat,i) { res2 += 100*i }
  }
  for i in 1..mat[0].len() {
   if test_vref(&mat,i) { res1 += i }
   if test2_vref(&mat,i) { res2 += i }
  }
 }
 (res1,res2)
}

fn problem_14() -> (usize, usize) {

 fn calculate_weight(mat : &Vec<Vec<u8>>) -> usize {
  let mut res = 0;
  for i in 0..mat.len() { for j in 0..mat[i].len() {
   if mat[i][j] == b'O' { res += (mat.len()-i) }
  }}; res
 }

 fn write_vslice(dst: &mut Vec<Vec<u8>>, idx : usize, src: Vec<u8>) {
  for i in 0..dst.len() { dst[i][idx] = src[i] }
 }

 fn vslice(mat: &Vec<Vec<u8>>, idx : usize) -> Vec<u8> {
  (0..mat.len()).map(|i| mat[i][idx]).collect()
 }

 fn roll_vert(mat: &mut Vec<Vec<u8>>, up: bool) {
  for idx in 0..mat[0].len() {
   let mut slice = vslice(&mat, idx);
   for mut grp in slice.chunk_by_mut(|&a,&b| a != b'#' && b != b'#') {
    if grp[0] != b'#' {
     let rocks = grp.iter().fold(0, |acc,&x| if x == b'O' { acc + 1 } else { acc });
     grp.swap_with_slice(
      if up { [vec![b'O'; rocks],vec![b'.'; grp.len()-rocks]] }
      else  { [vec![b'.'; grp.len()-rocks],vec![b'O'; rocks]] }
      .concat().as_mut_slice())
    }
   }
   write_vslice(mat, idx, slice);
  }
 }
 fn roll_up(mat: &mut Vec<Vec<u8>>) { roll_vert(mat,true) }
 fn roll_down(mat: &mut Vec<Vec<u8>>) { roll_vert(mat,false) }

 fn roll_horiz(mat: &mut Vec<Vec<u8>>, left: bool) {
  for idx in 0..mat.len() {
   for mut grp in mat[idx].chunk_by_mut(|&a,&b| a != b'#' && b != b'#') {
    if grp[0] != b'#' {
     let rocks = grp.iter().fold(0, |acc,&x| if x == b'O' { acc + 1 } else { acc });
     grp.swap_with_slice(
      if left { [vec![b'O'; rocks],vec![b'.'; grp.len()-rocks]] }
      else    { [vec![b'.'; grp.len()-rocks],vec![b'O'; rocks]] }
      .concat().as_mut_slice())
    }
   }
  }
 }
 fn roll_left(mat: &mut Vec<Vec<u8>>) { roll_horiz(mat,true) }
 fn roll_right(mat: &mut Vec<Vec<u8>>) { roll_horiz(mat,false) }

 let input_raw = input_all("14.txt").expect("file op error");
 let mut input : Vec<Vec<u8>> = input_raw.lines().map(|s| s.bytes().collect()).collect();
 roll_up(&mut input);
 let res1 = calculate_weight(&input);

 // first complete cycle 1
 roll_left(&mut input);
 roll_down(&mut input);
 roll_right(&mut input);

 // continue cycling to magic number
 for _ in 1..1000 {
  roll_up(&mut input);
  roll_left(&mut input);
  roll_down(&mut input);
  roll_right(&mut input);
 }
 let res2 = calculate_weight(&input);
 (res1,res2)
}

fn problem_14_smart() -> (usize, usize) {

 fn calculate_weight(mat : &Vec<Vec<u8>>) -> usize {
  let mut res = 0;
  for i in 0..mat.len() { for j in 0..mat[i].len() {
   if mat[i][j] == b'O' { res += (mat.len()-i) }
  }}; res
 }

 fn write_vslice(dst: &mut Vec<Vec<u8>>, idx : usize, src: Vec<u8>) {
  for i in 0..dst.len() { dst[i][idx] = src[i] }
 }

 fn vslice(mat: &Vec<Vec<u8>>, idx : usize) -> Vec<u8> {
  (0..mat.len()).map(|i| mat[i][idx]).collect()
 }

 enum Direction {
  Up,
  Down,
  Left,
  Right
 }

 // Although the code is almost identical, these two functions treat
 // lifetimes and ownership differently depending on the type of slice.
 // The horiz function operates directly on mat, while vert must first 
 // make a clone of each column with vslice before copying back into mat.

 fn roll_vert(mat: &mut Vec<Vec<u8>>, up: bool) {
  for idx in 0..mat[0].len() {
   let mut slice = vslice(&mat, idx);
   for mut grp in slice.chunk_by_mut(|a,b| *a != b'#' && *b != b'#') {
    if grp[0] != b'#' {
     let rocks = grp.iter().fold(0, |acc,x| if *x == b'O' { acc + 1 } else { acc });
     grp.swap_with_slice(
      if up { [vec![b'O'; rocks],vec![b'.'; grp.len()-rocks]] }
      else  { [vec![b'.'; grp.len()-rocks],vec![b'O'; rocks]] }
      .concat().as_mut_slice())
    }
   }
   write_vslice(mat, idx, slice);
  }
 }

 fn roll_horiz(mat: &mut Vec<Vec<u8>>, left: bool) {
  for idx in 0..mat.len() {
   for mut grp in mat[idx].chunk_by_mut(|a,b| *a != b'#' && *b != b'#') {
    if grp[0] != b'#' {
     let rocks = grp.iter().fold(0, |acc,x| if *x == b'O' { acc + 1 } else { acc });
     grp.swap_with_slice(
      if left { [vec![b'O'; rocks],vec![b'.'; grp.len()-rocks]] }
      else    { [vec![b'.'; grp.len()-rocks],vec![b'O'; rocks]] }
      .concat().as_mut_slice())
    }
   }
  }
 }

 // readable interface
 fn roll(mat: &mut Vec<Vec<u8>>, dir: Direction){
  match dir {
   Direction::Up    => roll_vert(mat,true),
   Direction::Down  => roll_vert(mat,false),
   Direction::Left  => roll_horiz(mat,true),
   Direction::Right => roll_horiz(mat,false),
  }
 }

 let input_raw = input_all("14.txt").expect("file op error");
 let mut input : Vec<Vec<u8>> = input_raw.lines().map(|s| s.bytes().collect()).collect();
 let mut weight_stack = Vec::new();
 let mut cycle_length = None;
 let mut offset = 0;

 roll(&mut input, Direction::Up);
 let res1 = calculate_weight(&input);

 // first complete cycle 1
 roll(&mut input, Direction::Left);
 roll(&mut input, Direction::Down);
 roll(&mut input, Direction::Right);

 weight_stack.push(calculate_weight(&input));

 // expect an early break from this loop
 for i in 1..1000 {
  // i == weight_stack.len()
  roll(&mut input, Direction::Up);
  roll(&mut input, Direction::Left);
  roll(&mut input, Direction::Down);
  roll(&mut input, Direction::Right);
  let w = calculate_weight(&input);
  cycle_length = weight_stack.iter().rposition(|&x| x == w).map(|x| i - x);
  match cycle_length {
   None => (),
   Some(x) if x * 2 >= i || x < 5 => (),
   Some(x) if weight_stack[(i - x)..] == weight_stack[(i - x * 2)..(i - x)] => {
    // rolling i+1 = steady_state(0)
    offset = (1_000_000_000 - (i+1)) % x;
    break
   },
   Some(_) => (),
  }
  weight_stack.push(w);
 }
 
 let res2 = weight_stack[weight_stack.len()-cycle_length.unwrap()+offset];
 (res1,res2)
}

fn problem_15 () -> (usize,usize) {

 fn digest (b : &[u8]) -> usize {
  b.iter().fold(0u8,|acc,&x| ((acc as u16 + x as u16) * 17u16 & 0xFF) as u8) as usize
 }

 let input_raw = input_all("15.txt").expect("file op error");
 // trim to remove trailing newline
 let res1 = input_raw.trim().split(',').fold(0,|acc,s| acc + digest(s.as_bytes()));

 struct Lens {
  flen : u8,
  lbl : Vec<u8>,
 }

 // let mut boxes = vec![Vec::<Lens>::new();256];
 let mut boxes = Vec::new();
 for _ in 0..256 { boxes.push(Vec::<Lens>::new()) };

 for mut instr in input_raw.trim().split(',').map(|s| s.bytes().collect::<Vec<_>>()) {
  match instr.pop() {
   Some(b'-') => {
    let id = digest(&instr[..]);
    if let Some(pos) = boxes[id].iter().position(|x| instr.iter().eq(x.lbl.iter())) { boxes[id].remove(pos); }
   },
   Some(flen) => {
    let flen = flen - b'0'; // convert ascii to digit
    instr.pop();
    let id = digest(&instr[..]);
    if let Some(pos) = boxes[id].iter().position(|x| instr.iter().eq(x.lbl.iter())) { boxes[id][pos].flen = flen; }
    else { boxes[id].push( Lens { flen : flen, lbl : instr } ); }
   },
   None => { assert!(false) }
  }
 }

 let res2 =
  boxes.iter().enumerate()
   .fold(0, |acc, (i,b)| acc + b.iter().enumerate()
    .fold(0, |acc, (j,x)|acc + (i+1) * (j+1) * (x.flen as usize)));

 (res1, res2)
}

fn main () -> () {
 // Call Problem Solutions
 /*
 println!("Problem 01: {}, {}", problem_01a(), problem_01b());
 let res = problem_02();
 println!("Problem 02: {}, {}", res.0, res.1);
 println!("Problem 03: {}, {}", problem_03a(), problem_03b());
 let res = problem_04();
 println!("Problem 04: {}, {}", res.0, res.1);
 let res = problem_05();
 println!("Problem 05: {}, {}", res.0, res.1);
 let res = problem_06();
 println!("Problem 06: {}, {}", res.0, res.1);
 println!("Problem 07: {}, {}", problem_07a(), problem_07b());
 let res = problem_09();
 println!("Problem 09: {}, {}", res.0, res.1);
 println!("Problem 08: {}, {}", problem_08a(), problem_08b());
 println!("Problem 10: {}, {}", problem_10a(), problem_10b());
 let res = problem_12();
 println!("Problem 12: {}, {}", res.0, res.1);
 let res = problem_13();
 println!("Problem 13: {}, {}", res.0, res.1);
 let res = problem_14();
 println!("Problem 14: {}, {}", res.0, res.1);
 let res = problem_14_smart();
 println!("Problem 14: {}, {}", res.0, res.1);
 */
 let res = problem_15();
 println!("Problem 15: {}, {}", res.0, res.1);
}

