# aoc.py

import re
import math
import itertools
import functools
# import operator

def problem_01a():
 with open("01.txt", 'r') as f:
  count = 0
  for line in f.readlines():
   first = 0; last = 0
   for c in line:
    if c >= '0' and c <= '9':
     last = int(c)
     if first == 0: first = last
   count += first * 10 + last
 print("Problem 01a: {}".format(count))

def problem_01b():
 numbers = [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]
 with open("01.txt", 'r') as f:
  count = 0
  for line in f.readlines():
   first = 0; last = 0
   for i, c in enumerate(line):
    if c >= '0' and c <= '9': last = int(c)
    else:
     for j in range(0,9):
      if numbers[j] == line[i:i+len(numbers[j])]: last = j+1
    if first == 0: first = last
   count += first * 10 + last
 print("Problem 01b: {}".format(count))

# flatten in python (list comprehension)
# return [item for row in matrix for item in row]

def problem_02():
 with open("02.txt", 'r') as f:
  acc_a = 0; acc_b = 0
  for line in f.readlines():
   [id_s, data] = line.split(':')[0:2]
   id = int(id_s.strip().split(' ')[1])
   r = 0; g = 0; b = 0
   for fragment in re.split('[,;]',data):
    [n_s, color] = fragment.strip().split(' ')[0:2]
    n = int(n_s)
    match_s = color.strip()
    if   match_s == "red"   : r = max(r,n)
    elif match_s == "green" : g = max(g,n)
    elif match_s == "blue"  : b = max(b,n)
# Requires Python 3.10+
#     match color.strip() :
#      case "red"   : r = max(r,n)
#      case "green" : g = max(g,n)
#      case "blue"  : b = max(b,n)
#      case _ : pass
   if r <= 12 and g <= 13 and b <= 14 : acc_a += id
   acc_b += r*g*b
 print("Problem 02: {}, {}".format(acc_a,acc_b))

def problem_03a():
 def addr_of_number(s,x):
  idx = -1
  for i in range(x,-1,-1):
   if s[i] >= '0' and s[i] <= '9':
    idx = i
   else:
    break
  return idx
 def number_of_addr(s,x): 
  end = len(s)
  for i in range (x,len(s)):
   if not (s[i] >= '0' and s[i] <= '9'):
    end = i
    break
  return int(s[x:end])
 def raw_of_addr(y,x):
  return (y << 8) + x
 def addr_of_raw(r):
  return (r >> 8, r & 0xFF)
 with open("03.txt", 'r') as f:
  lines = f.readlines()
 h = len(lines); w = len(lines[0])
 addrs = set()
 for y in range(0,h):
  for x in range(0,w):
   c = lines[y][x]
   if not(c  >= '0' and c <= '9') and c != '.':
    for y_p in range(max(y,1)-1,min(y+2,h)):
     for x_p in range(max(x,1)-1,min(x+2,w)):
      c_p = lines[y_p][x_p]
      if (c_p >= '0' and c_p <= '9'):
       # print("{} {}".format(y_p,addr_of_number(lines[y_p],x_p)))
       addrs.add(raw_of_addr(y_p,addr_of_number(lines[y_p],x_p)))
 # for r in addrs:
 #  addr = addr_of_raw(r)
 #  print(number_of_addr(lines[addr[0]],addr[1]))
 print("Problem 03a: {}".format(sum(map(lambda r: number_of_addr(lines[addr_of_raw(r)[0]],addr_of_raw(r)[1]), addrs))))

def problem_03b():
 def addr_of_number(s,x):
  idx = -1
  for i in range(x,-1,-1):
   if s[i] >= '0' and s[i] <= '9':
    idx = i
   else:
    break
  return idx
 def number_of_addr(s,x): 
  end = len(s)
  for i in range (x,len(s)):
   if not (s[i] >= '0' and s[i] <= '9'):
    end = i
    break
  return int(s[x:end])
 def raw_of_addr(y,x):
  return (y << 8) + x
 def addr_of_raw(r):
  return (r >> 8, r & 0xFF)
 with open("03.txt", 'r') as f:
  lines = f.readlines()
 h = len(lines); w = len(lines[0])
 acc = 0; addrs = set()
 for y in range(0,h):
  for x in range(0,w):
   c = lines[y][x]
   if c == '*':
    for y_p in range(max(y,1)-1,min(y+2,h)):
     for x_p in range(max(x,1)-1,min(x+2,w)):
      c_p = lines[y_p][x_p]
      if (c_p >= '0' and c_p <= '9'):
       # print("{} {}".format(y_p,addr_of_number(lines[y_p],x_p)))
       addrs.add(raw_of_addr(y_p,addr_of_number(lines[y_p],x_p)))
    if len(addrs) == 2:
     a = addr_of_raw(addrs.pop())
     b = addr_of_raw(addrs.pop())
     acc += number_of_addr(lines[a[0]],a[1]) * number_of_addr(lines[b[0]],b[1])
     # acc += math.prod(map(lambda r: number_of_addr(lines[addr_of_raw(r)[0]],addr_of_raw(r)[1]), addrs))
    addrs.clear()
 print("Problem 03b: {}".format(acc))

def problem_04():
 def parse(s):
  [win_raw, picks_raw] = s.split(':')[1].split('|')[0:2]
  wins = set(); picks = set()
  for s in re.split(' +', win_raw.strip()): wins.add(s)
  for s in re.split(' +', picks_raw.strip()): picks.add(s)
  # for x in wins.intersection(picks):
  #  print(":{}".format(x))
  return len(wins.intersection(picks))
 with open("04.txt", 'r') as f:
  lines = f.readlines()
 # map iterators are IterOnce so we must collect into a list!
 wins = list(map(parse, lines))
 cards = list(map(lambda x: 1,wins))
 print("Problem 04a: {}".format(sum(map(lambda x: (1 << x) >> 1,wins))))
 for i in range(0,len(wins)-1):
  for j in range(i+1,min(i+1+wins[i],len(wins))):
   cards[j] += cards[i]
 print("Problem 04b: {}".format(sum(cards)))

# non numpy version : use for other problems, maybe?
def problem_06():
 def wins (t,d):
  tf = float(t); df = float(d);
  det =  math.sqrt(tf * tf * 0.25 - df);
  low =  math.floor(0.5 * tf - det)+1
  high = math.ceil(0.5 * tf + det)-1
  return high-low+1
 with open("06e.txt", 'r') as f:
  lines = f.readlines()
 t_1 = map(int,re.split(' +',lines[0].split(':')[1].strip()))
 d_1 = map(int,re.split(' +',lines[1].split(':')[1].strip()))
 wins_1 = map(wins,t_1,d_1)
 t_2 = int(''.join(re.split(' +',lines[0].split(':')[1])))
 d_2 = int(''.join(re.split(' +',lines[1].split(':')[1])))
 print("Problem 06a: {}".format(functools.reduce(int.__mul__,wins_1)))
 print("Problem 06b: {}".format(wins(t_2,d_2)))

# ocaml: 4s/40ms, rust: 2s, python:?
def problem_05a():
 def parse_books(acc,s):
  # acc[0] = acc, acc[1] = almanac
  # if len(s.strip()) == 0: return acc
  # these strings include '\n' so are not empty unless you strip first!
  # thus: if you do not strip it, an empty line can be true - python is dumb
  if not(s.strip()): return acc
  if s.count(':') > 0: return (list(),acc[1] + [acc[0]])
  return (acc[0] + [list(map(int,re.split(' +',s.strip())))],acc[1])
 def map_value_with_book(v,book):
  for [dst,src,length] in book:
   if v >= src and v < src+length: return (v-src+dst)
  return v
 with open("05.txt", 'r') as f:
  lines = f.readlines()
 seeds = list(map(int,re.split(' +',lines[0].split(':')[1].strip())))
 (last_book, almanac) = functools.reduce(parse_books,lines[3:],(list(),list()))
 almanac.append(last_book)
 final_res = functools.reduce(min,map(lambda seed: functools.reduce(map_value_with_book,almanac,seed),seeds))
 print("Problem 05a: {}".format(final_res))

# ocaml: 4s/20ms, rust: 2s, python: 4min12s
def problem_05b():
 def parse_books(acc,s):
  # acc[0] = acc, acc[1] = almanac
  # if len(s.strip()) == 0: return acc
  # these strings include '\n' so are not empty unless you strip first!
  # thus: if you do not strip it, an empty line can be true - python is dumb
  if not(s.strip()): return acc
  if s.count(':') > 0: return (list(),acc[1] + [acc[0]])
  return (acc[0] + [list(map(int,re.split(' +',s.strip())))],acc[1])
 def map_value_with_book(v,book):
  for [dst,src,length] in book:
   if v >= src and v < src+length: return (v-src+dst)
  return v
 def map_value_with_book_inverted(v,book):
  for [src,dst,length] in book:
   if v >= src and v < src+length: return (v-src+dst)
  return v
 with open("05.txt", 'r') as f:
  lines = f.readlines()
 seeds = list(map(int,re.split(' +',lines[0].split(':')[1].strip())))
 (last_book, almanac) = functools.reduce(parse_books,lines[3:],(list(),list()))
 almanac.append(last_book)
 almanac.reverse() # modifies in place
 def pair_reduce(a, s):
  if a[0] is None: return (s,a[1])
  return (None,a[1] + [(a[0],s)])
 def is_valid_seed(s,ranges):
  for (src,length) in ranges:
   if s >= src and s < src+length: return True
  return False
 def is_valid_dst(d,almanac_rev,ranges):
  return is_valid_seed(functools.reduce(map_value_with_book_inverted,almanac_rev,d), ranges)
 (_,seed_ranges) = functools.reduce(pair_reduce,seeds,(None,list()))
 # final_res = functools.reduce(min,map(lambda seed: functools.reduce(map_value_with_book,almanac,seed),seeds))
 res = 0
 while True:
  if is_valid_dst(res,almanac,seed_ranges): break
  res += 1
 print("Problem 05b: {}".format(res))

def problem_07a():
 def parse(s):
  [hand, bid_raw] = s.strip().split(' ')[0:2]
  return (hand, int(bid_raw))
 def n_of_card(c):
  c = ord(c)
  if c >= 0x32 and c <= 0x39: return c - 0x32
  if c == ord('T'): return 8
  if c == ord('J'): return 9
  if c == ord('Q'): return 10
  if c == ord('K'): return 11
  return 12 # if c == ord('A'): return 12
 def hand_rank(hand):
  card_set = [0] * 13
  for i in map(n_of_card, hand): card_set[i] += 1
  card_set.sort(reverse=True)
  if card_set[0] == 5: return 6
  if card_set[0] == 4: return 5
  if card_set[0] == 3 and card_set[1] == 2: return 4
  if card_set[0] == 3: return 3
  if card_set[0] == 2 and card_set[1] == 2: return 2
  if card_set[0] == 2: return 1
  return 0
 def cmp_hand(hand1, hand2):
  res = hand_rank(hand1) - hand_rank(hand2)
  if res != 0: return res
  for (x,y) in zip(map(n_of_card,hand1),map(n_of_card,hand2)):
   res = x - y
   if res != 0: return res
  return res
 with open("07.txt") as f:
  inputs = list(map(parse,f.readlines()))
 key_hand = functools.cmp_to_key(cmp_hand)
 inputs.sort(key=lambda x: key_hand(x[0]))
 print("Problem 07a: {}".format(sum(inputs[i][1]*(i+1) for i in range(0,len(inputs)))))

def problem_07b():
 def parse(s):
  [hand, bid_raw] = s.strip().split(' ')[0:2]
  return (hand, int(bid_raw))
 def n_of_card(c):
  c = ord(c)
  if c >= 0x32 and c <= 0x39: return c - 0x31
  if c == ord('T'): return 9
  if c == ord('J'): return 0
  if c == ord('Q'): return 10
  if c == ord('K'): return 11
  return 12 # if c == ord('A'): return 12
 def hand_rank(hand):
  card_set = [0] * 13
  for i in map(n_of_card, hand): card_set[i] += 1
  jokers = card_set[0]; card_set[0] = 0
  card_set.sort(reverse=True)
  if card_set[0] + jokers == 5: return 6
  if card_set[0] + jokers == 4: return 5
  if card_set[0] + jokers == 3 and card_set[1] == 2: return 4
  if card_set[0] + jokers == 3: return 3
  if card_set[0] + jokers == 2 and card_set[1] == 2: return 2
  if card_set[0] + jokers == 2: return 1
  return 0
 def cmp_hand(hand1, hand2):
  res = hand_rank(hand1) - hand_rank(hand2)
  if res != 0: return res
  for (x,y) in zip(map(n_of_card,hand1),map(n_of_card,hand2)):
   res = x - y
   if res != 0: return res
  return res
 with open("07.txt") as f:
  inputs = list(map(parse,f.readlines()))
 key_hand = functools.cmp_to_key(cmp_hand)
 inputs.sort(key=lambda x: key_hand(x[0]))
 print("Problem 07a: {}".format(sum(inputs[i][1]*(i+1) for i in range(0,len(inputs)))))

def problem_09():
 def parse(s):
  return list(map(int,s.strip().split(' ')))
 def next_value(values):
  length = len(values)
  mat =[[values[j] if i == 0 and j < length else 0 for j in range(length+1)] for i in range(length+1)]
  for i in range(1,length+1):
   for j in range(0,length-i):
    mat[i][j] = mat[i-1][j+1] - mat[i-1][j]
  lowest_level = 0
  for i in range(1,length+1):
   lowest_level = i
   if functools.reduce(lambda a,x: a and (x==0),mat[i],True): break
  for i in range(lowest_level-1,-1,-1):
   mat[i][length-i] = mat[i+1][length-i-1] + mat[i][length-i-1]
  return mat[0][length]
 with open("09.txt") as f:
  inputs = list(map(parse,f.readlines()))
 print("Problem 09a: {}".format(sum(map(next_value,inputs))))
 for line in inputs: line.reverse()
 print("Problem 09b: {}".format(sum(map(next_value,inputs))))

def problem_08a():
 def n_of_c(c):
  if c[0] >= 'A' and c[0] <= 'Z': return ord(c[0])-ord('A')
  if c[0] >= '0' and c[0] <= '9': return ord(c[0])-ord('0')+26
 def addr_of_s(s):
  return n_of_c(s[0])*36*36 + n_of_c(s[1])*36 + n_of_c(s[2])
 data = [(36*36*36, 36*36*36) for i in range(0,36*36*36)]
 def parse(s):
  data[addr_of_s(s[0:3])] = (addr_of_s(s[7:10]),addr_of_s(s[12:15]))
 with open("08.txt") as f:
  for (i,s) in enumerate(f.readlines()):
   if i == 0: directions = s.strip()
   elif i == 1: pass
   elif i >= 2: parse(s.strip())
 ptr = 0; acc = 0; idx = 0
 idx_len = len(directions)
 while ptr != addr_of_s("ZZZ"):
  if directions[idx] == 'L': ptr = data[ptr][0]
  else: ptr = data[ptr][1]
  acc += 1
  idx = (idx + 1) % idx_len
 print("Problem 08a: {}".format(acc))

def problem_08b():
 def n_of_c(c):
  if c[0] >= 'A' and c[0] <= 'Z': return ord(c[0])-ord('A')
  if c[0] >= '0' and c[0] <= '9': return ord(c[0])-ord('0')+26
 def addr_of_s(s):
  return n_of_c(s[0])*36*36 + n_of_c(s[1])*36 + n_of_c(s[2])
 data = [(36*36*36, 36*36*36) for i in range(0,36*36*36)]
 zs = list()
 def parse(s):
  data[addr_of_s(s[0:3])] = (addr_of_s(s[7:10]),addr_of_s(s[12:15]))
  if n_of_c(s[2]) == n_of_c('Z'): zs.append(addr_of_s(s[0:3]))
 with open("08.txt") as f:
  for (i,s) in enumerate(f.readlines()):
   if i == 0: directions = s.strip()
   elif i == 1: pass
   elif i >= 2: parse(s.strip())
 idx_len = len(directions)
 loops = list()
 for z in zs:
  ptr = z; acc = 1; idx = 1; end = z
  if directions[0] == 'L': ptr = data[ptr][0]
  else: ptr = data[ptr][1]
  while ptr != end:
   if directions[idx] == 'L': ptr = data[ptr][0]
   else: ptr = data[ptr][1]
   acc += 1
   idx = (idx + 1) % idx_len
  loops.append(acc // idx_len)
 print(loops)
 print("Problem 08b: {}".format(functools.reduce(int.__mul__,loops,idx_len)))

def problem_11():
 exp_rate = (2,1000000)
 star_map = list()
 with open("11.txt") as f:
  for s in f.readlines():
   star_map.append(bytearray(s[:-1].encode('ascii')))
 dim = len(star_map)
 empty_row = True
 empty_col = True

 # markup star_map
 for y in range(0,dim):
  for x in range(0,dim):
   if star_map[y][x] == ord('#'): empty_row = False
   if star_map[x][y] == ord('#'): empty_col = False
  if empty_row :
   for x in range(0,dim):
    star_map[y][x] += 1
  if empty_col :
   for x in range(0,dim):
    star_map[x][y] += 1
  empty_row = True
  empty_col = True

 galax1 = list()
 galax2 = list()
 true_yx1 = [0,0]
 true_yx2 = [0,0]

 # build galaxy lists
 for y in range(0,dim):
  for x in range(0,dim):
   if star_map[y][x] == ord('#'):
    galax1.append((true_yx1[1],true_yx1[0]))
    galax2.append((true_yx2[1],true_yx2[0]))
   if star_map[y][x] > ord('.'):
    true_yx1[1] += exp_rate[0]
    true_yx2[1] += exp_rate[1]
   else:
    true_yx1[1] += 1
    true_yx2[1] += 1
  if star_map[y][0] > ord('.'):
   true_yx1[0] += exp_rate[0]
   true_yx2[0] += exp_rate[1]
  else:
   true_yx1[0] += 1
   true_yx2[0] += 1
  true_yx1[1] = 0
  true_yx2[1] = 0

 def manhattan_distance(p1,p2):
  return abs(p1[0]-p2[0])+abs(p1[1]-p2[1])

 res = [0,0]
 for i in range(0,len(galax1)-1):
  for j in range(i,len(galax1)):
   res[0] += manhattan_distance(galax1[i],galax1[j])
   res[1] += manhattan_distance(galax2[i],galax2[j])

 print("Problem 11: {}, {}".format(res[0],res[1]))

def problem_12():

 def extend_fmt1(fmt1):
  fmt1_ext = bytes()
  for _ in range(4): fmt1_ext += fmt1; fmt1_ext += b'?'
  fmt1_ext += fmt1
  return fmt1_ext

 def extend_fmt2(fmt2):
  fmt2_ext = list()
  for _ in range(5): fmt2_ext.extend(fmt2)
  return fmt2_ext

 def dp_solve(fmt1, fmt2):
  # fmt1 : ascii bytes, fmt2 : int list
  # caution : python passes all objects by mutable reference so mask fmt1,2
  fmt1 = fmt1 + b'.'
  fmt2 = fmt2 + [0]
  dim1 = len(fmt1)
  dim2 = len(fmt2)
  dim3 = max(fmt2) + 1
  dp = [[[ 0 for _ in range (dim3)] for _ in range (dim2)] for _ in range(dim1)]
  # base cases
  if fmt1[0] != ord('.'): dp[0][0][1] = 1 # base case "increment" k
  if fmt1[0] != ord('#'): dp[0][0][0] = 1 # base case do not "increment" k
  for i in range(1,dim1):
   for j in range(0,dim2):
    for k in range(0,dim3):
     if_dot = 0
     if_hash = 0
     if k == 0: if_dot += dp[i-1][j][k] # '..' case
     if k == 0 and j > 0: if_dot += dp[i-1][j-1][fmt2[j-1]] # '#.' case
     if k > 0: if_hash += dp[i-1][j][k-1]
     if   fmt1[i] == ord('.'): dp[i][j][k] = if_dot
     elif fmt1[i] == ord('#'): dp[i][j][k] = if_hash
     elif fmt1[i] == ord('?'): dp[i][j][k] = if_dot + if_hash
  return dp[dim1-1][dim2-1][0]

 inputs = list()
 with open("12.txt") as f:
  for s in f.readlines():
   fmts = list(s.strip().split(' '))
   if len(fmts) > 1:
    fmt1 = bytes(fmts[0], encoding='ascii')
    fmt2 = list(map(int,fmts[1].split(',')))
    inputs.append((fmt1,fmt2))

 res = [0,0]
 res[0] = sum(map(lambda x: dp_solve(x[0],x[1]),inputs))
 res[1] = sum(map(lambda x: dp_solve(extend_fmt1(x[0]),extend_fmt2(x[1])),inputs))

 print("Problem 12: {}, {}".format(res[0],res[1]))

def problem_13():

 def vslice_eq(mat,i,j):
  return all(map(lambda idx: mat[idx][i] == mat[idx][j],range(0,len(mat))))
 def vslice_lev(mat,i,j):
  return sum(map(lambda idx: 0 if mat[idx][i] == mat[idx][j] else 1,range(0,len(mat))))
 def hslice_lev(mat,i,j):
  return sum(map(lambda idx: 0 if mat[i][idx] == mat[j][idx] else 1,range(0,len(mat[0]))))
 def test_href(mat,n):
  return all(map(lambda x: mat[x[0]] == mat[x[1]],zip(reversed(range(0,n)),range(n,len(mat)))))
 def test_vref(mat,n):
  return all(map(lambda x: vslice_eq(mat,x[0],x[1]),zip(reversed(range(0,n)),range(n,len(mat[0])))))
 def test2_href(mat,n):
  return sum(map(lambda x: hslice_lev(mat,x[0],x[1]),zip(reversed(range(0,n)),range(n,len(mat))))) == 1
 def test2_vref(mat,n):
  return sum(map(lambda x: vslice_lev(mat,x[0],x[1]),zip(reversed(range(0,n)),range(n,len(mat[0]))))) == 1

 inputs = list()
 with open("13.txt") as f:
  mat = list()
  for s in f.readlines():
   if len(s.strip()) < 1:
    inputs.append(list(mat))
    mat.clear()
   else:
    mat.append(s.strip())
  if len(mat) != 0: inputs += [mat]

 res = [0,0]
 for mat in inputs:
  for i in range(1,len(mat)):
   if test_href(mat,i): res[0] += 100 * i
   if test2_href(mat,i): res[1] += 100 * i
  for i in range(1,len(mat[0])):
   if test_vref(mat,i): res[0] += i
   if test2_vref(mat,i): res[1] += i

 print("Problem 13: {}, {}".format(res[0],res[1]))

def problem_14():
 from itertools import groupby, islice
 from collections import deque

 def roll_NS (mat,N=True):
  for i in range(0,len(mat[0])):
   new_slice = bytearray()
   # itertools' groupby returns (key,group_iterator) not just group
   for k,g in groupby(map(lambda idx: mat[idx][i], range(0,len(mat))),lambda x: x == ord('#')):
    grp = bytearray(g)
    if k:
     new_slice.extend(grp)
    else:
     rocks = sum(map(lambda x: 1 if x == ord('O') else 0, grp))
     if N:
      new_slice.extend([ord('O') for _ in range(0,rocks)])
      new_slice.extend([ord('.') for _ in range(0,len(grp)-rocks)])
     else:
      new_slice.extend([ord('.') for _ in range(0,len(grp)-rocks)])
      new_slice.extend([ord('O') for _ in range(0,rocks)])
   for idx in range(0,len(new_slice)): mat[idx][i] = new_slice[idx]

 def roll_WE (mat,W=True):
  for i in range(0,len(mat)):
   new_slice = list()
   for k,g in groupby(mat[i],lambda x: x == ord('#')):
    grp = bytearray(g)
    if k:
     new_slice.extend(grp)
    else:
     rocks = sum(map(lambda x: 1 if x == ord('O') else 0, grp))
     if W:
      new_slice.extend([ord('O') for _ in range(0,rocks)])
      new_slice.extend([ord('.') for _ in range(0,len(grp)-rocks)])
     else:
      new_slice.extend([ord('.') for _ in range(0,len(grp)-rocks)])
      new_slice.extend([ord('O') for _ in range(0,rocks)])
   for idx in range(0,len(new_slice)): mat[i][idx] = new_slice[idx]

 def weights(mat):
  res = 0
  h = len(mat)
  w = len(mat[0])
  for i in range(0,h):
   for j in range(0,w):
    if mat[i][j] == ord('O'): res += h-i

  return res

 inputs = list()
 with open("14.txt") as f:
  for s in f.readlines(): inputs.append(bytearray(s.strip(),'ascii'))

 res = [0,0]
 roll_NS(inputs)
 res[0] = weights(inputs)

 roll_WE(inputs)
 roll_NS(inputs,False)
 roll_WE(inputs,False)

 weight_stack = deque()
 cycle_length = None
 offset = 0
 weight_stack.appendleft(weights(inputs))

 for i in range(1,1000):
  # N W S E
  roll_NS(inputs)
  roll_WE(inputs)
  roll_NS(inputs,False)
  roll_WE(inputs,False)
  w = weights(inputs)
  try:
   cycle_length = weight_stack.index(w)+1
   if ( cycle_length > 5 # protect against overly small cycles, coincidental repeats
    and len(weight_stack) >= cycle_length * 2
    and all(map(lambda x: x[0]==x[1],
        zip(islice(weight_stack,cycle_length),
            islice(weight_stack,cycle_length, None))))):
    offset = (1000000000 - (i+1)) % cycle_length
    break
  except ValueError:
   cycle_length = None
  weight_stack.appendleft(weights(inputs))

 print(offset)
 res[1] = weight_stack[cycle_length-offset-1]

 print("Problem 14: {}, {}".format(res[0],res[1]))
 pass

def problem_15():
 from functools import reduce
 def digest(s):
  return reduce(lambda a,x: (a+x) * 17 & 0xFF,bytes(s,'ascii'),0)

 res = [0,0]
 codes = None

 with open("15.txt") as f:
  codes = list(f.readline().strip().split(','))

 res[0] = sum(map(digest,codes))

 boxes = [list() for _ in range(0,256)]
 # lens type [flen, lbl]

 for code in codes:
  if ord(code[-1]) == ord ('-'):
   id = digest(code[:-1])
   idx = next(filter(lambda x: x[1][1] == code[:-1], enumerate(boxes[id])),None)
   if idx != None: boxes[id].pop(idx[0])
  else :
   id = digest(code[:-2])
   flen = ord(code[-1]) - ord('0')
   idx = next(filter(lambda x: x[1][1] == code[:-2], enumerate(boxes[id])),None)
   if idx != None:
    boxes[id][idx[0]][0] = flen
   else:
    boxes[id].append([flen, code[:-2]])

 # unfortunately python makes this syntax obtuse
 # res[1] = sum(map(lambda ib: sum(map(lambda jx: ib[0]*jx[0]*jx[1][0],enumerate(ib[1], start=1))),enumerate(boxes, start=1)))
 for i,box in enumerate(boxes,start=1):
  for j,obj in enumerate(box,start=1):
   # print("{},{},{},{}".format(i,j,obj[0],obj[1]))
   res[1] += i*j*obj[0]

 print("Problem 15: {}, {}".format(res[0],res[1]))

if __name__ == '__main__':
#  problem_01a()
#  problem_01b()
#  problem_02()
#  problem_03a()
#  problem_03b()
#  problem_04()
#  problem_06()
#  problem_05a()
#  problem_05b()
#  problem_07a()
#  problem_07b()
#  problem_09()
#  problem_08a()
 problem_08b()
#  problem_11()
#  problem_12()
#  problem_13()
 problem_14()
#  problem_15()
 pass

