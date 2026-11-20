#!/usr/bin/env python3
"""Lays out Clojure source by the 200-column hierarchy rule (skill scrip-c-code, CEO-1565, said of Clojure): a form that fits goes on ONE line of
at most 200 characters; one that does not opens on its own line with its head and the atoms that lead it, and each other child goes on its own
line one level deeper (four columns), by the same rule, the closing brackets ending the last line. Between every run of one-line top-level forms, and around
every multi-line one, stands one separator, a 200-character comment line. Strings, regexes and character literals are never touched (a string
holding a newline keeps its lines). Usage: python3 script/reflow200.py [--check] FILE...  (--check: rc 1 naming each file not in this form, writing nothing)"""
import re, sys

# ── a Clojure tokenizer that keeps the text: strings, comments, chars and regexes are copied as they are ──────────────────────
TOKCH = re.compile(r'[^\s()\[\]{}",;\'`~@^\\][^\s()\[\]{}",;`~@^\\]*')   # a ' after a symbol's first character is the symbol's own (ch')
def tokenize(s):
    """Yields (kind, text, depth): kind is 'sym' for a symbol-like token, 'open'/'close' for a bracket, 'raw' for the rest."""
    i, n, depth = 0, len(s), 0
    out = []
    while i < n:
        c = s[i]
        if c == ';':
            j = s.find('\n', i); j = n if j < 0 else j
            out.append(('raw', s[i:j], depth)); i = j
        elif c == '"' or (c == '#' and i + 1 < n and s[i + 1] == '"'):
            j = i + (2 if c == '#' else 1)
            while s[j] != '"':
                j += 2 if s[j] == '\\' else 1
            out.append(('raw', s[i:j + 1], depth)); i = j + 1
        elif c == '\\':
            j = i + 2
            if s[i + 1].isalnum():
                while j < n and s[j].isalnum(): j += 1
            out.append(('raw', s[i:j], depth)); i = j
        elif c in '([{':
            out.append(('open', c, depth)); depth += 1; i += 1
        elif c in ')]}':
            depth -= 1; out.append(('close', c, depth)); i += 1
        elif c.isspace() or c in "'`~@^,":
            out.append(('raw', c, depth)); i += 1
        elif c == '#' and i + 1 < n and s[i + 1] in "'({_?=#":
            out.append(('raw', c, depth)); i += 1
        else:
            m = TOKCH.match(s, i)
            out.append(('sym', m.group(0), depth)); i = m.end()
    assert depth == 0, 'unbalanced'
    return out


W = 200
SEP = ';' + '-' * 199
CLOSE = {'(': ')', '[': ']', '{': '}'}
PAIRS_AFTER = {'cond': 1, 'cond->': 2, 'cond->>': 2, 'case': 2, 'condp': 3}          # the child where a form's clause pairs begin
BINDINGS = {'let', 'loop', 'binding', 'with-open', 'with-redefs', 'when-let', 'if-let', 'when-some', 'if-some', 'when-first', 'doseq', 'for', 'dotimes',
            'with-local-vars'}                                                    # forms whose second child is a vector of pairs

def parse(toks):
    i, n = 0, len(toks)
    def skip(i):
        while i < n and toks[i][0] == 'raw' and (toks[i][1].isspace() or toks[i][1] == ',' or toks[i][1].startswith(';')):   # separators are made again
            i += 1
        return i
    def one(i):
        i = skip(i); kind, text, depth = toks[i]
        if kind == 'open':
            kids, j = [], i + 1
            while True:
                j = skip(j)
                if toks[j][0] == 'close' and toks[j][2] == depth:
                    return ('coll', text, kids), j + 1
                k, j = one(j); kids.append(k)
        if kind == 'sym':
            return ('atom', text), i + 1
        if kind == 'raw' and text in ("'", '`', '@'):
            c, j = one(i + 1); return ('pre', text, c), j
        if kind == 'raw' and text == '~':
            if toks[i + 1][0] == 'raw' and toks[i + 1][1] == '@':
                c, j = one(i + 2); return ('pre', '~@', c), j
            c, j = one(i + 1); return ('pre', '~', c), j
        if kind == 'raw' and text == '^':
            m, j = one(i + 1); t, j = one(j); return ('meta', m, t), j
        if kind == 'raw' and text == '#':
            if toks[i + 1][0] == 'raw' and toks[i + 1][1] == "'":
                c, j = one(i + 2); return ('pre', "#'", c), j
            if toks[i + 1][0] == 'open':
                c, j = one(i + 1); return ('pre', '#', c), j
            if toks[i + 1][0] == 'sym':
                return ('atom', '#' + toks[i + 1][1]), i + 2      # ##NaN, ##Inf
            raise ValueError(f'unhandled reader dispatch at token {i}')
        if kind == 'raw':
            return ('atom', text), i + 1          # a string, a regex, a character literal
        raise ValueError(f'unexpected {kind} {text!r} at token {i}')
    forms = []
    while True:
        i = skip(i)
        if i >= n: return forms
        f, i = one(i); forms.append(f)

def flat(x):
    t = x[0]
    if t == 'atom': return x[1]
    if t == 'pre': return x[1] + flat(x[2])
    if t == 'meta': return '^' + flat(x[1]) + ' ' + flat(x[2])
    return x[1] + ' '.join(flat(c) for c in x[2]) + CLOSE[x[1]]

def atomic(x):
    return x[0] == 'atom' or (x[0] in ('pre', 'meta') and atomic(x[2]))

def fits(s, col, reserve):
    return '\n' not in s and col + len(s) + reserve <= W

def render(x, col, reserve, pairs=False):
    """Lines of x placed at column col with reserve characters after it on its last line; the first line carries no indentation."""
    f = flat(x)
    if fits(f, col, reserve) or x[0] == 'atom':
        return f.split('\n')
    if x[0] == 'pre':
        ls = render(x[2], col + len(x[1]), reserve); ls[0] = x[1] + ls[0]; return ls
    if x[0] == 'meta':
        p = '^' + flat(x[1]) + ' '
        ls = render(x[2], col + len(p), reserve); ls[0] = p + ls[0]; return ls
    o, kids = x[1], x[2]; c = CLOSE[o]
    head = flat(kids[0]) if kids and kids[0][0] == 'atom' and o == '(' else None
    ind = col + len(o) if (o == '{' or pairs) else col + 4      # pairs of a map or a binding vector line up under the first; else four columns deeper
    start = 0 if (o == '{' or pairs) else PAIRS_AFTER.get(head, len(kids)) if head else len(kids)
    lines = [o]                  # the first line has no indentation (it starts at col); later lines carry theirs
    width = lambda k: (col if k == 0 else 0) + len(lines[k])
    joinable = True              # may the next atom go on the last line: after the bracket or an atom, never after a list
    def place(kid, j, res, kid_pairs=False):
        """Puts one child on a line of its own (or on the bracket's line when it is the first thing after it)."""
        if len(lines) == 1 and lines[0] == o:
            ls = render(kid, col + len(o), res, kid_pairs); lines[0] = o + ls[0]; lines.extend(ls[1:])
        else:
            ls = render(kid, ind, res, kid_pairs); lines.append(' ' * ind + ls[0]); lines.extend(ls[1:])
    j = 0
    while j < len(kids):
        kid = kids[j]; last = j == len(kids) - 1; res = (reserve + 1) if last else 0
        if head in BINDINGS and j == 1 and kid[0] == 'coll' and kid[1] == '[' and len(lines) == 1:   # (let [a 1 ...: the bindings on the head's line
            pre = lines[0] + ' '
            ls = render(kid, col + len(pre), res, True); lines[0] = pre + ls[0]; lines.extend(ls[1:])
            joinable = False; j += 1; continue
        if j >= start and j + 1 < len(kids):                    # a clause pair: test and result on one line when they fit
            a, b = kids[j], kids[j + 1]; res2 = (reserve + 1) if j + 1 == len(kids) - 1 else 0
            pf = flat(a) + ' ' + flat(b)
            at = col + len(o) if (len(lines) == 1 and lines[0] == o) else ind
            if fits(pf, at, res2):
                if at == ind: lines.append(' ' * ind + pf)
                else: lines[0] = o + pf
            else:
                place(a, j, 0)
                ls = render(b, ind + 4, res2); lines.append(' ' * (ind + 4) + ls[0]); lines.extend(ls[1:])
            joinable = False; j += 2; continue
        s_ = flat(kid)
        if atomic(kid) and '\n' not in s_ and j < start:
            first = lines[-1] == o and len(lines) == 1
            if joinable and (first or width(len(lines) - 1) + 1 + len(s_) + res <= W):
                lines[-1] = lines[-1] + ('' if first else ' ') + s_
            else:
                lines.append(' ' * ind + s_)
            joinable = True
        else:
            place(kid, j, res, kid_pairs=(head in BINDINGS and j == 1 and kid[0] == 'coll' and kid[1] == '['))
            joinable = False
        j += 1
    lines[-1] += c
    return lines

def layout(src):
    out, prev_one = [], None
    for f in parse(tokenize(src)):
        ls = render(f, 0, 0)
        one = len(ls) == 1
        if out and not (one and prev_one):
            out.append(SEP)
        out.extend(ls); prev_one = one
    return '\n'.join(out) + '\n'

if __name__ == '__main__':
    check = '--check' in sys.argv
    bad = 0
    for path in [a for a in sys.argv[1:] if a != '--check']:
        s = open(path, encoding='utf-8').read(); t = layout(s)
        over = [i + 1 for i, l in enumerate(t.split('\n')) if len(l) > W]
        if check:
            if t != s: bad += 1; print(f'{path}: NOT IN 200-COLUMN FORM')
        else:
            open(path, 'w', encoding='utf-8').write(t)
            print(f'{path}: {s.count(chr(10))} -> {t.count(chr(10))} lines; over 200: {len(over)} {over[:6]}')
    sys.exit(1 if bad else 0)
