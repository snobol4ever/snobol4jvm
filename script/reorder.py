#!/usr/bin/env python3
"""Orders the top-level forms of a Clojure file so that what a form uses stands above it, as near it as it can: a depth-first walk emits
each form's dependencies just before it. Roots (forms nothing uses) go in their order in the file, the file's entry (jar-main) last, so
the file reads up from the bottom. A defmulti's defmethods follow it; a name's several definitions keep their order; top-level expressions
keep theirs; a forward reference a cycle needs is a (declare ...) right above the form that needs it, the file's own declares dropped.
Usage: python3 script/reorder.py [--check] FILE [ENTRY]  (--check: rc 1 when FILE is not in this order, writing nothing)"""
import sys, os, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import reflow200 as R

CHECK = '--check' in sys.argv
args = [a for a in sys.argv[1:] if a != '--check']
path = args[0]; ENTRY = args[1] if len(args) > 1 else 'jar-main'
src_text = open(path, encoding='utf-8').read()
forms = R.parse(R.tokenize(src_text))
ns, forms = forms[0], forms[1:]
DEFS = {'def', 'defn', 'defn-', 'defmacro', 'defmulti', 'defonce', 'definterface', 'defprotocol', 'deftype', 'defrecord', 'numeric-predicate', 'defparser'}
BINDERS = {'let', 'loop', 'when-let', 'if-let', 'when-some', 'if-some', 'when-first', 'doseq', 'for', 'dotimes', 'with-open'}   # binding and with-redefs name vars, not locals
RESOLVERS = {'resolve', 'ns-resolve', 'requiring-resolve', 'var'}

def head(f):
    return R.flat(f[2][0]) if f[0] == 'coll' and f[2] and f[2][0][0] == 'atom' else None

def strip_meta(n):
    while n[0] == 'meta': n = n[2]
    return n

def defined(f):
    """(kind, names) of a form."""
    h = head(f)
    if h in DEFS and len(f[2]) > 1:
        n = R.flat(strip_meta(f[2][1]))
        names = {n}
        if h in ('deftype', 'defrecord'): names |= {n + '.', '->' + n} | ({'map->' + n} if h == 'defrecord' else set())
        if h == 'defprotocol':
            names |= {R.flat(strip_meta(c[2][0])) for c in f[2][2:] if c[0] == 'coll' and c[2]}
        return h, names
    if h == 'declare': return 'declare', {R.flat(strip_meta(c)) for c in f[2][1:]}
    if h == 'defmethod': return 'defmethod', {R.flat(f[2][1])}
    if h == 'eval' and len(f[2]) == 2 and f[2][1][0] == 'coll' and f[2][1][2] and R.flat(f[2][1][2][0]) == 'primitive':
        q = f[2][1][2][1]
        return 'def', {R.flat(q[2]) if q[0] == 'pre' else R.flat(q)}
    return 'expr', set()

def binding_syms(node, out):
    """every symbol in a binding target (a symbol, or a destructuring vector or map)."""
    t = node[0]
    if t == 'atom':
        s = node[1]
        if not s.startswith((':', '&')) and s != '_': out.add(s)
    elif t == 'coll':
        for c in node[2]: binding_syms(c, out)
    elif t in ('pre', 'meta'): binding_syms(node[2], out)
    return out

def locals_of(node, out):
    """names bound anywhere in a form: fn and defn parameters, let-like bindings."""
    if node[0] == 'coll':
        kids = node[2]; h = R.flat(kids[0]) if kids and kids[0][0] == 'atom' else None
        if node[1] == '(' and h == 'letfn' and len(kids) > 1 and kids[1][0] == 'coll':     # (letfn [(name [params] body) ...] ...)
            for spec in kids[1][2]:
                if spec[0] == 'coll' and spec[2]:
                    binding_syms(spec[2][0], out)
                    for c in spec[2][1:]:
                        if c[0] == 'coll' and c[1] == '[': binding_syms(c, out)
                        if c[0] == 'coll' and c[1] == '(' and c[2] and c[2][0][0] == 'coll' and c[2][0][1] == '[': binding_syms(c[2][0], out)
        elif node[1] == '(' and h in ('fn', 'fn*', 'defn', 'defn-', 'defmacro', 'defmethod'):
            rest = kids[3:] if h == 'defmethod' else kids[1:]          # a defmethod's dispatch value is no parameter vector
            for c in rest:                                              # the parameters are the first vector, or each arity's first
                if c[0] == 'coll' and c[1] == '[':
                    binding_syms(c, out); break
                if c[0] == 'coll' and c[1] == '(':
                    for arity in rest:
                        if arity[0] == 'coll' and arity[1] == '(' and arity[2] and arity[2][0][0] == 'coll' and arity[2][0][1] == '[':
                            binding_syms(arity[2][0], out)
                    break
        elif node[1] == '(' and h in ('deftype', 'defrecord') and len(kids) > 2 and kids[2][0] == 'coll' and kids[2][1] == '[':
            binding_syms(kids[2], out)                                  # the fields; each method's parameters are its own first vector
            for m in kids[3:]:
                if m[0] == 'coll' and m[1] == '(' and len(m[2]) > 1 and m[2][1][0] == 'coll' and m[2][1][1] == '[': binding_syms(m[2][1], out)
        if node[1] == '(' and h in BINDERS and len(kids) > 1 and kids[1][0] == 'coll' and kids[1][1] == '[':
            bk = kids[1][2]
            for i in range(0, len(bk), 2): binding_syms(bk[i], out)
        if node[1] == '(' and h == 'catch' and len(kids) > 2: binding_syms(kids[2], out)
        for c in kids: locals_of(c, out)
    elif node[0] in ('pre', 'meta'):
        locals_of(node[2], out)
    return out

def refs(node, out, quoted=False):
    t = node[0]
    if t == 'atom':
        s = node[1]
        if not quoted and not s.startswith(('"', '#"', '\\', ':')):
            if s.startswith('SNOBOL4clojure.core/'): s = s.split('/', 1)[1]
            elif s.startswith('SNOBOL4clojure.core.'): s = s[len('SNOBOL4clojure.core.'):].rstrip('.')      # a class of the namespace by its full name
            out.append(s)
            if s.endswith('.') and len(s) > 1: out.append(s[:-1])
    elif t == 'pre':
        q = True if node[1] == "'" else (False if node[1] in ('`', '~', '~@') else quoted)
        refs(node[2], out, q)
    elif t == 'meta':
        hint = []; refs(node[1], hint, False); out.extend(('meta', h) for h in hint); refs(node[2], out, quoted)
    else:
        kids = node[2]; h = R.flat(kids[0]) if kids and kids[0][0] == 'atom' else None
        if h == 'quote' and not quoted: return out
        if h in RESOLVERS and not quoted:                       # a name looked up at run time: the walk keeps it near
            for c in kids[1:]:
                if c[0] == 'pre' and c[1] == "'" and c[2][0] == 'atom':
                    s = c[2][1]; out.append(s.split('/', 1)[1] if s.startswith('SNOBOL4clojure.core/') else s)
        for c in kids: refs(c, out, quoted)
    return out

class_names = set()
for f in forms:
    k, ns_ = defined(f)
    if k in ('deftype', 'defrecord', 'definterface', 'defprotocol'): class_names |= ns_
info = []
by_name = collections.defaultdict(list)
for i, f in enumerate(forms):
    kind, names = defined(f)
    loc = locals_of(f, set())
    rs = refs(f, [])
    dep_names = []
    for s in rs:
        if isinstance(s, tuple):
            s = s[1]
            if s not in class_names: continue                    # a type hint names a class, never a var
        if s in names or s in loc: continue
        if s not in dep_names: dep_names.append(s)
    info.append({'i': i, 'kind': kind, 'names': names, 'deps': dep_names, 'form': f})
    if kind not in ('declare', 'defmethod', 'expr'):
        for n in names: by_name[n].append(i)
# a declare that is a name's only definition (a var set at run time, as init-inline-fns! sets the inline IFns) is that name's definition
for x in info:
    if x['kind'] == 'declare':
        only = {n for n in x['names'] if not by_name.get(n)}
        if only:
            x['kind'] = 'def'; x['names'] = only
            x['form'] = R.parse(R.tokenize('(declare ' + ' '.join(sorted(only, key=lambda n: R.flat(x['form']).index(n))) + ')'))[0]
            for n in only: by_name[n].append(x['i'])
methods = collections.defaultdict(list)
for x in info:
    if x['kind'] == 'defmethod': methods[next(iter(x['names']))].append(x['i'])
manual = {ENTRY: ['-main']}                                        # jar-main runs -main through resolve
for x in info:
    for n in x['names']:
        if n in manual: x['deps'] = manual[n] + [d for d in x['deps'] if d not in manual[n]]

CLASSES = {'deftype', 'defrecord', 'definterface', 'defprotocol'}
declared = set()
state = {}          # i -> 'active' | 'done'
order, need_declare = [], collections.defaultdict(list)
defined_so_far = set()
def visit(i):
    if state.get(i) == 'done' or state.get(i) == 'active': return
    x = info[i]; state[i] = 'active'
    for d in x['deps']:
        for j in by_name.get(d, []):
            if j == i: continue
            if x['kind'] in CLASSES and info[j]['kind'] not in CLASSES | {'defmacro'} and state.get(j) != 'done':
                # a class is never declared ahead: the functions its methods call are, and they follow it
                if d not in defined_so_far and d not in need_declare[i]: need_declare[i].append(d)
                continue
            if state.get(j) == 'active':
                if d not in defined_so_far and d not in need_declare[i]: need_declare[i].append(d)
            else:
                visit(j)
    # every definition of a name this form defines goes in its old order: an earlier one first
    for n in x['names']:
        for j in by_name.get(n, []):
            if j < i and state.get(j) is None: visit(j)
    state[i] = 'done'; order.append(i); defined_so_far.update(x['names'])
    if x['kind'] == 'defmulti':
        for j in methods.get(next(iter(x['names'])), []): visit(j)

roots_used = set()
for x in info:
    for d in x['deps']:
        for j in by_name.get(d, []):
            if j != x['i']: roots_used.add(j)
entry = [x['i'] for x in info if ENTRY in x['names']]
for x in info:
    if x['kind'] == 'declare': state[x['i']] = 'done'
for x in info:
    if x['i'] in entry: continue
    if x['kind'] == 'defmethod': continue                      # placed by its defmulti, in its order
    if x['i'] not in roots_used or x['kind'] == 'expr': visit(x['i'])
for x in info:
    if state.get(x['i']) is None and x['i'] not in entry: visit(x['i'])
for i in entry: visit(i)

out = [ns]
for i in order:
    late = [d for d in need_declare.get(i, []) if d in by_name and d not in declared and info[by_name[d][0]]['kind'] not in CLASSES]
    declared.update(late)
    if late:
        out.append(R.parse(R.tokenize('(declare ' + ' '.join(late) + ')'))[0])
    out.append(info[i]['form'])
lines, prev_one = [], None
for f in out:
    ls = R.render(f, 0, 0); one = len(ls) == 1
    if lines and not (one and prev_one): lines.append(R.SEP)
    lines.extend(ls); prev_one = one
new_text = '\n'.join(lines) + '\n'
if CHECK:
    if new_text != src_text: print(f'{path}: NOT IN DEPENDENCY ORDER'); sys.exit(1)
    sys.exit(0)
open(path, 'w', encoding='utf-8').write(new_text)
kept = sum(1 for x in info if x['kind'] != 'declare')
print(f'forms {len(forms)}: {kept} placed, {sum(1 for x in info if x["kind"] == "declare")} declares dropped, '
      f'{sum(1 for i in order if need_declare.get(i))} declares made for {sum(len(v) for v in need_declare.values())} names; entry last: {info[order[-1]]["names"]}')
