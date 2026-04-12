(ns SNOBOL4clojure.test-csnobol4-suite
  "CSNOBOL4 test suite wired into snobol4jvm.
   116 Budne-suite tests + 10 FENCE crosscheck tests = 126 total.
   
   Stdin tests (atn crlf longrec rewind1 sudoku trim0 trim1 uneval2):
   data below END in the .sno file is fed as *in* via with-open / StringReader.
   
   File-I/O, popen, preload, include, loaderr, scanerr tests are included
   with :expected-fail metadata where snobol4jvm does not yet support the
   underlying syscall — they compile and run but may not match .ref."
  (:require [clojure.test :refer :all]
            [SNOBOL4clojure.core :refer :all]
            [SNOBOL4clojure.test-helpers :refer [prog-timeout run-with-timeout]]))

;; ── stdin helper ────────────────────────────────────────────────────────────
(defn- run-with-input
  "Run a SNOBOL4 source string with stdin-data fed as *in*.
   Returns the same map as run-with-timeout: {:exit :ok :stdout <str> ...}."
  [src stdin-data budget-ms]
  (let [reader (java.io.BufferedReader.
                 (java.io.StringReader. stdin-data))]
    (binding [*in* reader]
      (run-with-timeout src budget-ms))))

;; ── FENCE crosscheck tests (058–067) ───────────────────────────────────────

(deftest fence-058-pat-fence-keyword
  "058 - FENCE keyword (0-arg): matches null, aborts match on backtrack"
  (let [r (run-with-timeout "*  058 - FENCE keyword (0-arg): matches null, aborts match on backtrack
        X = 'AB'
        X  LEN(1)  FENCE  LEN(2)                              :S(YES)F(NO)
YES     OUTPUT = 'should not reach'                            :(END)
NO      OUTPUT = 'aborted correctly'
END
" 2000)]
    (is (= "aborted correctly
" (:stdout r)))))

(deftest fence-059-pat-fence-fn-basic
  "059 - FENCE(P) basic: P matches, overall match succeeds"
  (let [r (run-with-timeout "*  059 - FENCE(P) basic: P matches, overall match succeeds
        X = 'hello'
        X  FENCE('hello')                                      :F(NO)
        OUTPUT = 'matched'                                     :(END)
NO      OUTPUT = 'failed'
END
" 2000)]
    (is (= "matched
" (:stdout r)))))

(deftest fence-060-pat-fence-fn-fail
  "060 - FENCE(P) failure: P fails, statement takes F branch (no abort)"
  (let [r (run-with-timeout "*  060 - FENCE(P) failure: P fails, statement takes F branch (no abort)
        X = 'hello'
        X  FENCE('world')                                      :S(YES)F(NO)
YES     OUTPUT = 'should not reach'                            :(END)
NO      OUTPUT = 'failed gracefully'
END
" 2000)]
    (is (= "failed gracefully
" (:stdout r)))))

(deftest fence-061-pat-fence-fn-seal
  "061 - FENCE(P) seals: backtrack cannot re-enter P's alternatives"
  (let [r (run-with-timeout "*  061 - FENCE(P) seals: backtrack cannot re-enter P's alternatives
*  Without fence: FENCE(LEN(1)|LEN(2)) RPOS(0) on 'AB' would succeed
*  via LEN(2). With fence: LEN(1) chosen, sealed, RPOS(0) fails -> match fails.
        X = 'AB'
        X  FENCE(LEN(1) | LEN(2))  RPOS(0)                   :S(YES)F(NO)
YES     OUTPUT = 'should not reach'                            :(END)
NO      OUTPUT = 'sealed correctly'
END
" 2000)]
    (is (= "sealed correctly
" (:stdout r)))))

(deftest fence-062-pat-fence-fn-outer
  "062 - FENCE(P) outer alts: enclosing pattern can still backtrack outside fence"
  (let [r (run-with-timeout "*  062 - FENCE(P) outer alts: enclosing pattern can still backtrack outside fence
        X = 'bx'
        X  ('a' | 'b')  FENCE('x')                            :F(NO)
        OUTPUT = 'outer alt worked'                            :(END)
NO      OUTPUT = 'failed'
END
" 2000)]
    (is (= "outer alt worked
" (:stdout r)))))

(deftest fence-063-pat-fence-fn-optional
  "063 - FENCE(P) optional suffix: FENCE(SPAN(digits)|'') — greedy or empty, committed"
  (let [r (run-with-timeout "*  063 - FENCE(P) optional suffix: FENCE(SPAN(digits)|'') — greedy or empty, committed
        digits = '0123456789'
        &ANCHOR = 1
        X = '123abc'
        X  FENCE(SPAN(digits) | '') . N
        OUTPUT = N
END
" 2000)]
    (is (= "123
" (:stdout r)))))

(deftest fence-064-pat-fence-fn-capture
  "064 - FENCE(P) with $ capture: identifier pattern from corpus"
  (let [r (run-with-timeout "*  064 - FENCE(P) with $ capture: identifier pattern from corpus
        alnum = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_'
        &ANCHOR = 1
        X = 'hello_world rest'
        X  ANY(&UCASE &LCASE)  FENCE(SPAN(alnum) | '') . ID
        OUTPUT = ID
END
" 2000)]
    (is (= "hello_world
" (:stdout r)))))

(deftest fence-065-pat-fence-fn-decimal
  "065 - FENCE(P) decimal literal pattern: ANY(1-9) FENCE(SPAN(digits)|'')"
  (let [r (run-with-timeout "*  065 - FENCE(P) decimal literal pattern: ANY(1-9) FENCE(SPAN(digits)|'')
        digits = '0123456789'
        &ANCHOR = 1
        X = '42rest'
        X  ANY('123456789')  FENCE(SPAN(digits) | '') . N
        OUTPUT = N
END
" 2000)]
    (is (= "42
" (:stdout r)))))

(deftest fence-066-pat-fence-fn-nested
  "066 - FENCE(P) nested: real number pattern with inner FENCE"
  (let [r (run-with-timeout "*  066 - FENCE(P) nested: real number pattern with inner FENCE
        digits = '0123456789'
        &ANCHOR = 1
        Real = SPAN(digits) '.' FENCE(SPAN(digits) | '')
+          |   SPAN(digits)
        X = '3.14rest'
        X  FENCE(Real) . N
        OUTPUT = N
END
" 2000)]
    (is (= "3.14
" (:stdout r)))))

(deftest fence-067-pat-fence-fn-vs-kw
  "067 - FENCE(P) vs &FENCE: FENCE(P) fails gracefully, &FENCE aborts entire match"
  (let [r (run-with-timeout "*  067 - FENCE(P) vs &FENCE: FENCE(P) fails gracefully, &FENCE aborts entire match
*  &FENCE: backtrack into it kills the whole statement (goes to F, not S)
        X = 'AX'
        X  LEN(1)  FENCE  LEN(2)                              :S(BAD)
*  FENCE(P): when P fails, statement takes F branch normally
        X  FENCE('Z')                                         :S(BAD)
        OUTPUT = 'both correct'                               :(END)
BAD     OUTPUT = 'wrong'
END
" 2000)]
    (is (= "both correct
" (:stdout r)))))

;; ── Budne suite (116 tests) ─────────────────────────────────────────────────

(deftest csnobol4-100func
  "Budne suite: 100func.sno"
  (let [r (run-with-timeout "	DEFINE('A1(X)')
	DEFINE('A2(X)')
	DEFINE('A3(X)')
	DEFINE('A4(X)')
	DEFINE('A5(X)')
	DEFINE('A6(X)')
	DEFINE('A7(X)')
	DEFINE('A8(X)')
	DEFINE('A9(X)')
	DEFINE('A10(X)')
	DEFINE('A11(X)')
	DEFINE('A12(X)')
	DEFINE('A13(X)')
	DEFINE('A14(X)')
	DEFINE('A15(X)')
	DEFINE('A16(X)')
	DEFINE('A17(X)')
	DEFINE('A18(X)')
	DEFINE('A19(X)')
	DEFINE('A20(X)')
	DEFINE('A21(X)')
	DEFINE('A22(X)')
	DEFINE('A23(X)')
	DEFINE('A24(X)')
	DEFINE('A25(X)')
	DEFINE('A26(X)')
	DEFINE('A27(X)')
	DEFINE('A28(X)')
	DEFINE('A29(X)')
	DEFINE('A30(X)')
	DEFINE('A31(X)')
	DEFINE('A32(X)')
	DEFINE('A33(X)')
	DEFINE('A34(X)')
	DEFINE('A35(X)')
	DEFINE('A36(X)')
	DEFINE('A37(X)')
	DEFINE('A38(X)')
	DEFINE('A39(X)')
	DEFINE('A40(X)')
	DEFINE('A41(X)')
	DEFINE('A42(X)')
	DEFINE('A43(X)')
	DEFINE('A44(X)')
	DEFINE('A45(X)')
	DEFINE('A46(X)')
	DEFINE('A47(X)')
	DEFINE('A48(X)')
	DEFINE('A49(X)')
	DEFINE('A50(X)')
	DEFINE('A51(X)')
	DEFINE('A52(X)')
	DEFINE('A53(X)')
	DEFINE('A54(X)')
	DEFINE('A55(X)')
	DEFINE('A56(X)')
	DEFINE('A57(X)')
	DEFINE('A58(X)')
	DEFINE('A59(X)')
	DEFINE('A60(X)')
	DEFINE('A61(X)')
	DEFINE('A62(X)')
	DEFINE('A63(X)')
	DEFINE('A64(X)')
	DEFINE('A65(X)')
	DEFINE('A66(X)')
	DEFINE('A67(X)')
	DEFINE('A68(X)')
	DEFINE('A69(X)')
	DEFINE('A70(X)')
	DEFINE('A71(X)')
	DEFINE('A72(X)')
	DEFINE('A73(X)')
	DEFINE('A74(X)')
	DEFINE('A75(X)')
	DEFINE('A76(X)')
	DEFINE('A77(X)')
	DEFINE('A78(X)')
	DEFINE('A79(X)')
	DEFINE('A80(X)')
	DEFINE('A81(X)')
	DEFINE('A82(X)')
	DEFINE('A83(X)')
	DEFINE('A84(X)')
	DEFINE('A85(X)')
	DEFINE('A86(X)')
	DEFINE('A87(X)')
	DEFINE('A88(X)')
	DEFINE('A89(X)')
	DEFINE('A90(X)')
	DEFINE('A91(X)')
	DEFINE('A92(X)')
	DEFINE('A93(X)')
	DEFINE('A94(X)')
	DEFINE('A95(X)')
	DEFINE('A96(X)')
	DEFINE('A97(X)')
	DEFINE('A98(X)')
	DEFINE('A99(X)')
	DEFINE('A100(X)')
				:(BEGIN)
****************

A1
A2
A3
A4
A5
A6
A7
A8
A9
A10
A11
A12
A13
A14
A15
A16
A17
A18
A19
A20
A21
A22
A23
A24
A25
A26
A27
A28
A29
A30
A31
A32
A33
A34
A35
A36
A37
A38
A39
A40
A41
A42
A43
A44
A45
A46
A47
A48
A49
A50
A51
A52
A53
A54
A55
A56
A57
A58
A59
A60
A61
A62
A63
A64
A65
A66
A67
A68
A69
A70
A71
A72
A73
A74
A75
A76
A77
A78
A79
A80
A81
A82
A83
A84
A85
A86
A87
A88
A89
A90
A91
A92
A93
A94
A95
A96
A97
A98
A99
A100	OUTPUT = X		:(RETURN)

****************

BEGIN

	A1(1)
	A2(2)
	A3(3)
	A4(4)
	A5(5)
	A6(6)
	A7(7)
	A8(8)
	A9(9)
	A10(10)
	A11(11)
	A12(12)
	A13(13)
	A14(14)
	A15(15)
	A16(16)
	A17(17)
	A18(18)
	A19(19)
	A20(20)
	A21(21)
	A22(22)
	A23(23)
	A24(24)
	A25(25)
	A26(26)
	A27(27)
	A28(28)
	A29(29)
	A30(30)
	A31(31)
	A32(32)
	A33(33)
	A34(34)
	A35(35)
	A36(36)
	A37(37)
	A38(38)
	A39(39)
	A40(40)
	A41(41)
	A42(42)
	A43(43)
	A44(44)
	A45(45)
	A46(46)
	A47(47)
	A48(48)
	A49(49)
	A50(50)
	A51(51)
	A52(52)
	A53(53)
	A54(54)
	A55(55)
	A56(56)
	A57(57)
	A58(58)
	A59(59)
	A60(60)
	A61(61)
	A62(62)
	A63(63)
	A64(64)
	A65(65)
	A66(66)
	A67(67)
	A68(68)
	A69(69)
	A70(70)
	A71(71)
	A72(72)
	A73(73)
	A74(74)
	A75(75)
	A76(76)
	A77(77)
	A78(78)
	A79(79)
	A80(80)
	A81(81)
	A82(82)
	A83(83)
	A84(84)
	A85(85)
	A86(86)
	A87(87)
	A88(88)
	A89(89)
	A90(90)
	A91(91)
	A92(92)
	A93(93)
	A94(94)
	A95(95)
	A96(96)
	A97(97)
	A98(98)
	A99(99)
	A100(100)
END
" 5000)]
    (is (= "1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
51
52
53
54
55
56
57
58
59
60
61
62
63
64
65
66
67
68
69
70
71
72
73
74
75
76
77
78
79
80
81
82
83
84
85
86
87
88
89
90
91
92
93
94
95
96
97
98
99
100
" (:stdout r)))))

(deftest csnobol4-8bit
  "Budne suite: 8bit.sno"
  (let [r (run-with-timeout "* 6/1/94 -plb
	define(\"ascii(n)\")				:(eascii)
ascii	&alphabet (pos(0) tab(n)) len(1) . ascii	:s(return)f(freturn)
eascii

	output = ascii(65) ascii(66) ascii(67)
	output = ascii(1) ascii(127) ascii(128) ascii(255)

	delim = ascii(255)
	test = delim 'hello world' delim
	test delim break(delim) . output delim
end
" 2000)]
    (is (= "ABC
��
hello world
" (:stdout r)))))

(deftest csnobol4-8bit2
  "Budne suite: 8bit2.sno"
  (let [r (run-with-timeout "	�tude = 'fran�ais'
	fran�ais = \"Bonjour Monde\"			:(�tude)
	output = 'quel domage'				:(end)

�tude
	output = $�tude
end
" 2000)]
    (is (= "Bonjour Monde
" (:stdout r)))))

(deftest csnobol4-a
  "Budne suite: a.sno"
  (let [r (run-with-timeout "	a = 1
	&DUMP = 1
END
" 2000)]
    (is (= "Dump of variables at termination

Natural variables

A = 1
ABORT = PATTERN
ARB = PATTERN
BAL = PATTERN
FAIL = PATTERN
FENCE = PATTERN
REM = PATTERN
SUCCEED = PATTERN

Unprotected keywords

&ABEND = 0
&ANCHOR = 0
&CASE = 1
&CODE = 0
&DUMP = 1
&ERRLIMIT = 0
&FATALLIMIT = 0
&FTRACE = 0
&FULLSCAN = 0
&GTRACE = 0
&INPUT = 1
&MAXLNGTH = xxx
&OUTPUT = 1
&STLIMIT = -1
&TRACE = 0
&TRIM = 0
" (:stdout r)))))

(deftest csnobol4-alis
  "Budne suite: alis.sno"
  (let [r (run-with-timeout "
*
*	THIS PROGRAM USES PROGRAMMER-DEFINED DATA TYPES TO
*	REPRESENT AN ARBITRARILY LONG INTEGER AS A LINKED LIST
*	CALLED ALI.  OPSYN IS USED TO DEFINE A BINARY OPERATOR
*	AND TWO UNARY OPERATORS FOR MANIPULATING ALIS.
*
*	% APPENDS A NODE TO THE HEAD OF A LIST.  # AND / RETURN
*	THE VALUE OF THE HEAD OF THE LIST, AND THE LIST LINKED
*	FROM THE HEAD, RESPECTIVELY.
*
*	THE OPERATORS + AND * ARE GENERALIZED TO RETURN INTEGERS
*	IF THE OPERANDS ARE INTEGERS AND THE RESULT LESS THAN
*	MAX (10000).  IF THE RESULT IS GREATER THAN MAX, AN ALI
*	IS GENERATED WITH THE VALUE OF THE HEAD EQUAL TO THE LOW
*	ORDER DIGITS, AND THE LINK POINTING TO AN ALI WITH THE
*	HIGHER DIGITS.  IF EITHER OPERAND IS AN ALI, THE RESULT
*	IS AN ALI.
*
*	THE USE OF ALIS IS ILLUSTRATED BY COMPUTING THE FIRST K
*	POWERS OF AN INTEGER N.
*
*
	&ANCHOR = 1
	OPSYN('SUM','+',2)
	OPSYN('PROD','*',2)
	DATA('ALI(V,L)')
	DEFINE('OUT(OUT)')
	DEFINE('APPEND(V,L)')
	DEFINE('ADD(I1,I2)C')
	DEFINE('MUL(I1,I2)C')
	DEFINE('VAL(VAL)')
	DEFINE('LINK(I)')
	OPSYN('+','ADD',2)
	OPSYN('*','MUL',2)
	OPSYN('%','APPEND',2)
	OPSYN('/','LINK',1)
	OPSYN('#','VAL',1)
	MAX	=  10000
	ADDFIX  =  RTAB(SIZE(MAX) - 1) . C REM . ADD
	MULFIX  =  RTAB(SIZE(MAX) - 1) . C REM . MUL
*		FUNCTION DEFINITIONS
*
						:(FEND)
*
APPEND	APPEND   =  ALI(V,L)			:(RETURN)
*
*
ADD	ADD      =  IDENT(I2)  I1		:S(RETURN)
	ADD	 =  IDENT(I1)  I2		:S(RETURN)
	ADD	 =  SUM(#I1,#I2)
	 LT(ADD,MAX)  INTEGER(I1)  INTEGER(I2)	:S(RETURN)
	ADD	 =  LT(ADD,MAX)  ADD % (/I1 + /I2)   :S(RETURN)
	ADD	ADDFIX
	ADD	= ADD % (C + (/I1 + /I2))	:(RETURN)
*
LINK	LINK	=  \\INTEGER(I)  L(I)		:(RETURN)
*
VAL	VAL	=  \\INTEGER(VAL)  V(VAL)		:(RETURN)
*
*
*
OUT	OUT	=  IDENT(/OUT)  #OUT		:S(RETURN)
	OUT	=  OUT(/OUT) DUPL('0',SIZE(MAX) - SIZE(#OUT) - 1)
+				#OUT		:(RETURN)
*
*
MUL	MUL	=  DIFFER(#I1) DIFFER(#I2) PROD(#I1,#I2) :F(RETURN)
	LT(MUL,MAX) INTEGER(I1)  INTEGER(I2)	:S(RETURN)
	MUL	=  LT(MUL,MAX)  MUL % ( I1 * /I2  +  I2 * /I1)
+						:S(RETURN)
	MUL	MULFIX
	MUL	=  MUL % (C +  I1 * /I2  +  I2 * /I1)
+						:(RETURN)
FEND
	N	=  256
	K	=  15
	P	=  1
	OUTPUT	=  'POWERS OF '  N
	OUTPUT	=
L	I	=  LT(I,K)  I + 1		:F(END)
	P	=  P  *  N
	OUTPUT	=  I  ': '  OUT(P)		:(L)
END
" 2000)]
    (is (= "POWERS OF 256

1: 256
2: 65536
3: 16777216
4: 4294967296
5: 1099511627776
6: 281474976710656
7: 72057594037927936
8: 18446744073709551616
9: 4722366482869645213696
10: 1208925819614629174706176
11: 309485009821345068724781056
12: 79228162514264337593543950336
13: 20282409603651670423947251286016
14: 5192296858534827628530496329220096
15: 1329227995784915872903807060280344576
" (:stdout r)))))

(deftest csnobol4-alph
  "Budne suite: alph.sno"
  (let [r (run-with-timeout "* 5/29/94 -plb
	output = &alphabet
	output = size(&alphabet)

	output = &lcase
	output = size(&lcase)

	output = &ucase
	output = size(&ucase)
end
" 2000)]
    (is (= " 	
 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~��������������������������������������������������������������������������������������������������������������������������������
256
abcdefghijklmnopqrstuvwxyz
26
ABCDEFGHIJKLMNOPQRSTUVWXYZ
26
" (:stdout r)))))

(deftest csnobol4-alt1
  "Budne suite: alt1.sno"
  (let [r (run-with-timeout "*	bug found by shafto's atn.sno

	S = 'NETWORK PARSE_CLAUSE END PARSE_CLAUSE'

* Keyword settings

	&ANCHOR = 0
	&TRIM	= 1

* Utility patterns

	BLANK	= ' '

	LEFT_END  = POS(0)
	RIGHT_END = RPOS(0)

	BLANKS	= SPAN(BLANK)
	OPT_BLANKS = BLANKS | null
	BB	= BREAK(BLANK)

****************************************************************

	COMPLETE_PAT =
+		(LEFT_END 'EXEC' BLANKS) |
+		(LEFT_END BB BLANKS (BB $ TNAME) BLANKS FAIL) |
+		('END' OPT_BLANKS *TNAME RIGHT_END)

****************************************************************

	S COMPLETE_PAT					:F(DONE)
	OUTPUT = 'yes'
DONE	OUTPUT = 'NAME: ' TNAME
END
" 2000)]
    (is (= "yes
NAME: PARSE_CLAUSE
" (:stdout r)))))

(deftest csnobol4-alt2
  "Budne suite: alt2.sno"
  (let [r (run-with-timeout "*	bug found by shafto's atn.sno

	'AAA' ('FOO' | (SPAN('A') $ TNAME) | 'BAR')	:F(DONE)
	OUTPUT = 'yes'
DONE	OUTPUT = 'NAME: ' TNAME
END
" 2000)]
    (is (= "yes
NAME: AAA
" (:stdout r)))))

(deftest csnobol4-any
  "Budne suite: any.sno"
  (let [r (run-with-timeout "	&alphabet any(\"xyzabc123\")  $ output fail
	\"the quick brown fox jumped\" notany(\"etian\")  $ output fail
end
" 2000)]
    (is (= "1
2
3
a
b
c
x
y
z
h
 
q
u
c
k
 
b
r
o
w
 
f
o
x
 
j
u
m
p
d
" (:stdout r)))))

(deftest csnobol4-atn
  "Budne suite: atn.sno — reads from INPUT (stdin below END)"
  (let [r (run-with-input "*-LIST LEFT
*	&FTRACE = 100

* ATN.SPT - SPITBOL Version
*
* SNOBOL4 program to implement a compiler for an
*		Augmented Transition Network Language.
*
*	This program is slightly modified from the version in the report
*	to provide faster compilation of the network.
*
*	Sample input in the ATN source language is contained in file ATN.IN.
*
*	Run the program by typing:
*	spitbol -u slist <atn.in atn.spt
*
*	where -u slist specifies an optional listing file for the ATN compiler, and
*	may be omitted.

*-CASE 0

* Keyword settings

	&ANCHOR = 0
	&TRIM	= 1

*
* Set CODE_PRINT to 1 to get listing of generated code
*
	CODE_PRINT = 1

* I/O Associations

*	INPUT(.ATNSOURCE,2,\" -f0\")
*	DIFFER(HOST(0)) OUTPUT(.SLIST, 1, HOST(0))

* Defined data types

	DATA( 'STACK(TOP,REST)' )

* Global constants

	null = ''
	nil  = STACK()
	TOP(nil)  = nil
	REST(nil) = nil

	SENTENCES = nil
	LEX_STACK = nil
	LEXICAL_FEATURES = TABLE()

* Utility patterns

	BLANK	= ' '
	SC	= ';'
	Q1	= \"'\"
	Q2	= '\"'
	COMMA	= ','
	STAR	= '*'
	LP	= '('
	RP	= ')'
	UL	= '_'
	PUSHER	= '>'
	POPPER	= '<'
	TAB	= '	'

	LEFT_END  = POS(0)
	RIGHT_END = RPOS(0)

	BLANKS	= SPAN(BLANK)
	OPT_BLANKS = BLANKS | null
	BB	= BREAK(BLANK)
	BXB	= BREAKX(BLANK)

	BBSC	= BREAK(BLANK SC)
	SPSC	= SPAN(SC)
	SPBSC	= SPAN(BLANK SC)
	SPBSCN	= SPBSC | null
	BSC	= BREAK(SC)

	LEN1	= LEN(1)
	L1REM	= LEN1 REM

	BRP	= BREAK(RP)
	BRPN	= BRP | null

* Utility functions

* Return ascii char with value N

	DEFINE(\"XCHAR(N)\")		:(END_CHAR)
XCHAR	&ALPHABET TAB(N) LEN(1) . XCHAR	:S(RETURN)F(FRETURN)
END_CHAR


* Print X to the source listing file and output file

	DEFINE('PRT(X)')					:(PRT_END)
PRT	OUTPUT = X
	SLIST = X 						:(RETURN)
PRT_END

* Error MSG to source listing file and output file

	DEFINE('ERROR(MSG)')					:(ERROR_END)
ERROR	( PRT() PRT(MSG) PRT() )				:(RETURN)
ERROR_END

* Readable display of SNOBOL4 code

	DEFINE( 'DISPLAY(SNOCODE)S,L' )				:(DISPLAY_END)
DISPLAY	EQ(CODE_PRINT,0)					:S(RETURN)
	(PRT() PRT('------ Code ------') PRT())
DISPLAY1
	SNOCODE LEFT_END (BSC $ S) SPSC =			:F(DISPLAY3)
	S LEFT_END (NOTANY(BLANK) (BB | null)) $ L =		:F(DISPLAY2)
	PRT('     | ' L)
DISPLAY2
	S LEFT_END BLANKS =
	S = TRIM(S)
	NULL(S)							:S(DISPLAY1)
	PRT('     |  ' S)					:(DISPLAY1)
DISPLAY3
	(PRT() PRT('------ End of Code ------') PRT())		:(RETURN)
DISPLAY_END

* Succeeds if X is nil, null, or zero

	DEFINE('NULL(X)')					:(NULL_END)
NULL	IDENT(X,nil)						:S(RETURN)
	IDENT(X,null)						:S(RETURN)
* XXX was 'NUMERIC' -phil What about REAL??
	X = CONVERT(X,'INTEGER')				:F(FRETURN)
	EQ(X,0)			     			:S(RETURN)F(FRETURN)
NULL_END

* Put COAT on RACK using HANGER

	DEFINE( 'PUT(RACK,COAT,HANGER)' )			:(PUT_END)
PUT	PROP<RACK> =
+		DIFFER('TABLE',DATATYPE(PROP<RACK>)) TABLE()
	ITEM(PROP<RACK>,HANGER) = COAT				:(RETURN)
PUT_END

* Get contents of HANGER off RACK

	DEFINE( 'GET(RACK,HANGER)' )				:(GET_END)
GET	PROP<RACK> =
+		DIFFER('TABLE',DATATYPE(PROP<RACK>)) TABLE()	:S(RETURN)
	GET = ITEM(PROP<RACK>,HANGER)				:(RETURN)
GET_END

* Program text semi-constants used in code generation

	REPLACE_LIT = XCHAR(1) 'RePlAcE' XCHAR(1)

	BEGIN_TEXT =
+		' HOLD = REMAINING_WORDS ;'
+		' REMAINING_WORDS (BREAK(\" \") $ CURRENT_WORD) ;'
+		' THIS_NODE = GENNAME(\"' REPLACE_LIT '\") ;'
+		' :(' REPLACE_LIT '_START) ;'

	WIN_TEXT =
+		REPLACE_LIT '_WIN'
+		' TESTF(THIS_NODE,FEATS) :F(' REPLACE_LIT '_LOSE) ;'
+		' ATTACH(THIS_NODE,PARENT) ;'
+		' LAST_PARSED = THIS_NODE :(RETURN) ;'

	LOSE_TEXT =
+		REPLACE_LIT '_LOSE'
+		' REMAINING_WORDS = HOLD ;'
+		' REMAINING_WORDS (BREAK(\" \") $ CURRENT_WORD) :(FRETURN) ;'

	INITIAL_ROUTINE =
+		REPLACE_LIT BEGIN_TEXT
+		WIN_TEXT LOSE_TEXT REPLACE_LIT '_START ;'

* Patterns used in COMPILE routine

	COMMENT_PAT = (LEFT_END OPT_BLANKS STAR) | (LEFT_END RIGHT_END)

	KEYWORD_PAT = 'NETWORK' | 'FUNCTION' | 'LEXICON'
+		| 'SENTENCES' | 'EXEC'

	NAME_PAT    = (BB $ NAME) BLANKS FENCE

	LEGAL_PAT   = LEFT_END KEYWORD_PAT (BLANKS | RIGHT_END)

	COMPLETE_PAT = (LEFT_END 'EXEC' BLANKS)
	COMPLETE_PAT = COMPLETE_PAT |
+		(LEFT_END BB BLANKS (BB $ TNAME) BLANKS FAIL)
	COMPLETE_PAT = COMPLETE_PAT |
+		('END' OPT_BLANKS *TNAME RIGHT_END)

* Definitions of semantic (code-generating) functions

	DEFINE( 'S(NA)' )
	DEFINE( 'S_(NA)T' )

* Recognizer/compiler patterns for the five types of blocks:
*  EXEC, SENTENCES, LEXICON, FUNCTION, and NETWORK

* Recognizer for EXEC statement

	EXEC_PAT = LEFT_END 'EXEC' BLANKS (REM $ NAME) S('EX')

* Recognizer/Compiler for SENTENCES block

	SENTENCES_LIT = 'SENTENCES' BLANKS FENCE
	SENTENCES_HEADER = LEFT_END SENTENCES_LIT NAME_PAT
	SENTENCE_PAT   = (BSC $ SENT) SPBSC S('SENT')
	SENTENCES_BODY = ARBNO(SENTENCE_PAT)
	SENTENCES_ENDER = 'END' OPT_BLANKS *NAME RIGHT_END
	SENTENCES_PAT = SENTENCES_HEADER SENTENCES_BODY SENTENCES_ENDER

* Recognizer/Compiler for LEXICON block

	LEXICON_LIT = 'LEXICON' BLANKS FENCE
	LEXICON_HEADER = LEFT_END LEXICON_LIT NAME_PAT
	LEX_PUSH_PAT = PUSHER (BB $ F) BLANKS S('PSH')
	LEX_POP_PAT = POPPER (BB $ F) BLANKS S('POP')
	WORD_PAT = NOTANY(PUSHER POPPER) (BB | null)
	LEX_W_PAT = (WORD_PAT $ W) BLANKS S('LEX')
	ENTRY_PAT = LEX_W_PAT | LEX_PUSH_PAT | LEX_POP_PAT
	ENTRIES_PAT = ARBNO(ENTRY_PAT)
	LEXICON_ENDER = SENTENCES_ENDER
	LEXICON_PAT = LEXICON_HEADER ENTRIES_PAT LEXICON_ENDER

* Recognizer/Compiler for FUNCTION block

	FUNCTION_LIT = 'FUNCTION' BLANKS FENCE
	FUNCTION_HEADER = LEFT_END FUNCTION_LIT NAME_PAT
	ARG_PAT = (( LP BRPN RP ) $ ARG ) BLANKS S('ARG')
	LOC_PAT = LP (BRPN $ LOC) RP BLANKS S('LOC')
	FUNCTION_HEADER = FUNCTION_HEADER ARG_PAT LOC_PAT
	STATEMENT_PAT = BSC SPSC
	STATEMENTS_PAT = ARBNO(STATEMENT_PAT) $ BODY BLANKS
	FUNCTION_ENDER = SENTENCES_ENDER
	FUNCTION_PAT = FUNCTION_HEADER S('FN') STATEMENTS_PAT
+		FUNCTION_ENDER S('F')

* Recongnizer/Compiler for NETWORK block

* The IF part

	IF_LIT = 'IF' BLANKS FENCE

* The conditional clause

	CLAUSE_PAT = BXB
	COND_PAT = (CLAUSE_PAT $ COND) BLANKS

* The GOTO clause

	GOTO_LIT = 'GO' OPT_BLANKS 'TO' BLANKS FENCE
	GOTO_LABEL_PAT = (BB $ GOTO_LABEL) BLANKS FENCE
	GOTO_PAT = GOTO_LIT GOTO_LABEL_PAT

* The AFTER clause (which may be null)

	AFTER_LIT = 'AFTER' BLANKS FENCE
	SIDE_PAT = (CLAUSE_PAT $ SIDE) BLANKS
	ENDIF_PAT = 'END' OPT_BLANKS 'IF' BLANKS FENCE
	AFTER_PAT =
+		((null $ SIDE) ENDIF_PAT)
+		| (AFTER_LIT SIDE_PAT ENDIF_PAT)
	IF_PAT = IF_LIT COND_PAT GOTO_PAT AFTER_PAT S('IF')

* The labelled set of IF statments (the RULE)

	LABEL_PAT = (BB $ LABEL) BLANKS FENCE
	IFS_PAT = ARBNO(IF_PAT)
	END_LABEL_PAT = 'END' OPT_BLANKS *LABEL BLANKS FENCE
	RULE_PAT = LABEL_PAT S('LAB') IFS_PAT END_LABEL_PAT S('ELB')

* The set of RULEs (the NETWORK)

	NETWORK_LIT = 'NETWORK' BLANKS FENCE
	NETWORK_HEADER = LEFT_END NETWORK_LIT NAME_PAT
	RULES_PAT = ARBNO(RULE_PAT)
	NETWORK_ENDER = SENTENCES_ENDER
	NETWORK_PAT = NETWORK_HEADER S('NTW')
+			RULES_PAT
+			NETWORK_ENDER S('ENW')

* Grand pattern for compiling any legal block

	COMPILE_PAT = NETWORK_PAT
+		    | FUNCTION_PAT
+		    | LEXICON_PAT
+		    | SENTENCES_PAT
+		    | EXEC_PAT

* Read and compile all text from INPUT
*   (source listing with comments goes to SLIST)

	DEFINE( 'COMPILE()NEXT,TEXT' )				:(COMPILE_END)

* Comment or first line of block

COMPILE	TEXT = INPUT					:F(RETURN)
*	OUTPUT = '1>>' TEXT

* List the record, trim leading blanks, and check for legal syntax

COMPILE1
	PRT( TEXT )
COMP6	TEXT TAB = BLANK					:S(COMP6)
	TEXT COMMENT_PAT					:S(COMPILE)
	TEXT LEFT_END BLANKS = null
	TEXT LEGAL_PAT						:S(COMPILE2)
	ERROR('Illegal record')					:(FRETURN)

* Check for a complete block.  If block is incomplete, keep reading

COMPILE2
*	OUTPUT = '3>>' TEXT
	TEXT COMPLETE_PAT					:S(COMPILE4)
*	OUTPUT = '4>>' TEXT
	NEXT = INPUT						:S(COMPILE3)
	ERROR('Unexpected end of file on INPUT')		:(FRETURN)

* List the record, convert leading blanks to a single blank,
*  and concatenate with TEXT

COMPILE3
*	OUTPUT = '2>>' NEXT
	PRT( NEXT )
COMP7	TEXT TAB = BLANK					:S(COMP7)
	NEXT COMMENT_PAT					:S(COMPILE2)
	NEXT LEFT_END BLANKS = BLANK
	TEXT = TEXT NEXT					:(COMPILE2)

* Use COMPILE_PAT to compile TEXT

COMPILE4
	TIME_ZERO = TIME()
	TEXT COMPILE_PAT					:F(COMPILE5)
	PRT()
**	PRT(TIME() - TIME_ZERO ' milliseconds compile time')
	PRT()							:(COMPILE)

COMPILE5
	ERROR('Compilation failed')				:(FRETURN)
COMPILE_END

* Semantic (code-generation) functions

	:(S_END)

* For immediate code generation
*	The code is generated after a part of a syntactic
*	pattern has successfully matched

S	S = EVAL( \"(NULL $ *S_('\"  NA  \"')) FENCE \" )		:(RETURN)

* This is a big computed GOTO with a branch for every
*	semantic contigency.

S_	S_ = .DUMMY						:($( 'S_' NA ))

* Initialize the code for a network

S_NTW	ROUTINE = INITIAL_ROUTINE				:(NRETURN)

* The label for a rule

S_LAB	ROUTINE = ROUTINE REPLACE_LIT UL LABEL BLANK		:(NRETURN)

* One IF statement is a network

S_IF	ROUTINE = ROUTINE
+		' ?( ' COND BLANK SIDE ' ) '
+		':S(' REPLACE_LIT UL GOTO_LABEL ') ;'		:(NRETURN)

* The end of a labelled rule:  If none of the IF statements
*	has been satisfied, then the LOSE branch is take

S_ELB	ROUTINE = ROUTINE ' :(' REPLACE_LIT '_LOSE) ;'		:(NRETURN)

* Wrap-up network:  (1) insert NAME in all the right places;
*	(2) translate into machine language via CODE.

S_ENW	ROUTINE REPLACE_LIT = NAME				:S(S_ENW)
	DISPLAY( ROUTINE )
	CODE( ROUTINE )						:F(S_ENW1)
	DEFINE( NAME '(PARENT,FEATS)THIS_NODE,HOLD' )
	DISPLAY(' DEFINE(' Q1 NAME '(PARENT,FEATS)THIS_NODE,HOLD' Q1 ') ;')
								:(NRETURN)
S_ENW1	ERROR('Compilation error')				:(FRETURN)

* Push a sentence onto the stack of sentences

S_SENT	SENTENCES = STACK(SENT,SENTENCES)			:(NRETURN)

* Push F onto the stack of lexical features

S_PSH	LEX_STACK = STACK(F,LEX_STACK)				:(NRETURN)

* Pop lexical features up to, NOT INCLUDING, F

S_POP	NULL(LEX_STACK)						:S(NRETURN)
	IDENT(F,TOP(LEX_STACK))					:S(NRETURN)
	LEX_STACK = REST(LEX_STACK)				:(S_POP)

* Attach all stacked features to W

S_LEX	LEX_STACK_SAVE = LEX_STACK
S_LEX1	NULL(LEX_STACK)						:S(S_LEX2)
	LEXICAL_FEATURES<W> = TOP(LEX_STACK) BLANK
+			LEXICAL_FEATURES<W>
	LEX_STACK = REST(LEX_STACK)				:(S_LEX1)
S_LEX2	PRT('     | ' W ':  ' LEXICAL_FEATURES<W>)
	LEX_STACK = LEX_STACK_SAVE				:(NRETURN)

* Remove all blanks from the formal argument list for a FUNCTION

S_ARG	ARG BLANKS =					:S(S_ARG)F(NRETURN)

* Remove all blanks from the local variable list for a FUNCTION

S_LOC	LOC BLANKS = 					:S(S_LOC)F(NRETURN)

* Initialize FUNCTION

S_FN								:(NRETURN)

* Compile a FUNCTION

S_F	BODY = BODY \" ERROR('No return from ' \"
+		Q1 NAME Q1 \") :(END) ;\"
	DISPLAY( NAME BLANK BODY )
	CODE( NAME BLANK BODY )					:F(S_F1)
	DISPLAY(' DEFINE(' Q1 NAME ARG LOC Q1 ') ;')
	DEFINE( NAME ARG LOC )					:(NRETURN)
S_F1	ERROR('Compilation error')				:(FRETURN)

* For EXEC, call MAIN with NAME = name of first network to be called

S_EX	( PRT() PRT() )
	PRT( '****** EXECUTION BEGINS WITH ' NAME ' ******') PRT()
	MAIN(NAME)						:(NRETURN)
S_END


* This routine is triggered by the EXEC statement

	DEFINE( 'MAIN(FIRST_PROC)LAST_PARSED,'
+	'CURRENT_WORD,REMAINING_WORDS,S,PROP' )			:(MAIN_END)
MAIN	NULL(SENTENCES)						:S(RETURN)
	S = TOP(SENTENCES)
	SENTENCES = REST(SENTENCES)
	( PRT() PRT() )
	PRT(DUPL('-',SIZE(S)))
	( PRT() PRT(S) PRT() )
	PRT(DUPL('-',SIZE(S)))
	PRT()
	LAST_PARSED = null
	CURRENT_WORD = null
	REMAINING_WORDS = S BLANK
	PROP = TABLE()
	TIME_ZERO = TIME()
	EVAL(FIRST_PROC)					:S(MAIN1)
	( PRT() PRT('Parsing failed') PRT() )			:(MAIN)

MAIN1	( PRT() PRT('Parsing Succeeded') PRT() )
**	( PRT(TIME() - TIME_ZERO ' milliseconds used') PRT() )
	DUMP_PROP()						:(MAIN)
MAIN_END

* Dump registers after parse is completed

	DEFINE( 'DUMP_PROP()T,N,R,M,TN1,TN2,RM1,RM2' )		:(DUMP_PROP_END)
DUMP_PROP
	T = CONVERT(PROP,\"ARRAY\")				:F(RETURN)
	PROP = null
	N = 1

DUMP1	TN1 = T<N,1>						:F(RETURN)
	TN2 = T<N,2>
	T<N,1> = null
	T<N,2> = null
	R = CONVERT(TN2,\"ARRAY\")				:F(DUMP3)
	PRT()
	PRT( 'NODE: ' TN1 )
	M = 1

DUMP2		RM1 = R<M,1>					:F(DUMP3)
		RM2 = R<M,2>
		PRT( DUPL(' ',10) RM1 ' = ' RM2 )
		M = M + 1					:(DUMP2)

DUMP3	N = N + 1						:(DUMP1)
DUMP_PROP_END


* Compile main program starts here

	COMPILE()						:S(END)
	ERROR('****** FATAL ERROR ******')
END"
                          "**********************************
 NETWORK PARSE_CLAUSE
**********************************
    S1
        IF PARSE_NOUN_GROUP(THIS_NODE) GOTO S2
        AFTER SETR('SUBJECT',LAST_PARSED)
        ENDIF
    END S1
**********************************
    S2
        IF PARSE_WORD(THIS_NODE,'VERB TENSED ') GOTO S3
        AFTER SETR('VERB',LAST_PARSED)
        ENDIF
    END S2
**********************************
    S3
        IF    TESTF(LAST_PARSED,'BE ')
        PARSE_WORD(THIS_NODE,'PASTPARTICIPLE ') GOTO S4
        AFTER    SETR('OBJECT',GETR('SUBJECT'))
            SETR('SUBJECT')
            SETR('VERB',LAST_PARSED)
        ENDIF
        IF    TESTF(GETR('VERB'),'TRANSITIVE ')
        PARSE_NOUN_GROUP(THIS_NODE) GOTO S4
        AFTER SETR('OBJECT',LAST_PARSED)
        ENDIF
        IF TESTF(GETR('VERB'),'INTRANSITIVE ') GOTO S4 ENDIF
        IF ~NULL(GETR('OBJECT')) GOTO S4 ENDIF
    END S3
**********************************
    S4
        IF    ~NULL(GETR('SUBJECT'))
        NULL(REMAINING_WORDS) GOTO WIN
        ENDIF
        IF    NULL(GETR('SUBJECT'))
        IDENT(CURRENT_WORD,'BY')
        PARSE_WORD(THIS_NODE) GOTO S5
        ENDIF
        IF NULL(GETR('SUBJECT')) GOTO S4
        AFTER SETR('SUBJECT','SOMEONE')
        ENDIF
    END S4
**********************************
    S5
        IF PARSE_NOUN_GROUP(THIS_NODE) GOTO S4
        AFTER SETR('SUBJECT',LAST_PARSED)
        ENDIF
    END S5
 END PARSE_CLAUSE

**********************************
 NETWORK PARSE_NOUN_GROUP
**********************************
    S1
        IF PARSE_WORD(THIS_NODE,'DETERMINER ') GOTO S2
        AFTER SETR('NUMBER',
               SELECT('SINGULAR PLURAL ',
                  GETF(LAST_PARSED)))
              SETR('DETERMINER',
               SELECT('DEFINITE INDEFINITE ',
                  GETF(LAST_PARSED)))
        ENDIF
    END S1
**********************************
    S2
        IF PARSE_WORD(THIS_NODE,'ADJECTIVE ') GOTO S2
         AFTER ADDR('ADJECTIVES',LAST_PARSED)
        ENDIF
        IF PARSE_WORD(THIS_NODE,'NOUN ') GOTO WIN
         AFTER SETR('NUMBER',
                SELECT('SINGULAR PLURAL ',
                   GETF(LAST_PARSED)))
               SETR('NOUN',LAST_PARSED)
        ENDIF
    END S2
 END PARSE_NOUN_GROUP

**********************************
 NETWORK PARSE_WORD
    S1
        IF NULL(null) GOTO WIN
        AFTER PARSE_WORD_1()
        ENDIF
    END S1
 END PARSE_WORD

**********************************
 FUNCTION PARSE_WORD_1 () ()
    THIS_NODE = CURRENT_WORD ;
    REMAINING_WORDS BREAK(\" \") SPAN(\" \") = ;
    REMAINING_WORDS (BREAK(\" \") | null) $ CURRENT_WORD    :(RETURN) ;
 END PARSE_WORD_1

**********************************
 FUNCTION SETR (REGISTER,VALUE) ()
    PUT(THIS_NODE,VALUE,REGISTER)        :(RETURN) ;
 END SETR

**********************************
 FUNCTION GETR (REGISTER) ()
    GETR = GET(THIS_NODE,REGISTER)        :(RETURN) ;
 END GETR

**********************************
 FUNCTION ADDR (REGISTER,VALUE) ()
    SETR(REGISTER,GETR(REGISTER) VALUE \" \")    :(RETURN) ;
 END ADDR

**********************************
 FUNCTION GENNAME (X) ()
    GENNAME =
        '*' X '_' (NEXTNUM = NEXTNUM + 1) '*'
        :(RETURN) ;
 END GENNAME

**********************************
 FUNCTION ATTACH (C,P) ()
    PUT(C,P,'PARENT') ;
    PUT(P,GET(P,'CHILDREN') C \" \",'CHILDREN')
        :(RETURN) ;
 END ATTACH

**********************************
 FUNCTION SELECT (S,T) ()
    S (BREAK(\" \") $ SELECT) SPAN(\" \") =    :F(FRETURN) ;
    T (POS(0) | \" \") SELECT \" \"
        :S(RETURN)F(SELECT) ;
 END SELECT

**********************************
 FUNCTION TESTF (X,F) (W,G)
    NULL(F)        :S(RETURN) ;
    G = GETF(X) ;
TESTF1
    F (BREAK(\" \") $ W) SPAN(\" \") =    :F(RETURN) ;
    G (POS(0) | \" \") W \" \"    :S(TESTF)F(FRETURN) ;
 END TESTF

**********************************
 FUNCTION GETF (X) ()
    GETF = LEXICAL_FEATURES<X> :(RETURN) ;
 END GETF

**********************************
 LEXICON L1
    <* >NOUN >SINGULAR BLOCK BOY
    <* >DETERMINER >SINGULAR >INDEFINITE A
               <SINGULAR >DEFINITE THE
    <* >VERB >TENSED >TRANSITIVE >INTRANSITIVE >PASTPARTICIPLE DROPPED
         <TENSED >BE WAS
    <* >ADJECTIVE BIG RED
    <* >PREPOSITION BY
    <*
 END L1

**********************************
 SENTENCES S1
    A BIG RED BLOCK WAS DROPPED BY THE BOY ;
    THE BOY DROPPED A BIG RED BLOCK ;
    A BLOCK WAS DROPPED ;
    THE BLOCK DROPPED ;
 END S1

**********************************
 EXEC PARSE_CLAUSE(\"SENTENCE\",null)
"
                          10000)]
    (is (= "**********************************
 NETWORK PARSE_CLAUSE
**********************************
    S1
        IF PARSE_NOUN_GROUP(THIS_NODE) GOTO S2
        AFTER SETR('SUBJECT',LAST_PARSED)
        ENDIF
    END S1
**********************************
    S2
        IF PARSE_WORD(THIS_NODE,'VERB TENSED ') GOTO S3
        AFTER SETR('VERB',LAST_PARSED)
        ENDIF
    END S2
**********************************
    S3
        IF    TESTF(LAST_PARSED,'BE ')
        PARSE_WORD(THIS_NODE,'PASTPARTICIPLE ') GOTO S4
        AFTER    SETR('OBJECT',GETR('SUBJECT'))
            SETR('SUBJECT')
            SETR('VERB',LAST_PARSED)
        ENDIF
        IF    TESTF(GETR('VERB'),'TRANSITIVE ')
        PARSE_NOUN_GROUP(THIS_NODE) GOTO S4
        AFTER SETR('OBJECT',LAST_PARSED)
        ENDIF
        IF TESTF(GETR('VERB'),'INTRANSITIVE ') GOTO S4 ENDIF
        IF ~NULL(GETR('OBJECT')) GOTO S4 ENDIF
    END S3
**********************************
    S4
        IF    ~NULL(GETR('SUBJECT'))
        NULL(REMAINING_WORDS) GOTO WIN
        ENDIF
        IF    NULL(GETR('SUBJECT'))
        IDENT(CURRENT_WORD,'BY')
        PARSE_WORD(THIS_NODE) GOTO S5
        ENDIF
        IF NULL(GETR('SUBJECT')) GOTO S4
        AFTER SETR('SUBJECT','SOMEONE')
        ENDIF
    END S4
**********************************
    S5
        IF PARSE_NOUN_GROUP(THIS_NODE) GOTO S4
        AFTER SETR('SUBJECT',LAST_PARSED)
        ENDIF
    END S5
 END PARSE_CLAUSE

------ Code ------

     | PARSE_CLAUSE
     |  HOLD = REMAINING_WORDS
     |  REMAINING_WORDS (BREAK(\" \") $ CURRENT_WORD)
     |  THIS_NODE = GENNAME(\"PARSE_CLAUSE\")
     |  :(PARSE_CLAUSE_START)
     | PARSE_CLAUSE_WIN
     |  TESTF(THIS_NODE,FEATS) :F(PARSE_CLAUSE_LOSE)
     |  ATTACH(THIS_NODE,PARENT)
     |  LAST_PARSED = THIS_NODE :(RETURN)
     | PARSE_CLAUSE_LOSE
     |  REMAINING_WORDS = HOLD
     |  REMAINING_WORDS (BREAK(\" \") $ CURRENT_WORD) :(FRETURN)
     | PARSE_CLAUSE_START
     | PARSE_CLAUSE_S1
     |  ?( PARSE_NOUN_GROUP(THIS_NODE) SETR('SUBJECT',LAST_PARSED) ) :S(PARSE_CLAUSE_S2)
     |  :(PARSE_CLAUSE_LOSE)
     | PARSE_CLAUSE_S2
     |  ?( PARSE_WORD(THIS_NODE,'VERB TENSED ') SETR('VERB',LAST_PARSED) ) :S(PARSE_CLAUSE_S3)
     |  :(PARSE_CLAUSE_LOSE)
     | PARSE_CLAUSE_S3
     |  ?( TESTF(LAST_PARSED,'BE ') PARSE_WORD(THIS_NODE,'PASTPARTICIPLE ') SETR('OBJECT',GETR('SUBJECT')) SETR('SUBJECT') SETR('VERB',LAST_PARSED) ) :S(PARSE_CLAUSE_S4)
     |  ?( TESTF(GETR('VERB'),'TRANSITIVE ') PARSE_NOUN_GROUP(THIS_NODE) SETR('OBJECT',LAST_PARSED) ) :S(PARSE_CLAUSE_S4)
     |  ?( TESTF(GETR('VERB'),'INTRANSITIVE ')  ) :S(PARSE_CLAUSE_S4)
     |  ?( ~NULL(GETR('OBJECT'))  ) :S(PARSE_CLAUSE_S4)
     |  :(PARSE_CLAUSE_LOSE)
     | PARSE_CLAUSE_S4
     |  ?( ~NULL(GETR('SUBJECT')) NULL(REMAINING_WORDS)  ) :S(PARSE_CLAUSE_WIN)
     |  ?( NULL(GETR('SUBJECT')) IDENT(CURRENT_WORD,'BY') PARSE_WORD(THIS_NODE)  ) :S(PARSE_CLAUSE_S5)
     |  ?( NULL(GETR('SUBJECT')) SETR('SUBJECT','SOMEONE') ) :S(PARSE_CLAUSE_S4)
     |  :(PARSE_CLAUSE_LOSE)
     | PARSE_CLAUSE_S5
     |  ?( PARSE_NOUN_GROUP(THIS_NODE) SETR('SUBJECT',LAST_PARSED) ) :S(PARSE_CLAUSE_S4)
     |  :(PARSE_CLAUSE_LOSE)

------ End of Code ------


------ Code ------

     |  DEFINE('PARSE_CLAUSE(PARENT,FEATS)THIS_NODE,HOLD')

------ End of Code ------




**********************************
 NETWORK PARSE_NOUN_GROUP
**********************************
    S1
        IF PARSE_WORD(THIS_NODE,'DETERMINER ') GOTO S2
        AFTER SETR('NUMBER',
               SELECT('SINGULAR PLURAL ',
                  GETF(LAST_PARSED)))
              SETR('DETERMINER',
               SELECT('DEFINITE INDEFINITE ',
                  GETF(LAST_PARSED)))
        ENDIF
    END S1
**********************************
    S2
        IF PARSE_WORD(THIS_NODE,'ADJECTIVE ') GOTO S2
         AFTER ADDR('ADJECTIVES',LAST_PARSED)
        ENDIF
        IF PARSE_WORD(THIS_NODE,'NOUN ') GOTO WIN
         AFTER SETR('NUMBER',
                SELECT('SINGULAR PLURAL ',
                   GETF(LAST_PARSED)))
               SETR('NOUN',LAST_PARSED)
        ENDIF
    END S2
 END PARSE_NOUN_GROUP

------ Code ------

     | PARSE_NOUN_GROUP
     |  HOLD = REMAINING_WORDS
     |  REMAINING_WORDS (BREAK(\" \") $ CURRENT_WORD)
     |  THIS_NODE = GENNAME(\"PARSE_NOUN_GROUP\")
     |  :(PARSE_NOUN_GROUP_START)
     | PARSE_NOUN_GROUP_WIN
     |  TESTF(THIS_NODE,FEATS) :F(PARSE_NOUN_GROUP_LOSE)
     |  ATTACH(THIS_NODE,PARENT)
     |  LAST_PARSED = THIS_NODE :(RETURN)
     | PARSE_NOUN_GROUP_LOSE
     |  REMAINING_WORDS = HOLD
     |  REMAINING_WORDS (BREAK(\" \") $ CURRENT_WORD) :(FRETURN)
     | PARSE_NOUN_GROUP_START
     | PARSE_NOUN_GROUP_S1
     |  ?( PARSE_WORD(THIS_NODE,'DETERMINER ') SETR('NUMBER', SELECT('SINGULAR PLURAL ', GETF(LAST_PARSED))) SETR('DETERMINER', SELECT('DEFINITE INDEFINITE ', GETF(LAST_PARSED))) ) :S(PARSE_NOUN_GROUP_S2)
     |  :(PARSE_NOUN_GROUP_LOSE)
     | PARSE_NOUN_GROUP_S2
     |  ?( PARSE_WORD(THIS_NODE,'ADJECTIVE ') ADDR('ADJECTIVES',LAST_PARSED) ) :S(PARSE_NOUN_GROUP_S2)
     |  ?( PARSE_WORD(THIS_NODE,'NOUN ') SETR('NUMBER', SELECT('SINGULAR PLURAL ', GETF(LAST_PARSED))) SETR('NOUN',LAST_PARSED) ) :S(PARSE_NOUN_GROUP_WIN)
     |  :(PARSE_NOUN_GROUP_LOSE)

------ End of Code ------


------ Code ------

     |  DEFINE('PARSE_NOUN_GROUP(PARENT,FEATS)THIS_NODE,HOLD')

------ End of Code ------




**********************************
 NETWORK PARSE_WORD
    S1
        IF NULL(null) GOTO WIN
        AFTER PARSE_WORD_1()
        ENDIF
    END S1
 END PARSE_WORD

------ Code ------

     | PARSE_WORD
     |  HOLD = REMAINING_WORDS
     |  REMAINING_WORDS (BREAK(\" \") $ CURRENT_WORD)
     |  THIS_NODE = GENNAME(\"PARSE_WORD\")
     |  :(PARSE_WORD_START)
     | PARSE_WORD_WIN
     |  TESTF(THIS_NODE,FEATS) :F(PARSE_WORD_LOSE)
     |  ATTACH(THIS_NODE,PARENT)
     |  LAST_PARSED = THIS_NODE :(RETURN)
     | PARSE_WORD_LOSE
     |  REMAINING_WORDS = HOLD
     |  REMAINING_WORDS (BREAK(\" \") $ CURRENT_WORD) :(FRETURN)
     | PARSE_WORD_START
     | PARSE_WORD_S1
     |  ?( NULL(null) PARSE_WORD_1() ) :S(PARSE_WORD_WIN)
     |  :(PARSE_WORD_LOSE)

------ End of Code ------


------ Code ------

     |  DEFINE('PARSE_WORD(PARENT,FEATS)THIS_NODE,HOLD')

------ End of Code ------




**********************************
 FUNCTION PARSE_WORD_1 () ()
    THIS_NODE = CURRENT_WORD ;
    REMAINING_WORDS BREAK(\" \") SPAN(\" \") = ;
    REMAINING_WORDS (BREAK(\" \") | null) $ CURRENT_WORD    :(RETURN) ;
 END PARSE_WORD_1

------ Code ------

     | PARSE_WORD_1
     |  THIS_NODE = CURRENT_WORD
     |  REMAINING_WORDS BREAK(\" \") SPAN(\" \") =
     |  REMAINING_WORDS (BREAK(\" \") | null) $ CURRENT_WORD    :(RETURN)
     |  ERROR('No return from ' 'PARSE_WORD_1') :(END)

------ End of Code ------


------ Code ------

     |  DEFINE('PARSE_WORD_1()')

------ End of Code ------




**********************************
 FUNCTION SETR (REGISTER,VALUE) ()
    PUT(THIS_NODE,VALUE,REGISTER)        :(RETURN) ;
 END SETR

------ Code ------

     | SETR
     |  PUT(THIS_NODE,VALUE,REGISTER)        :(RETURN)
     |  ERROR('No return from ' 'SETR') :(END)

------ End of Code ------


------ Code ------

     |  DEFINE('SETR(REGISTER,VALUE)')

------ End of Code ------




**********************************
 FUNCTION GETR (REGISTER) ()
    GETR = GET(THIS_NODE,REGISTER)        :(RETURN) ;
 END GETR

------ Code ------

     | GETR
     |  GETR = GET(THIS_NODE,REGISTER)        :(RETURN)
     |  ERROR('No return from ' 'GETR') :(END)

------ End of Code ------


------ Code ------

     |  DEFINE('GETR(REGISTER)')

------ End of Code ------




**********************************
 FUNCTION ADDR (REGISTER,VALUE) ()
    SETR(REGISTER,GETR(REGISTER) VALUE \" \")    :(RETURN) ;
 END ADDR

------ Code ------

     | ADDR
     |  SETR(REGISTER,GETR(REGISTER) VALUE \" \")    :(RETURN)
     |  ERROR('No return from ' 'ADDR') :(END)

------ End of Code ------


------ Code ------

     |  DEFINE('ADDR(REGISTER,VALUE)')

------ End of Code ------




**********************************
 FUNCTION GENNAME (X) ()
    GENNAME =
        '*' X '_' (NEXTNUM = NEXTNUM + 1) '*'
        :(RETURN) ;
 END GENNAME

------ Code ------

     | GENNAME
     |  GENNAME = '*' X '_' (NEXTNUM = NEXTNUM + 1) '*' :(RETURN)
     |  ERROR('No return from ' 'GENNAME') :(END)

------ End of Code ------


------ Code ------

     |  DEFINE('GENNAME(X)')

------ End of Code ------




**********************************
 FUNCTION ATTACH (C,P) ()
    PUT(C,P,'PARENT') ;
    PUT(P,GET(P,'CHILDREN') C \" \",'CHILDREN')
        :(RETURN) ;
 END ATTACH

------ Code ------

     | ATTACH
     |  PUT(C,P,'PARENT')
     |  PUT(P,GET(P,'CHILDREN') C \" \",'CHILDREN') :(RETURN)
     |  ERROR('No return from ' 'ATTACH') :(END)

------ End of Code ------


------ Code ------

     |  DEFINE('ATTACH(C,P)')

------ End of Code ------




**********************************
 FUNCTION SELECT (S,T) ()
    S (BREAK(\" \") $ SELECT) SPAN(\" \") =    :F(FRETURN) ;
    T (POS(0) | \" \") SELECT \" \"
        :S(RETURN)F(SELECT) ;
 END SELECT

------ Code ------

     | SELECT
     |  S (BREAK(\" \") $ SELECT) SPAN(\" \") =    :F(FRETURN)
     |  T (POS(0) | \" \") SELECT \" \" :S(RETURN)F(SELECT)
     |  ERROR('No return from ' 'SELECT') :(END)

------ End of Code ------


------ Code ------

     |  DEFINE('SELECT(S,T)')

------ End of Code ------




**********************************
 FUNCTION TESTF (X,F) (W,G)
    NULL(F)        :S(RETURN) ;
    G = GETF(X) ;
TESTF1
    F (BREAK(\" \") $ W) SPAN(\" \") =    :F(RETURN) ;
    G (POS(0) | \" \") W \" \"    :S(TESTF)F(FRETURN) ;
 END TESTF

------ Code ------

     | TESTF
     |  NULL(F)        :S(RETURN)
     |  G = GETF(X)
     | TESTF1
     |  F (BREAK(\" \") $ W) SPAN(\" \") =    :F(RETURN)
     |  G (POS(0) | \" \") W \" \"    :S(TESTF)F(FRETURN)
     |  ERROR('No return from ' 'TESTF') :(END)

------ End of Code ------


------ Code ------

     |  DEFINE('TESTF(X,F)W,G')

------ End of Code ------




**********************************
 FUNCTION GETF (X) ()
    GETF = LEXICAL_FEATURES<X> :(RETURN) ;
 END GETF

------ Code ------

     | GETF
     |  GETF = LEXICAL_FEATURES<X> :(RETURN)
     |  ERROR('No return from ' 'GETF') :(END)

------ End of Code ------


------ Code ------

     |  DEFINE('GETF(X)')

------ End of Code ------




**********************************
 LEXICON L1
    <* >NOUN >SINGULAR BLOCK BOY
    <* >DETERMINER >SINGULAR >INDEFINITE A
               <SINGULAR >DEFINITE THE
    <* >VERB >TENSED >TRANSITIVE >INTRANSITIVE >PASTPARTICIPLE DROPPED
         <TENSED >BE WAS
    <* >ADJECTIVE BIG RED
    <* >PREPOSITION BY
    <*
 END L1
     | BLOCK:  NOUN SINGULAR 
     | BOY:  NOUN SINGULAR 
     | A:  DETERMINER SINGULAR INDEFINITE 
     | THE:  DETERMINER SINGULAR DEFINITE 
     | DROPPED:  VERB TENSED TRANSITIVE INTRANSITIVE PASTPARTICIPLE 
     | WAS:  VERB TENSED BE 
     | BIG:  ADJECTIVE 
     | RED:  ADJECTIVE 
     | BY:  PREPOSITION 



**********************************
 SENTENCES S1
    A BIG RED BLOCK WAS DROPPED BY THE BOY ;
    THE BOY DROPPED A BIG RED BLOCK ;
    A BLOCK WAS DROPPED ;
    THE BLOCK DROPPED ;
 END S1



**********************************
 EXEC PARSE_CLAUSE(\"SENTENCE\",null)


****** EXECUTION BEGINS WITH PARSE_CLAUSE(\"SENTENCE\",null) ******



------------------

THE BLOCK DROPPED 

------------------


Parsing Succeeded


NODE: THE
          PARENT = *PARSE_NOUN_GROUP_2*

NODE: *PARSE_NOUN_GROUP_2*
          CHILDREN = THE BLOCK 
          NUMBER = SINGULAR
          DETERMINER = DEFINITE
          NOUN = BLOCK
          PARENT = *PARSE_CLAUSE_1*

NODE: BLOCK
          PARENT = *PARSE_NOUN_GROUP_2*

NODE: *PARSE_CLAUSE_1*
          CHILDREN = *PARSE_NOUN_GROUP_2* DROPPED 
          SUBJECT = *PARSE_NOUN_GROUP_2*
          VERB = DROPPED
          PARENT = SENTENCE

NODE: DROPPED
          PARENT = *PARSE_CLAUSE_1*

NODE: SENTENCE
          CHILDREN = *PARSE_CLAUSE_1* 


--------------------

A BLOCK WAS DROPPED 

--------------------


Parsing Succeeded


NODE: A
          PARENT = *PARSE_NOUN_GROUP_10*

NODE: *PARSE_NOUN_GROUP_10*
          CHILDREN = A BLOCK 
          NUMBER = SINGULAR
          DETERMINER = INDEFINITE
          NOUN = BLOCK
          PARENT = *PARSE_CLAUSE_9*

NODE: BLOCK
          PARENT = *PARSE_NOUN_GROUP_10*

NODE: *PARSE_CLAUSE_9*
          CHILDREN = *PARSE_NOUN_GROUP_10* WAS DROPPED 
          SUBJECT = SOMEONE
          VERB = DROPPED
          OBJECT = *PARSE_NOUN_GROUP_10*
          PARENT = SENTENCE

NODE: WAS
          PARENT = *PARSE_CLAUSE_9*

NODE: DROPPED
          PARENT = *PARSE_CLAUSE_9*

NODE: SENTENCE
          CHILDREN = *PARSE_CLAUSE_9* 


--------------------------------

THE BOY DROPPED A BIG RED BLOCK 

--------------------------------


Parsing Succeeded


NODE: THE
          PARENT = *PARSE_NOUN_GROUP_17*

NODE: *PARSE_NOUN_GROUP_17*
          CHILDREN = THE BOY 
          NUMBER = SINGULAR
          DETERMINER = DEFINITE
          NOUN = BOY
          PARENT = *PARSE_CLAUSE_16*

NODE: BOY
          PARENT = *PARSE_NOUN_GROUP_17*

NODE: *PARSE_CLAUSE_16*
          CHILDREN = *PARSE_NOUN_GROUP_17* DROPPED *PARSE_NOUN_GROUP_22* 
          SUBJECT = *PARSE_NOUN_GROUP_17*
          VERB = DROPPED
          OBJECT = *PARSE_NOUN_GROUP_22*
          PARENT = SENTENCE

NODE: DROPPED
          PARENT = *PARSE_CLAUSE_16*

NODE: A
          PARENT = *PARSE_NOUN_GROUP_22*

NODE: *PARSE_NOUN_GROUP_22*
          CHILDREN = A BIG RED BLOCK 
          NUMBER = SINGULAR
          DETERMINER = INDEFINITE
          ADJECTIVES = BIG RED 
          NOUN = BLOCK
          PARENT = *PARSE_CLAUSE_16*

NODE: BIG
          PARENT = *PARSE_NOUN_GROUP_22*

NODE: RED
          PARENT = *PARSE_NOUN_GROUP_22*

NODE: BLOCK
          PARENT = *PARSE_NOUN_GROUP_22*

NODE: SENTENCE
          CHILDREN = *PARSE_CLAUSE_16* 


---------------------------------------

A BIG RED BLOCK WAS DROPPED BY THE BOY 

---------------------------------------


Parsing Succeeded


NODE: A
          PARENT = *PARSE_NOUN_GROUP_29*

NODE: *PARSE_NOUN_GROUP_29*
          CHILDREN = A BIG RED BLOCK 
          NUMBER = SINGULAR
          DETERMINER = INDEFINITE
          ADJECTIVES = BIG RED 
          NOUN = BLOCK
          PARENT = *PARSE_CLAUSE_28*

NODE: BIG
          PARENT = *PARSE_NOUN_GROUP_29*

NODE: RED
          PARENT = *PARSE_NOUN_GROUP_29*

NODE: BLOCK
          PARENT = *PARSE_NOUN_GROUP_29*

NODE: *PARSE_CLAUSE_28*
          CHILDREN = *PARSE_NOUN_GROUP_29* WAS DROPPED BY *PARSE_NOUN_GROUP_38* 
          SUBJECT = *PARSE_NOUN_GROUP_38*
          VERB = DROPPED
          OBJECT = *PARSE_NOUN_GROUP_29*
          PARENT = SENTENCE

NODE: WAS
          PARENT = *PARSE_CLAUSE_28*

NODE: DROPPED
          PARENT = *PARSE_CLAUSE_28*

NODE: BY
          PARENT = *PARSE_CLAUSE_28*

NODE: THE
          PARENT = *PARSE_NOUN_GROUP_38*

NODE: *PARSE_NOUN_GROUP_38*
          CHILDREN = THE BOY 
          NUMBER = SINGULAR
          DETERMINER = DEFINITE
          NOUN = BOY
          PARENT = *PARSE_CLAUSE_28*

NODE: BOY
          PARENT = *PARSE_NOUN_GROUP_38*

NODE: SENTENCE
          CHILDREN = *PARSE_CLAUSE_28* 


" (:stdout r)))))

(deftest csnobol4-bal
  "Budne suite: bal.sno"
  (let [r (run-with-timeout "	DEFINE(\"X(S)\")					:(EX)
X	S BAL . B					:F(F)
	OUTPUT = '  ' S ' => ' B			:(RETURN)
F	OUTPUT = '  ' S ' => *FAIL*'			:(RETURN)
EX

	DEFINE(\"Y()\")					:(EY)
Y	X('A')
	X('A(X)')
	X('AA(X)')
	X('()')
	X('((()))')
	X('((())')
	X(')(')
							:(RETURN)
EY

	OUTPUT = '&ANCHOR = 0'
	&ANCHOR = 0
	Y()

	OUTPUT = '&ANCHOR = 1'
	&ANCHOR = 1
	Y()
END
" 2000)]
    (is (= "&ANCHOR = 0
  A => A
  A(X) => A
  AA(X) => A
  () => ()
  ((())) => ((()))
  ((()) => (())
  )( => *FAIL*
&ANCHOR = 1
  A => A
  A(X) => A
  AA(X) => A
  () => ()
  ((())) => ((()))
  ((()) => *FAIL*
  )( => *FAIL*
" (:stdout r)))))

(deftest csnobol4-base
  "Budne suite: base.sno"
  (let [r (run-with-timeout "-include 'basename.sno'

	output = basename('a', '/')
	output = basename('a/b', '/')
	output = basename('a/b/c', '/')
	output = basename('a/b/c/', '/')

	output = basename('a/b', '/\\')
	output = basename('a/b/c', '/\\')
	output = basename('a/b/c/', '/\\')

	output = basename('a\\b', '\\')
	output = basename('a\\b\\c', '\\')
	output = basename('a\\b\\c\\', '\\')

	output = basename('a\\b', '/\\')
	output = basename('a\\b\\c', '/\\')
	output = basename('a\\b\\c\\', '/\\')

	output = basename('a\\b', '/\\')
	output = basename('a/b\\c', '/\\')
	output = basename('a\\b/c\\', '/\\')



end
" 2000)]
    (is (= "a
b
c
c
b
c
c
b
c
c
b
c
c
b
c
c
" (:stdout r)))))

(deftest csnobol4-breakx
  "Budne suite: breakx.sno"
  (let [r (run-with-timeout "	&anchor = 1
	subj = \"this is a test .\"
	subj breakx(' ') $ output '.'
	subj (break(' ') arbno(len(1) break(' '))) $ output '.'
end
" 2000)]
    (is (= "this
this is
this is a
this is a test
this
this is
this is a
this is a test
" (:stdout r)))))

(deftest csnobol4-case1
  "Budne suite: case1.sno"
  (let [r (run-with-timeout "* test case (in)sensitivity
* 5/25/94 -plb

* should start case insensitive (-case 1)
	output = .one
	OUTPUT = .two
* switch to case sensitive
-case
	OUTPUT = .three
* no longer fails;
-case 1
	OUTPUT = .four
* switch back
-CASE 1
	output = .five
	OUTPUT = .six
* do it differently this time
-case 0
	OUTPUT = .seven
* noop
	output = 'here1'

**** test functions, labels, special labels
	&CASE = 0
* XXX test locals too?
* XXX test sensitivity better
	DEFINE('same(x,y)')				:(esame)
same	IDENT(x,y)					:s(RETURN)f(FRETURN)
esame

	output = 'here2'
	same('a','b')					:s(end)
	output = 'here3'
	same('a','a')					:f(end)
	OUTPUT = 'yes!'

* test END label!!
END
" 2000)]
    (is (= "ONE
TWO
three
FOUR
FIVE
SIX
seven
yes!
" (:stdout r)))))

(deftest csnobol4-case2
  "Budne suite: case2.sno"
  (let [r (run-with-timeout "**** test functions, labels, special labels
* 5/25/94 -plb

	define('same(x,y)')				:(esame)
same	ident(x,y)					:s(return)f(freturn)
esame

	same('a','b')					:s(end)
	same('a','a')					:f(end)

* XXX test data(), load(), arrays, tables
	output = 'yes!'

* test END label!!
end
" 2000)]
    (is (= "yes!
" (:stdout r)))))

(deftest csnobol4-cat
  "Budne suite: cat.sno"
  (let [r (run-with-timeout "		OUTPUT = 'Hello' \" \" 'World'
END
" 2000)]
    (is (= "Hello World
" (:stdout r)))))

(deftest csnobol4-char
  "Budne suite: char.sno"
  (let [r (run-with-timeout "	output = char(65)
	output = char(97)
	output = char(126)
	output = char(1)
	output = char(9)
end
" 2000)]
    (is (= "A
a
~

	
" (:stdout r)))))

(deftest csnobol4-collect
  "Budne suite: collect.sno"
  (let [r (run-with-timeout "	COLLECT()
END
" 2000)]
    (is (= "" (:stdout r)))))

(deftest csnobol4-collect2
  "Budne suite: collect2.sno"
  (let [r (run-with-timeout "* reported by Laurence Battin <beamtuner@att.net>
	A B =
* any \"external\" function will do!
	C = SQRT(0.0)
*	C = ORD(\"A\")
	D = COLLECT()
	OUTPUT = \"GOT HERE\"
END
" 2000)]
    (is (= "GOT HERE
" (:stdout r)))))

(deftest csnobol4-comment
  "Budne suite: comment.sno"
  (let [r (run-with-timeout "	Q1 = (POS(0) ('X' | '') '*')
	Q2 = (POS(0) (' ' | '') '*')

	P1 = (POS(0) (SPAN(' ') | '') '*')
	P2 = (POS(0) RPOS(0))

	P3 = P1 | P2

	DEFINE(\"TEST(PAT,STR)\")				:(ETEST)
TEST	STR $PAT					:F(RETURN)
	OUTPUT = PAT \" '\" STR \"' ok\"			:(RETURN)
ETEST

	TEST(.Q1,' *')
	TEST(.Q1,'*')
	TEST(.Q1,'')

	TEST(.Q2,' *')
	TEST(.Q2,'*')
	TEST(.Q2,'')

	TEST(.P1,' *')
	TEST(.P1,'*')
	TEST(.P1,'')

	TEST(.P2,' *')
	TEST(.P2,'*')
	TEST(.P2,'')

	TEST(.P3,' *')
	TEST(.P3,'*')
	TEST(.P3,'')
END
" 2000)]
    (is (= "Q1 '*' ok
Q2 ' *' ok
Q2 '*' ok
P1 ' *' ok
P1 '*' ok
P2 '' ok
P3 ' *' ok
P3 '*' ok
P3 '' ok
" (:stdout r)))))

(deftest csnobol4-contin
  "Budne suite: contin.sno"
  (let [r (run-with-timeout " OUTPUT = &STNO; OUTPUT = &STNO
 OUTPUT = &STNO ;* THIS IS A COMMENT
END
" 2000)]
    (is (= "1
2
3
" (:stdout r)))))

(deftest csnobol4-conv2
  "Budne suite: conv2.sno"
  (let [r (run-with-timeout "* spitbol folds typenames when &case set!!
	output = convert(3,\"real\")
end
" 2000)]
    (is (= "3.
" (:stdout r)))))

(deftest csnobol4-convert
  "Budne suite: convert.sno"
  (let [r (run-with-timeout "*-LIST LEFT
	A = TABLE()

	A<'A'> = 1
	A<'B'> = 2
	A<'C'> = 3
	A<'D'> = 4

	B = CONVERT(A,'ARRAY')

	I = 1
L
*	OUTPUT = DATATYPE(B<I,1>) ': ' DATATYPE(B<I,2>) :F(END)
	OUTPUT = B<I,1> ': ' B<I,2>		:F(END)
	I = I + 1				:(L)
END
" 2000)]
    (is (= "A: 1
B: 2
C: 3
D: 4
" (:stdout r)))))

(deftest csnobol4-crlf
  "Budne suite: crlf.sno — reads from INPUT (stdin below END)"
  (let [r (run-with-input "loop	output = input		:s(loop)
end"
                          "this line has no CR
this line ends in a CR
this line has no CR
"
                          2000)]
    (is (= "this line has no CR
this line ends in a CR
this line has no CR
" (:stdout r)))))

(deftest csnobol4-diag1
  "Budne suite: diag1.sno"
  (let [r (run-with-timeout "*-title	snobol test program #1 -- diagnostics phase one
*
*	 this is a standard test program for spitbol which tests
*	 out functions,	operators and datatype manipulations
*
	 &dump = 2
	 trace(.test)
	 &trace	= 1000000
	 stars =		     '	error detected		***'
	 &errlimit = 1000
	 setexit(.errors)
	 output	= '************************************************'
	 output	= '****	s n o b o l	 d i a g n o s t i c s ****'
	 output	= '****		 p h a s e    o	n e	       ****'
	 output	= '************************************************'
	 output	= '****	 any trace output indicates an error   ****'
	 output	= '************************************************'
-eject
*
*	 test replace function
*
	 test =	differ(replace('axxbyyy','xy','01'),'a00b111') stars
	 a = replace(&alphabet,'xy','ab')
	 test =	differ(replace('axy',&alphabet,a),'aab') stars
*
*	 test convert function
*
	 test =	differ(convert('12','integer') , 12) stars
	 test =	differ(convert(2.5,'integer'),2)       stars
	 test =	differ(convert(2,'real'),2.0) stars
	 test =	differ(convert('.2','real'),0.2) stars
*
*	 test datatype function
*
	 test =	differ(datatype('jkl'),'STRING') stars
	 test =	differ(datatype(12),'INTEGER') stars
	 test =	differ(datatype(1.33),'REAL') stars
	 test =	differ(datatype(null),'STRING')	stars
-eject
*
*	 test arithmetic operators
*
	 test =	differ(3 + 2,5)	stars
	 test =	differ(3 - 2,1)	stars
	 test =	differ(3 * 2,6)	stars
	 test =	differ(5 / 2,2)	stars
	 test =	differ(2 ** 3,8) stars
	 test =	differ(3 + 1,4)	stars
	 test =	differ(3 - 1,2)	stars
	 test =	differ('3' + 2,5) stars
	 test =	differ(3 + '-2',1) stars
	 test =	differ('1' + '0',1) stars
	 test =	differ(5 + null,5) stars
	 test =	differ(-5,0 - 5) stars
	 test =	differ(+'4',4) stars
	 test =	differ(2.0 + 3.0,5.0) stars
	 test =	differ(3.0 - 1.0,2.0) stars
	 test =	differ(3.0 * 2.0,6.0) stars
	 test =	differ(3.0 / 2.0,1.5) stars
	 test =	differ(3.0 ** 3,27.0) stars
	 test =	differ(-1.0,0.0	- 1.0) stars
*
*	 test mixed mode
*
	 test =	differ(1 + 2.0,3.0) stars
	 test =	differ(3.0 / 2,1.5) stars
-eject
*
*	 test functions
*
*	 first,	a simple test of a factorial function
*
	 define('fact(n)')		    :(factend)
fact	 fact =	eq(n,1)	1	  :s(return)
	 fact =	n * fact(n - 1)		    :(return)
factend	 test =	ne(fact(5),120)	stars
	 test =	differ(opsyn(.facto,'fact')) stars
	 test =	differ(facto(4),24) stars
*
*	 see if	alternate entry	point works ok
*
	 define('fact2(n)',.fact2ent)	    :(fact2endf)
fact2ent fact2 = eq(n,1) 1	  :s(return)
	 fact2 = n * fact2(n - 1) :(return)
fact2endf output = ne(fact(6),720) stars
*
*	 test function redefinition and	case of	argument = func	name
*
	 test =	differ(define('fact(fact)','fact3')) stars
.					    :(fact2end)
fact3	 fact =	ne(fact,1) fact	* fact(fact - 1)
.					    :(return)
fact2end
	 test =	ne(fact(4),24) stars
*
*	 test out locals
*
	 define('lfunc(a,b,c)d,e,f')	    :(lfuncend)
lfunc	 test =	~(ident(a,'a') ident(b,'b') ident(c,'c')) stars
	 test =	~(ident(d) ident(e) ident(f)) stars
	 a = 'aa' ; b =	'bb' ; c = 'cc'	; d = 'dd' ; e = 'ee' ;	f = 'ff'
.				  :(return)
lfuncend aa = 'a' ; bb = 'b' ; cc = 'c'
	 d = 'd' ; e = 'e' ; f = 'f'
	 a = 'x' ; b = 'y' ; c = 'z'
	 test =	differ(lfunc(aa,bb,cc))	stars
	 test =	~(ident(a,'x') ident(b,'y') ident(c,'z')) stars
	 test =	~(ident(aa,'a')	ident(bb,'b') ident(cc,'c')) stars
	 test =	~(ident(d,'d') ident(e,'e') ident(f,'f')) stars
*
*	 test nreturn
*
	 define('ntest()')		    :(endntest)
ntest	 ntest = .a			    :(nreturn)
endntest a = 27
	 test =	differ(ntest(),27) stars
.	       :f(st59)		   ;st59
	 ntest() = 26
.	       :f(st60)		   ;st60
	 test =	differ(a,26) stars
-eject
*
*	 continue test of functions
*
*
*	 test failure return
*
	 define('failure()')		    :(failend)
failure				  :(freturn)
failend	 test =	failure() stars
-eject
*
*	 test opsyn for	operators
*
	 opsyn('@',.dupl,2)
	 opsyn('|',.size,1)
	 test =	differ('a' @ 4,'aaaa') stars
	 test =	differ(|'string',6) stars
*
*	 test out array	facility
*
	 a = array(3)
	 test =	differ(a<1>) stars
	 a<2> =	4.5
	 test =	differ(a<2>,4.5) stars
	 test =	?a<4> stars
	 test =	?a<0> stars
	 test =	differ(prototype(a),'3') stars
	 b = array(3,10)
	 test =	differ(b<2>,10)	stars
	 b = array('3')
	 b<2> =	'a'
	 test =	differ(b<2>,'a') stars
	 c = array('2,2')
	 c<1,2>	= '*'
	 test =	differ(c<1,2>,'*') stars
	 test =	differ(prototype(c),'2,2') stars
	 d = array('-1:1,2')
	 d<-1,1> = 0
	 test =	differ(d<-1,1>,0) stars
	 test =	?d<-2,1> stars
	 test =	?d<2,1>	stars
-eject
*
*	 test program defined datatype functions
*
	 data('node(val,lson,rson)')
	 a = node('x','y','z')
	 test =	differ(datatype(a),'NODE') stars
	 test =	differ(val(a),'x') stars
	 b = node()
	 test =	differ(rson(b))	stars
	 lson(b) = a
	 test =	differ(rson(lson(b)),'z') stars
	 test =	differ(value('b'),b) stars
*
*	 test multiple use of field function name
*
	 data('clunk(value,lson)')
	 test =	differ(rson(lson(b)),'z') stars
	 test =	differ(value('b'),b) stars
	 c = clunk('a','b')
	 test =	differ(lson(c),'b') stars
-eject
*
*	 test numerical	predicates
*
	 test =	lt(5,4)	stars
	 test =	lt(4,4)	stars
	 test =	~lt(4,5) stars
	 test =	le(5,2)	stars
	 test =	~le(4,4) stars
	 test =	~le(4,10) stars
	 test =	eq(4,5)	stars
	 test =	eq(5,4)	stars
	 test =	~eq(5,5) stars
	 test =	ne(4,4)	stars
	 test =	~ne(4,6) stars
	 test =	~ne(6,4) stars
	 test =	gt(4,6)	stars
	 test =	gt(4,4)	stars
	 test =	~gt(5,2) stars
	 test =	ge(5,7)	stars
	 test =	~ge(4,4) stars
	 test =	~ge(7,5) stars
	 test =	ne(4,5 - 1) stars
	 test =	gt(4,3 + 1) stars
	 test =	le(20,5	+ 6) stars
	 test =	eq(1.0,2.0) stars
	 test =	gt(-2.0,-1.0) stars
	 test =	gt(-3.0,4.0) stars
	 test =	ne('12',12) stars
	 test =	ne('12',12.0) stars
	 test =	~convert(bal,'pattern')	stars
-eject
*
*	 test integer
*
	 test =	integer('abc') stars
	 test =	~integer(12) stars
	 test =	~integer('12') stars
*
*	 test size
*
	 test =	ne(size('abc'),3) stars
	 test =	ne(size(12),2) stars
	 test =	ne(size(null),0) stars
*
*	 test lgt
*
	 test =	lgt('abc','xyz') stars
	 test =	lgt('abc','abc') stars
	 test =	~lgt('xyz','abc') stars
	 test =	lgt(null,'abc')	stars
	 test =	~lgt('abc',null) stars
*
*	 test indirect addressing
*
	 test =	differ($'bal',bal) stars
	 test =	differ($.bal,bal) stars
	 $'qq' = 'x'
	 test =	differ(qq,'x') stars
	 test =	differ($'garbage') stars
	 a = array(3)
	 a<2> =	'x'
	 test =	differ($.a<2>,'x') stars
*
*	 test concatenation
*
	 test =	differ('a' 'b','ab')	    stars
	 test =	differ('a' 'b' 'c','abc') stars
	 test =	differ(1 2,'12') stars
	 test =	differ(2 2 2,'222') stars
	 test =	differ(1 3.4,'13.4') stars
	 test =	differ(bal null,bal)	    stars
	 test =	differ(null bal,bal) stars
-eject
*
*	 test remdr
*
	 test =	differ(remdr(10,3),1) stars
	 test =	differ(remdr(11,10),1) stars
*
*	 test dupl
*
	 test =	differ(dupl('abc',2),'abcabc') stars
	 test =	differ(dupl(null,10),null) stars
	 test =	differ(dupl('abcdefg',0),null) stars
	 test =	differ(dupl(1,10),'1111111111')	 stars
*
*	 test table facility
*
	 t = table(10)
	 test =	differ(t<'cat'>) stars
	 t<'cat'> = 'dog'
	 test =	differ(t<'cat'>,'dog')	 stars
	 t<7> =	45
	 test =	differ(t<7>,45)	  stars
	 test =	differ(t<'cat'>,'dog')	stars
	 ta = convert(t,'array')
	 test =	differ(prototype(ta),'2,2') stars
	 ata = convert(ta,'table')
	 test =	differ(ata<7>,45) stars
	 test =	differ(ata<'cat'>,'dog') stars
*
*	 test item function
*
	 aaa = array(10)
	 item(aaa,1) = 5
	 test =	differ(item(aaa,1),5) stars
	 test =	differ(aaa<1>,5) stars
	 aaa<2>	= 22
	 test =	differ(item(aaa,2),22) stars
	 ama = array('2,2,2,2')
	 item(ama,1,2,1,2) = 1212
	 test =	differ(item(ama,1,2,1,2),1212) stars
	 test =	differ(ama<1,2,1,2>,1212) stars
	 ama<2,1,2,1> =	2121
	 test =	differ(item(ama,2,1,2,1),2121) stars
-eject
*
*	 test eval
*
	 expr =	*('abc'	'def')
	 test =	differ(eval(expr),'abcdef') stars
	 q = 'qqq'
	 sexp =	*q
	 test =	differ(eval(sexp),'qqq') stars
	 fexp =	*ident(1,2)
	 test =	eval(fexp) stars
*
*	 test arg
*
jlab	 define('jlab(a,b,c)d,e,f')
	 test =	differ(arg(.jlab,1),'A') stars
	 test =	differ(arg(.jlab,3),'C') stars
	 test =	arg(.jlab,0) stars
	 test =	arg(.jlab,4) stars
*
*	 test local
*
	 test =	differ(local(.jlab,1),'D') stars
	 test =	differ(local(.jlab,3),'F') stars
	 test =	local(.jlab,0) stars
	 test =	local(.jlab,4) stars
*
*	 test apply
*
	 test =	apply(.eq,1,2) stars
	 test =	~apply(.eq,1,1)	stars
	 test =	~ident(apply(.trim,'abc	'),'abc') stars
-eject
*
*	 final processing
*
	 output	= '************************************************'
	 diagnostics = 1000000 - &trace
	 eq(diagnostics,0)	  :s(terminate)
	 &dump = 2
	 output	= '****	   number of errors detected  '
.				  diagnostics '	   ****'
	 output	= '****	e n d	 o f	 d i a g n o s t i c s ****'
	 output	= '************************************************'
.					    :(end)
terminate output = '**** n o	 e r r o r s	d e t e	c t e d	****'
	 output	= '****	e n d	 o f	 d i a g n o s t i c s ****'
	 output	= '************************************************'
 :(end)
*
*	 error handling	routine
*
errors	 eq(&errtype,0)                     :(continue)
	 output	= '****	 error at '
.	 lpad(&lastno,4)   '	  &errtype = ' lpad(&errtype,7,' ')
.					    ' ****'
	 &trace	= &trace - 1
	 setexit(.errors)		    :(continue)
end
" 5000)]
    (is (= "************************************************
****	s n o b o l	 d i a g n o s t i c s ****
****		 p h a s e    o	n e	       ****
************************************************
****	 any trace output indicates an error   ****
************************************************
************************************************
**** n o	 e r r o r s	d e t e	c t e d	****
****	e n d	 o f	 d i a g n o s t i c s ****
************************************************
Dump of variables at termination

Natural variables

A = ARRAY('3')
AA = 'a'
AAA = ARRAY('10')
ABORT = PATTERN
AMA = ARRAY('2,2,2,2')
ARB = PATTERN
ATA = TABLE(2,10)
B = NODE
BAL = PATTERN
BB = 'b'
C = CLUNK
CC = 'c'
D = ARRAY('-1:1,2')
DIAGNOSTICS = 0
E = 'e'
EXPR = EXPRESSION
F = 'f'
FAIL = PATTERN
FENCE = PATTERN
FEXP = EXPRESSION
OUTPUT = '************************************************'
Q = 'qqq'
QQ = 'x'
REM = PATTERN
SEXP = EXPRESSION
STARS = '	error detected		***'
SUCCEED = PATTERN
T = TABLE(10,10)
TA = ARRAY('2,2')

Unprotected keywords

&ABEND = 0
&ANCHOR = 0
&CASE = 1
&CODE = 0
&DUMP = 2
&ERRLIMIT = 999
&FATALLIMIT = 0
&FTRACE = 0
&FULLSCAN = 0
&GTRACE = 0
&INPUT = 1
&MAXLNGTH = xxx
&OUTPUT = 1
&STLIMIT = -1
&TRACE = 1000000
&TRIM = 0
" (:stdout r)))))

(deftest csnobol4-diag2
  "Budne suite: diag2.sno"
  (let [r (run-with-timeout "* title snobol test program #2 -- diagnostics phase two
*
*
*	 this is the standard test program for spitbol which
*	 tests pattern matching	using both fullscan and	quickscan
*
	 &dump = 2
	 define('error()')
	 &trace	= 1000
	 &errlimit = 00
	 trace(.errtype,'keyword')
	 &fullscan = 0
	 output	= '**********************************************'
	 output	= '****	snobol	diagnostics -- phase two     ****'
	 output	= '**********************************************'
floop	 errcount = 0
	 output	= '****		  &fullscan = '	&fullscan
.	 '		****'
	 test =	'abcdefghijklmnopqrstuvwxyz'
*
*	 test pattern matching against simple string
*
	 test  'abc' :s(s01) ; error()
s01	 test 'bcd' :s(s02) ; error()
s02	 test 'xyz' :s(s03) ; error()
s03	 test 'abd' :f(s04) ; error()
s04	 &anchor = 1
	 test 'abc' :s(s05) ; error()
s05	 test 'bcd' :f(s06) ; error()
s06	 test test :s(s06a) ; error()
*
*	 test simple cases of $
*
s06a	 test 'abc' $ var :s(s07) ; error()
s07	 ident(var,'abc') :s(s08) ; error()
s08	 test 'abc' . vard :s(s09) ; error()
s09	 ident(vard,'abc') :s(s10) ; error()
*
*	 test len
*
s10	 &anchor = 0
	 test len(3) $ varl :s(s11) ; error()
s11	 ident(varl,'abc') :s(s12) ; error()
s12	 test len(26) $	varl :s(s13) ; error()
s13	 ident(varl,test) :s(s14) ; error()
s14	 test len(27) :f(s15) ;	error()
*
*	 test tab
*
s15	 test tab(3) $ vart :s(s16) ; error()
s16	 ident(vart,'abc') :s(s17) ; error()
s17	 test tab(26) $	vart :s(s18) ; error()
s18	 ident(test,vart) :s(s19) ; error()
s19	 test tab(0) $ vart :s(s20) ; error()
s20	 ident(vart) :s(s21) ; error()
-eject
*
*	 test arb
*
s21	 test arb $ vara 'c' :s(s22) ; error()
s22	 ident(vara,'ab') :s(s23) ; error()
s23	 &anchor = 1
	 test arb $ vara pos(60) :f(s24) ; error()
s24	 ident(vara,test) :s(s25) ; error()
*
*	 test pos
*
s25	 test arb $ vara pos(2)	$ varp :s(s26) ; error()
s26	 (ident(vara,'ab') ident(varp))	:s(s27)	; error()
s27	 &anchor = 0
	 test arb $ vara pos(26) $ varp	:s(s28)	; error()
s28	 (ident(vara,test) ident(varp))	: s(s29) ; error()
s29	 test arb $ vara pos(0)	$ varp :s(s30) ; error()
s30	 ident(vara varp) :s(s31) ; error()
s31	 test pos(0) arb $ vara	pos(26)	:s(s32)	; error()
s32	 ident(test,vara) :s(s33) ; error()
s33	 test pos(2) arb $ vara	pos(3) :s(s34) ; error()
s34	 ident(vara,'c') :s(s35) ; error()
s35	 test pos(27) :f(s36) ;	error()
*
*	 test rpos
*
s36	 test arb $ vara rpos(25) :s(s37) ; error()
s37	 ident(vara,'a') :s(s38) ; error()
s38	 test arb $ vara rpos(0) :s(s39) ; error()
s39	 ident(test,vara) :s(s39a) ; error()
s39a	 test arb $ vara rpos(26) :s(s40) ; error()
s40	 ident(vara) :s(s41) ; error()
s41	 test rpos(27) :f(s42) ; error()
*
*	 test rtab
*
s42	 test rtab(26) $ vara :s(s43) ;	error()
s43	 ident(vara) :s(s44) ; error()
s44	 test rtab(27) :f(s45) ; error()
s45	 test rtab(0) $	vara :s(s46) ; error()
s46	 ident(vara,test) :s(s47) ; error()
s47	 test rtab(25) $ vara :s(s48) ;	error()
s48	 ident(vara,'a') :s(s49) ; error()
*
*	 test @
*
s49	 test len(6) @vara :s(s50) ; error()
s50	 ident(vara,6) :s(s51) ; error()
s51	 test @vara :s(s52) ; error()
s52	 ident(vara,0) :s(s53) ; error()
s53	 test len(26) @vara :s(s54) ; error()
s54	 ident(vara,26)	:s(s55)	; error()
-eject
*
*	 test break
*
s55	 test break('c') $ vara	:s(s56)	; error()
s56	 ident(vara,'ab') :s(s57) ; error()
s57	 test break('z()') $ vara :s(s58)     ;	error()
s58	 ident(vara,'abcdefghijklmnopqrstuvwxy') :s(s59) ; error()
s59	 test break(',') :f(s60) ; error()
s60
*
*	 test span
*
s63	 test span(test) $ vara	:s(s64)	; error()
s64	 ident(test,vara) :s(s65) ;error()
s65	 test span('cdq') $ vara :s(s66) ; error()
s66	 ident(vara,'cd') :s(s67) ; error()
s67	 test span(',')	:f(s68)	; error()
s68
*
*
*	 test any
*
s73	 test any('mxz') $ vara	:s(s74)	; error()
s74	 ident(vara,'m') :s(s75) ; error()
s75	 test any(',.')	:f(s76)	; error()
-eject
*
*	 test notany
*
s76	 test notany('abcdefghjklmpqrstuwxyz') $ vara :s(s77) ;	error()
s77	 ident(vara,'i') :s(s78) ; error()
s78	 test notany(test) :f(s79) ; error()
*
*	 test rem
*
s79	 test rem $ vara :s(s80) ; error()
s80	 ident(vara,test) :s(s81) ; error()
s81	 test len(26) rem $ vara :s(s82) ; error()
s82	 ident(vara) :s(s83) ; error()
*
*	 test alternation
*
s83	 test ('abd' | 'ab') $ vara :s(d84) ; error()
d84	 ident(vara,'ab') :s(d85) ; error()
d85	 test (test 'a'	| test)	$ varl :s(d86) ; error()
d86	 ident(varl,test) :s(d00) ; error()
*
*	 test deferred strings
*
d00	 test *'abc' :s(d01) ; error()
d01	 test *'abd' :f(d06) ; error()
*
*	 test $	. with deferred	name arguments
*
d06	 test 'abc' $ *var :s(d07) ; error()
d07	 ident(var,'abc') :s(d08) ; error()
d08	 test 'abc' . *$'vard' :s(d09) ; error()
d09	 ident(vard,'abc') :s(d10) ; error()
*
*	 test len with deferred	argument
*
d10	 &anchor = 0
	 test len(*3) $	varl :s(d11) ; error()
d11	 ident(varl,'abc') :s(d15) ; error()
*
*	 test tab with deferred	argument
*
d15	 test tab(*3) $	vart :s(d16) ; error()
d16	 ident(vart,'abc') :s(d21) ; error()
-eject
*
*	 test pos with deferred	argument
*
d21	 &anchor = 1
	 test arb $ vara pos(*2) $ varp	:s(d26)	; error()
d26	 (ident(vara,'ab') ident(varp))	:s(d27)	; error()
d27	 &anchor = 0
	 test arb $ vara pos(*0) $ varp	:s(d35)	; error()
d35	 ident(vara varp) :s(d36) ; error()
*
*	 test rpos with	deferred argument
*
d36	 test arb $ vara rpos(*25) :s(d37) ; error()
d37	 ident(vara,'a') :s(d38) ; error()
*
*	 test rtab with	deferred argument
*
d38	 test rtab(*26)	$ vara :s(d43) ; error()
d43	 ident(vara) :s(d49) ; error()
*
*	 test @	with deferred argument
*
d49	 test len(6) @*vara :s(d50) ; error()
d50	 ident(vara,6) :s(d51) ; error()
d51	 test @*$'vara'	:s(d52)	; error()
d52	 ident(vara,0) :s(d55) ; error()
*
*	 test break with deferred argument
*
d55	 test break(*'c') $ vara :s(d56) ; error()
d56	 ident(vara,'ab') :s(d57) ; error()
*
*	 test span with	deferred argument
*
d57	 test span(*test) $ vara :s(d64) ; error()
d64	 ident(test,vara) :s(d70) ; error()
*
*	 test breakx with deferred argument
*
d70
*      (test test) pos(*0) breakx(*'e')	$ vara '.' :f(d71) ; error()
*d71	  ident(vara,test 'abcd') :s(d73) ; error()
-eject
*
*	 test any with deferred	argument
*
d73	 test any(*'mxz') $ vara :s(d74) ; error()
d74	 ident(vara,'m') :s(d75) ; error()
*
*	 test notany with deferred argument
*
d75	 test notany(*'abcdefghjklmpqrstuwxyz')	$ vara :s(d77) ;
.							   error()
d77	 ident(vara,'i') :s(d79) ; error()
d79	 :(alldone)
	 eject
*
*	 error handling	routine
*
error	 output	= '****** error	detected at ' &lastno '	********'
	 errcount = errcount + 1
	 output	= '***** resuming execution *******'	   :(return)
*
*	 termination routine
*
alldone
	 errcount = errcount + &errlimit - 100
	 &errlimit = 100
	 output	= eq(errcount,0)
.		  '****		  no errors detected	     ****'
	 output	= '**********************************************'
	 &fullscan = eq(&fullscan,0) 1		 :s(floop)
	 output	= '****		  end of diagnostics	     ****'
	 output	= '**********************************************'
end
" 5000)]
    (is (= "**********************************************
****	snobol	diagnostics -- phase two     ****
**********************************************
****		  &fullscan = 0		****
****** error	detected at 67	********
***** resuming execution *******
**********************************************
****		  &fullscan = 1		****
****		  no errors detected	     ****
**********************************************
****		  end of diagnostics	     ****
**********************************************
Dump of variables at termination

Natural variables

ABORT = PATTERN
ARB = PATTERN
BAL = PATTERN
ERRCOUNT = 0
FAIL = PATTERN
FENCE = PATTERN
OUTPUT = '**********************************************'
REM = PATTERN
SUCCEED = PATTERN
TEST = 'abcdefghijklmnopqrstuvwxyz'
VAR = 'abc'
VARA = 'i'
VARD = 'abc'
VARL = 'abc'
VART = 'abc'

Unprotected keywords

&ABEND = 0
&ANCHOR = 0
&CASE = 1
&CODE = 0
&DUMP = 2
&ERRLIMIT = 99
&FATALLIMIT = 0
&FTRACE = 0
&FULLSCAN = 1
&GTRACE = 0
&INPUT = 1
&MAXLNGTH = xxx
&OUTPUT = 1
&STLIMIT = -1
&TRACE = 1000
&TRIM = 0
" (:stdout r)))))

(deftest csnobol4-digits
  "Budne suite: digits.sno"
  (let [r (run-with-timeout "	OUTPUT = &UCASE
	OUTPUT = &LCASE
	OUTPUT = &DIGITS
END
" 2000)]
    (is (= "ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789
" (:stdout r)))))

(deftest csnobol4-dump
  "Budne suite: dump.sno"
  (let [r (run-with-timeout "	a = 1
	b = 'hello world'
	&DUMP = 1
END
" 2000)]
    (is (= "Dump of variables at termination

Natural variables

A = 1
ABORT = PATTERN
ARB = PATTERN
B = 'hello world'
BAL = PATTERN
FAIL = PATTERN
FENCE = PATTERN
REM = PATTERN
SUCCEED = PATTERN

Unprotected keywords

&ABEND = 0
&ANCHOR = 0
&CASE = 1
&CODE = 0
&DUMP = 1
&ERRLIMIT = 0
&FATALLIMIT = 0
&FTRACE = 0
&FULLSCAN = 0
&GTRACE = 0
&INPUT = 1
&MAXLNGTH = xxx
&OUTPUT = 1
&STLIMIT = -1
&TRACE = 0
&TRIM = 0
" (:stdout r)))))

(deftest csnobol4-end
  "Budne suite: end.sno"
  (let [r (run-with-timeout "END
" 2000)]
    (is (= "" (:stdout r)))))

(deftest csnobol4-err
  "Budne suite: err.sno"
  (let [r (run-with-timeout "	&errlimit = 2
	a[1] =
	output = &errtype
	output = &errtext
* should be the same as before if &errtext reference ok
	output = &errtype
end
" 2000)]
    (is (= "3
Erroneous array or table reference
3
" (:stdout r)))))

(deftest csnobol4-fact
  "Budne suite: fact.sno"
  (let [r (run-with-timeout "*	test functions

	DEFINE(\"FACT(X)\")			:(END_FACT)
FACT	FACT = EQ(X,0) 1			:S(RETURN)
	FACT = X * FACT(X - 1)			:(RETURN)
END_FACT

	DEFINE(\"DOIT(X)\")			:(END_DOIT)
DOIT	OUTPUT = \"FACT(\" X \") = \" FACT(X)	:(RETURN)
END_DOIT

	DOIT(0)
	DOIT(1)
	DOIT(2)
	DOIT(3)
	DOIT(4)
	DOIT(5)
	DOIT(6)
END
" 2000)]
    (is (= "FACT(0) = 1
FACT(1) = 1
FACT(2) = 2
FACT(3) = 6
FACT(4) = 24
FACT(5) = 120
FACT(6) = 720
" (:stdout r)))))

(deftest csnobol4-factor
  "Budne suite: factor.sno"
  (let [r (run-with-timeout "* THIS PROGRAM COMPUTES AND PRINTS A TABLE OF N FACTORIAL
* FOR VALUES OF N FROM 1 THROUGH AN UPPER LIMIT \"NX\"

* TAKEN FROM PAGE 203 OF THE REFERENCE MANUAL
* INITIALIZATION
 NX = 45
 N = 1
 NSET = 1
 NUM = ARRAY(1000)
 NUM<1> = 1
 FILL = ARRAY('0:3')
 FILL<0> = '000'
 FILL<1> = '00'
 FILL<2> = '0'

 OUTPUT = '     TABLE OF FACTORIALS FOR 1 THROUGH ' NX
 OUTPUT = 
*COMPUTE THE NEXT VALUE FROM THE PREVIOUS ONE
L1 I = 1
L2 NUM<I> = NUM<I> * N    :F(ERR)
 I = LT(I,NSET) I + 1    :S(L2)
 I = 1
L3 LT(NUM<I>,1000)    :S(L4)
 NUMX = NUM<I> / 1000    :F(ERR)
 NUM<I + 1> = NUM<I + 1> + NUMX  :F(ERR)
 NUM<I> = NUM<I> - 1000 * NUMX   :F(ERR)
L4 I = LT(I,NSET) I + 1   :S(L3)
*FORM A  STRING REPRESENTING THE FACTORIAL
L5 NSET = DIFFER(NUM<NSET + 1>) NSET + 1
 NUMBER = NUM<NSET>   :F(ERR)
 I = GT(NSET,1) NSET - 1   :F(L7)
L6 NUMBER = NUMBER ',' FILL<SIZE(NUM<I>)> NUM<I>
 I = GT(I,1) I - 1   :S(L6)
*OUTPUT A LINE OF THE TABLE
L7 OUTPUT = N '!=' NUMBER
 N = LT(N,NX) N + 1  :S(L1)F(END)
*ERROR TERMINATION
ERR OUTPUT = N '! CANNOT BE COMPUTED BECAUSE OF TABLE OVERFLOW.'
 OUTPUT = '  INCREASE THE SIZE OF ARRAY \"NUM\".'
*
END
                                                                                                               " 5000)]
    (is (= "     TABLE OF FACTORIALS FOR 1 THROUGH 45

1!=1
2!=2
3!=6
4!=24
5!=120
6!=720
7!=5,040
8!=40,320
9!=362,880
10!=3,628,800
11!=39,916,800
12!=479,001,600
13!=6,227,020,800
14!=87,178,291,200
15!=1,307,674,368,000
16!=20,922,789,888,000
17!=355,687,428,096,000
18!=6,402,373,705,728,000
19!=121,645,100,408,832,000
20!=2,432,902,008,176,640,000
21!=51,090,942,171,709,440,000
22!=1,124,000,727,777,607,680,000
23!=25,852,016,738,884,976,640,000
24!=620,448,401,733,239,439,360,000
25!=15,511,210,043,330,985,984,000,000
26!=403,291,461,126,605,635,584,000,000
27!=10,888,869,450,418,352,160,768,000,000
28!=304,888,344,611,713,860,501,504,000,000
29!=8,841,761,993,739,701,954,543,616,000,000
30!=265,252,859,812,191,058,636,308,480,000,000
31!=8,222,838,654,177,922,817,725,562,880,000,000
32!=263,130,836,933,693,530,167,218,012,160,000,000
33!=8,683,317,618,811,886,495,518,194,401,280,000,000
34!=295,232,799,039,604,140,847,618,609,643,520,000,000
35!=10,333,147,966,386,144,929,666,651,337,523,200,000,000
36!=371,993,326,789,901,217,467,999,448,150,835,200,000,000
37!=13,763,753,091,226,345,046,315,979,581,580,902,400,000,000
38!=523,022,617,466,601,111,760,007,224,100,074,291,200,000,000
39!=20,397,882,081,197,443,358,640,281,739,902,897,356,800,000,000
40!=815,915,283,247,897,734,345,611,269,596,115,894,272,000,000,000
41!=33,452,526,613,163,807,108,170,062,053,440,751,665,152,000,000,000
42!=1,405,006,117,752,879,898,543,142,606,244,511,569,936,384,000,000,000
43!=60,415,263,063,373,835,637,355,132,068,513,997,507,264,512,000,000,000
44!=2,658,271,574,788,448,768,043,625,811,014,615,890,319,638,528,000,000,000
45!=119,622,220,865,480,194,561,963,161,495,657,715,064,383,733,760,000,000,000
" (:stdout r)))))

(deftest csnobol4-file
  "Budne suite: file.sno"
  (let [r (run-with-timeout "	DEFINE(\"TEST(FUNC)\")				:(ETEST)
TEST	TEST2(FUNC, \".\")
	TEST2(FUNC, \"file.sno\")
	TEST2(FUNC, \"aa\")
	TEST2(FUNC, \"/\")				:(RETURN)
ETEST

	DEFINE(\"TEST2(FUNC,PATH)\")			:(ETEST2)
TEST2	OUTPUT = APPLY(FUNC,PATH) FUNC \" \" PATH		:(RETURN)
ETEST2

	TEST(\"FILE\")
	TEST(\"FILE_ISDIR\")
	TEST(\"FILE_ABSPATH\")
END
" 2000)]
    (is (= "FILE .
FILE file.sno
FILE aa
FILE /
FILE_ISDIR .
FILE_ISDIR aa
FILE_ISDIR /
FILE_ABSPATH /
" (:stdout r)))))

(deftest csnobol4-float
  "Budne suite: float.sno"
  (let [r (run-with-timeout "	OUTPUT = 3.14159
	OUTPUT = 1.0
	OUTPUT = 1.0 / 1000
	OUTPUT = 1.0 / 1000000
	OUTPUT = 1.0 / 1000000000
END
" 2000)]
    (is (= "3.14159
1.
0.001
1e-06
1e-09
" (:stdout r)))))

(deftest csnobol4-float2
  "Budne suite: float2.sno"
  (let [r (run-with-timeout "-include 'host.sno'

	A = 0.1
	B = 0.2
	OUTPUT = A
	OUTPUT = B
	OUTPUT = A + B

	BITS = HOST(HOST_REAL_BITS)
	FT = 1. / 3
	OUTPUT = FT * 3
	THIRD = CONVERT(FT, 'STRING')
	EQ(BITS,64)					:S(DOUBLE)
	OUTPUT = IDENT(THIRD,\"0.333333\") \"OK\"		:(END)
DOUBLE	OUTPUT = IDENT(THIRD,\"0.333333333333333\") \"OK\"
END
" 2000)]
    (is (= "0.1
0.2
0.3
1.
OK
" (:stdout r)))))

(deftest csnobol4-ftrace
  "Budne suite: ftrace.sno"
  (let [r (run-with-timeout "	&FTRACE = 10000

	DEFINE(\"A()\")
	DEFINE(\"B()\")
	DEFINE(\"C()\")
	DEFINE(\"D()\")

	:(BEGIN)

A	B(1)						:(RETURN)
B	C(1)						:(RETURN)
C	D(0)
	D(1)						:(RETURN)
D							:(RETURN)

BEGIN	A(\"hello\")
END
" 2000)]
    (is (= "ftrace.sno:16 stmt 16: level 0 call of A(), time = xxx
ftrace.sno:10 stmt 10: level 1 call of B(), time = xxx
ftrace.sno:11 stmt 11: level 2 call of C(), time = xxx
ftrace.sno:12 stmt 12: level 3 call of D(), time = xxx
ftrace.sno:14 stmt 14: level 3 RETURN of D = '', time = xxx
ftrace.sno:13 stmt 13: level 3 call of D(), time = xxx
ftrace.sno:14 stmt 14: level 3 RETURN of D = '', time = xxx
ftrace.sno:13 stmt 13: level 2 RETURN of C = '', time = xxx
ftrace.sno:11 stmt 11: level 1 RETURN of B = '', time = xxx
ftrace.sno:10 stmt 10: level 0 RETURN of A = '', time = xxx
" (:stdout r)))))

(deftest csnobol4-fun1
  "Budne suite: fun1.sno"
  (let [r (run-with-timeout "*	test functions (arg values)

	DEFINE(\"DOIT(X)\")			:(END_DOIT)
DOIT	OUTPUT = DATATYPE(X) \" {\" X \"}\"		:(RETURN)
END_DOIT

	DOIT(0)
	DOIT(1)
	DOIT(2)
	DOIT(3)
	DOIT(4)
	DOIT(5)
	DOIT(6)
END
" 2000)]
    (is (= "INTEGER {0}
INTEGER {1}
INTEGER {2}
INTEGER {3}
INTEGER {4}
INTEGER {5}
INTEGER {6}
" (:stdout r)))))

(deftest csnobol4-fun2
  "Budne suite: fun2.sno"
  (let [r (run-with-timeout "*	test functions (arg & return values)

	DEFINE(\"DOIT(X)\")		:(END_DOIT)
DOIT	DOIT = X			:(RETURN)
END_DOIT

	OUTPUT = DOIT(0)
	OUTPUT = DOIT(1)
	OUTPUT = DOIT(2)
	OUTPUT = DOIT(3)
	OUTPUT = DOIT(4)
	OUTPUT = DOIT(5)
	OUTPUT = DOIT(6)
END
" 2000)]
    (is (= "0
1
2
3
4
5
6
" (:stdout r)))))

(deftest csnobol4-func2
  "Budne suite: func2.sno"
  (let [r (run-with-timeout "* BUG: alt entry label was not folded!!!!

	define(\"Foo(X)\", \"y\")	:(efoo)
y	foo = \"Hello \" x	:(return)
efoo

	OUTPUT = fOo(\"World\")
end
" 2000)]
    (is (= "Hello World
" (:stdout r)))))

(deftest csnobol4-function
  "Budne suite: function.sno"
  (let [r (run-with-timeout "	DEFINE(\"TEST(NAME)\")				:(ETEST)
TEST	OUTPUT = FUNCTION(NAME) NAME			:(RETURN)
ETEST

* turn off case folding
-CASE 0
	DEFINE(\"foo()\")					:(EFOO)
foo	OUTPUT = 'got here'				:(RETURN)
EFOO
	DEFINE(\"BAR()\")					:(EBAR)
BAR	OUTPUT = 'got here2'				:(RETURN)

* refererence to undefined function
	XXX = UNK2()
EBAR

	TEST(\"FOO\")
	TEST(\"foo\")
	TEST(\"BAR\")
	TEST(\"bar\")
	TEST(\"TEST\")
	TEST(\"test\")
	TEST(\"DEFINE\")
	TEST(\"define\")
	TEST(\"UNK\")
	TEST(\"unk\")
	TEST(\"UNK2\")
	TEST(\"unk2\")

* turn case folding on
	&CASE = 1

	TEST(\"FOO\")
	TEST(\"foo\")
	TEST(\"BAR\")
	TEST(\"bar\")
	TEST(\"TEST\")
	TEST(\"test\")
	TEST(\"DEFINE\")
	TEST(\"define\")
	TEST(\"UNK\")
	TEST(\"unk\")
	TEST(\"UNK2\")
	TEST(\"unk2\")

END

" 2000)]
    (is (= "foo
BAR
TEST
DEFINE
BAR
bar
TEST
test
DEFINE
define
" (:stdout r)))))

(deftest csnobol4-hello
  "Budne suite: hello.sno"
  (let [r (run-with-timeout "	OUTPUT = 'hello world'
END
" 2000)]
    (is (= "hello world
" (:stdout r)))))

(deftest csnobol4-hide
  "Budne suite: hide.sno"
  (let [r (run-with-timeout "* test of -HIDE (for sdb)
-hide
	output = &stno
	output = &stno
-list
	output = &stno
	output = &stno
end
" 2000)]
    (is (= "        -LIST
1       	OUTPUT = &STNO
2       	OUTPUT = &STNO
3       END
0
0
1
2
" (:stdout r)))))

(deftest csnobol4-include
  "Budne suite: include.sno"
  (let [r (run-with-timeout "	output = &file ':' &line
-include \"line2.sno\"
	output = &file ':' &line
-include \"line2.sno \"
	output = &file ':' &line
-include \"line2.sno\"
	output = &file ':' &line
end
" 2000)]
    (is (= "include.sno:1
include.sno:1 (last)
line2.sno:2 (line2)
include.sno:3
include.sno:3 (last)
line2.sno:2 (line2)
include.sno:5
include.sno:7
" (:stdout r)))))

(deftest csnobol4-include2
  "Budne suite: include2.sno"
  (let [r (run-with-timeout "-include 'aa.sno'
end
" 2000)]
    (is (= "aa
" (:stdout r)))))

(deftest csnobol4-include3
  "Budne suite: include3.sno"
  (let [r (run-with-timeout "-include 'aa.sno'
-include 'bb.sno'
end
" 2000)]
    (is (= "aa
bb
" (:stdout r)))))

(deftest csnobol4-include4
  "Budne suite: include4.sno"
  (let [r (run-with-timeout "-include 'aa.sno'
-include 'bb.sno'
end
" 2000)]
    (is (= "aa
bb
" (:stdout r)))))

(deftest csnobol4-ind
  "Budne suite: ind.sno"
  (let [r (run-with-timeout "	foo = 'hello world'
	bar = .foo
	output = $bar

* XXX SIGH; SPITBOL will output 'hello world'
	output = $('f' 'o' 'o')
end
" 2000)]
    (is (= "hello world
hello world
" (:stdout r)))))

(deftest csnobol4-intval
  "Budne suite: intval.sno"
  (let [r (run-with-timeout "	&trim = 1.0
	output = &trim
	collect(999.9)
	output = char(65.0)
	output = dupl(\"*\", 5.9999)

	&trim = '1.0'
	output = &trim
	collect('999.9')
	output = char('65.0')
	output = dupl(\"*\", '5.9999')
end
" 2000)]
    (is (= "1
A
*****
1
A
*****
" (:stdout r)))))

(deftest csnobol4-json1
  "Budne suite: json1.sno"
  (let [r (run-with-timeout "-include 'json.sno'

	DEFINE('loadump(x,x2)y,y2')			:(e.loadump)
loadump
	y = JSON_DECODE(x)				:F(loadump.df)
	y2 = JSON_ENCODE(y)				:F(loadump.lf)
	x2 = ident(x2) x
	output = ident(x2,y2) 'OK: ' x			:S(RETURN)
	OUTPUT = 'ERROR: <' x '> got <' y2 '> exp <' x2 '>' :(loadump.fail)
loadump.df OUTPUT = 'ERROR: dump failed: ' x		:(loadump.fail)
loadump.lf OUTPUT = 'ERROR: load failed: ' y
loadump.fail &CODE = &CODE + 1				:(FRETURN)
e.loadump

* NOTE! Internal function!
	OUTPUT = json.escape('hello world')
	OUTPUT = json.escape(json.bs json.ht json.lf json.ff json.cr '\"\\')
	OUTPUT = json.escape(json.ctrl)

	loadump('1')
	loadump('-1')
	loadump('1.')
	loadump('2.0', '2.')
	loadump('3e2', '300.')
	loadump('1.234e3', '1234.')
	loadump('-1234e-3', '-1.234')
	loadump('null', '\"\"')
	loadump('\"foo\"')
	loadump('[\"foo\"]')
	loadump('[null]', '[\"\"]')
	loadump('[1,2,3]')
	loadump('{\"a\":\"hello\",\"z\":\"goodbye\"}')
	loadump('[{\"a\":1.,\"b\":-2},{\"c\":3,\"d\":[4,5,6]}]')
	loadump('{}')
	loadump('\"\\b\\t\\n\\f\\r\\\"\\\\\"')
	loadump('\"hello\\nworld\"')
	loadump('\"hello\\u000Aworld\"','\"hello\\nworld\"')
	loadump(JSON_ENCODE(json.ctrl))
	loadump('\"hello world\"')
* http://doc.cat-v.org/plan_9/4th_edition/papers/utf (1993)
* greek (two-byte UTF-8 sequences)
	loadump('\"Καλημέρα κόσμε\"')
* kanji (three-byte UTF-8 sequences)
	loadump('\"こんにちは 世界\"')
* all emoji are off the \"Basic Multilingual Plane\"
* (and are four-byte UTF-8 sequences)
	loadump('\"👍😊🥺😉😍😘😚😜😂😝😳😁😣😢😭😰🥰🥴😏🍆🍑\"')
	loadump('\"\\U0001F44D\"', '\"👍\"')
* errors:
*	OUTPUT = JSON_ENCODE(json.cons())
*	OUTPUT = JSON_ENCODE(ARRAY('1:10'))
END
" 2000)]
    (is (= "\"hello world\"
\"\\b\\t\\n\\f\\r\\\"\\\\\"
\"\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007\\b\\t\\n\\u000b\\f\\r\\u000e\\u000f\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017\\u0018\\u0019\\u001a\\u001b\\u001c\\u001d\\u001e\\u001f\"
OK: 1
OK: -1
OK: 1.
OK: 2.0
OK: 3e2
OK: 1.234e3
OK: -1234e-3
OK: null
OK: \"foo\"
OK: [\"foo\"]
OK: [null]
OK: [1,2,3]
OK: {\"a\":\"hello\",\"z\":\"goodbye\"}
OK: [{\"a\":1.,\"b\":-2},{\"c\":3,\"d\":[4,5,6]}]
OK: {}
OK: \"\\b\\t\\n\\f\\r\\\"\\\\\"
OK: \"hello\\nworld\"
OK: \"hello\\u000Aworld\"
OK: \"\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007\\b\\t\\n\\u000b\\f\\r\\u000e\\u000f\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017\\u0018\\u0019\\u001a\\u001b\\u001c\\u001d\\u001e\\u001f\"
OK: \"hello world\"
OK: \"Καλημέρα κόσμε\"
OK: \"こんにちは 世界\"
OK: \"👍😊🥺😉😍😘😚😜😂😝😳😁😣😢😭😰🥰🥴😏🍆🍑\"
OK: \"\\U0001F44D\"
" (:stdout r)))))

(deftest csnobol4-keytrace
  "Budne suite: keytrace.sno"
  (let [r (run-with-timeout "* tests all types of keyword trace, including STNO/BREAKPOINT
	&STLIMIT = &TRACE = &ERRLIMIT = 10000
	TRACE(\"STNO\", \"KEYWORD\")
	TRACE(\"STCOUNT\", \"KEYWORD\")
	TRACE(\"FNCLEVEL\", \"KEYWORD\")
	TRACE(\"STFCOUNT\", \"KEYWORD\")
	TRACE(\"ERRTYPE\", \"KEYWORD\")
	DEFINE(\"A()\")
	BREAKPOINT(15, 1)
	BREAKPOINT(17, 1)
	BREAKPOINT(19, 1)
	BREAKPOINT(21, 1)
	LOOPS = 0
LOOP	OUTPUT = \"LOOPS = \" LOOPS \" ****\"
	X = 1 / 0
	A()
	A()
	A()
	A()
	A()
	A()
	A()
	A()
	A()
	X = LEN(-1)
	LOOPS = LOOPS + 1
	BREAKPOINT(13, 0)
	BREAKPOINT(17, 0)
	EQ(LOOPS, 1)	:S(LOOP)
	STOPTR(\"STCOUNT\", \"KEYWORD\")
	STOPTR(\"FNCLEVEL\", \"KEYWORD\")
	EQ(LOOPS,2)	:S(LOOP)
	TRACE(\"STCOUNT\", \"KEYWORD\")
	EQ(LOOPS,3)	:S(LOOP)
	:(END)

A	OUTPUT = (N = N + 1) :(RETURN)
END
" 5000)]
    (is (= "keytrace.sno:5 stmt 4: &STCOUNT = 3, time = xxx
keytrace.sno:6 stmt 5: &STCOUNT = 4, time = xxx
keytrace.sno:7 stmt 6: &STCOUNT = 5, time = xxx
keytrace.sno:8 stmt 7: &STCOUNT = 6, time = xxx
keytrace.sno:9 stmt 8: &STCOUNT = 7, time = xxx
keytrace.sno:10 stmt 9: &STCOUNT = 8, time = xxx
keytrace.sno:11 stmt 10: &STCOUNT = 9, time = xxx
keytrace.sno:12 stmt 11: &STCOUNT = 10, time = xxx
keytrace.sno:13 stmt 12: &STCOUNT = 11, time = xxx
keytrace.sno:14 stmt 13: &STCOUNT = 12, time = xxx
LOOPS = 0 ****
keytrace.sno:15 stmt 14: &STCOUNT = 13, time = xxx
keytrace.sno:15 stmt 14: &ERRTYPE = 2, time = xxx
keytrace.sno:15 stmt 14: &STFCOUNT = 1, time = xxx
keytrace.sno:16 stmt 15: &STNO = 15, time = xxx
keytrace.sno:16 stmt 15: &STCOUNT = 14, time = xxx
keytrace.sno:16 stmt 15: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 15, time = xxx
1
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:17 stmt 16: &STCOUNT = 16, time = xxx
keytrace.sno:17 stmt 16: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 17, time = xxx
2
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:18 stmt 17: &STNO = 17, time = xxx
keytrace.sno:18 stmt 17: &STCOUNT = 18, time = xxx
keytrace.sno:18 stmt 17: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 19, time = xxx
3
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:19 stmt 18: &STCOUNT = 20, time = xxx
keytrace.sno:19 stmt 18: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 21, time = xxx
4
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:20 stmt 19: &STNO = 19, time = xxx
keytrace.sno:20 stmt 19: &STCOUNT = 22, time = xxx
keytrace.sno:20 stmt 19: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 23, time = xxx
5
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:21 stmt 20: &STCOUNT = 24, time = xxx
keytrace.sno:21 stmt 20: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 25, time = xxx
6
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:22 stmt 21: &STNO = 21, time = xxx
keytrace.sno:22 stmt 21: &STCOUNT = 26, time = xxx
keytrace.sno:22 stmt 21: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 27, time = xxx
7
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:23 stmt 22: &STCOUNT = 28, time = xxx
keytrace.sno:23 stmt 22: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 29, time = xxx
8
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:24 stmt 23: &STCOUNT = 30, time = xxx
keytrace.sno:24 stmt 23: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 31, time = xxx
9
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:25 stmt 24: &STCOUNT = 32, time = xxx
keytrace.sno:25 stmt 24: &ERRTYPE = 14, time = xxx
keytrace.sno:25 stmt 24: &STFCOUNT = 2, time = xxx
keytrace.sno:26 stmt 25: &STCOUNT = 33, time = xxx
keytrace.sno:27 stmt 26: &STCOUNT = 34, time = xxx
keytrace.sno:28 stmt 27: &STCOUNT = 35, time = xxx
keytrace.sno:29 stmt 28: &STCOUNT = 36, time = xxx
keytrace.sno:14 stmt 13: &STCOUNT = 37, time = xxx
LOOPS = 1 ****
keytrace.sno:15 stmt 14: &STCOUNT = 38, time = xxx
keytrace.sno:15 stmt 14: &ERRTYPE = 2, time = xxx
keytrace.sno:15 stmt 14: &STFCOUNT = 3, time = xxx
keytrace.sno:16 stmt 15: &STNO = 15, time = xxx
keytrace.sno:16 stmt 15: &STCOUNT = 39, time = xxx
keytrace.sno:16 stmt 15: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 40, time = xxx
10
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:17 stmt 16: &STCOUNT = 41, time = xxx
keytrace.sno:17 stmt 16: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 42, time = xxx
11
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:18 stmt 17: &STCOUNT = 43, time = xxx
keytrace.sno:18 stmt 17: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 44, time = xxx
12
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:19 stmt 18: &STCOUNT = 45, time = xxx
keytrace.sno:19 stmt 18: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 46, time = xxx
13
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:20 stmt 19: &STNO = 19, time = xxx
keytrace.sno:20 stmt 19: &STCOUNT = 47, time = xxx
keytrace.sno:20 stmt 19: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 48, time = xxx
14
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:21 stmt 20: &STCOUNT = 49, time = xxx
keytrace.sno:21 stmt 20: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 50, time = xxx
15
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:22 stmt 21: &STNO = 21, time = xxx
keytrace.sno:22 stmt 21: &STCOUNT = 51, time = xxx
keytrace.sno:22 stmt 21: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 52, time = xxx
16
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:23 stmt 22: &STCOUNT = 53, time = xxx
keytrace.sno:23 stmt 22: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 54, time = xxx
17
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:24 stmt 23: &STCOUNT = 55, time = xxx
keytrace.sno:24 stmt 23: &FNCLEVEL = 1, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 56, time = xxx
18
keytrace.sno:37 stmt 36: &FNCLEVEL = 0, time = xxx
keytrace.sno:25 stmt 24: &STCOUNT = 57, time = xxx
keytrace.sno:25 stmt 24: &ERRTYPE = 14, time = xxx
keytrace.sno:25 stmt 24: &STFCOUNT = 4, time = xxx
keytrace.sno:26 stmt 25: &STCOUNT = 58, time = xxx
keytrace.sno:27 stmt 26: &STCOUNT = 59, time = xxx
keytrace.sno:28 stmt 27: &STCOUNT = 60, time = xxx
keytrace.sno:29 stmt 28: &STCOUNT = 61, time = xxx
keytrace.sno:29 stmt 28: &STFCOUNT = 5, time = xxx
keytrace.sno:30 stmt 29: &STCOUNT = 62, time = xxx
LOOPS = 2 ****
keytrace.sno:15 stmt 14: &ERRTYPE = 2, time = xxx
keytrace.sno:15 stmt 14: &STFCOUNT = 6, time = xxx
keytrace.sno:16 stmt 15: &STNO = 15, time = xxx
19
20
21
22
keytrace.sno:20 stmt 19: &STNO = 19, time = xxx
23
24
keytrace.sno:22 stmt 21: &STNO = 21, time = xxx
25
26
27
keytrace.sno:25 stmt 24: &ERRTYPE = 14, time = xxx
keytrace.sno:25 stmt 24: &STFCOUNT = 7, time = xxx
keytrace.sno:29 stmt 28: &STFCOUNT = 8, time = xxx
keytrace.sno:30 stmt 29: &STFCOUNT = 9, time = xxx
keytrace.sno:31 stmt 30: &STFCOUNT = 10, time = xxx
keytrace.sno:32 stmt 31: &STFCOUNT = 11, time = xxx
keytrace.sno:34 stmt 33: &STCOUNT = 94, time = xxx
keytrace.sno:14 stmt 13: &STCOUNT = 95, time = xxx
LOOPS = 3 ****
keytrace.sno:15 stmt 14: &STCOUNT = 96, time = xxx
keytrace.sno:15 stmt 14: &ERRTYPE = 2, time = xxx
keytrace.sno:15 stmt 14: &STFCOUNT = 12, time = xxx
keytrace.sno:16 stmt 15: &STNO = 15, time = xxx
keytrace.sno:16 stmt 15: &STCOUNT = 97, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 98, time = xxx
28
keytrace.sno:17 stmt 16: &STCOUNT = 99, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 100, time = xxx
29
keytrace.sno:18 stmt 17: &STCOUNT = 101, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 102, time = xxx
30
keytrace.sno:19 stmt 18: &STCOUNT = 103, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 104, time = xxx
31
keytrace.sno:20 stmt 19: &STNO = 19, time = xxx
keytrace.sno:20 stmt 19: &STCOUNT = 105, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 106, time = xxx
32
keytrace.sno:21 stmt 20: &STCOUNT = 107, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 108, time = xxx
33
keytrace.sno:22 stmt 21: &STNO = 21, time = xxx
keytrace.sno:22 stmt 21: &STCOUNT = 109, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 110, time = xxx
34
keytrace.sno:23 stmt 22: &STCOUNT = 111, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 112, time = xxx
35
keytrace.sno:24 stmt 23: &STCOUNT = 113, time = xxx
keytrace.sno:37 stmt 36: &STCOUNT = 114, time = xxx
36
keytrace.sno:25 stmt 24: &STCOUNT = 115, time = xxx
keytrace.sno:25 stmt 24: &ERRTYPE = 14, time = xxx
keytrace.sno:25 stmt 24: &STFCOUNT = 13, time = xxx
keytrace.sno:26 stmt 25: &STCOUNT = 116, time = xxx
keytrace.sno:27 stmt 26: &STCOUNT = 117, time = xxx
keytrace.sno:28 stmt 27: &STCOUNT = 118, time = xxx
keytrace.sno:29 stmt 28: &STCOUNT = 119, time = xxx
keytrace.sno:29 stmt 28: &STFCOUNT = 14, time = xxx
keytrace.sno:30 stmt 29: &STCOUNT = 120, time = xxx
keytrace.sno:31 stmt 30: &STFCOUNT = 15, time = xxx
keytrace.sno:32 stmt 31: &STFCOUNT = 16, time = xxx
keytrace.sno:34 stmt 33: &STCOUNT = 124, time = xxx
keytrace.sno:34 stmt 33: &STFCOUNT = 17, time = xxx
keytrace.sno:35 stmt 34: &STCOUNT = 125, time = xxx
" (:stdout r)))))

(deftest csnobol4-label
  "Budne suite: label.sno"
  (let [r (run-with-timeout "-CASE 0
foo
BAR
	DEFINE(\"TEST(L)\")	:(ETEST)
TEST	OUTPUT = LABEL(L) L	:(RETURN)
ETEST

	TEST('foo')
	TEST('FOO')
	TEST('TEST')
	TEST('test')
	TEST('END')
	TEST('end')
	X = 'B'
	Y = 'AR'
	TEST(X Y)
	Y = 'AZ'
	TEST(X Z)

* turn case folding on
	&CASE = 1
	TEST('bar')

END
" 2000)]
    (is (= "foo
TEST
END
BAR
bar
" (:stdout r)))))

(deftest csnobol4-labelcode
  "Budne suite: labelcode.sno"
  (let [r (run-with-timeout "	CCC = LABELCODE('FOO')
	OUTPUT = CCC		:<CCC>
	OUTPUT = 'SNH'
FOO	OUTPUT = 'HERE'
END
" 2000)]
    (is (= "CODE
HERE
" (:stdout r)))))

(deftest csnobol4-len
  "Budne suite: len.sno"
  (let [r (run-with-timeout "*       LENGTH.SNO
*
*       Sample program from Chapter 5 of the Tutorial
*
LOOP    S = TRIM(INPUT)                 :F(END)
        OUTPUT = SIZE(S) ' ' S          :(LOOP)
END
hello
world
this is a longer line
" 2000)]
    (is (= "5 hello
5 world
21 this is a longer line
" (:stdout r)))))

(deftest csnobol4-lexcmp
  "Budne suite: lexcmp.sno"
  (let [r (run-with-timeout "* lexical compariston tests

*	return argument in quotes if it's a STRING
	define(\"q(x)t\")					:(eq)
q	t = datatype(x)
	q = leq(t,\"STRING\") \"'\" x \"'\"			:s(return)
	q = x						:(return)
eq

*	try given function, disply result
	define(\"try(fn,a,b)\")				:(etry)
try	output = apply(fn,a,b) fn '(' q(a) ',' q(b) ')'	:(return)
etry

*	given args, try all the functions, plus ident/differ
	define(\"check(a,b)\")				:(echeck)
check	try(.llt,a,b)
	try(.lle,a,b)
	try(.leq,a,b)	
	try(.lne,a,b)
	try(.lgt,a,b)
	try(.lge,a,b)
	try(.ident,a,b)
	try(.differ,a,b)				:(return)
echeck

* check this case manually -- q() depends on it!!
	leq(datatype('x'),\"STRING\")			:f(end)

	check('','')
	check('a','a')

	check('a','')
	check('','a')

	check('a','z')
	check('z','a')

	check(1,'1')
	check('1',1)

	check(1,'2')
	check('2',1)

	check(2,'1')
	check('1',2)
end
" 5000)]
    (is (= "LLE('','')
LEQ('','')
LGE('','')
IDENT('','')
LLE('a','a')
LEQ('a','a')
LGE('a','a')
IDENT('a','a')
LNE('a','')
LGT('a','')
LGE('a','')
DIFFER('a','')
LLT('','a')
LLE('','a')
LNE('','a')
DIFFER('','a')
LLT('a','z')
LLE('a','z')
LNE('a','z')
DIFFER('a','z')
LNE('z','a')
LGT('z','a')
LGE('z','a')
DIFFER('z','a')
LLE(1,'1')
LEQ(1,'1')
LGE(1,'1')
DIFFER(1,'1')
LLE('1',1)
LEQ('1',1)
LGE('1',1)
DIFFER('1',1)
LLT(1,'2')
LLE(1,'2')
LNE(1,'2')
DIFFER(1,'2')
LNE('2',1)
LGT('2',1)
LGE('2',1)
DIFFER('2',1)
LNE(2,'1')
LGT(2,'1')
LGE(2,'1')
DIFFER(2,'1')
LLT('1',2)
LLE('1',2)
LNE('1',2)
DIFFER('1',2)
" (:stdout r)))))

(deftest csnobol4-lgt
  "Budne suite: lgt.sno"
  (let [r (run-with-timeout "* Date: Thu, 19 Sep 1996 17:52:56 -0500
* From: Keith Waclena <keith@brick.lib.uchicago.edu>

        longer  = \"aa\"
        shorter = \"a\"
        output  = lgt(shorter, longer) \"wrong\"
        output  = lgt(longer, shorter) \"right\"
end
" 2000)]
    (is (= "right
" (:stdout r)))))

(deftest csnobol4-line
  "Budne suite: line.sno"
  (let [r (run-with-timeout "	output = &file ':' &line
-include \"line2.sno\"
	output = &file ':' &line
-line 1234 \"foo\"
	output = &file ':' &line
-line 2000 \"foo\"
	output = &file ':' &line
-include \"line2.sno \"
	output = &file ':' &line
* test multiple includes 02/22/02
-include \"line2.sno\"
	output = &file ':' &line
end
" 2000)]
    (is (= "line.sno:1
line.sno:1 (last)
line2.sno:2 (line2)
line.sno:3
foo:1234
foo:2000
foo:2000 (last)
line2.sno:2 (line2)
foo:2002
foo:2005
" (:stdout r)))))

(deftest csnobol4-loaderr
  "Budne suite: loaderr.sno"
  (let [r (run-with-timeout "* load leaves function undefined
	LOAD(\"ZZZ()\", \"xyzzy\")
	OUTPUT = FUNCTION(.ZZZ) \"ZZZ\"

* load leaves defined function in place
	DEFINE(\"QQQ(X)\")				:(EQQQ)
QQQ	QQQ = X						:(RETURN)
EQQQ

	OUTPUT = FUNCTION(.QQQ) QQQ(\"QQQ1\")
	LOAD(\"QQQ()\", \"xyzzy\")
	OUTPUT = FUNCTION(.QQQ) QQQ(\"QQQ2\")
END
" 2000)]
    (is (= "QQQ1
QQQ2
" (:stdout r)))))

(deftest csnobol4-local
  "Budne suite: local.sno"
  (let [r (run-with-timeout "	define(\"f(a,b)x\")				:(f_end)
f	output = \"f: \" x
	x = a
	g(b)
	output = \"f: \" x				:(return)
f_end

	define(\"g(a)x\")					:(g_end)
g	output = \"g: \" x
	x = a
	output = \"g: \" x				:(return)
g_end

	x = 0
	f('a','b')
	output = \"main: \" x
end
" 2000)]
    (is (= "f: 
g: 
g: b
f: a
main: 0
" (:stdout r)))))

(deftest csnobol4-longline
  "Budne suite: longline.sno"
  (let [r (run-with-timeout "	aaa = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
	output = size(aaa) + size(\"	aaa = ''\")
end



" 2000)]
    (is (= "1024
" (:stdout r)))))

(deftest csnobol4-longrec
  "Budne suite: longrec.sno — reads from INPUT (stdin below END)"
  (let [r (run-with-input "	output = input
	output = input
end"
                          "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
"
                          2000)]
    (is (= "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
" (:stdout r)))))

(deftest csnobol4-loop
  "Budne suite: loop.sno"
  (let [r (run-with-timeout "	LOOPS = 10000
	&STLIMIT = LOOPS * 2 + 10
	A = 0
LOOP	A = A + 1
	LT(A,LOOPS)		:S(LOOP)
END
" 2000)]
    (is (= "" (:stdout r)))))

(deftest csnobol4-match
  "Budne suite: match.sno"
  (let [r (run-with-timeout "	'AAA' LEN(1) . X
	OUTPUT = X
END
" 2000)]
    (is (= "A
" (:stdout r)))))

(deftest csnobol4-match2
  "Budne suite: match2.sno"
  (let [r (run-with-timeout "	'AAA' LEN(1) $ X
	OUTPUT = X
END
" 2000)]
    (is (= "A
" (:stdout r)))))

(deftest csnobol4-match3
  "Budne suite: match3.sno"
  (let [r (run-with-timeout "	X = \"Hello World?\"
LOOP	X '?' = '!'
	OUTPUT = X
END
" 2000)]
    (is (= "Hello World!
" (:stdout r)))))

(deftest csnobol4-match4
  "Budne suite: match4.sno"
  (let [r (run-with-timeout "	X = \"Hello World!\"
LOOP	X 'o' = 'x'		:S(LOOP)
	OUTPUT = X
END
" 2000)]
    (is (= "Hellx Wxrld!
" (:stdout r)))))

(deftest csnobol4-matchloop
  "Budne suite: matchloop.sno"
  (let [r (run-with-timeout "	&STLIMIT = 200000
	A = 0
LOOP	A = A + 1
	'FOO' 'BAR'
	LT(A,50000)		:S(LOOP)
END
" 2000)]
    (is (= "" (:stdout r)))))

(deftest csnobol4-maxint
  "Budne suite: maxint.sno"
  (let [r (run-with-timeout "	OUTPUT = GT(&MAXINT, 0) 'OK'
	OUTPUT = &MAXINT + &MAXINT
	OUTPUT = &MAXINT * 2
END
" 2000)]
    (is (= "OK
-2
-2
" (:stdout r)))))

(deftest csnobol4-noexec
  "Budne suite: noexec.sno"
  (let [r (run-with-timeout "-NOEXECUTE
	output = 'error'
end
" 2000)]
    (is (= "" (:stdout r)))))

(deftest csnobol4-nqueens
  "Budne suite: nqueens.sno"
  (let [r (run-with-timeout "* N queens problem, a string oriented version to
*   demonstrate the power of pattern matching.
* A numerically oriented version will run faster than this.
	N = 5
	NM1 = N - 1; NP1 = N + 1; NSZ = N * NP1; &STLIMIT = 10 ** 9; &ANCHOR = 1
	DEFINE('SOLVE(B)I')
* This pattern tests if the first queen attacks any of the others:
	TEST = BREAK('Q') 'Q' (ARBNO(LEN(N) '-') LEN(N) 'Q'
+	      | ARBNO(LEN(NP1) '-') LEN(NP1) 'Q'
+	      | ARBNO(LEN(NM1) '-') LEN(NM1) 'Q')
	P = LEN(NM1) . X LEN(1); L = 'Q' DUPL('-',NM1) ' '
	SOLVE()        :(END)
SOLVE	EQ(SIZE(B),NSZ) 	    :S(PRINT)
* Add another row with a queen:
	B = L B
LOOP	I = LT(I,N) I + 1 :F(RETURN)
	B TEST :S(NEXT)
	SOLVE(B)
* Try queen in next square:
NEXT	B P = '-' X :(LOOP)
PRINT	SOLUTION = SOLUTION + 1
	OUTPUT = 'Solution number ' SOLUTION ' is:'
PRTLOOP B LEN(NP1) . OUTPUT = :S(PRTLOOP)F(RETURN)
END
" 10000)]
    (is (= "Solution number 1 is:
---Q- 
-Q--- 
----Q 
--Q-- 
Q---- 
Solution number 2 is:
--Q-- 
----Q 
-Q--- 
---Q- 
Q---- 
Solution number 3 is:
----Q 
--Q-- 
Q---- 
---Q- 
-Q--- 
Solution number 4 is:
---Q- 
Q---- 
--Q-- 
----Q 
-Q--- 
Solution number 5 is:
----Q 
-Q--- 
---Q- 
Q---- 
--Q-- 
Solution number 6 is:
Q---- 
---Q- 
-Q--- 
----Q 
--Q-- 
Solution number 7 is:
-Q--- 
----Q 
--Q-- 
Q---- 
---Q- 
Solution number 8 is:
Q---- 
--Q-- 
----Q 
-Q--- 
---Q- 
Solution number 9 is:
--Q-- 
Q---- 
---Q- 
-Q--- 
----Q 
Solution number 10 is:
-Q--- 
---Q- 
Q---- 
--Q-- 
----Q 
" (:stdout r)))))

(deftest csnobol4-openi
  "Budne suite: openi.sno"
  (let [r (run-with-timeout "	INPUT(.FOO,10,,\"openi.sno\")

LOOP	OUTPUT = FOO			:S(LOOP)
END
" 2000)]
    (is (= "	INPUT(.FOO,10,,\"openi.sno\")

LOOP	OUTPUT = FOO			:S(LOOP)
END
" (:stdout r)))))

(deftest csnobol4-openo
  "Budne suite: openo.sno"
  (let [r (run-with-timeout "	OUTPUT(.FOO,10,,\"openo.tst\")

	FOO = \"hello\"
	FOO = \"world\"
END
" 2000)]
    (is (= "" (:stdout r)))))

(deftest csnobol4-openo2
  "Budne suite: openo2.sno"
  (let [r (run-with-timeout "	INPUT(.FOO,10,,\"openo.tst\")

LOOP	OUTPUT = FOO			:S(LOOP)
END
" 2000)]
    (is (= "hello
world
" (:stdout r)))))

(deftest csnobol4-ops
  "Budne suite: ops.sno"
  (let [r (run-with-timeout "* INSERT bug uncovered by PLUSOPS and GNAT tools

-PLUSOPS 1

* worked;
	a = 'hello world'
	a ' ' = 'x' 'y'
	output = a

	a = 'hello world'
	a ' ' = 1 + 2 / 3
	output = a

	a = 'hello world'
	a ' ' = 1 2 / 3
	output = a

	a = 'hello world'
	a ' ' = 1 + 2 * 3
	output = a

* failed;
	a = 'hello world'
	a ' ' = 'x' 'y' 'z'
	output = a

	a = 'hello world'
	a ' ' = 1 + 2 / 3 4
	output = a

	a = 'hello world'
	a ' ' = 1 + 2 3
	output = a

	a = 'hello world'
	a ' ' = 1 + 2 + 3
	output = a

	a = 'hello world'
	a ' ' = 1 * 2 * 3
	output = a
end
" 2000)]
    (is (= "helloxyworld
hello1world
hello10world
hello7world
helloxyzworld
hello14world
hello33world
hello6world
hello6world
" (:stdout r)))))

(deftest csnobol4-ord
  "Budne suite: ord.sno"
  (let [r (run-with-timeout "	DEFINE(\"TEST(C)O,SC,CC\")			:(ETEST)
TEST	C = 'A'
	O = ORD(C)
	SC = SUBSTR(C,1,1)
	CC = CHAR(O)
	OUTPUT = IDENT(CC,SC) 'OK'
							:(RETURN)
ETEST

* test for all characters in &ALPHABET??
	TEST('A')
	TEST('XYZ')
	TEST(CHAR(0))
	TEST(CHAR(255))
	TEST('')
	OUTPUT = ORD() 'SHOULD NOT HAPPEN'
END
" 2000)]
    (is (= "OK
OK
OK
OK
OK
" (:stdout r)))))

(deftest csnobol4-pad
  "Budne suite: pad.sno"
  (let [r (run-with-timeout "	output = rpad(\"hello\",10,\"!\")
	output = lpad(\"world\",10)
	output = rpad(\"long\",2)
end
" 2000)]
    (is (= "hello!!!!!
     world
long
" (:stdout r)))))

(deftest csnobol4-popen
  "Budne suite: popen.sno"
  (let [r (run-with-timeout "* popen input test

	input(.i, 99,, \"|echo hello\")
	output = i
end
" 2000)]
    (is (= "hello
" (:stdout r)))))

(deftest csnobol4-popen2
  "Budne suite: popen2.sno"
  (let [r (run-with-timeout "* popen output test
	output(.o, 99,, \"|cat >popen2.dat\")
	o = \"Hello World\"
	endfile(99)

	input(.i, 98,, \"popen2.dat\")
	output = i
	endfile(98)

	delete(\"popen2.dat\")
end
" 2000)]
    (is (= "Hello World
" (:stdout r)))))

(deftest csnobol4-pow
  "Budne suite: pow.sno"
  (let [r (run-with-timeout "	DEFINE(\"POW(X,Y)\")				:S(EPOW)
POW	OUTPUT = X \"^\" Y \" = \" X ^ Y			:S(RETURN)
	OUTPUT = X \"^\" Y \" FAILED\"			:(RETURN)
EPOW

*	POW(0,-1)

* mainbol allows this one, but spitbol doesn't?
*	POW(0,0)

	POW(0,1)

	POW(1,0)
	POW(2,0)
	POW(3,0)

	POW(1,1)
	POW(2,1)
	POW(3,1)

	POW(1,2)
	POW(2,2)
	POW(3,2)

* spitbol returns real?!
	POW(9,-1)

****************************************************************

*	POW(0.,-1)

* mainbol allows this one., but spitbol doesn't?
*	POW(0.,0)

	POW(0.,1)

	POW(1.,0)
	POW(2.,0)
	POW(3.,0)

	POW(1.,1)
	POW(2.,1)
	POW(3.,1)

	POW(1.,2)
	POW(2.,2)
	POW(3.,2)

	POW(10.,-1)
	POW(9,0.5)

	POW(4,0.5)
	POW(4,-0.5)

END
" 5000)]
    (is (= "0^1 = 0
1^0 = 1
2^0 = 1
3^0 = 1
1^1 = 1
2^1 = 2
3^1 = 3
1^2 = 1
2^2 = 4
3^2 = 9
9^-1 = 0
0.^1 = 0.
1.^0 = 1.
2.^0 = 1.
3.^0 = 1.
1.^1 = 1.
2.^1 = 2.
3.^1 = 3.
1.^2 = 1.
2.^2 = 4.
3.^2 = 9.
10.^-1 = 0.1
9^0.5 = 3.
4^0.5 = 2.
4^-0.5 = 0.5
" (:stdout r)))))

(deftest csnobol4-preload1
  "Budne suite: preload1.sno"
  (let [r (run-with-timeout "end
" 2000)]
    (is (= "aa
" (:stdout r)))))

(deftest csnobol4-preload2
  "Budne suite: preload2.sno"
  (let [r (run-with-timeout "end
" 2000)]
    (is (= "aa
bb
" (:stdout r)))))

(deftest csnobol4-preload3
  "Budne suite: preload3.sno"
  (let [r (run-with-timeout "end
" 2000)]
    (is (= "pa
pb
" (:stdout r)))))

(deftest csnobol4-preload4
  "Budne suite: preload4.sno"
  (let [r (run-with-timeout "end
" 2000)]
    (is (= "pa
pb
" (:stdout r)))))

(deftest csnobol4-punch
  "Budne suite: punch.sno"
  (let [r (run-with-timeout "	PUNCH = \"Hello World!\"
END
" 2000)]
    (is (= "" (:stdout r)))))

(deftest csnobol4-random
  "Budne suite: random.sno"
  (let [r (run-with-timeout "* without SRANDOMDEV() call, results should always be identical!
-include '../modules/random/random.sno'
	bins = array('0:99')
	n = sn = 10000
	t = 0
loop	ident(n,0)				:s(done)
	n = n - 1
	i = remdr(random(), 100)
	t = t + i
	bins[i] = bins[i] + 1			:(loop)

done	output = 'avg: ' ((t * 100) / sn)
	i = 0
dloop	ident(i,100)				:s(end)
	output = bins[i]
	i = i + 1				:(dloop)

end
" 5000)]
    (is (= "avg: 4975
109
92
108
96
114
98
90
95
102
86
99
91
93
85
87
104
99
96
111
89
101
96
91
117
95
103
104
113
128
104
98
95
101
96
93
97
103
95
117
109
108
86
107
90
88
90
112
99
73
101
102
113
93
94
114
97
86
123
99
104
120
93
85
85
105
93
95
113
97
108
95
98
114
98
95
91
109
97
75
91
106
108
100
98
105
94
125
102
89
104
115
108
110
100
113
81
113
108
96
89
" (:stdout r)))))

(deftest csnobol4-repl
  "Budne suite: repl.sno"
  (let [r (run-with-timeout "	UC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
	LC = 'abcdefghijklmnopqrstuvwxyz'
	&TRIM = 1
LOOP	OUTPUT = REPLACE(INPUT, UC, LC)			:S(LOOP)
END
ABCDEFGHIJKLMNOPQRSTUVWXYZ
FOUR SCORE AND SEVEN YEARS AGO
HELLO WORLD
" 2000)]
    (is (= "abcdefghijklmnopqrstuvwxyz
four score and seven years ago
hello world
" (:stdout r)))))

(deftest csnobol4-reverse
  "Budne suite: reverse.sno"
  (let [r (run-with-timeout "	output = reverse(\"dlrow olleh\")
	output = reverse(\"daed si luap\")
	output = reverse(reverse(&UCASE))
end
" 2000)]
    (is (= "hello world
paul is dead
ABCDEFGHIJKLMNOPQRSTUVWXYZ
" (:stdout r)))))

(deftest csnobol4-rewind1
  "Budne suite: rewind1.sno — reads from INPUT (stdin below END)"
  (let [r (run-with-input "copy	output = input		:s(copy)
	rewind(5)
copy2	output = input		:s(copy2)
end"
                          "hello
world
"
                          2000)]
    (is (= "hello
world
hello
world
" (:stdout r)))))

(deftest csnobol4-roman
  "Budne suite: roman.sno"
  (let [r (run-with-timeout "* ROMAN(N) - Convert integer N to Roman numeral form
*	N must be positive and less than 4000
*	An asterisk appears in the result if N >+ 4000
*	The function fails if N is not an integer

	DEFINE('ROMAN(N)UNITS')		:(ROMAN_END)

* Get rightmost digit to UNITS and remove it from N
* Return null result if argument is null
ROMAN	N RPOS(1) LEN(1) . UNITS =	:F(RETURN)

* Search for digit, replace with its Roman form.
* Return failing if not a digit.
	'0,1I,2II,3III,4IV,5V,6VI,7VII,8VIII,9IX,' UNITS
+		BREAK(',') . UNITS		:F(FRETURN)

* Convert rest of N and multiply by 10.  Propagate a
* failure return from recursive call back to caller
	ROMAN = REPLACE(ROMAN(N),'IVXLCDM','XLCDM**') UNITS
+			:S(RETURN) F(FRETURN)

ROMAN_END


	DEFINE(\"TEST(I,J)\")				:(TEST_END)
TEST	OUTPUT = I ' -> ' ROMAN(I)
	EQ(I,J)						:S(RETURN)
	I = I + 1					:(TEST)
TEST_END

	TEST(1,100)
	TEST(149,151)
	TEST(480,520)
	TEST(1900,2100)

END
" 5000)]
    (is (= "1 -> I
2 -> II
3 -> III
4 -> IV
5 -> V
6 -> VI
7 -> VII
8 -> VIII
9 -> IX
10 -> X
11 -> XI
12 -> XII
13 -> XIII
14 -> XIV
15 -> XV
16 -> XVI
17 -> XVII
18 -> XVIII
19 -> XIX
20 -> XX
21 -> XXI
22 -> XXII
23 -> XXIII
24 -> XXIV
25 -> XXV
26 -> XXVI
27 -> XXVII
28 -> XXVIII
29 -> XXIX
30 -> XXX
31 -> XXXI
32 -> XXXII
33 -> XXXIII
34 -> XXXIV
35 -> XXXV
36 -> XXXVI
37 -> XXXVII
38 -> XXXVIII
39 -> XXXIX
40 -> XL
41 -> XLI
42 -> XLII
43 -> XLIII
44 -> XLIV
45 -> XLV
46 -> XLVI
47 -> XLVII
48 -> XLVIII
49 -> XLIX
50 -> L
51 -> LI
52 -> LII
53 -> LIII
54 -> LIV
55 -> LV
56 -> LVI
57 -> LVII
58 -> LVIII
59 -> LIX
60 -> LX
61 -> LXI
62 -> LXII
63 -> LXIII
64 -> LXIV
65 -> LXV
66 -> LXVI
67 -> LXVII
68 -> LXVIII
69 -> LXIX
70 -> LXX
71 -> LXXI
72 -> LXXII
73 -> LXXIII
74 -> LXXIV
75 -> LXXV
76 -> LXXVI
77 -> LXXVII
78 -> LXXVIII
79 -> LXXIX
80 -> LXXX
81 -> LXXXI
82 -> LXXXII
83 -> LXXXIII
84 -> LXXXIV
85 -> LXXXV
86 -> LXXXVI
87 -> LXXXVII
88 -> LXXXVIII
89 -> LXXXIX
90 -> XC
91 -> XCI
92 -> XCII
93 -> XCIII
94 -> XCIV
95 -> XCV
96 -> XCVI
97 -> XCVII
98 -> XCVIII
99 -> XCIX
100 -> C
149 -> CXLIX
150 -> CL
151 -> CLI
480 -> CDLXXX
481 -> CDLXXXI
482 -> CDLXXXII
483 -> CDLXXXIII
484 -> CDLXXXIV
485 -> CDLXXXV
486 -> CDLXXXVI
487 -> CDLXXXVII
488 -> CDLXXXVIII
489 -> CDLXXXIX
490 -> CDXC
491 -> CDXCI
492 -> CDXCII
493 -> CDXCIII
494 -> CDXCIV
495 -> CDXCV
496 -> CDXCVI
497 -> CDXCVII
498 -> CDXCVIII
499 -> CDXCIX
500 -> D
501 -> DI
502 -> DII
503 -> DIII
504 -> DIV
505 -> DV
506 -> DVI
507 -> DVII
508 -> DVIII
509 -> DIX
510 -> DX
511 -> DXI
512 -> DXII
513 -> DXIII
514 -> DXIV
515 -> DXV
516 -> DXVI
517 -> DXVII
518 -> DXVIII
519 -> DXIX
520 -> DXX
1900 -> MCM
1901 -> MCMI
1902 -> MCMII
1903 -> MCMIII
1904 -> MCMIV
1905 -> MCMV
1906 -> MCMVI
1907 -> MCMVII
1908 -> MCMVIII
1909 -> MCMIX
1910 -> MCMX
1911 -> MCMXI
1912 -> MCMXII
1913 -> MCMXIII
1914 -> MCMXIV
1915 -> MCMXV
1916 -> MCMXVI
1917 -> MCMXVII
1918 -> MCMXVIII
1919 -> MCMXIX
1920 -> MCMXX
1921 -> MCMXXI
1922 -> MCMXXII
1923 -> MCMXXIII
1924 -> MCMXXIV
1925 -> MCMXXV
1926 -> MCMXXVI
1927 -> MCMXXVII
1928 -> MCMXXVIII
1929 -> MCMXXIX
1930 -> MCMXXX
1931 -> MCMXXXI
1932 -> MCMXXXII
1933 -> MCMXXXIII
1934 -> MCMXXXIV
1935 -> MCMXXXV
1936 -> MCMXXXVI
1937 -> MCMXXXVII
1938 -> MCMXXXVIII
1939 -> MCMXXXIX
1940 -> MCMXL
1941 -> MCMXLI
1942 -> MCMXLII
1943 -> MCMXLIII
1944 -> MCMXLIV
1945 -> MCMXLV
1946 -> MCMXLVI
1947 -> MCMXLVII
1948 -> MCMXLVIII
1949 -> MCMXLIX
1950 -> MCML
1951 -> MCMLI
1952 -> MCMLII
1953 -> MCMLIII
1954 -> MCMLIV
1955 -> MCMLV
1956 -> MCMLVI
1957 -> MCMLVII
1958 -> MCMLVIII
1959 -> MCMLIX
1960 -> MCMLX
1961 -> MCMLXI
1962 -> MCMLXII
1963 -> MCMLXIII
1964 -> MCMLXIV
1965 -> MCMLXV
1966 -> MCMLXVI
1967 -> MCMLXVII
1968 -> MCMLXVIII
1969 -> MCMLXIX
1970 -> MCMLXX
1971 -> MCMLXXI
1972 -> MCMLXXII
1973 -> MCMLXXIII
1974 -> MCMLXXIV
1975 -> MCMLXXV
1976 -> MCMLXXVI
1977 -> MCMLXXVII
1978 -> MCMLXXVIII
1979 -> MCMLXXIX
1980 -> MCMLXXX
1981 -> MCMLXXXI
1982 -> MCMLXXXII
1983 -> MCMLXXXIII
1984 -> MCMLXXXIV
1985 -> MCMLXXXV
1986 -> MCMLXXXVI
1987 -> MCMLXXXVII
1988 -> MCMLXXXVIII
1989 -> MCMLXXXIX
1990 -> MCMXC
1991 -> MCMXCI
1992 -> MCMXCII
1993 -> MCMXCIII
1994 -> MCMXCIV
1995 -> MCMXCV
1996 -> MCMXCVI
1997 -> MCMXCVII
1998 -> MCMXCVIII
1999 -> MCMXCIX
2000 -> MM
2001 -> MMI
2002 -> MMII
2003 -> MMIII
2004 -> MMIV
2005 -> MMV
2006 -> MMVI
2007 -> MMVII
2008 -> MMVIII
2009 -> MMIX
2010 -> MMX
2011 -> MMXI
2012 -> MMXII
2013 -> MMXIII
2014 -> MMXIV
2015 -> MMXV
2016 -> MMXVI
2017 -> MMXVII
2018 -> MMXVIII
2019 -> MMXIX
2020 -> MMXX
2021 -> MMXXI
2022 -> MMXXII
2023 -> MMXXIII
2024 -> MMXXIV
2025 -> MMXXV
2026 -> MMXXVI
2027 -> MMXXVII
2028 -> MMXXVIII
2029 -> MMXXIX
2030 -> MMXXX
2031 -> MMXXXI
2032 -> MMXXXII
2033 -> MMXXXIII
2034 -> MMXXXIV
2035 -> MMXXXV
2036 -> MMXXXVI
2037 -> MMXXXVII
2038 -> MMXXXVIII
2039 -> MMXXXIX
2040 -> MMXL
2041 -> MMXLI
2042 -> MMXLII
2043 -> MMXLIII
2044 -> MMXLIV
2045 -> MMXLV
2046 -> MMXLVI
2047 -> MMXLVII
2048 -> MMXLVIII
2049 -> MMXLIX
2050 -> MML
2051 -> MMLI
2052 -> MMLII
2053 -> MMLIII
2054 -> MMLIV
2055 -> MMLV
2056 -> MMLVI
2057 -> MMLVII
2058 -> MMLVIII
2059 -> MMLIX
2060 -> MMLX
2061 -> MMLXI
2062 -> MMLXII
2063 -> MMLXIII
2064 -> MMLXIV
2065 -> MMLXV
2066 -> MMLXVI
2067 -> MMLXVII
2068 -> MMLXVIII
2069 -> MMLXIX
2070 -> MMLXX
2071 -> MMLXXI
2072 -> MMLXXII
2073 -> MMLXXIII
2074 -> MMLXXIV
2075 -> MMLXXV
2076 -> MMLXXVI
2077 -> MMLXXVII
2078 -> MMLXXVIII
2079 -> MMLXXIX
2080 -> MMLXXX
2081 -> MMLXXXI
2082 -> MMLXXXII
2083 -> MMLXXXIII
2084 -> MMLXXXIV
2085 -> MMLXXXV
2086 -> MMLXXXVI
2087 -> MMLXXXVII
2088 -> MMLXXXVIII
2089 -> MMLXXXIX
2090 -> MMXC
2091 -> MMXCI
2092 -> MMXCII
2093 -> MMXCIII
2094 -> MMXCIV
2095 -> MMXCV
2096 -> MMXCVI
2097 -> MMXCVII
2098 -> MMXCVIII
2099 -> MMXCIX
2100 -> MMC
" (:stdout r)))))

(deftest csnobol4-scanerr
  "Budne suite: scanerr.sno"
  (let [r (run-with-timeout "*	test out RCERSX error processing for merged scanner
*	10/29/97

	&ERRLIMIT = 100

	X = -1
	\"FOO\" *TAB(X)					:S(END)
	OUTPUT = \"> \" &ERRTEXT

	X =
	\"FOO\" *ANY(X)					:S(END)
	OUTPUT = \"> \" &ERRTEXT

	X = 1.0
	\"FOO\" *LEN(X)					:S(END)
	OUTPUT = \"> \" &ERRTEXT

	\"FOO\" @1					:S(END)
	OUTPUT = \"> \" &ERRTEXT

	&MAXLNGTH = 9
	&ALPHABET REM . FOO				:S(END)
	&MAXLNGTH = 1024
	OUTPUT = \"> \" &ERRTEXT

**	gives bus error w/ &ERRLIMIT set!
*	&MAXLNGTH = 9
*	&ALPHABET REM $ FOO				:S(END)
*	&MAXLNGTH = 1024
*	OUTPUT = \"> \" &ERRTEXT

END
" 2000)]
    (is (= "> Negative number in illegal context
> Null string in illegal context
> Illegal data type
> Variable not present where required
> String overflow
" (:stdout r)))))

(deftest csnobol4-setexit
  "Budne suite: setexit.sno"
  (let [r (run-with-timeout "	&ERRLIMIT = 1000

* Arith Error:
	setexit(.cont)
	output = 'DIVIDE: ' (1 / 0)	:f(l1)
	output = 'CONTINUE returned success?'

* Illegal Data Type:
l1	output = \"@l1: last: \" &LASTFILE ':' &LASTLINE ' S' &LASTNO
	setexit(.scont)
	output = -code('LBL')		:s(l2)
	output = 'SCONTINUE returned failure?' :(END)

l2	output = \"back from scont: last \" &LASTFILE ':' &LASTLINE ' S' &LASTNO

* try before setting new handler; should be silently ignored
* Illegal Argument
	output = char(1000)
	output = \"HERE: \" &ERRTEXT

	setexit(.abrt)
* Length error
	output = len(-1)
	output = 'ABORT returned?'
	:(end)

cont	output = \"cont: \" &LASTFILE ':' &LASTLINE ' ' &ERRTEXT  ' S' &LASTNO
	:(CONTINUE)
	output = 'CONTINUE did not branch' :(END)

scont	output = \"scont: \" &LASTFILE ':' &LASTLINE ' ' &ERRTEXT ' S' &LASTNO
	:(SCONTINUE)
	output = 'SCONTINUE did not branch' :(END)

abrt	output = \"abrt: \" &LASTFILE ':' &LASTLINE ' ' &ERRTEXT  ' S' &LASTNO
	:(ABORT)
	output = 'ABORT did not branch'

end
" 2000)]
    (is (= "cont: setexit.sno:5 Error in arithmetic operation S4
@l1: last: setexit.sno:5 S4
scont: setexit.sno:11 Illegal data type S9
back from scont: last setexit.sno:11 S9
HERE: Illegal argument to primitive function
abrt: setexit.sno:23 Negative number in illegal context S18
setexit.sno:23: Error 14 in statement 18 at level 0
Negative number in illegal context
" (:stdout r)))))

(deftest csnobol4-setexit2
  "Budne suite: setexit2.sno"
  (let [r (run-with-timeout "* test SETEXIT trap on END

	:(begin)

xxx	output = &LASTFILE ':' &LASTLINE ' stmt ' &LASTNO
	output = &ERRTYPE ' \"' &ERRTEXT '\"'
	:(continue)

begin
	&errlimit = 10
	setexit(.xxx)
end
" 2000)]
    (is (= "setexit2.sno:11 stmt 10
0 \"\"
" (:stdout r)))))

(deftest csnobol4-setexit3
  "Budne suite: setexit3.sno"
  (let [r (run-with-timeout "* test SETEXIT return value

	OUTPUT = SETEXIT('FOO')
	OUTPUT = SETEXIT('BAR')
	OUTPUT = SETEXIT('')
	OUTPUT = SETEXIT('')
FOO
BAR
END
" 2000)]
    (is (= "
FOO
BAR

" (:stdout r)))))

(deftest csnobol4-setexit4
  "Budne suite: setexit4.sno"
  (let [r (run-with-timeout "	setexit(.f)
	&errlimit = 10
	&stlimit = 1000
	define(\"s()\")
	trace('STCOUNT','KEYWORD',,.s)
	&trace = 1000

	output = 'a'
	&z = 1
	output = 'b'
	output = 'c'	:(end)

f	output = 'err: ' &ERRTYPE ' (' &ERRTEXT ') @S' &LASTNO
	setexit(.f)
	&errlimit = 10
	:(scontinue)

s	output = 'S' &LASTNO	:(return)

end
" 2000)]
    (is (= "S8
a
S9
S13
err: 7 (Unknown keyword) @S9
S14
S15
S16
S10
b
S11
c
err: 0 () @S11
S14
S15
S16
" (:stdout r)))))

(deftest csnobol4-setexit5
  "Budne suite: setexit5.sno"
  (let [r (run-with-timeout "* test return/freturn from setexit handler, &LASTxxx
	&errlimit = 100

	define(\"f()\")		:(ef)
f	output = 'f'
	setexit(.fr)
	&z = 1
	output = 'oops1'
	:(return)

fr	output = &LASTFILE ':' &LASTLINE ' S' &LASTNO ' ' &ERRTEXT
	:(freturn)
ef

	define(\"g()\")		:(eg)
g	output = 'g'
	setexit(.gr)
	x = 1 / 0
	output = 'oops2'
	:(freturn)

gr	output = &LASTFILE ':' &LASTLINE ' S' &LASTNO ' ' &ERRTEXT
	:(return)
eg

	define(\"h()\")		:(eh)
h
	setexit(.hh)
	x = -code(\"foo\")
	output = \"h scontinue\"
	:(return)

hh	output = 'hh'
	output = &LASTFILE ':' &LASTLINE ' S' &LASTNO ' ' &ERRTYPE ' ' &ERRTEXT
	:(scontinue)
eh

****************

	f()			:f(fok)
	output = 'f succeeded?'

fok	g()			:s(gok)
	output = 'g failed?'

gok

end
" 5000)]
    (is (= "f
setexit5.sno:7 S6 Unknown keyword
g
setexit5.sno:18 S17 Error in arithmetic operation
" (:stdout r)))))

(deftest csnobol4-setexit6
  "Budne suite: setexit6.sno"
  (let [r (run-with-timeout "* setexit.sno w/ statment tracing (like sdb does)
* to make sure &LAST{FILE,LINE} are restored.
* should also test
	DEFINE('t(type,tag)')
	TRACE('STCOUNT','KEYWORD', 'tag', 't')
* change to zero to generate reference output:
	&TRACE = &STLIMIT = 10000

	&ERRLIMIT = 1000

* Arith Error:
	setexit(.cont)
	output = 'DIVIDE: ' (1 / 0)	:f(l1)
	output = 'CONTINUE returned success?'

* Illegal Data Type:
l1	output = \"@l1: last: \" &LASTFILE ':' &LASTLINE ' S' &LASTNO
	setexit(.scont)
	output = -code('LBL')		:s(l2)
	output = 'SCONTINUE returned failure?' :(END)

l2	output = \"back from scont: last \" &LASTFILE ':' &LASTLINE ' S' &LASTNO

* try before setting new handler; should be silently ignored
* Illegal Argument
	output = char(1000)
	output = \"HERE: \" &ERRTEXT

	setexit(.abrt)
* Length error
	output = len(-1)
	output = 'ABORT returned?'
	:(end)

cont	output = \"cont: \" &LASTFILE ':' &LASTLINE ' ' &ERRTEXT  ' S' &LASTNO
	:(CONTINUE)
	output = 'CONTINUE did not branch' :(END)

scont	output = \"scont: \" &LASTFILE ':' &LASTLINE ' ' &ERRTEXT ' S' &LASTNO
	:(SCONTINUE)
	output = 'SCONTINUE did not branch' :(END)

abrt	output = \"abrt: \" &LASTFILE ':' &LASTLINE ' ' &ERRTEXT  ' S' &LASTNO
	:(ABORT)
	output = 'ABORT did not branch'

* trace handler: must have a real statement
t
*	output = '@ ' &LASTFILE ':' &LASTLINE ' S' &LASTNO
	a = 1
	:(RETURN)
end
" 5000)]
    (is (= "cont: setexit6.sno:13 Error in arithmetic operation S8
@l1: last: setexit6.sno:13 S8
scont: setexit6.sno:19 Illegal data type S13
back from scont: last setexit6.sno:19 S13
HERE: Illegal argument to primitive function
abrt: setexit6.sno:31 Negative number in illegal context S22
setexit6.sno:31: Error 14 in statement 22 at level 0
Negative number in illegal context
" (:stdout r)))))

(deftest csnobol4-setexit7
  "Budne suite: setexit7.sno"
  (let [r (run-with-timeout "	&errlimit = &fatallimit = 100

* MUST BE FIRST!!!
* CNTERR 35	   \"Not in a SETEXIT handler\"
	setexit(.eh35)
	:(continue)
eh35	output = 'S' &LASTNO ' ' &ERRTEXT ' (' &ERRTYPE ')'

* not yet:
* MAIN1	 18	   Return from level zero
*	setexit(.eh18)
*	:(return)
*eh18	output = 'S' &LASTNO ' ' &ERRTEXT ' ' &ERRTYPE

* INTR5  19	   \"Failure during goto evaluation\"
	setexit(.eh19)
	define('freturn()')
	:<freturn()>
eh19	output = 'S' &LASTNO ' ' &ERRTEXT ' (' &ERRTYPE ')'

** EXEX  22	   
*	setexit(.eh22)
*	&STLIMIT = 2
*loop	:(loop)
*eh22	output = 'S' &LASTNO ' ' &ERRTEXT ' (' &ERRTYPE ')'

* INTR4  24	   \"Undefined or erroneous goto\"
	setexit(.eh24)
	:(foo)
eh24	output = 'S' &LASTNO ' ' &ERRTEXT ' (' &ERRTYPE ')'

end
" 2000)]
    (is (= "S4 Not in a SETEXIT handler (35)
S10 Failure during goto evaluation (19)
S15 Undefined or erroneous goto (24)
" (:stdout r)))))

(deftest csnobol4-space
  "Budne suite: space.sno"
  (let [r (run-with-timeout "	output = convert(\" \", .integer)
	output = convert(\" \", .real)
end
" 2000)]
    (is (= "0
0.
" (:stdout r)))))

(deftest csnobol4-space2
  "Budne suite: space2.sno"
  (let [r (run-with-timeout "-PLUSOPS 0
	output = convert(\" \", .integer)
	output = convert(\" \", .real)
end
" 2000)]
    (is (= "" (:stdout r)))))

(deftest csnobol4-spit
  "Budne suite: spit.sno"
  (let [r (run-with-timeout "*	test SPITBOL (-PLUSOPS) operators added in [PLB32]

	&trace = 100
	trace(.a, .value)
	trace(.b, .value)
	trace(.c, .value)

	c = 'yes'

-PLUSOPS 1

****************
*	embeded assign, tracing thereof
	a = (b = c '!!')
	output = a
	output = b

****************
*	exponents on reals (not an operator)
	output = 4.2e1
	output = 50.e-3

****************
*	nested table/array access
	tt = table()
	tt['blue'] = table()
	tt<'blue'><'green'> = 'cyan'
	output = tt<'blue'>['green']
*
****************
*	'numeric' conversion
	x = convert('17', .numeric)
	output = datatype(x) ': ' x

	x = convert('3.14159', .numeric)
	output = datatype(x) ': ' x

****************
*	alternation

	define(\"x()\")
	define(\"y()\")
	define(\"z()\")					:(ez)
x	output = 'TOO BIG'				:(freturn)
y	output = 'too small'				:(freturn)
z	output = 'Just Right'				:(return)
ez

	(x(), y(), z(), death())

****************
*	match operator
	a = b = 'yes!!'

	(a ? b . c)					:f(end)
	(a ? b $ c)					:f(end)

****************
*	tracing of match assign, replace
	a b . c
	a b $ c
	a ? b . c
	a ? b $ c

****************************************************************
* not yet working;

	p = 'yes'
* either of next two cause \"Variable not present where required\" err
	b p =
*	b 'yes' =

*	variable not present...
	(a ? '!!' = '')

****************
*	the very end
	output = 'all done'

end
" 5000)]
    (is (= "spit.sno:8 stmt 7: C = 'yes', time = xxx
spit.sno:14 stmt 10: B = 'yes!!', time = xxx
spit.sno:14 stmt 10: A = 'yes!!', time = xxx
yes!!
yes!!
42.
0.05
cyan
INTEGER: 17
REAL: 3.14159
TOO BIG
too small
Just Right
spit.sno:53 stmt 38: B = 'yes!!', time = xxx
spit.sno:53 stmt 38: A = 'yes!!', time = xxx
spit.sno:55 stmt 40: C = 'yes!!', time = xxx
spit.sno:56 stmt 41: C = 'yes!!', time = xxx
spit.sno:60 stmt 43: C = 'yes!!', time = xxx
spit.sno:61 stmt 44: C = 'yes!!', time = xxx
spit.sno:62 stmt 45: C = 'yes!!', time = xxx
spit.sno:63 stmt 46: C = 'yes!!', time = xxx
spit.sno:70 stmt 50: B = '!!', time = xxx
spit.sno:74 stmt 52: A = 'yes', time = xxx
all done
" (:stdout r)))))

(deftest csnobol4-str
  "Budne suite: str.sno"
  (let [r (run-with-timeout "	a = 'hello world'
	OUTPUT = a
END
" 2000)]
    (is (= "hello world
" (:stdout r)))))

(deftest csnobol4-substr
  "Budne suite: substr.sno"
  (let [r (run-with-timeout "	S = \"hello world\"
	M = SIZE(S)

	O = -1
LLOOP	L = -1
LOOP	OUTPUT = O ',' L ': \"' SUBSTR(S,O,L) '\"'
	L = L + 1
	LE(L,M)						:S(LOOP)
	O = O + 1
	LE(O,M)						:S(LLOOP)
END
" 5000)]
    (is (= "1,0: \"hello world\"
1,1: \"h\"
1,2: \"he\"
1,3: \"hel\"
1,4: \"hell\"
1,5: \"hello\"
1,6: \"hello \"
1,7: \"hello w\"
1,8: \"hello wo\"
1,9: \"hello wor\"
1,10: \"hello worl\"
1,11: \"hello world\"
2,0: \"ello world\"
2,1: \"e\"
2,2: \"el\"
2,3: \"ell\"
2,4: \"ello\"
2,5: \"ello \"
2,6: \"ello w\"
2,7: \"ello wo\"
2,8: \"ello wor\"
2,9: \"ello worl\"
2,10: \"ello world\"
3,0: \"llo world\"
3,1: \"l\"
3,2: \"ll\"
3,3: \"llo\"
3,4: \"llo \"
3,5: \"llo w\"
3,6: \"llo wo\"
3,7: \"llo wor\"
3,8: \"llo worl\"
3,9: \"llo world\"
4,0: \"lo world\"
4,1: \"l\"
4,2: \"lo\"
4,3: \"lo \"
4,4: \"lo w\"
4,5: \"lo wo\"
4,6: \"lo wor\"
4,7: \"lo worl\"
4,8: \"lo world\"
5,0: \"o world\"
5,1: \"o\"
5,2: \"o \"
5,3: \"o w\"
5,4: \"o wo\"
5,5: \"o wor\"
5,6: \"o worl\"
5,7: \"o world\"
6,0: \" world\"
6,1: \" \"
6,2: \" w\"
6,3: \" wo\"
6,4: \" wor\"
6,5: \" worl\"
6,6: \" world\"
7,0: \"world\"
7,1: \"w\"
7,2: \"wo\"
7,3: \"wor\"
7,4: \"worl\"
7,5: \"world\"
8,0: \"orld\"
8,1: \"o\"
8,2: \"or\"
8,3: \"orl\"
8,4: \"orld\"
9,0: \"rld\"
9,1: \"r\"
9,2: \"rl\"
9,3: \"rld\"
10,0: \"ld\"
10,1: \"l\"
10,2: \"ld\"
11,0: \"d\"
11,1: \"d\"
" (:stdout r)))))

(deftest csnobol4-sudoku
  "Budne suite: sudoku.sno — reads from INPUT (stdin below END)"
  (let [r (run-with-input "* Sudoku solving program written in the SNOBOL4 programming language
* Look at end for sample input

*       &STAT = 0
* cells are numbered 0 through 80
        a = ARRAY(\"0:80\")
        b = ARRAY(\"0:80\")
        DATA(\"c(index,z1,z2,z3)\")
* z1 is row link, z2 is column link, z3 is 3x3 block link
        DEFINE('prop(b)n,m,p,q,r,s,t')
        DEFINE('compute(b)did,n,mxl,mxs,mxn,tryc,ncopy,z')

* setup loop
        n = -1
sulp1   n = n + 1
        a<n> = c(n)
        LT(n,80) :S(sulp1)
* set up zone links
        n = -1
sulp2   n = n + 1
        r = n / 9
        c = REMDR(n,9)
* set row and column zones
        rp = REMDR(r + 1, 9)
        cp = REMDR(c + 1, 9)
        z1(a<n>) = a<r * 9 + cp>
        z2(a<n>) = a<rp * 9 + c>
* do little 3x3 block zones
* xr and xc are which block number (row colum)  0-2 0-2
* br and bc are which cell with in the little block (row col) 0-2 0-2
        xr = r / 3
        xc = c / 3
        br = REMDR(r,3)
        bc = REMDR(c,3)
        newbc = REMDR(bc + 1,3)
        newbr = REMDR(br + (bc + 1) / 3,3)
        z3(a<n>) = a<xr * 27 + newbr * 9 + xc * 3 + newbc>
        LT(n,80) :S(sulp2)

* Read input puzzle
        &TRIM = 1
        n = -1
sulp3   inline = INPUT :F(solve)
        IDENT(inline) :S(solve)
	IDENT(inline, 'STOP') :S(END)
        OUTPUT = inline
sulp4   inline LEN(1) . x = :F(sulp3)
        n = n + 1
        b<n> = '123456789'
        x NOTANY('123456789') :S(sulp4)
        b<n> = x :(sulp4)

* solve puzzle
solve   compute(b) :S(nextpuzzle)
        OUTPUT = 'No solution'
nextpuzzle OUTPUT =
        n = -1
findnextpuzzle inline = INPUT :F(END)
        IDENT(inline) :S(findnextpuzzle)
	IDENT(inline, 'STOP') :S(END)
        OUTPUT = inline
        :(sulp4)

* Descend trying different alternatives
compute did = 0
        DIFFER(prop(b)) :F(FRETURN)
        GT(did,0) :S(compute)
* scan to see if solved and find longest string
        n = 0
        mxl = 0
scnlp1  xs = b<n>
        GT(SIZE(xs),mxl) :F(scnlp1nx)
        mxs = xs
        mxl = SIZE(mxs)
        mxn = n
scnlp1nx n = n + 1 LT(n,80) :S(scnlp1)
        EQ(mxl,1)            :F(trylp)
* Print out the solution stored in array b
printout n = 0
printoutlp
        z = z b<n>
        n = n + 1 LT(n,80) :S(printoutlp)
        OUTPUT =
printoutlp3 z LEN(9) . OUTPUT = :S(printoutlp3)F(RETURN)

trylp   mxs LEN(1) . tryc = :F(FRETURN)
        ncopy = COPY(b)
        ncopy<mxn> = tryc
        compute(ncopy) :S(RETURN)
        :(trylp)

*propagate restrictions
prop    n = 0
proplp  s = b<n>
        m = 8
        p = index(z1(a<n>))
        q = index(z2(a<n>))
        r = index(z3(a<n>))

proplp2 EQ(SIZE(s),1) :F(propskip)

        t = b<p>
proprm1 t s = :F(proprm1n)
        did = did + 1
proprm1n
        b<p> = t DIFFER(t) :F(FRETURN)
        p = index(z1(a<p>))

        t = b<q>
proprm2 t s = :F(proprm2n)
        did = did + 1
proprm2n
        b<q> = t DIFFER(t) :F(FRETURN)
        q = index(z2(a<q>))

        t = b<r>
proprm3 t s = :F(proprm3n)
        did = did + 1
proprm3n
        b<r> = t DIFFER(t) :F(FRETURN)
        r = index(z3(a<r>))

        m = m - 1 GT(m,1) :S(proplp2)
propskip n = n + 1 LT(n,80) :S(proplp)
propdone prop = b :(RETURN)


END"
                          "---17--35
7--------
--2--3697
---3-7-82
---------
51-6-4---
8639--7--
--------9
49--56---

-7--8--6-
---32-5--
-95--7---
--34----6
--78-51--
1----29--
---7--38-
--9-53---
-3--6--5-


2-7----8-
--12---36
----6-5--
-----8--3
6-83-52-7
4--6-----
--5-7----
34---69--
-7----4-2

STOP

69-----2-
-----18--
-15-8--6-
----48--6
--95-67--
5--91----
-3--5-61-
--78-----
-4-----73

----5---1
---4--8-9
-9-6-1--4
7------1-
-61---73-
-4------8
6--7-2-9-
2-7--3---
9---1----

---9----8
---3--2-6
7-86---5-
------6-1
3-91-85-4
6-4------
-8---34-2
9-7--4---
1----6---

-8-7--1--
-62-9--45
5---649--
85-9---3-
9-38-26--
-2---3--8
--842---7
64--7--2-
--7----1-

98----1--
-6-----45
5---649--
8--9---3-
--38-26--
-2---3--8
--842---7
64-----2-
--7----19

"
                          30000)]
    (is (= "---17--35
7--------
--2--3697
---3-7-82
---------
51-6-4---
8639--7--
--------9
49--56---

684179235
739265814
152483697
946317582
327598146
518624973
863942751
275831469
491756328

-7--8--6-
---32-5--
-95--7---
--34----6
--78-51--
1----29--
---7--38-
--9-53---
-3--6--5-

372589461
814326597
695147238
953471826
267895143
148632975
526714389
789253614
431968752

2-7----8-
--12---36
----6-5--
-----8--3
6-83-52-7
4--6-----
--5-7----
34---69--
-7----4-2

267953184
591284736
834761529
759128643
618345297
423697815
985472361
342516978
176839452

" (:stdout r)))))

(deftest csnobol4-t
  "Budne suite: t.sno"
  (let [r (run-with-timeout "* test single letter trace type
	&trace = &stlimit = 1000
	trace('f', 'f')
	trace('xxx', 'v')
	trace('stcount', 'k')

	define(\"f(x)\")	:(ef)
f	xxx = x		:(return)
ef

	f(1)
	f(2)
	f(3)
	f(4)
end
" 2000)]
    (is (= "t.sno:7 stmt 6: &STCOUNT = 4, time = xxx
t.sno:11 stmt 10: &STCOUNT = 5, time = xxx
t.sno:11 stmt 10: level 0 call of F(1), time = xxx
t.sno:8 stmt 7: &STCOUNT = 6, time = xxx
t.sno:8 stmt 7: XXX = 1, time = xxx
t.sno:8 stmt 7: level 0 RETURN of F = '', time = xxx
t.sno:12 stmt 11: &STCOUNT = 7, time = xxx
t.sno:12 stmt 11: level 0 call of F(2), time = xxx
t.sno:8 stmt 7: &STCOUNT = 8, time = xxx
t.sno:8 stmt 7: XXX = 2, time = xxx
t.sno:8 stmt 7: level 0 RETURN of F = '', time = xxx
t.sno:13 stmt 12: &STCOUNT = 9, time = xxx
t.sno:13 stmt 12: level 0 call of F(3), time = xxx
t.sno:8 stmt 7: &STCOUNT = 10, time = xxx
t.sno:8 stmt 7: XXX = 3, time = xxx
t.sno:8 stmt 7: level 0 RETURN of F = '', time = xxx
t.sno:14 stmt 13: &STCOUNT = 11, time = xxx
t.sno:14 stmt 13: level 0 call of F(4), time = xxx
t.sno:8 stmt 7: &STCOUNT = 12, time = xxx
t.sno:8 stmt 7: XXX = 4, time = xxx
t.sno:8 stmt 7: level 0 RETURN of F = '', time = xxx
" (:stdout r)))))

(deftest csnobol4-tab
  "Budne suite: tab.sno"
  (let [r (run-with-timeout "*	tabulate -- test SORT/RSORT

*	&dump = 1
	&anchor = 0
	words = table(100)
	wpat = span(&ucase &lcase \"'\") . word

top	line = replace(input,&ucase,&lcase)		:f(eof)
wloop	line wpat =					:f(top)
	total = total + 1
	words<word> = words<word> + 1			:(wloop)

eof	output = \"total words: \" total

* dump words by use, most frequently used word first; test rsort of array
	arr = convert(words,\"array\")
*	output = prototype(arr)
	arr = rsort(arr,2)
	i = 1
dump	output = rpad(arr<i,2>,5) arr<i,1>		:f(edump)
	i = i + 1					:(dump)

* dump words alphabeticly; test sort of table
edump	arr = sort(words,1)
	i = 1
dump2	output = rpad(arr<i,1>,15) arr<i,2>		:f(end)
	i = i + 1					:(dump2)

end

Four score and seven years ago our fathers brought forth on this
continent, a new nation, conceived in Liberty, and dedicated to the
proposition that all men are created equal.

Now we are engaged in a great civil war, testing whether that nation,
or any nation so conceived and so dedicated, can long endure. We are
met on a great battlefield of that war. We have come to dedicate a
portion of that field, as a final resting place for those who here
gave their lives that that nation might live. It is altogether fitting
and proper that we should do this.

But, in a larger sense, we can not dedicate--we can not consecrate--
we can not hallow--this ground. The brave men, living and dead, who
struggled here, have consecrated it, far above our poor power to add
or detract. The world will little note, nor long remember what we say
here, but it can never forget what they did here. It is for us the
living, rather, to be dedicated here to the unfinished work which they
who fought here have thus far so nobly advanced. It is rather for us
to be here dedicated to the great task remaining before us--that from
these honored dead we take increased devotion to that cause for which
they gave the last full measure of devotion--that we here highly
resolve that these dead shall not have died in vain--that this nation,
under God, shall have a new birth of freedom--and that government of
the people, by the people, for the people, shall not perish from the
earth.
" 5000)]
    (is (= "total words: 271
13   that
11   the
10   we
8    to
8    here
7    a
6    and
5    nation
5    can
5    of
5    have
5    for
5    it
5    not
4    this
4    in
4    dedicated
3    are
3    great
3    so
3    who
3    is
3    dead
3    they
3    us
3    shall
3    people
2    our
2    on
2    new
2    conceived
2    men
2    war
2    or
2    long
2    dedicate
2    gave
2    but
2    living
2    far
2    what
2    rather
2    be
2    which
2    from
2    these
2    devotion
1    four
1    score
1    seven
1    years
1    ago
1    fathers
1    brought
1    forth
1    continent
1    liberty
1    proposition
1    all
1    created
1    equal
1    now
1    engaged
1    civil
1    testing
1    whether
1    any
1    endure
1    met
1    battlefield
1    come
1    portion
1    field
1    as
1    final
1    resting
1    place
1    those
1    their
1    lives
1    might
1    live
1    altogether
1    fitting
1    proper
1    should
1    do
1    larger
1    sense
1    consecrate
1    hallow
1    ground
1    brave
1    struggled
1    consecrated
1    above
1    poor
1    power
1    add
1    detract
1    world
1    will
1    little
1    note
1    nor
1    remember
1    say
1    never
1    forget
1    did
1    unfinished
1    work
1    fought
1    thus
1    nobly
1    advanced
1    task
1    remaining
1    before
1    honored
1    take
1    increased
1    cause
1    last
1    full
1    measure
1    highly
1    resolve
1    died
1    vain
1    under
1    god
1    birth
1    freedom
1    government
1    by
1    perish
1    earth
a              7
above          1
add            1
advanced       1
ago            1
all            1
altogether     1
and            6
any            1
are            3
as             1
battlefield    1
be             2
before         1
birth          1
brave          1
brought        1
but            2
by             1
can            5
cause          1
civil          1
come           1
conceived      2
consecrate     1
consecrated    1
continent      1
created        1
dead           3
dedicate       2
dedicated      4
detract        1
devotion       2
did            1
died           1
do             1
earth          1
endure         1
engaged        1
equal          1
far            2
fathers        1
field          1
final          1
fitting        1
for            5
forget         1
forth          1
fought         1
four           1
freedom        1
from           2
full           1
gave           2
god            1
government     1
great          3
ground         1
hallow         1
have           5
here           8
highly         1
honored        1
in             4
increased      1
is             3
it             5
larger         1
last           1
liberty        1
little         1
live           1
lives          1
living         2
long           2
measure        1
men            2
met            1
might          1
nation         5
never          1
new            2
nobly          1
nor            1
not            5
note           1
now            1
of             5
on             2
or             2
our            2
people         3
perish         1
place          1
poor           1
portion        1
power          1
proper         1
proposition    1
rather         2
remaining      1
remember       1
resolve        1
resting        1
say            1
score          1
sense          1
seven          1
shall          3
should         1
so             3
struggled      1
take           1
task           1
testing        1
that           13
the            11
their          1
these          2
they           3
this           4
those          1
thus           1
to             8
under          1
unfinished     1
us             3
vain           1
war            2
we             10
what           2
whether        1
which          2
who            3
will           1
work           1
world          1
years          1
" (:stdout r)))))

(deftest csnobol4-trace1
  "Budne suite: trace1.sno"
  (let [r (run-with-timeout "        &TRACE = 10
        TRACE('foo')
        foo = 1
        FOO = 2
        foo = 3
        FOO = 4
END
" 2000)]
    (is (= "trace1.sno:3 stmt 3: FOO = 1, time = xxx
trace1.sno:4 stmt 4: FOO = 2, time = xxx
trace1.sno:5 stmt 5: FOO = 3, time = xxx
trace1.sno:6 stmt 6: FOO = 4, time = xxx
" (:stdout r)))))

(deftest csnobol4-trace2
  "Budne suite: trace2.sno"
  (let [r (run-with-timeout "-CASE 0
        &TRACE = 10
        TRACE('foo')
        foo = 1
        FOO = 2
        foo = 3
        FOO = 4
END
" 2000)]
    (is (= "trace2.sno:4 stmt 3: foo = 1, time = xxx
trace2.sno:6 stmt 5: foo = 3, time = xxx
" (:stdout r)))))

(deftest csnobol4-trfunc
  "Budne suite: trfunc.sno"
  (let [r (run-with-timeout "	DEFINE('A()')
	DEFINE('B()')
	DEFINE('C()')

	DEFINE('D()')
	DEFINE('E()')
	DEFINE('F()')

	TRACE(.A, \"F\")
	TRACE(.B, \"C\")
	TRACE(.C, \"R\")

	TRACE(.D, \"FUNCTION\")
	TRACE(.E, \"CALL\")
	TRACE(.F, \"RETURN\")

	&TRACE = 100
	A()
	B()
	C()
	D()
	E()
	F()
	:(END)

A	A = 1	:(RETURN)
B	B = 1	:(RETURN)
C	C = 1	:(RETURN)
D	D = 1	:(RETURN)
E	E = 1	:(RETURN)
F	F = 1	:(RETURN)

END
" 2000)]
    (is (= "trfunc.sno:18 stmt 18: level 0 call of A(), time = xxx
trfunc.sno:26 stmt 26: level 0 RETURN of A = 1, time = xxx
trfunc.sno:19 stmt 19: level 0 call of B(), time = xxx
trfunc.sno:28 stmt 28: level 0 RETURN of C = 1, time = xxx
trfunc.sno:21 stmt 21: level 0 call of D(), time = xxx
trfunc.sno:29 stmt 29: level 0 RETURN of D = 1, time = xxx
trfunc.sno:22 stmt 22: level 0 call of E(), time = xxx
trfunc.sno:31 stmt 31: level 0 RETURN of F = 1, time = xxx
" (:stdout r)))))

(deftest csnobol4-trim0
  "Budne suite: trim0.sno — reads from INPUT (stdin below END)"
  (let [r (run-with-input "* test lines with trailing spaces w/ trim off
* 5/25/94 -plb
	&trim = 0
top	line = input		:f(end)
loop	line ' ' = '@'		:s(loop)
	tab = '	'
loop2	line tab = '!'		:s(loop2)
	output = line		:(top)
end"
                          "this line has three trailing spaces   
    
        
                                                                             
                                                                              
                                                                               
	this line has a leading tab
this line has a trailing tab	
	
			
"
                          2000)]
    (is (= "this@line@has@three@trailing@spaces@@@
@@@@
@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!this@line@has@a@leading@tab
this@line@has@a@trailing@tab!
!
!!!
" (:stdout r)))))

(deftest csnobol4-trim1
  "Budne suite: trim1.sno — reads from INPUT (stdin below END)"
  (let [r (run-with-input "* test lines with trailing spaces w/ trim on
* 5/25/94 -plb
	&trim = 1
top	line = input		:f(end)
loop	line ' ' = '@'		:s(loop)
	tab = '	'
loop2	line tab = '!'		:s(loop2)
	output = line		:(top)
end"
                          "this line has three trailing spaces   
    
        
                                                                             
                                                                              
                                                                               
	this line has a leading tab
this line has a trailing tab	
	
			
"
                          2000)]
    (is (= "this@line@has@three@trailing@spaces





!this@line@has@a@leading@tab
this@line@has@a@trailing@tab


" (:stdout r)))))

(deftest csnobol4-uneval
  "Budne suite: uneval.sno"
  (let [r (run-with-timeout "	&ANCHOR = 1
	H = 'H'
	'HELLO' *H 'ELLO'				:F(END)
	OUTPUT = 'YES!!'
END
" 2000)]
    (is (= "YES!!
" (:stdout r)))))

(deftest csnobol4-uneval2
  "Budne suite: uneval2.sno — reads from INPUT (stdin below END)"
  (let [r (run-with-input "	A = \"'\"
	Q = '\"'

TOP	LINE = INPUT					:F(END)
	LINE (A | Q) $ QUOTE *BREAK(QUOTE) . STR *QUOTE	:F(TOP)
	OUTPUT = STR					:(TOP)
END"
                          "'HELLO HELLO'
\"mr ed\"
'hi hi
\"It's a small world\"
"
                          2000)]
    (is (= "HELLO HELLO
mr ed
It's a small world
" (:stdout r)))))

(deftest csnobol4-unsc
  "Budne suite: unsc.sno"
  (let [r (run-with-timeout "        STR = '12 x 23434 x 123'
        P = SPAN('0123456789')

	STR (*P $ OUTPUT) FAIL
END
" 2000)]
    (is (= "12
2
23434
3434
434
34
4
123
23
3
" (:stdout r)))))

(deftest csnobol4-update
  "Budne suite: update.sno"
  (let [r (run-with-timeout "	output(.bout, 10, \"B,U\", \"test.bin\")
*	recl will be stringified
	input(.bin, 10, 5)

	bout = \"hello\"
	bout = \"world\"
	bout = \"test1\"
	bout = \"test2\"

*	dump initial
	rewind(10)
	output = bin
	output = bin
	output = bin
	output = bin
	output = '-----'

** useful constants;
	SEEK_SET = 0
	SEEK_CUR = 1
	SEEK_END = 2

*	skip first
	set(10, 5, SEEK_SET)
	output = bin

*	last
	set(10, -5, SEEK_END)
	output = bin

*	last again
	set(10, -5, SEEK_CUR)
	output = bin

* modify 2nd to last, look at last
	set(10, 10, SEEK_SET)
	bout = \"XXXXX\"
	output = bin

* change first, output second
	rewind(10)
	bout = \"Hello\"
	output = bin

* change last
	set(10, -5, SEEK_END)
	bout = \"YYYYY\"

* change second, output third
	set(10, 5, SEEK_SET)
	bout = \"World\"
	output = bin

* dump them all
	output = \"-----\"
	rewind(10)
	output = bin
	output = bin
	output = bin
	output = bin

end
" 5000)]
    (is (= "hello
world
test1
test2
-----
world
test2
test2
test2
world
XXXXX
-----
Hello
World
XXXXX
YYYYY
" (:stdout r)))))

(deftest csnobol4-vdiffer
  "Budne suite: vdiffer.sno"
  (let [r (run-with-timeout "	OUTPUT = VDIFFER('A')
	OUTPUT = VDIFFER('A', 'B')
	OUTPUT = VDIFFER('B', 'B')
	OUTPUT = VDIFFER('A', 1)
	OUTPUT = VDIFFER(1, 'A')
END
" 2000)]
    (is (= "A
A
A
1
" (:stdout r)))))

(deftest csnobol4-words
  "Budne suite: words.sno"
  (let [r (run-with-timeout "*   WORDS.SNO -- word counting program
*
*	Sample program from Chapter 6 of the Tutorial
*
*   A word is defined to be a contiguous run of letters,
*   digits, apostrophe and hyphen.  This definition of
*   legal letters in a word can be altered for specialized
*   text.
*
*   If the file to be counted is TEXT.IN, run this program
*   by typing:
*	B>SNOBOL4 WORDS /I=TEXT
*
	&TRIM	=  1
	UCASE	= \"ABCDEFGHIJLKMNOPQRSTUVWXYZ\"
	LCASE	= \"abcdefghijlkmnopqrstuvwxyz\"
	WORD	=  \"'-\"  '0123456789' UCASE LCASE
	BP = BREAK(WORD)
	SP = SPAN(WORD)
	WPAT	=  BREAK(WORD) SPAN(WORD)

NEXTL	LINE	=  INPUT			:F(DONE)
*	OUTPUT	= '>' LINE
NEXTW
*	LINE WPAT =				:F(NEXTL)
	LINE BP =
	LINE SP =				:F(NEXTL)
*	OUTPUT	= '>>' LINE
	N	=  N + 1			:(NEXTW)

DONE	OUTPUT =  +N ' words'
END
Four-score and seven years ago
this
is
a
test
" 2000)]
    (is (= "9 words
" (:stdout r)))))

(deftest csnobol4-words1
  "Budne suite: words1.sno"
  (let [r (run-with-timeout "*   WORDS.SNO -- word counting program
*
*	Sample program from Chapter 6 of the Tutorial
*
*   A word is defined to be a contiguous run of letters,
*   digits, apostrophe and hyphen.  This definition of
*   legal letters in a word can be altered for specialized
*   text.
*
*   If the file to be counted is TEXT.IN, run this program
*   by typing:
*	B>SNOBOL4 WORDS /I=TEXT
*
	&TRIM	=  1
	UCASE	= \"ABCDEFGHIJLKMNOPQRSTUVWXYZ\"
	LCASE	= \"abcdefghijlkmnopqrstuvwxyz\"
	WORD	=  \"'-\"  '0123456789' UCASE LCASE
	WPAT	=  BREAK(WORD) SPAN(WORD)

NEXTL	LINE	=  INPUT			:F(DONE)
*	OUTPUT	= '>' LINE
NEXTW	LINE WPAT =				:F(NEXTL)
*	OUTPUT	= '>>' LINE
	N	=  N + 1			:(NEXTW)

DONE	OUTPUT =  +N ' words'
END
Four-score and seven years ago
this
is
a
test
" 2000)]
    (is (= "9 words
" (:stdout r)))))
