About the Project

This is a sample interactive calculator for rationals built using ML-Yacc and ML-Lex.

The calculator is defined by the files

  Rational_Calci_Assignment3.lex               (* defines lexer *)
  Rational_Calci_Assignment3.grm               (* defines grammar *)
  Rational_Calci_Assignment3_decimal.sml       (* defines driver function, Calc.parse, gives output to calculator in decimal form *)
  Rational_Calci_Assignment3_rational.sml      (* defines driver function, Calc.parse, gives output to calculator in rational form *)
  Calculator_rational.cm                       (* cm description file for rational output *)
  Calculator_decimal.cm                        (* cm description file for decimal output  *)


There are 2 options sml files for running the Calculator : 
1. rational.sml                 (* For Calculator with rational input in decimal form and output in p/q form*)
2. rational_decimal_ans.sml     (* For Calculator with rational input in decimal form and output in decimal form*)

To compile this example, type either of the 2 commands for the type of calculator you want to invoke
	- sudo sml rational.sml                             
  - sudo sml rational_decimal_ans.sml

in this directory.  

CM will invoke ml-lex and ml-yacc to process the
lexer specification calc.lex and the grammar specification calc.grm
respectively.  Then it will compile the resulting SML source files

  Rational_Calci_Assignment3.lex.sml
  Rational_Calci_Assignment3.grm.sig
  Rational_Calci_Assignment3.grm.sml

The end result of loading these files is a structure Calc containing a
top-level driver function named parse.  

  Calc.parse : unit -> unit

The calculator has been invoked through the rational.sml file by running the command -Calc.parse();

The calculator reads a sequence of expressions from the standard input and prints the value of each expression after reading the expression.
Expressions must be separated by semicolons.  An expression is not evaluated until the semicolon is encountered.  The calculator
terminates when an end-of-file is encountered. There is no attempt to fix input errors: a lexical error will cause exception LexError to be
raised, while a syntax error will cause ParseError to be raised.

NOTE: The CM description file sources.cm mentions the ml-yacc library
(ml-yacc-lib.cm). CM's search path should be configured so that this
library will be found.  This should normally be the case if SML/NJ is
properly installed.



Design Decision : 

1. The '+' symbol should only be used as a binary operator and not a unary operator.
2. Assuming that only rationals are fed into the Rational functions for example 1/0 is not a rational number also 1/~2 is not a valid rational
3. Throwing rat_error when the string input in fromDecimal does not match the decimal form specification

Grammars  : 

For all the grammers [a-b] is a shorthand for a|.|.|...|b where a and b are the ends of a range
for eg. [3-6] represents 3|4|5|6 (note 3 and 6 are both inclusive)


Grammar for decimal form 

G1 = < N, T , PR , Start> 
{
  Where N = {I,S,N,R,Initial_decimal}
  Where T = {"+","~","(" , ")" , 0,1,2,3,4,5,6,7,8,9, "." }
  where Production Rules are written below 
  and Start = Initial_decimal
}

I -> I digit | eps 
S -> "+" | "~" | eps 
N -> N digit | eps 
R -> R digit | digit
Initial_decimal -> SI.N(R)
digit -> [0-9]





Grammar for fraction form 
G2 = < N, T , PR , Start> 
{
  Where N = {U,S,P,R,Q,Initial_fraction}
  Where T = {"+","~","/" , 0,1,2,3,4,5,6,7,8,9,  }
  where Production Rules are written below 
  and Start = Initial_decimal
}
P -> SR 
R -> R digit | digit
S -> "+" | "~" | eps 
Q -> digit1 U 
U -> digit U | eps 
digit -> [0-9]
digit1 -> [1-9]
Initial_fraction -> P/Q 






Grammar for calculator expression 

G3 = < N, T , PR , Start> 
{
  Where N = {E1,E2,E3,E4, A1, A2, nice, ,Integer, identifier, constant}
  Where T = {"+","~","/","+", "-" , 0,1,2,3,4,5,6,7,8,9, small case latin letters (a,b,c..) }
  where Production Rules are written below 
  and Start = E1
}

E1 -> E2 | E1 + E2 | E1 - E2 
E2 -> E3 | E2 * E3 | E2 / E3 
E3 -> (E1) | E4 
E4 -> identifier | constant 
constant -> Integer | Rational
identifier -> A1 A2 
A1 -> [a-zA-Z]
A2 -> eps | A2 nice
nice -> [a-zA-Z0-9_]
Integer -> Integer digit | digit
digit -> [0-9]

I -> I digit | eps 
S -> "+" | "~" | eps 
N -> N digit | eps 
R -> R digit | digit
Rational -> SI.N(R)

Acknowledgements  : 
Code in Rational_Calci_Assignment3.lex, Rational_Calci_Assignment3.grm, Rational_Calci_Assignment3_decimal.sml 
and Rational_Calci_Assignment3_rational.sml is inspired from the official SML NJ documentation
The structure is also taken from the official SML NJ documentation
