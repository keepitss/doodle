// Problem:
//   fully implement echo's string expansion in a simple text editor. Example outputs:
//   abc -> abc
//   {a,b}{c,d} -> ac ad bc bd
//   {a,b}{c,g{e,m}}p{q,r} -> acpq acpr agepq agepr agmpq agmpr bcpq bcpr bgepq bgepr bgmpq bgmpr
//
// Solution:
//  We solve it two steps:
//    1. parsing the string into an expression tree, then
//    2. traversing the tree and generating the expansions.
//
//  There are three possible types of nodes in the expression tree:
//    1. A raw string node representing strings such as 'abc'
//    2. A choices node representing choices such as '{a,b}', '{c,d}'
//    3. A node representing concatenation of values from child nodes.
//
//  The tricky parts on the node tree are:
//    1. The child nodes of a choices node or concatenation node can be any of the three types of nodes
//    2. There can be arbitrary levels of nodes (nested expressions).
//
//  Every choices node requires backtrack. The same expansion of the suffix string following a choices expression is needed
//  for every choice. We tackle this backtrack problem by supplying a continuation to the code which process a choices node.
//  The continuation contains info about the node(s) following the current choices node. So after getting a choice, we can
//  continue to generate expansion of the nodes (represents suffix string of the choices string expression) following it.
//  
//  A new continuation is created for every choices node which is followed by some other node. Note a continuation tracks
//  only nodes at the same level as the choices node. Each continuation has a link to the existing continuation, which is
//  used when the current continuation is exhausted.
//

import java.util.List;
import java.util.Objects;
import java.util.ArrayList;


public class ExpEchoVar {
	// Base class representing a component of an expandable expression
	class VarExpr {
		public VarExpr() {}
	}

	// Raw string value
	class StrExpr extends VarExpr {
		public StrExpr(String s) {
			_RawString = s;
		}

		String getStr() {
			return _RawString;
		}

		protected String _RawString;
		
		@Override
		public String toString() {
			return _RawString;
		}
	}

	// A list of value options
	class ChoiceExpr extends VarExpr {
		public ChoiceExpr(List<VarExpr> choices) {
			_Choices = (choices == null) ? new ArrayList<VarExpr>() : choices;
		}

		public List<VarExpr> getChoices() {
			return _Choices;
		}

		protected List<VarExpr> _Choices;

		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			sb.append('{');
			for (VarExpr e : _Choices) {
				if (sb.length() == 1)
					sb.append(e.toString());
				else
					sb.append("," + e.toString());
			}
			sb.append('}');
			return sb.toString();
		}
	}

	// Its comprising expressions will concatenated to form an output.
	// it's an recursive structure like an expression tree
	class ConcatExpr extends VarExpr {
		public ConcatExpr(List<VarExpr> exprList) {
			_ExprList = (exprList == null) ? new ArrayList<VarExpr>() : exprList;
		}

		public List<VarExpr> getExprs() {
			return _ExprList;
		}

		protected List<VarExpr> _ExprList;

		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			for (VarExpr e : _ExprList) {
				sb.append(e.toString());
			}
			return sb.toString();
		}
	}

	private int adjustDepth(char c, int depth) {
		if (c == '{')
			depth += 1;
		else if (c == '}')
			depth -= 1;

		return depth;
	}

	// Build an express tree
	public VarExpr parseExpr(String str, int curIdx, int endIdx) {
		if (curIdx > endIdx)
			throw new ArithmeticException(String.format("%d,%d", curIdx, endIdx));
		else if (curIdx == endIdx)
			return new StrExpr(new String());

		ConcatExpr ccExpr = new ConcatExpr(null);

		while (curIdx < endIdx)
		{
			if (str.charAt(curIdx) != '{') {
				//
				// Scan raw string
				//
				int startIdx = curIdx;
				while (curIdx < endIdx && str.charAt(curIdx) != '{') {
					curIdx += 1;
				}

				// Add a raw string
				ccExpr.getExprs().add(new StrExpr(str.substring(startIdx, curIdx)));
			}
			else {
				// Find '{'
				if (curIdx < endIdx) {
					//
					// Find and parse all choices at this level
					//
					curIdx += 1; // Move past '{'
					int depth = 0; // level of nested '{'
					int curChoiceStart = curIdx; // Start of current choice

					ChoiceExpr coExpr = new ChoiceExpr(null);

					// find the matching '}'
					for (;
							curIdx < endIdx && (str.charAt(curIdx) != '}' || depth > 0);
							curIdx++)
					{
						depth = adjustDepth(str.charAt(curIdx), depth);

						// Find a choice?
						if (depth == 0 && str.charAt(curIdx) == ',') {
							// process the expression recursively
							VarExpr ve = parseExpr(str, curChoiceStart, curIdx);
							coExpr.getChoices().add(ve);

							curChoiceStart = curIdx + 1; // Move to next choice
						}
					}

					// Add last choice
					VarExpr lastChoice = parseExpr(str, curChoiceStart, curIdx);
					coExpr.getChoices().add(lastChoice);

					// if there's only one choice, return it directly, otherwise add the choices.
					if (coExpr.getChoices().size() == 1) {
						ccExpr.getExprs().add(coExpr.getChoices().get(0));
					}
					else {
						ccExpr.getExprs().add(coExpr);
					}

					// Either found matching '}' or reach the end of string?
					if (curIdx < endIdx && str.charAt(curIdx) == '}') {
						curIdx += 1;
					}
				}
			}
		} // while

		// If there's only one nest content, return that directly
		if (ccExpr.getExprs().size() == 1) {
			return ccExpr.getExprs().get(0);
		}
		else {
			return ccExpr;
		}
	}

	class Continuation {
		public Continuation(Continuation oldCont, List<VarExpr> exprs, int index) {
			OldCont = oldCont;
			Exprs = exprs;
			Index = index;
		}
		
		public VarExpr GetCurExpr() { return Exprs.get(Index); }

		public Continuation OldCont; // Continuation to call after currently level of expression are all consumes
		public List<VarExpr> Exprs; // List of expressions
		public int Index; // Index of the next expression to print
	}
	
	private Continuation GetNextCont(Continuation c) {
		if (c.Index + 1 >= c.Exprs.size())
			return c.OldCont;
		else
			return new Continuation(c.OldCont, c.Exprs, c.Index+1);
	}
	
	public void ExpandExpr(String prefix, VarExpr expr, Continuation suffix, List<String> exps) {
		if (expr instanceof StrExpr) {
			StrExpr strExpr = (StrExpr)expr;
			prefix += strExpr.getStr();
			if (suffix != null)
				ExpandExpr(prefix, suffix.GetCurExpr(), GetNextCont(suffix), exps);
			else
				exps.add(prefix);
		}
		else if (expr instanceof ChoiceExpr) {
			ChoiceExpr coExpr = (ChoiceExpr)expr;
	
			if (coExpr.getChoices().size() > 0) {
				for (VarExpr e : coExpr.getChoices()) {
					ExpandExpr(prefix, e, suffix, exps);
				}
			}
			else if (suffix != null) {
				ExpandExpr(prefix, suffix.GetCurExpr(), GetNextCont(suffix), exps);
			}
			else {
				exps.add(prefix);
			}
		}
		else if (expr instanceof ConcatExpr) {
			ConcatExpr ccExpr = (ConcatExpr)expr;
			
			if (ccExpr.getExprs().size() > 0) {
				ExpandExpr(prefix, ccExpr.getExprs().get(0), ccExpr.getExprs().size() == 1 ? suffix : new Continuation(suffix, ccExpr.getExprs(), 1), exps);
			}
			else if (suffix != null) {
				ExpandExpr(prefix, suffix.GetCurExpr(), GetNextCont(suffix), exps);
			}
			else {
				exps.add(prefix);
			}
		}
	}

	public void TestExpansion(String str, String expected) {
		VarExpr ve = parseExpr(str, 0, str.length());
		
		List<String> exps = new ArrayList<String>();
		ExpandExpr("", ve, null, exps);
		
		String result = String.join(" ", exps);
		System.out.println(result);
		//System.out.println(expected);
		
		assert Objects.equals(result, expected) : "Expected: " + expected + " Actual: " + result;
	}

	public static void main(String[] args) {
		String expr1 = "abc";
		String result1 = "abc";
		
		String expr2 = "{a,b}{c,d}";
		String result2 = "ac ad bc bd";
		
		String expr3 = "{a,b}{c,g{e,m}}p{q,r}";
		String result3 = "acpq acpr agepq agepr agmpq agmpr bcpq bcpr bgepq bgepr bgmpq bgmpr";

		ExpEchoVar eev = new ExpEchoVar();
		eev.TestExpansion(expr1, result1);
		eev.TestExpansion(expr2, result2);
		eev.TestExpansion(expr3, result3);
	}
}


