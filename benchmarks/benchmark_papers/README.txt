Executable Benchmark Papers
===========================

Examples showing the use of the system are in the
directory `Examples` (see also the README there).


Further files in this directory:

* currycode.sty: Definition of LaTeX macros to be included by
  `\usepackage{currycode}` into the LaTeX document containing
  Curry code to implement benchmarks.

* ExecuteBenchmarkPaper.curry: Implementation of the tool which extracts
  code snippets from a LaTeX documents, executes them, and creates a
  macro file containing the results of the code snippets to be
  included in the formatted document.

* Benchmarks.curry: Implementation of an embedded DSL to support
  the construction of benchmarks.

* BenchmarkGoodies.curry: Some goodies which might be helpful
  to format benchmark results, e.g., to translate them to LaTeX
  or produce graphics via gnuplot.
