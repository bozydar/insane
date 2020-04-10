#### TODO ####

* Generate embedded functions for n-ary built-ins - DONE
* Autogenerate curryable for multiparameter functions - DONE
  * Introduce many parameters functions - DONE
* Allow for calling functions declared in the let/and - DONE
  ```
  let flip = fun a => 
    flop < a > -1 > add 
  and
  let flop = fun a => 
    if a > 0 > neq 
    then flip < print < a 
    else a
  in 
    10 > flip 
  ```
* Clone only when needed - DONE
* Add line information to Err - DONE

* Show parsing errors in a sane way - DONE (showing pest error but doesn't matter for now)
* Add line/column information on the parse level 
  * Looks like what pest have to offer is rather internal and line/column
    is not accessible from Pair directly but `pest-2.1.2/src/position.rs:135` looks promising as copy/paste
* REPL
  * hash to enter a command:
    * #help - show shortcuts of editor
    * #edit - open editor and then execute the code
    * #reset-scope - clean scope
    * #show-scope - list all the declared identifiers
  * other text goes to executer BUT the scope ramains
  * "end" finishes "multiline" readline mode
* left side application
  * automatic function composition is not possible without types because it is
    hard to determine if function is an argument or it should be autoapplied.
* zero-arity functions 
* modules/namespaces
  * Rule::file returns many Expr which are added to the stack
  * introduce public/private
  ```
  module iter with map, count 
  def map = fun ...
  def count = fun ...
  def each = fun ...
  ``` 
* tails recursion optimization
  * if the last instruction of function is binding itself -> replace the argument on the stack instead of pushing a new one (and replace all the captured values)
  * if the last instruction of function is binding to another -> replace itself with the called function on the AST and remove parameter?
* struct 
* pattern matching
* types
  * let map : (Int -> Int) -> \[Int\] -> \[Int\] 
  * let map : (Int>Int>Fn) > (Int>List) > (Int>List) > Fn = fun fn list => ..
  