#### TODO ####

* Generate embedded functions for n-ary built-ins - DONE
* Autogenerate curryable for multiparameter functions
* Allow for calling functions declared in the same module
* Clone only when needed
* Add line information to Err
* REPL
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
  