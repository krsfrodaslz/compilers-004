
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class A {
i:IO <- new IO;
foo(a:Int):Int {
a<-2
};
};

class B inherits A {
b:Int <- new Int;
e:Int <- foo(b);
boo(a:Int, c:Int):Int {
a+b+c
};
};

class Main {
  main():Int { 0 };
};

