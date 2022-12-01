---
title: Test the Java plugin
---

This is a test for the java plugin

The following will be added to the start of each snippet with the default key.

~~~java(place:"atStart")
// Comment added to the top
~~~

Class C1 does not compile without C2.

~~~java
import java.util.*;

class C1 {
    C2 f;
    List<String> g;
}
~~~

C2 is defined in the next snippet with `append:true`

~~~java(append:true)
class C2 {}
~~~

~~~java(append:true)
class OtherC2 {}
~~~

Another way of expressing the same is using `standalone:false`

~~~java(standalone:false)
class C3 {
    C4 f;
    List<String> g;
    C5 h;
}
~~~

and then

~~~java(standalone:false)
interface C4 {}
~~~

~~~java
class C5{}
~~~

The snippets above are all concatenated together. Two different .java files are generated:
one with C1 and C2, the other with C1, C2, C3, and C4.

If a snippet declares a package, no concatentation is done. So the following snippet
starts from scratch:

~~~java
package foo;
~~~

The file for this snippet is Main. The plugin can also search for a main class.

~~~java
package foo;

class C1 {} // main
class C2 {}
~~~

~~~java
package foo;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class C1 {
    void bar() {}
}
public class C2 {} // main
~~~

Methods foo and main are added to the main class, method bar to C1.

~~~java
int foo() { return 1; }
public static void main(String[] args) {
    C2 c = new C2();
    System.out.println(c.foo());
}
void bar() { System.out.println("bar"); }
~~~

We can replace a previously defined method (and add a new one):

~~~java
double foo() { return 2; }
void spam() {}
~~~

Methods are matched by name only, so the following method also replaces the one previously defined.

~~~java
double foo(int x) { return x + 1; }
void egg() {}
public static void main(String[] args) {
    C2 c = new C2();
    System.out.println(c.foo(1));
    c.spam();
    c.egg();
}
~~~

We can replace whole classes of the snippet:

~~~java
record C1(String name) {}
~~~

Or we can add new things:

~~~java
class C3 {
    List<Integer> list; // import java.util.* added by the very first snippet
}
~~~

We can also have statements at the toplevel. These are written to an auto-generated method:

~~~java(body:true)
C3 c = new C3();
System.out.println(c);
~~~

Using the attribute `method:true` adds a method to the artificial __CodeContainer class
(and not the main class).

~~~java(method:true)
public static void foo() {}
~~~

~~~java(method:true)
public static void bar() { foo(); }
~~~

We can also define tests:

~~~java(test:"blub")
assertEquals(1, 1);
~~~

Using the file argument, we can have a completely different branch of snippets.

~~~java(file:"alternative")
class C1{}
~~~

~~~java(file:"alternative")
class C2{}
~~~

clear:true starts from scratch

~~~java(file:"alternative", clear:true)
class C3{}
~~~

~~~java(file:"alternative2")
class C1{}
~~~

~~~java(file:"alternative2", mode:"showOnly")
class Foo { Bar f; } // type Bar does not exist
~~~

append:true appends to the nearest snippet of the same type

~~~java(file:"Append", method:true)
void foo() {
~~~

~~~java(file:"Append")
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class C1 {
  C2 f;
}
~~~

~~~java(file:"Append", append:true)
class C2 {}
~~~

~~~java(file:"Append", method:true, append:true)
}
~~~

~~~java(file:"Append", body:"blub")
int a = 2;
~~~

~~~java(file:"Append", body:true, standalone:false)
int x = 1;
~~~

~~~java(file:"Append", body:true)
int y = x + 5;
~~~

~~~java(file:"Append", test:"fooTest")
int a = 1;
~~~

~~~java(file:"Append", test:true, append:true)
int b = a + 2;
~~~

~~~java(file:"Append", body:"blub")
int b = a + 1;
~~~

With class:NAME we can specify explicitly to which class method is added.
Either, we do it at the snippet.

~~~java
package foo1;
public class D {
}
class C {
  static void bar() {
  }
}
~~~

~~~java(class:"C")
    public static void main(String[] args) {
        bar();
    }
~~~

~~~java(class:"C")
    public static void main(String[] args) {
        bar();
        bar();
    }
~~~

Or globally:

~~~java(class:"C")
package foo2;
class C {
    void bar() {
    }
}
public class D {
}
~~~

~~~java
void foo() { bar(); }
~~~
