This is a test for the java plugin

The following will be added to the start of each snippet with the default key.

~~~java(place:"atStart")
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
~~~

Class C1 does not compile without C2.

~~~java
class C1 {
    C2 f;
    List<String> g;
}
~~~

C2 is defined in the next snippet with `append:true`

~~~java(append:true)
class C2 {}
~~~

Another way of expressing the same is using `standalone:false`

~~~java(standalone:false)
class C3 {
    C4 f;
    List<String> g;
}
~~~

and then

~~~java
class C4 {}
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

class C1 {
    void bar() {}
}
public class C2 {} // main
~~~

The following two methods are added to the main class:

~~~java
int foo() { return 1; }
public static void main(String[] args) {
    C2 c = new C2();
    System.out.println(c.foo());
}
~~~

We can replace a previously defined method:

~~~java
double foo() { return 2; }
~~~

Methods are matched by name only, so the following method also replaces the one previously defined.

~~~java
double foo(int x) { return x + 1; }
~~~

We can also replace methods in classes that are not the main class:

~~~java
int bar(int x) { return x; }
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

~~~java(test:true)
public void testBlub() {
    assertEqual(1, 1);
}
~~~

Using the file argument, we can have a completely different branch of snippets.

~~~java(file:"alternative")
class C1{}
~~~

~~~java(file:"alternative")
class C2{}
~~~
