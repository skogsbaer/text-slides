//Automatically extracted from test_java.md on 2022-09-27 19:49:10.436604 UTC


//[test_java.md:9]
// Comment added to the top


//[test_java.md:15]
import java.util.*;

class C1 {
    C2 f;
    List<String> g;
}


//[test_java.md:26]
class C2 {}


//[test_java.md:32]
class C3 {
    C4 f;
    List<String> g;
}


//[test_java.md:41]
class C4 {}


//[test_java.md:51]
package foo;


//[test_java.md:57]
package foo;

class C1 {} // main
class C2 {}


//[test_java.md:64]
package foo;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class C1 {
    void bar() {}
}
public class C2 {} // main


//[test_java.md:80]
int foo() { return 1; }
public static void main(String[] args) {
    C2 c = new C2();
    System.out.println(c.foo());
}


//[test_java.md:90]
double foo() { return 2; }
void spam() {}


//[test_java.md:97]
double foo(int x) { return x + 1; }
void egg() {}
public static void main(String[] args) {
    C2 c = new C2();
    System.out.println(c.foo(1));
    c.spam();
    c.egg();
}


//[test_java.md:110]
record C1(String name) {}


//[test_java.md:116]
class C3 {
    List<Integer> list; // import java.util.* added by the very first snippet
}


//[test_java.md:124]
C3 c = new C3();
System.out.println(c);


//[test_java.md:132]
public static void foo() {}


//[test_java.md:136]
public static void bar() { foo(); }


//[test_java.md:142]
assertEquals(1, 1);


//[test_java.md:148]
class C1{}


//[test_java.md:152]
class C2{}


//[test_java.md:158]
class C3{}

class Foo { Bar f; } // type Bar does not exist
