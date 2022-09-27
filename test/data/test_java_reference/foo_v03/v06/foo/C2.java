//Automatically extracted from test_java.md on 2022-09-27 19:49:10.436604 UTC


//[test_java.md:9]
// Comment added to the top


//[test_java.md:64]
package foo;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;


public class C2 {    
//[test_java.md:80]



    
//[test_java.md:90]


void spam() {}
    
//[test_java.md:97]
double foo(int x) { return x + 1; }

public static void main(String[] args) {
    C2 c = new C2();
    System.out.println(c.foo(1));
    c.spam();
    c.egg();
}

void egg() {}
} // main


//[test_java.md:110]
record C1(String name) {}

//[test_java.md:116]
class C3 {
    List<Integer> list; // import java.util.* added by the very first snippet
}

class __CodeContainer {

//[test_java.md:132]
public static void foo() {}




//[test_java.md:136]
public static void bar() { foo(); }



public static void __body_930919905fe2c68aeffe305fdcc74fcc() throws Exception {

//[test_java.md:124]
C3 c = new C3();
System.out.println(c);


}

@Test public void blub() throws Exception {

//[test_java.md:142]
assertEquals(1, 1);


}
}