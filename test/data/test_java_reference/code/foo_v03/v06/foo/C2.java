//Automatically extracted from test_java.md on 2022-09-29 06:12:27.250161 UTC


//[test_java.md:9]
// Comment added to the top


//[test_java.md:73]
package foo;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;



//[test_java.md:119]
record C1(String name) {}

public class C2 {

//[test_java.md:89]


//[test_java.md:99]


//[test_java.md:106]
double foo(int x) { return x + 1; }




public static void main(String[] args) {
    C2 c = new C2();
    System.out.println(c.foo(1));
    c.spam();
    c.egg();
}


void spam() {}

void egg() {}
} // main



//[test_java.md:125]
class C3 {
    List<Integer> list; // import java.util.* added by the very first snippet
}

class __CodeContainer {

//[test_java.md:141]
public static void foo() {}




//[test_java.md:145]
public static void bar() { foo(); }



public static void __body_1() throws Exception {

//[test_java.md:133]
C3 c = new C3();
System.out.println(c);


}

@Test public void blub() throws Exception {

//[test_java.md:151]
assertEquals(1, 1);


}
}