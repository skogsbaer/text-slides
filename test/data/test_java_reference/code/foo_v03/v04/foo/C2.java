//Automatically extracted from test_java.md on 2022-09-29 06:09:28.671326 UTC


//[test_java.md:9]
// Comment added to the top


//[test_java.md:73]
package foo;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class C1 {
    void bar() { System.out.println("bar"); }
}
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
