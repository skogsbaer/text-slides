//Automatically extracted from test_java.md on 2022-09-29 06:08:11.762175 UTC


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
double foo() { return 2; }


public static void main(String[] args) {
    C2 c = new C2();
    System.out.println(c.foo());
}

void spam() {}
} // main
