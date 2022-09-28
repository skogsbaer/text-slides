//Automatically extracted from test_java.md on 2022-09-27 19:49:10.436604 UTC


//[test_java.md:9]
// Comment added to the top


//[test_java.md:64]
package foo;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class C1 {
    void bar() {}
}
public class C2 {    
//[test_java.md:80]
int foo() { return 1; }

public static void main(String[] args) {
    C2 c = new C2();
    System.out.println(c.foo());
}
} // main

