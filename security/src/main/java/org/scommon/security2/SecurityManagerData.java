package org.scommon.security2;

public class SecurityManagerData {
  public final static ThreadLocal<Integer> FOO = new InheritableThreadLocal<Integer>() {
      @Override
      protected Integer childValue(Integer parentValue) {
          System.out.println("BLAH");
          return super.childValue(parentValue);
      }
  };
}
