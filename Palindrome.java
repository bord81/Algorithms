/*

This program takes a string as an argument and checks whether it is a palindrome.

 */
public class Palindrome {

    private boolean b = true;
    private char[] c1;
    private char[] c2;
    private int count = 0;
    private String s;

    public boolean palindrome(String text) {
        s = text.toLowerCase().replaceAll("\\s", "").replaceAll("\\W", "");
        count = s.length();
        c1 = new char[count];
        c2 = new char[count];
        for (int i = 0; i < count; i++) {
            c1[i] = s.charAt(i);
            c2[i] = c1[i];
        }
        count--;
        for (int i = 0; i < (count / 2)+1; i++) {
            if (c1[i] != c2[count - i]) {
                b = false;
            }
        }
        return b;
    }
}
