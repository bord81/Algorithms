//Basic method for finding GCD (greatest common divisor) using Euclidean algorithm
public class Euclid_GCD {

    public static void main(String[] args) {
        int a = 107002;
        int b = 462;
        System.out.println(euclidBase(a, b));
    }

    public static int euclidBase(int a, int b) {
        if (a == 0) {
            return b;
        } else if (b == 0) {
            return a;
        }
        if (a == b) {
            return a;
        }
        int swap_a;
        int swap_b;
        int div;
        int rem;
        if (a > b) {
            swap_a = a;
            swap_b = b;
        } else {
            swap_a = b;
            swap_b = a;
        }
        while (true) {
            div = swap_a / swap_b;
            rem = swap_a % swap_b;
            if (rem == 0) {
                break;
            } else {
                swap_a = swap_b;
                swap_b = rem;
            }
        }
        return swap_b;
    }
}
