/*
This is an example of binary search algorithm.

The binarySearch() method takes an Integer array and a value to be found and returns its index.

A returned index points to an element in a sorted version of incoming array.
*/
import java.util.Arrays;

public class BinarySearch {

    int len; //length of input array
    int base; //base for search
    int iter; //base iterations counter 
    int brut; //brute-force iterations counter
    int result; // index of a value in array

    public int binarySearch(Integer[] list, int value) {
        result = -1; 
        Arrays.sort(list);
        len = list.length;
        base = (int) Math.sqrt(len);
        iter = base;
        if(value<list[0]||value>list[len-1]){
            return result;
        }
        while (true) {
            if (iter >= len) {
                iter = len - 1;
            }
            if (list[iter].equals(value)) {
                result = iter;
                break;
            }
            if (list[iter] > value) {
                brut = iter - base;
                while (true) {
                    if (brut>iter){
                        result=-1;
                        break;
                    }
                    if (list[brut].equals(value)) {
                        result = brut;
                        break;
                    }
                    brut++;
                }
                break;
            }
            iter = iter + base;
        }
        return result;
    }
}