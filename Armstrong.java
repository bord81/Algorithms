public class Armstrong {
    private static long[][] degree = new long[10][19];

    public static ArrayList<Long> getNumbers(long N) {
        long x1 = N;
        long start = 0L;
        long strt_incr = 0L;
        boolean sw = false;
        byte count = 19;
        ArrayList<Long> a_nums = new ArrayList<>();
        while (true) {
            byte[] array = new byte[19];
            byte position = 18;
            while (start > 0) {
                array[position--] = (byte) (start % 10);
                start = start / 10;
            }
            for (int i = 0; i < array.length; i++) {
                if (array[i] != 0 || sw) {
                    sw = true;
                } else {
                    count--;
                }
            }
            if (count < 2) {
                a_nums.add(strt_incr);
            }
            boolean is_incr = false;
            for (int i = array.length - count; i < array.length - 1; i++) {
                if (array[i] == array[i + 1] || array[i] < array[i + 1] || count == 1 || array[i + 1] == 0) {
                    is_incr = true;
                } else {
                    is_incr = false;
                    break;
                }
            }
            if (is_incr) {
                long check = 0L;
                for (int i = array.length - count; i < array.length; i++) {
                    if (degree[array[i]][count] == 0) {
                        degree[array[i]][count] = (long) Math.pow(array[i], count);
                    }
                    check += degree[array[i]][count];
                }
                boolean found = false;
                for (Long l : a_nums
                        ) {
                    if (l == check) {
                        found = true;
                        break;
                    }
                }

                if (!found && checkSelf(check))
                    a_nums.add(check);
            }
            sw = false;
            count = 19;
            if (strt_incr < x1 + 1) {
                strt_incr++;
                start = strt_incr;
            } else {
                break;
            }
        }
        return a_nums;
    }

    private static boolean checkSelf(long value) {
        byte[] array = new byte[19];
        long to_check = value;
        byte count = 19;
        byte position = 18;
        boolean sw = false;
        while (to_check > 0) {
            array[position--] = (byte) (to_check % 10);
            to_check = to_check / 10;
        }
        for (int i = 0; i < array.length; i++) {
            if (array[i] != 0 || sw) {
                sw = true;
            } else {
                count--;
            }
        }
        long check = 0L;
        for (int i = array.length - count; i < array.length; i++) {
            if (degree[array[i]][count] == 0) {
                degree[array[i]][count] = (long) Math.pow(array[i], count);
            }
            check += degree[array[i]][count];
        }
        return check == value;
    }
}
