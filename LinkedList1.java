/*
Simple implementation of two-way linked list.
 */
public class LinkedList1<E> {
//helper class to create a Node instance
    private static class Node<E> {

        private E data;
        private Node prev;
        private Node next;

        public Node(E data, Node prev, Node next) { 
            this.data = data;
            this.prev = prev;
            this.next = next;
        }
    }

    private Node head;  //pointers to nodes
    private Node last;
    private int elements = 0;
//standard add method
    public void add(E data) { 
        if (head == null) {
            head = new Node(data, null, null);
            last = head;
            elements++;
        } else {
            Node temp = new Node(data, last, null);
            last.next = temp;
            last = temp;
            elements++;
        }
    }
//'ínverted' add method
    public void addInv(E data) { 
        if (head == null) {
            last = new Node(data, null, null);
            head = last;
            elements++;
        } else {
            Node temp = new Node(data, null, head);
            head.prev = temp;
            head = temp;
            elements++;
        }
    }
//add by index
    public void add(int index, E elem) { 
        if (elements == 0 || index < 0 || index >= elements) {
            outOfBounds();
        }
        Node prevN;
        if (index == 0) {
            prevN = new Node(elem, null, head);
            head = prevN;
            elements++;
        } else {
            Node nextN;
            if (index <= elements / 2) {
                prevN = head;
                for (int i = 0; i < index - 1; i++) {
                    prevN = prevN.next;
                }
                nextN = prevN;
                nextN = nextN.next;
                prevN.next = new Node(elem, prevN, nextN);
                nextN.prev = prevN.next;
                elements++;
            } else {

                nextN = last;
                for (int i = elements - 1; i > index; i--) {
                    nextN = nextN.prev;
                }
                prevN = nextN;
                prevN = prevN.prev;
                nextN.prev = new Node(elem, prevN, nextN);
                prevN.next = nextN.prev;
                elements++;

            }

        }
    }
//set the value of an element
    public void set(int index, E elem) { 
        if (elements == 0 || index < 0 || index >= elements) {
            outOfBounds();
        }
        if (index <= elements / 2) {
            Node temp = head;
            for (int i = 0; i < index; i++) {
                temp = temp.next;
            }
            temp.data = elem;
        } else {
            Node temp = last;
            for (int i = elements - 1; i > index; i--) {
                temp = temp.prev;
            }
            temp.data = elem;
        }

    }
//get the value of an element
    public E get(int index) {
        if (elements == 0 || index < 0 || index >= elements) {
            outOfBounds();
        }
        E elem = null;
        if (index <= elements / 2) {
            Node temp = head;
            for (int i = -1; i < index; i++) {
                elem = (E) temp.data;
                temp = temp.next;
            }
        } else {
            Node temp = last;
            for (int i = elements - 1; i >= index; i--) {
                elem = (E) temp.data;
                temp = temp.prev;
            }
        }

        return elem;
    }
//remove node
    public void remove(int index) {
        if (elements == 0 || index < 0 || index >= elements) {
            outOfBounds();
        }
        if (index == 0) {
            head = head.next;
            head.prev = null;
            elements--;
        } else {
            if (index <= elements / 2) {
                Node prevN = head;
                Node nextN;
                for (int i = 0; i < index - 1; i++) {
                    prevN = prevN.next;
                }
                nextN = prevN;
                for (int i = 0; i < 2; i++) {
                    nextN = nextN.next;
                }
                prevN.next = nextN;
                if (nextN != null) {
                    nextN.prev = prevN;
                }
                elements--;
            } else {
                Node nextN = last;
                Node prevN;
                for (int i = elements - 1; i > index + 1; i--) {
                    nextN = nextN.prev;
                }
                prevN = nextN;
                for (int i = 0; i < 2; i++) {
                    prevN = prevN.prev;
                }
                nextN.prev = prevN;
                elements--;

            }

        }
    }
//delete all nodes
    public void clear() {
        head = null;
        last = head;
        elements = 0;
    }
//check if list contains a specific element
    public boolean contains(E elem) {
        if (isEmpty()) {
            return false;
        }
        Node temp = head;
        for (int i = 0; i < elements; i++) {

            if (temp.data.equals(elem)) {
                return true;
            }
            temp = temp.next;
        }
        return false;
    }
//find the first index of a specific element
    public int indexOf(E elem) {
        if (isEmpty()) {
            return -1;
        }
        Node temp = head;
        for (int i = 0; i < elements; i++) {

            if (temp.data.equals(elem)) {
                return i;
            }
            temp = temp.next;
        }
        return -1;
    }
//find the first index of a specific element
    public int lastIndexOf(E elem) {
        if (isEmpty()) {
            return -1;
        }
        Node temp = last;
        int t = -1;
        for (int i = elements - 1; i > 0; i--) {

            if (temp.data.equals(elem)) {
                t = i;
            }
            temp = temp.prev;
        }
        return t;
    }
//determine the siz of the list
    public int size() {
        int size = elements;
        return size;
    }
//check if the list is empty
    public boolean isEmpty() {
        if (head != null) {
            return false;
        }
        return true;
    }
//carve out a block from the list
    public LinkedList1 subList(int beginIndex, int postEndIndex) {
        if (elements == 0 || beginIndex < 0 || postEndIndex >= elements) {
            outOfBounds();
        }
        LinkedList1 ll1 = new LinkedList1();
        if (beginIndex < elements / 2 && (postEndIndex - elements / 2) <= elements / 2 - beginIndex) {
            Node temp = head;
            for (int i = 0; i < beginIndex; i++) {
                temp = temp.next;
            }
            ll1.add(temp.data);
            for (int i = 0; i < postEndIndex - beginIndex; i++) {
                temp = temp.next;
                ll1.add(temp.data);
            }
        } else {
            Node temp = last;
            for (int i = elements - 1; i > postEndIndex; i--) {
                temp = temp.prev;
            }
            ll1.addInv(temp.data);
            for (int i = 0; i < postEndIndex - beginIndex; i++) {
                temp = temp.prev;
                ll1.addInv(temp.data);
            }
        }
        return ll1;
    }
//exception handling
    private void outOfBounds() {
        throw new IndexOutOfBoundsException("Specified index does not exist");
    }
}
