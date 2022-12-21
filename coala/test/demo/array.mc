void main(){
    prints("1. Array of Int of size 3");
    array<int, 3> I = [28, 1, 99];

    prints(" ");
    prints("Accessing the elements of this array");
    int i;
    for (i = 0; i < 3; i = i + 1) {
        print(I[i]);
    }

    prints(" ");
    prints("---------------------------------------");
    prints(" ");

    prints("2. Array of Bool of size 2");
    array<bool, 2> B = [true, false];

    prints(" ");
    prints("Accessing the elements of this array");
    int i;
    for (i = 0; i < 2; i = i + 1) {
        printb(B[i]);
    }
    return;
}
