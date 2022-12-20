void main(){
    int i;
    prints("For Loop");
    
    for (i = 0 ; i < 5 ; i = i + 1) {
        print(i);
    }

    i = 5;
    prints(" ");
    prints("While Loop");
    
    while (i > 0) {
        print(i);
        i = i - 1;
    }
    prints(" ");
    prints("If-Else");
    if (true)
        prints("true");
    else
        prints("false");
}