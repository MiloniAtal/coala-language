int for_check(int x)
{
    int i;
    for (i=0; i<3; i=i+1){
        x= x+1;
    }
    return x;
}

int main()
{
  print( for_check(10) );
  return 0;
}