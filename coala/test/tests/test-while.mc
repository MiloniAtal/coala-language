int while_check(int x)
{
    int i = 0;
    while (i<3){
        x= x+1;
        i=i+1;
    }
    return x;
}

int main()
{
  print( while_check(10) );
  return 0;
}