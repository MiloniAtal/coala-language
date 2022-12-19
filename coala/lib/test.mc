int prec(int x, int y, int z)
{
  return x+y/z;
}

int main()
{
  print( prec(5, 20,10) );
  return 0;
}