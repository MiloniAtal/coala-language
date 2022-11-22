int foo(int a, int b, int c) { }

void bar(int a, int b, int a) {} /* Error: duplicate formal a in bar */

int main()
{
  return 0;
}
