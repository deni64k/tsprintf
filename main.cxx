#include "tsprintf.hxx"

using namespace tsprintf;
using namespace tsprintf::printf_literal;

int main(int argc, char *argv[]) {
  int i = 42;

  "integer is %d\n"_printf(i);
  // "float is %f\n"_printf(42);
  // "float is %f\n"_printf("42");

  tsprint("integer is %d\n"_lit, i);

  char const *fmt1 = "integer is %d\n";
  tsprint(fmt1, i);

  char const fmt2[] = "integer is %d\n";
  tsprint(fmt2, i);
  tsprint("integer is %d\n", i);
}
