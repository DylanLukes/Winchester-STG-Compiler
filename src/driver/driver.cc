#include <ctime>
#include <cstdio>

#include <iostream>
#include "syntax/lexer.hh"

using namespace std;
using namespace wsc::lex;

int main (int argc, const char * argv[])
{
  clock_t tic = clock(), toc;

  if (argc < 2) {
    printf("Error: Missing parameter.\nUsage: wsc <filename>\n");
    exit(1);
  }

  Lexer l(argv[1]);
  l.lex();

  cout << endl;

  toc = clock();

  printf("Finished in %fms\n", ((double)(toc - tic)) / CLOCKS_PER_SEC * 1000);

  return 0;
}

