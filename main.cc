
#include "microParser.hpp"

using namespace std;

extern int yylex();
extern char* yytext;
extern FILE* yyin;
extern symbolTable_node * global_scope;

int main(int argc, char **argv)
{

  yyin = fopen(argv[1], "r");
  if (yyin == NULL) {
    cout << "file read failed\n";
    exit(1);
  }
  //yylex();
    int accept;
      accept = yyparse();
  
        if (accept == 0)
              cout << "Accepted" << endl;
           else
                cout << "Not Accepted" << endl;
  
                return 0;
                }
