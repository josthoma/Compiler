/* Header of bison file - describes GRAMMAR and TOKENS */
%{
  #include <iostream>
  #include <cassert>


  extern int yylex();
  extern int yylineno;
  extern char* yytext;

  void yyerror (const char *str) {
    //fprintf(stderr, "error: %s\n", str);
    //printf("Error Line %d token %s\n", yylineno,yytext);
  }

  int yywrap() {
    return 1;
  }

  // create stack of symbol table nodes and push global scope
  scope_stack ss;
  symbolTable_node * curr_scope;
  symbolTable_node * global_scope;
  int count = 1;
%}

/* Body of bison file - grammar and tokens */
/* add definition of yylval */
%union {
 int int_val;
  float float_val;
  char* str_val;
  str_entry * s_entry;
  var_entry * v_entry;
  list_id_t * list_id;
  var_entry_list * list_v_entry;
  symbolTable_node * tree_node;
  symbolTable_node_list * list_tree_node;
}

%token <str_val> IDENTIFIER
%token <str_val> STRINGLITERAL

%token <str_val> INTLITERAL FLOATLITERAL

/*keywords*/
%token <str_val> INT FLOAT

%token  PROGRAM _BEGIN END FUNCTION READ WRITE
IF ELSE ENDIF WHILE ENDWHILE RETURN
IN RANGE VOID STRING TRUE FALSE
FOR ENDFOR CONTINUE BREAK

/*operators*/
%token  ASSIGN ADD SUB MUL DIV EQL NEQL
EMPTY LESS GRT ST_BRACE END_BRACE SEMICOLON
COMMA LEQ GEQ 


/* start node */
%start dummy

/* add semantic value types for required nodes of parse tree */
%type <s_entry> string_decl;      /* symbol table entry for str type */
%type <str_val> id str_literal;           /* string class */
%type <list_v_entry> var_decl;         /* symbol table entry for int/float type*/
%type <str_val> var_type;
%type <list_id> id_list;          /* pointer to object consisting of list*/
%type <list_id> id_tail;
%type <v_entry> param_decl;       /* symbol table entry for function parameters */
%type <list_v_entry> param_decl_tail;  /* pointer to list of var_entry */
%type <list_v_entry> param_decl_list;
%type <list_v_entry> decl;
%type <tree_node> func_declarations func_decl func_body while_stmt for_stmt if_stmt else_part stmt loop_stmt base_stmt read_stmt write_stmt;
%type <list_tree_node> stmt_list;


%%
/* Program */

dummy: dummy1 program ;

dummy1: { /* create a global scope at start of program */
          symbolTable_node * scope_temp = new symbolTable_node ("GLOBAL");
          global_scope = scope_temp;
          ss.push(scope_temp);
        };

program           : PROGRAM id _BEGIN pgm_body END 
id                : IDENTIFIER {$$ = $1;};
pgm_body          : decl func_declarations {  /* add entries from param_list to curr_scope node's symbol table */
                                    curr_scope = ss.top();

                                    // add entries from decl to pgm_body symbol table
                                    addEntriesFromListToNode (curr_scope, $1);
                                    ASTNode * expr_list_ast = new ExprList ($2->getStmts());
                                    curr_scope->addStmt(expr_list_ast);
};

decl              : string_decl decl {  /* add entries from child nodes (string_decl and decl) to root node */
                          $$ = new var_entry_list;
                          $$->push_back($1);
                          addEntriesFromListToList ($$, $2);
                       }
    | var_decl decl { /* add entries from child nodes (string_decl and decl) to root node */
                      $$ = new var_entry_list;
                      addEntriesFromListToList ($$, $1);
                      addEntriesFromListToList ($$, $2);
                    }
    | { /* initliaze the pointer - empty list*/
        $$ = new var_entry_list;
};

/* Global String Declaration */
string_decl       : STRING id ASSIGN str_literal SEMICOLON { /* create a str_entry object */
                                              $$ = new str_entry($2, $4);
};
str_literal       : STRINGLITERAL {$$ = $1;};

/* Variable Declaration */
var_decl          : var_type id_list SEMICOLON { /* create list of int/float var_entry objects based on id_list and var_type */
                                       /* id_list is a list of ids parsed from subtress */
                                        $$ = new var_entry_list;
                                        addIdsFromListToList ($$, $2, $1);
};
var_type          :  INT {$$ = $1;}
| FLOAT {$$ = $1;};
any_type          : var_type | VOID;
id_list           : id id_tail  {  /* create list object to hold all ids traversed by far*/
                        $$ = new list_id_t;
                        $$->push_back($1);
                        addIdsFromListToList ($$, $2);
};
id_tail           : COMMA id id_tail {  /* create list object to hold all ids traversed by far*/
                              $$ = new list_id_t;
                              $$->push_back($2);
                              addIdsFromListToList ($$, $3);
        }
| {$$ = new list_id_t;};

/* Function Paramater List */
param_decl_list   : param_decl param_decl_tail { /* add entries from child nodes (param_decl and param_decl_tail) to root node */
                                                $$ = new var_entry_list;
                                                $$->push_back($1);
                                                addEntriesFromListToList ($$, $2);
                                              }
               |  { /* initliaze the pointer - empty list*/
                    $$ = new var_entry_list;
};
param_decl: var_type id { /* create a var_entry object */
                          $$ = new var_entry($2, $1);
                        };

param_decl_tail:  COMMA param_decl param_decl_tail  { /* add entries from child nodes (param_decl and param_decl_tail) to root node */
                                                      $$ = new var_entry_list;
                                                      $$->push_back($2);
                                                      addEntriesFromListToList ($$, $3);
                                                    }
               | {  /* initliaze the pointer - empty list*/
                    $$ = new var_entry_list;
};

/* Function Declarations */
func_declarations:  func_decl func_declarations {/* Add support for nested or multiple functions - PENDING ???*/
                                                    $$ = new symbolTable_node;
                                                    // add function from func_decl
                                                    /**** ASTNode * expr_list_ast = new ExprList ($1->getStmts()); ****/
                                                    //$$->addStmt(expr_list_ast);
                                                    addStmtsFromListToList ($$->getStmts(), $1->getStmts(), "front");

                                                    /**** add functions from func_declarations ****/
                                                    addStmtsFromListToList ($$->getStmts(), $2->getStmts(), "front");
                                                }
| { $$ = new symbolTable_node;};
func_decl:  FUNCTION any_type id ST_BRACE param_decl_list
            END_BRACE _BEGIN func_body END {  /* create a new scope at start of function, add to tree of nodes, push to stack */
                                              symbolTable_node * scope_temp = new symbolTable_node ($3);
                                              assert (ss.empty() != 1);
                                              curr_scope = ss.top();
                                              curr_scope->addNode (scope_temp);
                                              ss.push(scope_temp);
                                              $$ = scope_temp;

                                              /* create function activation record - here as addEntries is desctructive */
                                              map<string, string> * func_ar = create_activation_record ($5, $8->getEntries());
                                              int param_size = $5->size();
                                              int local_var_size = ($8->getEntries())->size();

                                              /* add entries from param_decl_list to curr_scope node's symbol table */
                                              curr_scope = ss.top();
                                              addEntriesFromListToNode (curr_scope, $5);

                                              /* add entries and nodes from func_body to curr_scope node's symbol table */
                                              addEntriesFromListToNode (curr_scope, $8->getEntries());
                                              addNodesFromListToNode (curr_scope, $8->getNodes());

                                              /* add ASTNodes of stmts found in function body as ExprList-astNode with func header/footer */
                                              ASTNode * expr_list_ast = new ExprList ($8->getStmts());
                                              // update variable types
                                              updateVarTypeInAST (expr_list_ast, curr_scope); // update types in VarRef ASTNodes by local scope's (function's) symbol-table lookup

                                              // update variable ids based on activation record - VarTypes would have already been updated by this point
                                              //updateVarIdInAST(expr_list_ast, func_ar);
                                              //updateRETIdInAST(expr_list_ast, param_size, local_var_size);

                                              // add function header and footer
                                              ASTNode * func_ast = new FuncExpr ($3);
                                              func_ast->addLeft(expr_list_ast);
                                              updateVarIdInAST(func_ast, func_ar);
                                              updateRETIdInAST(func_ast, param_size, local_var_size);

                                              curr_scope->addStmt(func_ast);

                                              /* pop the current scope as function ends */
                                              ss.pop();
                                           };

func_body:  decl stmt_list {  /* add variable declarations and blocks found at sub-tree to func_body */
                              $$ = new symbolTable_node;

                              // find string assigns and add them to func_body's symbolTableNode
                              list<ASTNode*> * str_assign_list = GetStringAssignsFromList($1);
                              addStmtsFromListToList ($$->getStmts(), str_assign_list, "front");

                              // add statements to node (This is all assign/read/write_stmt found in the stmt_list)
                              symbolTable_node * temp = $2->front();
                              assert (temp != NULL);
                              assert (temp->isStmt()); // top node will be STMT node
                              // addStmtsFromNodeToNode ($$, temp, "back");
                              // add as ExprList root-ast
                              ASTNode * exprList_ast = new ExprList (new list<ASTNode*>);
                              addStmtsFromListToList (exprList_ast->getList(), temp->getStmts(), "back");
                              $$->addStmt (exprList_ast);

                              // assign other entries and nodes found in sub-tree
                              addEntriesFromListToNode ($$, $1);
                              addNodesFromListToNode ($$, $2);
};
/* Statement List */
stmt_list:  stmt stmt_list {  /* add blocks (nodes) from stmt and stmt_list to stmr_list (root) */
                              /* Note blocks get added to this list (front to back) in program order*/
                              $$ = new symbolTable_node_list;

                              // first node of list is a STMT node (no symbol/scope), that stores all stmts
                              symbolTable_node * temp = $2->front();
                              assert (temp != NULL);
                              assert (temp->isStmt()); // top node will be STMT node
                              $2->pop_front();
                              // NOTE: this addition (statements from child-node to parent-node) should be destructtive
                              // i.e. instructions added to dummy node should be removed from stmt's symboltable node
                              // For eg: this removes the if_ast (and assocaited instructions) from $1, thereby preventing repetetion of instructions i.e.
                              // (once read in paren'ts stmt_list, ince in nested scope's stmt_list)
                              addStmtsFromNodeToNode (temp, $1, "front");
                              $$->push_back(temp);

                              // add non-stmt nodes (and non-empty) found in sub-tree
                              if (($1->isEmpty() == 0) && ($1->isStmt() == 0)) {
                                $$->push_back($1);
                              }
                              addNodesFromListToList ($$, $2); //remaining list of $2 (after pop), can't contaain any stmt
                            }

         |  {   $$ = new symbolTable_node_list;
                // STMT: marks a dummy symbolTable node, contains stmts only and no symbol table or nested scope
                symbolTable_node * stmt_node = new symbolTable_node("STMT");
                $$->push_back(stmt_node);
            };

stmt: base_stmt { /* stmt node (node has list of entries, list of nested scopes, list assign/read/write statements) coming from sub-tree */
                  $$ = $1;
                }
    | if_stmt { $$ = $1;}
    | loop_stmt { $$ = $1;};

base_stmt:  assign_stmt {   /*create an ASSIGN SymbolTableNode and add AST of assign_stmt*/
                            $$ = new symbolTable_node("STMT");
                            $$->addStmt ($1);
                        }
         | read_stmt {  $$ = $1; }
         | write_stmt { $$ = $1; }
         | control_stmt { /*create an RETURN SymbolableNode and add AST of return_stmt */
                          //$$ = new symbolTable_node("EMPTY");
                          $$ = new symbolTable_node("STMT");
                          $$->addStmt ($1);
                        };


/* Basic Statements */
assign_stmt: assign_expr SEMICOLON {$$ = $1;};

assign_expr: id ASSIGN expr {   /* create a assign ASTNode */
                                $$ = new AssignExpr();
                                ASTNode * var_temp = new VarRef ($1); //na - needs assignment
                                $$->addLeft(var_temp);
                                $$->addRight($3);
                                //print_ast($$); // for DEBUG
                                //cout << endl;  // for DEBUG
                            };

read_stmt: READ ST_BRACE id_list END_BRACE SEMICOLON {  /* create an STMT synbolTableNode and push all AST nodes */
                                                        $$ = new symbolTable_node("STMT");
                                                        addStmtsFromIdListToNode ($$, $3, "r");
                                                     };

write_stmt: WRITE ST_BRACE id_list END_BRACE SEMICOLON {    $$ = new symbolTable_node("STMT");
                                                            addStmtsFromIdListToNode ($$, $3, "w");
                                                       };

return_stmt: RETURN expr SEMICOLON { /*create a return ASTNode*/
                                     $$ = new RetExpr();
                                     $$->addRight($2);
                                   };


/* Expressions */
expr: expr_prefix factor {  /* assign factor to expr_prefix's right node */
                            if (!$1->isNull()) {
                                $1->addRight($2);
                                $$ = $1;
                            }
                            else
                                $$ = $2;
                         };

expr_prefix:  expr_prefix factor addop  { /* check if expr_prefix is null*/
                                          if ($1->isNull()) {
                                            $3->addLeft($2); // assign factor to addop's left node
                                          }
                                          else {
                                            $1->addRight($2); // assign factor to expr_prefix's right node
                                            $3->addLeft($1); // assign expr_prefix (with right node), the left node of addop
                                          }
                                          $$ = $3; // return modfied addop
                                        }
           | {$$ = new AddExpr();};

factor: factor_prefix postfix_expr  {   /* check if factor_prefix is null */
                                        if ($1->isNull())
                                            $$ = $2;
                                        else {
                                            $1->addRight($2);
                                            $$ = $1;
                                        }
                                    }

factor_prefix:  factor_prefix postfix_expr mulop {  /* check if factor_prefix is null*/
                                                    if ($1->isNull()) {
                                                        $3->addLeft($2); // assign postfix_expr to mulop's left node
                                                    }
                                                    else {
                                                        $1->addRight($2); // assign postfix_expr to factor_prefix's right node
                                                        $3->addLeft($1); // assign factor_prefix (with right node), the left node of mulop
                                                    }
                                                    $$ = $3; // return modified mulop
                                                 }
             | {$$ = new MulExpr();};

postfix_expr: primary {$$ = $1;}
            | call_expr {$$ = $1;};

call_expr: id ST_BRACE expr_list END_BRACE {    $$ = new CallExpr($1, $3->getList());
                                                //$$->print(); // fo DEBUG
                                           };

expr_list: expr expr_list_tail { // add expr(s) foudnin tail
                                 $$ = new ExprList();
                                 $$->addExpr($1);
                                 $$->addExprList($2->getList());
                               }
         | {$$ = new ExprList();};

expr_list_tail: COMMA expr expr_list_tail { // add expr(s) found in tail
                                            $$ = new ExprList();
                                            $$->addExpr($2);
                                            $$->addExprList($3->getList());
                                          }
              | {$$ = new ExprList();};

primary: ST_BRACE expr END_BRACE {$$ = $2;}
        | id { // PENDING - lookup entry in symboltable
               $$ = new VarRef ($1);
             }
        | INTLITERAL {$$ = new LitVal($1, "INT");}
        | FLOATLITERAL {$$ = new LitVal($1, "FLOAT");};

addop: ADD {$$ = new AddExpr("+");}
     | SUB {$$ = new AddExpr("-");};

mulop: MUL {$$ = new MulExpr("*");}
| DIV {$$ = new MulExpr("/");};

/* Complex Statements and Condition */ 
if_stmt:  IF ST_BRACE cond END_BRACE decl stmt_list else_part ENDIF { /* create a block (node), add sub-blocks and variable declarations */
                                                                      $$ = new symbolTable_node ();

                                                                      // create if_ast with cond and add then's stmt_list as ExprList
                                                                      IfExpr * if_ast = new IfExpr($3);
                                                                      list<ASTNode *> * then_list = new list<ASTNode *>;
                                                                      addStmtsFromListToList (then_list, ($6->front())->getStmts(), "back"); //stmt_list collects expressions in opposite to prog-order
                                                                      ASTNode * then_ast = new ExprList (then_list);
                                                                      if_ast->addLeft(then_ast);
                                                                      // add else's stmt_list as ExprList if applicable
                                                                      if ($7->isEmpty() == 0) {
                                                                            list<ASTNode *> * else_list = new list<ASTNode *>;
                                                                            addStmtsFromListToList (else_list, $7->getStmts(), "back");
                                                                            ASTNode * else_ast = new ExprList (else_list);
                                                                            if_ast->addRight(else_ast);
                                                                      }
                                                                      // add if_ast to if's symbolTbaleNode
                                                                      $$->addStmt(if_ast);

                                                                      // add sub-blocks and variable declarations
                                                                      addEntriesFromListToNode ($$, $5);
                                                                      addNodesFromListToNode ($$, $6);
                                                                      // adding else-node to be a sub-node (nested node) of if_stmt
                                                                      if ( $7->isEmpty() == 0) {
                                                                            $$->addNode($7);
                                                                      }
                                                                  };

else_part:  ELSE decl stmt_list {   /* create a new scope at start of while_stmt, add to tree, push to stack */
                                    $$ = new symbolTable_node ();

                                    // add statements to node (This is all assign/read/write_stmt found in the stmt_list)
                                    symbolTable_node * temp = $3->front();
                                    assert (temp != NULL);
                                    assert (temp->isStmt()); // top node will be STMT node
                                    addStmtsFromNodeToNode ($$, temp, "front");

                                    // add sub-blocks and variable declarations
                                    addEntriesFromListToNode ($$, $2);
                                    addNodesFromListToNode ($$, $3);
                                }
         | {$$ = new symbolTable_node("EMPTY");};

cond: expr compop expr { $$ = $2;
                         $$->addLeft($1);
                         $$->addRight($3);
                       }
    | TRUE {  // left and right expr not required
              $$ = new CondExpr("TRUE");}
    | FALSE { $$ = new CondExpr("FALSE"); };

compop: LESS { $$ = new CondExpr("<"); }
      | GRT { $$ = new CondExpr(">"); }
      | EQL { $$ = new CondExpr("="); }
      | NEQL { $$ = new CondExpr("!="); }
      | LEQ { $$ = new CondExpr("<="); }
      | GEQ { $$ = new CondExpr(">="); };

while_stmt: WHILE ST_BRACE cond END_BRACE decl stmt_list ENDWHILE { /* create a new scope at start of while_stmt, add to tree, push to stack */
                                                                    $$ = new symbolTable_node ();

                                                                    // create while_ast with cond and add then's stmt_list as ExprList
                                                                    WhileExpr * while_ast = new WhileExpr($3);
                                                                    list<ASTNode *> * then_list = new list<ASTNode *>;
                                                                    addStmtsFromListToList (then_list, ($6->front())->getStmts(), "back"); //stmt_list collects expressions in opposite to prog-order
                                                                    ASTNode * then_ast = new ExprList (then_list);
                                                                    while_ast->addLeft(then_ast);
                                                                    // add while_ast to while's symbolTbaleNode
                                                                    $$->addStmt(while_ast);

                                                                    // add sub-blocks and variable declarations
                                                                    addEntriesFromListToNode ($$, $5);
                                                                    addNodesFromListToNode ($$, $6);
                                                                  };

/* ECE573 ONLY */
control_stmt: return_stmt {$$ = $1;}
            | CONTINUE COMMA
            | BREAK COMMA;

loop_stmt: while_stmt { $$ = $1; }
         | for_stmt { $$ = $1; };

init_stmt: assign_expr { $$ = $1; }
         |;

incr_stmt: assign_expr { $$ = $1; }
         |;

for_stmt: FOR ST_BRACE init_stmt SEMICOLON cond SEMICOLON
          incr_stmt END_BRACE decl stmt_list ENDFOR { /* create a new node at start of for_stmt, add to tree, push to stack */
                                                      $$ = new symbolTable_node ();

                                                      // create for_ast with cond, init, incr and add then's stmt_list as ExprList
                                                      ForExpr * for_ast = new ForExpr($3, $5, $7);
                                                      list<ASTNode *> * then_list = new list<ASTNode *>;
                                                      addStmtsFromListToList (then_list, ($10->front())->getStmts(), "back"); //stmt_list collects expressions in opposite to prog-order
                                                      ASTNode * then_ast = new ExprList (then_list);
                                                      for_ast->addLeft(then_ast);
                                                      // add while_ast to for's symbolTableNode
                                                      $$->addStmt(for_ast);

                                                      // add sub-blocks and variable declarations
                                                      addEntriesFromListToNode ($$, $9);
                                                      addNodesFromListToNode ($$, $10);
                                                    };

%%

