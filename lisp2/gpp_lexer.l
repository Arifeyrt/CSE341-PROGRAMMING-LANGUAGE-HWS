
%%
;;.*              {printf("%s:COMMENT\n",yytext);}
and               {printf("%s: KW_AND\n",yytext);}
or                {printf("%s: KW_OR\n",yytext);}
not               {printf("%s: KW_NOT\n",yytext);}
equal             {printf("%s: KW_EQUAL\n",yytext);} 
less              {printf("%s: KW_LESS\n",yytext);} 
nil               {printf("%s: KW_NIL\n",yytext);} 
list              {printf("%s: KW_LIST\n",yytext);} 
append            {printf("%s: KW_APPEND\n",yytext);} 
concat            {printf("%s: KW_CONCAT\n",yytext);}
set               {printf("%s: KW_SET\n",yytext);} 
def               {printf("%s: KW_DEF\n",yytext);}
for               {printf("%s: KW_FOR\n",yytext);}
if                {printf("%s: KW_IF\n",yytext);}
exit              {printf("%s: KW_EXIT\n",yytext);}
load              {printf("%s: KW_LOAD\n",yytext);}
display           {printf("%s: KW_DISPLAY\n",yytext);}
true              {printf("%s: KW_TRUE\n",yytext);}
false             {printf("%s: KW_FALSE\n",yytext);}
\+               {printf("%s: OP_PLUS\n",yytext);}
\-               {printf("%s: OP_MINUS\n",yytext);}
\/             {printf("%s: OP_DIV\n",yytext);}
\*               {printf("%s: OP_MULT\n",yytext);}
\(               {printf("%s: OP_OP\n",yytext);}
\)               {printf("%s: OP_CP\n",yytext);}

[0-9]+[b][0-9]+ {
    printf("%s: VALUEF\n",yytext);
    }
   
[[:space:]]+ 		/* ignore space */

[a-zA-Z][a-zA-Z0-9]* {
        printf("%s: IDENTIFIER\n",yytext);
}
[0-9][0-9a-zA-Z]+ {printf("LEXICAL ERROR: %s \n", yytext); return 0;}
.	 {printf("LSYNTAX_ERROR %s \n", yytext); return 0;}
%%

/* Explanation: 
yywrap - wraps the above rule section 
yyin - takes the file pointer which contains the input
yylex - this is the main flex function which runs the Rule Section	
yytext - is the text in the buffer
*/

int yywrap(){}
int main(int argc, char *argv[]){
FILE *fp;
char *filename;
if(argv[1] != NULL){
    filename = argv[1];
    fp = fopen(filename, "r");
    if(fp == NULL){
        printf("File not found\n");
        return 0;
    }
    yyin = fp;
}
else{
    printf("Enter  ");
}
yylex();
return 0;
}












