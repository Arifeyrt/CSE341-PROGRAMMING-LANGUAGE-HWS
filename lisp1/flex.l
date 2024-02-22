%{
   #include "y.tab.h"
   #include <string.h>
   void yyerror (char const *s) {
   fprintf (stderr, "%s\n", s);
  ; exit(-1);
 }
 float findFvalue (char* text);
%}

%%

"+" {return OP_PLUS;}
"-" {return OP_MINUS;}
"/" {return OP_DIV;}
"*" {return OP_MULT;}
"(" {return OP_OP;}
")" {return OP_CP;}
" " {}
"," {return OP_COMMA;}
":;" {return COMMENT;}

[0-9][0-9]*[b][0-9]* {yylval.valuef = findFvalue(yytext);return VALUEF;}
[a-zA-Z_]+[a-zA-Z0-9_]* { strcpy(yylval.id ,yytext);return ID;}
. {printf("",  yytext);return 0;}

\n {return 0;}
%%

float findFvalue (char* text)
{
    int index = 0;
    int digitCount = 0;
    int multiplier = 1;
    int readType = 0;
    float tamKisim = 0;
    float fracKisim = 0;

    while(text[index+1]!='\0')
    {
        ++index;
    }
        
    
    int num = 0;

    while(index!=-1)
    {
        multiplier = 1;
        if(text[index]=='b')
        {
            digitCount = 0;
            index--;
            readType = 1;
            continue;
        }

        int i;
        for(i=0;i<digitCount;++i)
        {
            multiplier *= 10;
        }

        int readNum = text[index]-'0';

        if(readType==0)
            fracKisim += readNum * multiplier;
        

        else
            tamKisim += readNum * multiplier;
        
        index--;
        digitCount++;
    }

    return tamKisim / fracKisim;

}

int yywrap(){}

yylex();

