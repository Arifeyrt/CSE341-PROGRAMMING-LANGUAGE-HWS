%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>

void yyerror (char *s);
int yylex();



struct idvalue
{
        char id[50];
        float valuef;
};
//kaç adet eleman=number 
struct repo
{
        struct idvalue *pairs; //ıd ve valuef mı ona bakar
        int number;//eklenen eleman saysıı
};

struct repo *variables;
struct repo *funcs;

void startRepo()
{
        variables = (struct repo*)malloc(sizeof(struct repo));
        funcs = (struct repo*)malloc(sizeof(struct repo));
}
int checkIdExist(char id[50]) {
    int i = 0;

    // İlk önce lineer arama yapılır.
    while (i < variables->number && strcmp(id, variables->pairs[i].id) != 0) {
        i++;
    }

    if (i < variables->number) {
        return i;  // Bulundu
    } else {
        return -1; // Bulunamadı
    }
}
int checkFuncExist( char id[50]) {
    int i = 0;

    // İlk önce lineer arama yapılır.
    while (i < funcs->number && strcmp(id, funcs->pairs[i].id) != 0) {
        i++;
    }

    if (i < funcs->number) {
        return i;  // Bulundu
    } else {
        return -1; // Bulunamadı
    }
}
float valueOfId(char id[50])
{
        int i;
        for(i=0;i<variables->number;i++)
        {
                if(strcmp(id, variables->pairs[i].id)==0)
                        variables->pairs[i].valuef;
        }
}
//yeni bir oge ekliyor variable tipindeki pairse
void addRepo(float value, char id[50]) {
    // Yeni bir idvalue öğesi oluşturuluyor.
    struct idvalue newItem;
    strcpy(newItem.id, id);
    newItem.valuef = value;

    // Yeni bir idvalue dizisi oluşturuluyor ve önceki veriler kopyalanıyor.
    struct idvalue *newPairs = (struct idvalue*)malloc((variables->number + 1) * sizeof(struct idvalue));
    if (newPairs == NULL) {
        fprintf(stderr, "Bellek hatası: Yetersiz bellek!\n");
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < variables->number; i++) {
        strcpy(newPairs[i].id, variables->pairs[i].id);
        newPairs[i].valuef = variables->pairs[i].valuef;
    }

    // Yeni idvalue öğesi ekleniyor.
    strcpy(newPairs[variables->number].id, newItem.id);
    newPairs[variables->number].valuef = newItem.valuef;

    // Eski veri dizisi serbest bırakılıyor ve yeni veri dizisi atanıyor.
    variables->pairs = newPairs;

    // Öğe sayısı bir artırılıyor.
    variables->number += 1;
}


void addRepoFuncs(char id[50])
{
        struct idvalue *pairs;

        pairs = (struct idvalue*)calloc(funcs->number + 1, sizeof(struct idvalue));

        int i;
        for(i=0;i<funcs->number;i++)
        {
                strcpy(pairs[i].id, funcs->pairs[i].id);
                pairs[i].valuef = funcs->pairs[i].valuef;
        }

        strcpy(pairs[i].id, id);
        pairs[i].valuef = 1;//bu

        funcs->pairs = pairs;
        funcs->number += 1;
}

void updateRepo(float value, char id[50], int index)
{
        strcpy(variables->pairs[index].id, id);
        variables->pairs[index].valuef = value;
}
%}

%union{
    int value;
    char id[50];
    float valuef;
}

%start INPUT


%token OP_PLUS
%token OP_MINUS
%token OP_EQ
%token OP_DIV
%token OP_MULT
%token OP_CP
%token OP_OP
%token OP_COMMA

%token COMMENT
%token <valuef> VALUEF
%token <id> ID
%type <valuef> EXP



%%

INPUT:
       
        EXP {printf("Syntax OK.\nResult: %f\n",$1); } 
      

EXP:
        OP_OP OP_PLUS EXP EXP OP_CP {$$ = $3 + $4;} |
        OP_OP OP_MINUS EXP EXP OP_CP {$$ = $3 - $4;}|
        OP_OP OP_MULT EXP EXP OP_CP {$$ = $3 * $4;}|
        OP_OP OP_DIV EXP EXP OP_CP {$$ = $3 / $4;}|
        ID
        {
        int index = checkIdExist($1);

        if(index!=-1)
        {
                struct idvalue target = variables->pairs[index];
                $$ = target.valuef;
        }
        else
        {
                printf("No identifier named %s", $1);
                exit(1);
        }
        
        } | 
        VALUEF {$$ = $1;}
       





%%
        


int main(int argc, char **argv)
{       
        startRepo();
         while(1){
                printf("Enter line: ");
                        yyparse();
    }
}