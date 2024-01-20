/*
 * This is a snapshot of the interpreter that was written for a C Compiler frontend 
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define INITIAL_CAPACITY 100 // initial capacity of tokens array
#define MAX_LEXEME_SIZE 50
#define MAX_BUFF_SIZE 10000
#define KEYWORDS 22 // number of keywords, used for reserved word lookup

#define INITIAL_ROOT_CAPACITY 50 // initial capacity for nodes in translation unit

bool had_error = false;
bool lexer_at_end = false;

/* ============================== ERROR HANDLING ===============================
 *
 * ===========================================================================*/
void error(int line, char *msg) {
  printf("line %d | error: %s\n", line, msg);

  // TODO Halt Parser, enter panic mode
  had_error = true;
}

/* ============================== HELPERS ======================================
 * explcit ctype.h functions
 * ===========================================================================*/
bool is_digit(char c) { return c >= '0' && c <= '9'; }
//bool is_digit(char c) { return (c ^ 0x30) < 0xA; }

bool is_alpha(char c) { return ((c >= 'A' && c <= 'Z') ||
                                   (c >= 'a' && c <= 'z')); }
//bool is_alpha(char c) { return ((c ^ 0x40) - 1) < 0x5B; }

bool is_alphanum(char c) { return is_digit(c) || is_alpha(c); }

size_t get_file_size(FILE *f) {
  size_t file_size;
  if (fseek(f, 0, SEEK_END) != 0 ) exit(EXIT_FAILURE); 

  file_size = ftell(f);
  rewind(f);
  return file_size;
}

void read_file(FILE * f, char *buffer, size_t file_size) {
  if (file_size == 1) exit(EXIT_FAILURE);

  fread(buffer, 1, file_size, f);
  if (buffer == NULL) exit(EXIT_FAILURE);

  fclose(f); 
}

/* ============================== TOKEN ========================================
 *
 * ===========================================================================*/
typedef enum {
  /* Single Character Tokens */
  SEMI,                      /* ; */
  COMMA,                     /* , */
  COLON,                     /* : */
  OCB,                       /* { */
  CCB,                       /* } */
  OP,                        /* ( */
  CP,                        /* ) */
  OSB,                       /* [ */
  CSB,                       /* ] */
  QUEST,                     /* ? */
  DOT,                       /* . */

  /* Single or Double Character Tokens */
  ADD,                       /* + */
  INC,                       /* ++ */
  ADDEQ,                     /* += */

  SUB,                       /* - */
  DEC,                       /* -- */
  ARROW,                     /* -> */
  SUBEQ,                     /* -= */

  MUL,                       /* * */
  MULEQ,                     /* *= */

  DIV,                       /* / */
  DIVEQ,                     /* /= */

  MOD,                       /* % */
  MODEQ,                     /* %= */

  NOT,                       /* ! */
  NOTEQ,                     /* != */

  AMP,                       /* & */
  AND,                       /* && */
  
  LT,                        /* < */
  LTEQ,                        /* <= */

  GT,                        /* > */
  GTEQ,                        /* >= */

  ASSIGN,                    /* = */
  EQ,                        /* == */
  
  BWOR,                      /* | */ // bitwise OR
  OR,                        /* || */

  /* Keywords */
  INT,                       /* int */
  UNSIGN,                    /* unsigned*/
  CHAR,                      /* char */
  STAT,                      /* static */
  EXTERN,                    /* extern */
  CONST,                     /* const */
  IF,                        /* if */
  ELSE,                      /* else */
  WHILE,                     /* while */
  DO,                        /* do */
  FOR,                       /* for */
  SWITCH,                    /* switch */
  CASE,                      /* case */
  DEFAULT,                   /* default */
  RETURN,                    /* return */
  BREAK,                     /* break */
  CONTINUE,                    /* continue */
  GOTO,                      /* goto */
  SIZEOF,                    /* sizeof */
  STRUCT,                    /* struct */
  UNION,                     /* union */
  VOID,                      /* void */
  NIL,                       /* NULL */

  /* Misc */
  END,                       /* EOF */
  IDENTIFIER,                /* Identifiers */
  STRING, NUMBER,            /* Literals */
  ERROR,                     /* Error, used for keyword lookup */
  
  // TODO support assembly level operations
  // TODO support bitwise operators
} token_type_T;

// TODO refactor with hashmap
char *get_token_name(token_type_T type) {
  switch (type) {
  /* Single Character Tokens */
  case SEMI: return "SEMI";
  case COMMA: return "COMMA";
  case COLON: return "COLON";
  case OCB: return "OCB";
  case CCB: return "CCB";
  case OP: return "OP";
  case CP: return "CP";
  case OSB: return "OSB";
  case CSB: return "CSB";
  case QUEST: return "QUEST";
  case DOT: return "DOT";

  /* Single or Double Character Tokens */
  case ADD: return "ADD";
  case INC: return "INC";
  case ADDEQ: return "ADDEQ";

  case SUB: return "SUB";
  case DEC: return "DEC";
  case ARROW: return "ARROW";
  case SUBEQ: return "SUBEQ";
  
  case MUL: return "MUL";

  case DIV: return "DIV";

  case MOD: return "MOD";

  case MULEQ: return "MULEQ";
  case DIVEQ: return "DIVEQ";
  case MODEQ: return "MODEQ";

  case AMP: return "AMP";
  case AND: return "AND";

  case LT: return "LT";
  case LTEQ: return "LTEQ";

  case GT: return "GT";
  case GTEQ: return "GTEQ";

  case ASSIGN: return "ASSIGN";
  case EQ: return "EQ";
  
  case NOT: return "NOT";
  case NOTEQ: return "NOTEQ";
  
  case BWOR: return "BWOR";
  case OR: return "OR";

  /* Keywords */
  case INT: return "int";
  case UNSIGN: return "unsigned";
  case CHAR: return "char";
  case STAT: return "static";
  case EXTERN: return "extern";
  case CONST: return "const";
  case IF: return "if";
  case ELSE: return "if";
  case WHILE: return "while";
  case DO: return "do";
  case FOR: return "for";
  case SWITCH: return "switch";
  case CASE: return "case";
  case DEFAULT: return "default";
  case RETURN: return "return";
  case BREAK: return "break";
  case CONTINUE: return "continue";
  case GOTO: return "goto";
  case SIZEOF: return "sizeof";
  case STRUCT: return "struct";
  case UNION: return "union";
  case VOID: return "void";
  case NIL: return "NULL";

  /* Misc */
  case END: return "END";
  case IDENTIFIER: return "IDENTIFIER";
  case STRING: return "STRING";
  case NUMBER: return "NUMBER";
  case ERROR: return "ERROR";
  
  // TODO support assembly level operations
  // TODO support bitwise operators
  
  }
}

typedef struct {
  token_type_T type;
  char *lexeme; 
  // TODO add overloadable literal ? might help during runtime
  unsigned int line;
} token_T;

/* ============================== LEXER ========================================
 *
 * ===========================================================================*/
typedef struct {
  char *src;
  size_t src_len;
  unsigned int current; // index of current char being considered
  unsigned int start; // first character in lexeme being scanned
  unsigned int line;

  token_T *tokens;
  unsigned int tokens_index;
  unsigned int tokens_capacity;

  token_T *keywords;
  unsigned int keywords_capacity;
  unsigned int keyword_index;
} lexer_T;


bool load_keyword_lookup_table(lexer_T *lexer) {
  for (int i = INT; i <= NIL; i++) {
    char *curr= get_token_name(i);
    lexer->keywords[lexer->keyword_index].type = i;
    lexer->keywords[lexer->keyword_index].lexeme = curr;
    lexer->keywords[lexer->keyword_index].line = 0;
    lexer->keyword_index += 1;
  }

  return true;
}

lexer_T * init_lexer(char *src, size_t len) {
  lexer_T * lexer = calloc(1, sizeof(lexer_T));
  lexer->src = src;
  lexer->src_len = len;
  lexer->current = 0;
  lexer->start = 0;
  lexer->line = 1;

  // TODO support dynamic resizing
  lexer->tokens_capacity = INITIAL_CAPACITY;
  lexer->tokens = malloc(sizeof(token_T) * lexer->tokens_capacity); 
  lexer->tokens_index = 0;

  lexer->keywords_capacity = KEYWORDS;
  lexer->keywords = malloc(sizeof(token_T) * lexer->keywords_capacity); 
  lexer->keyword_index = 0;
  if (!load_keyword_lookup_table(lexer)) {
      perror("error: unable to load keyword table");
      exit(1);
  }

  return lexer;
}

token_type_T get_token_keyword(lexer_T *lexer, char *buf) {
  for (unsigned int i = 0; i < lexer->keyword_index; i++) {
    if (strcmp(lexer->keywords[i].lexeme, buf) == 0) return lexer->keywords[i].type;
  }

  return ERROR;
}

bool resize(lexer_T *lexer) {
  lexer->tokens = realloc(lexer->tokens, (lexer->tokens_capacity * 2));

  return (lexer->tokens == NULL);
}

bool is_end(lexer_T *lexer) { return lexer->current >= lexer->src_len; }

char advance(lexer_T *lexer) {
  if (lexer->current < lexer->src_len && !is_end(lexer)) {
    lexer->current++; 
    return lexer->src[lexer->current];
  }

  return -1;
}

void add_token(lexer_T *lexer, token_type_T t) {
  if (lexer->tokens_index + 1 > lexer->tokens_capacity) {
    if (!resize(lexer)) {
      // TODO create seperate handle for internal errors (errors that aren't from the lexer)
      perror("error: unable to resize token array");
      exit(1);
    }
  }

  lexer->tokens[lexer->tokens_index].type = t;

  char *c = get_token_name(t);
  unsigned int s = strlen(c);
  lexer->tokens[lexer->tokens_index].lexeme = malloc(sizeof(char) * s);
  strncpy(lexer->tokens[lexer->tokens_index].lexeme, c, s);

  lexer->tokens[lexer->tokens_index].lexeme = get_token_name(t);
  lexer->tokens[lexer->tokens_index].line = lexer->line;

  lexer->tokens_index += 1;
}

void add_token_keyword_identifier_literal(lexer_T *lexer, token_type_T t, char *c) {
  if (lexer->tokens_index + 1 > lexer->tokens_capacity) {
    if (!resize(lexer)) {
      // TODO create seperate handle for internal errors (errors that aren't from the lexer)
      perror("error: unable to resize token array");
      exit(1);
    }
  }
  
  lexer->tokens[lexer->tokens_index].type = t;

  unsigned int s = strlen(c);
  lexer->tokens[lexer->tokens_index].lexeme = malloc(sizeof(char) * s);
  strncpy(lexer->tokens[lexer->tokens_index].lexeme, c, s);

  lexer->tokens[lexer->tokens_index].line = lexer->line;

  lexer->tokens_index += 1;
}

char peek(lexer_T *lexer, int offset) {
  if ((lexer->current + offset) >= lexer->src_len) return '\0';
  return lexer->src[lexer->current + offset];
}

bool match(lexer_T *lexer, char expected) {
  if (is_end(lexer)) return false;
  if (peek(lexer, 1) != expected) {
    return false;
  }

  advance(lexer);

  return true;
}

void handle_string(lexer_T *lexer) {
  unsigned int i = 0;
  char buf[MAX_BUFF_SIZE];

  advance(lexer); //skip first double quote

  while (lexer->src[lexer->current] != '"' && !is_end(lexer)) {
    if (peek(lexer,1) == '\n') lexer->line += 1;
    buf[i] = lexer->src[lexer->current];
    i += 1;
    advance(lexer);
  }

  if (is_end(lexer)) {
    error(lexer->line, "unterminated string");
    return;
  }

  buf[i] = '\0';

  add_token_keyword_identifier_literal(lexer, STRING, buf);
}

// FIXME Handle all numeric types. For now, just integers
void handle_numeric(lexer_T *lexer) {
  unsigned int i = 0;
  char buf[MAX_BUFF_SIZE];

  while (is_digit(lexer->src[lexer->current]) && !is_end(lexer)) {
    buf[i] = lexer->src[lexer->current];
    i += 1;
    advance(lexer);
  }

  buf[i] = '\0';

  add_token_keyword_identifier_literal(lexer, NUMBER, buf);
}

void handle_keyword_or_identifier(lexer_T *lexer) {
  unsigned int i = 0;
  char buf[MAX_BUFF_SIZE];

  while (is_alphanum(lexer->src[lexer->current]) && !is_end(lexer)) {
    buf[i] = lexer->src[lexer->current];
    i += 1;
    advance(lexer);
  }

  buf[i] = '\0';

  token_type_T res;
  
  if ((res = get_token_keyword(lexer, buf)) != ERROR) {
    add_token_keyword_identifier_literal(lexer, res, buf);
    return;
  }
  
  add_token_keyword_identifier_literal(lexer, IDENTIFIER, buf);
}

void consume_token(lexer_T *lexer) {
  char c = lexer->src[lexer->current];

  // Keyword or Identifier
  if (is_alpha(c)) {
    handle_keyword_or_identifier(lexer);
    return;
  }

  // Numeric literals
  if (is_digit(c)) {
    handle_numeric(lexer);
    return;
  }

  switch (c) {
    // skip tabs, spaces, carriage returns and newlines
    case ' ':
    case '\t':
    case '\r':
      break;
    case '\n': lexer->line += 1; break;

    // single char 
    case ';': add_token(lexer, SEMI); break;
    case ',': add_token(lexer, COMMA); break;
    case ':': add_token(lexer, COLON); break;
    case '(': add_token(lexer, OP); break;
    case ')': add_token(lexer, CP); break;
    case '{': add_token(lexer, OCB); break;
    case '}': add_token(lexer, CCB); break;
    case '[': add_token(lexer, OSB); break;
    case ']': add_token(lexer, CSB); break;
    case '?': add_token(lexer, COMMA); break;
    case '.': add_token(lexer, DOT); break;

    // single or double char
    case '+': 
      if (match(lexer, '+')) {
        add_token(lexer, INC);
      }
      else if (match(lexer, '=')) {
        add_token(lexer, ADDEQ);
      }
      else add_token(lexer, ADD);
      break;

    case '-': 
      if (match(lexer, '-')) {
        add_token(lexer, DEC);
      }
      else if (match(lexer, '>')) {
        add_token(lexer, ARROW);
      }
      else if (match(lexer, '=')) {
        add_token(lexer, SUBEQ);
      }
      else add_token(lexer, SUB);
      break;

    case '*': 
      add_token(lexer, match(lexer, '=') ? MULEQ : MUL); break;
    
    // TODO Handle nested comments
    case '/': 
      // single line comment 
      if (match(lexer, '/')) {
        while (peek(lexer, 1) != '\n' && !is_end(lexer)) advance(lexer);
      }
      // multi-line comment 
      if (match(lexer, '*')) {
        while (!(peek(lexer, 1) == '*' && peek(lexer,2) == '/') && !is_end(lexer)) {
          advance(lexer);
        }
        
        if (is_end(lexer)) error(lexer->line, "unterminated comment"); 
        else advance(lexer); // move to the termination of the multi line comment
      }
      else if (match(lexer, '=')) add_token(lexer, DIVEQ);
      else add_token(lexer, DIV);
      break;

    case '%': 
      add_token(lexer, match(lexer, '=') ? MOD : MODEQ); break;

    case '!': 
      add_token(lexer, match(lexer, '=') ? NOT : NOTEQ); break;

    case '&': 
      add_token(lexer, match(lexer, '&') ? AMP : AND); break;

    case '<': 
      add_token(lexer, match(lexer, '=') ? LT : LTEQ); break;
    
    case '>': 
      add_token(lexer, match(lexer, '=') ? GT : GTEQ); break;

    case '=': 
      if (match(lexer, '=')) add_token(lexer, ASSIGN);
      else add_token(lexer, EQ);
      break;

    case '|':
      add_token(lexer, match(lexer, '|') ? BWOR : OR); break;

    // String literals
    case '"': handle_string(lexer); break;

    default: 
      error(lexer->line, "unexpected character."); break;
  }

  advance(lexer);
}

// Utility
void print_tokens(lexer_T *lexer) {
  for (unsigned int i = 0; i < lexer->tokens_index; i++) {
    printf("%s %s\n", get_token_name(lexer->tokens[i].type), lexer->tokens[i].lexeme);
  }
}

// tokens are added to lexer
void scan_tokens(lexer_T *lexer) {
  while (!is_end(lexer)) {
    consume_token(lexer);
  }

  add_token(lexer, END);
}

/* ============================== DRIVER =======================================
 *
 * ===========================================================================*/

int main(int argc, char **argv) {
  if (argc != 2) {
    printf("%s\n", "usage: source <filename>");
    exit(1);
  }

  FILE *f;
  if(!(f = fopen(argv[1], "r"))) {
    printf("%s\n", "error: file not found");
    exit(1);
  }

  size_t file_size = get_file_size(f);
  char *buffer = malloc(file_size * sizeof(char));

  read_file(f, buffer, file_size);

  /* Lexer Entry Point */
  lexer_T *lexer = init_lexer(buffer, file_size);

  printf("%s\n", buffer);

  scan_tokens(lexer);

  print_tokens(lexer);

  return 0;
}
