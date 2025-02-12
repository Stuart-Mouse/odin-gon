package gon

import "base:runtime"
import "core:reflect"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:mem"
import "core:math"
import "core:encoding/json"


Source_Location :: struct {
    line, char: int,
}

Token :: struct {
    type:       Token_Type,
    text:       string,
    location:   Source_Location,
}

Token_Type :: enum u8 {
    ERROR,
    EOF,
    
    STRING,
    
    OBJECT_BEGIN,
    OBJECT_END,
    ARRAY_BEGIN,
    ARRAY_END,
    
    REF_INDEX,
    REF_POINTER,
    REF_VALUE,
}

GON_Lexer :: struct {
    file:       string,
    location:   Source_Location,
}

Lexer :: struct {
    type:           File_Format,
    next_token:     Token,
    
    using _internal_lexer: struct #raw_union {
        using gon_lexer:  GON_Lexer,
        json_tokenizer:   json.Tokenizer,
    },
}

consume_token :: proc(using l: ^Lexer) {
    if next_token.type == .EOF do return 
    switch type {
        case .GON:  next_token = lex_next_token(l)
        case .JSON: next_token = lex_next_token_json(&l.json_tokenizer)
    }
}

get_token :: proc(using l: ^Lexer) -> Token {
    current_token := next_token
    consume_token(l)
    return current_token
}

peek_token :: proc(using l: ^Lexer) -> Token {
    return next_token
}

// mutates the passed string, advancing it to the position after the returned token
lex_next_token :: proc(using l: ^Lexer) -> Token {
    if len(file) <= 0 do return { .EOF, "", location }
    
    // skip whitespace and comments
    for {
        for is_whitespace(file[0]) {
            if !advance_lexer(l) {
                return { .EOF, "", location }
            }
        }
        if file[0] == '#' {
            for file[0] != '\n' {
                if !advance_lexer(l) {
                    return { .EOF, "", location }
                }
            }
            continue // go back to skipping whitespace after end of comment
        }
        break
    }
    
    // single character tokens
    switch file[0] {
      case '{':
        advance_lexer(l)
        return { .OBJECT_BEGIN, "", location }
      case '}':
        advance_lexer(l)
        return { .OBJECT_END,   "", location }
      case '[':
        advance_lexer(l)
        return { .ARRAY_BEGIN,  "", location }
      case ']':
        advance_lexer(l)
        return { .ARRAY_END,    "", location }
      case '&':
        advance_lexer(l)
        return { .REF_INDEX,    "", location }
      case '*':
        advance_lexer(l)
        return { .REF_POINTER,  "", location }
      case '$':
        advance_lexer(l)
        return { .REF_VALUE,    "", location }
    }
    
    // quoted string
    if file[0] == '"' /*|| file[0] == '\'' || file[0] == '`'*/ { 
        quote_char := file[0]
        
        if !advance_lexer(l) do return { .EOF, "", location }
        string_value := file[0:]
        string_len := 0
        
        for file[0] != quote_char {
            adv := 1 + int(file[0] == '\\') // TODO: handle escape sequences properly, this will eat a newlocation also
            string_len += adv
            if !advance_lexer(l, adv) do return { .EOF, "", location }
        }
        advance_lexer(l)
        
        return { .STRING, string_value[:string_len], location }
    }
    
    // unquoted string
    if is_char_permitted_in_unquoted_string(file[0]) {
        string_value := file[0:]
        string_len   := 0
        
        for is_char_permitted_in_unquoted_string(file[0]) {
            string_len += 1
            if !advance_lexer(l) do break
        }
        
        return { .STRING, string_value[:string_len], location }
    }
    
    char_string := transmute(string) runtime.Raw_Slice { raw_data(file), 1 }
    fmt.printfln("Unexpected character '%v' encountered.\n", char_string)
    return { .ERROR, "", location }
}

// permits alphanumeric characters and dash, underscore, period
is_char_permitted_in_unquoted_string :: proc(char: u8) -> bool {
    return (char >= '0' && char <='9') || 
           (char >= 'a' && char <='z') || 
           (char >= 'A' && char <='Z') || 
            char == '-' || 
            char == '_' || 
            char == '.'
}

// bascially wraps our slice operation so that we can handle an error in the case that we run out of characters
advance_lexer :: proc(using l: ^Lexer, amount := 1) -> bool {
    amount := min(amount, len(file));
    for i in 0..<amount {
        if file[i] == '\n' {
            location.line += 1;
            location.char  = 0;
        } else {
            location.char += 1;
        }
    }
    file = file[amount:];
    return len(file) > 0; // return false when we hit EOF
}

is_whitespace :: proc(char: u8) -> bool {
    return char == ' ' || char == ',' || char == '\t' || char == '\r' || char == '\n' 
}


