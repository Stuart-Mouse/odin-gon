package gon

import "base:runtime"
import "core:reflect"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:mem"
import "core:math"
import "core:encoding/json"


whitespace_chars :: " ,\t\r\n\x00"
reserved_chars   :: "~!@#$%^&*{}[]\""
whitespace_and_reserved_chars :: " ,\t\r\n#{}[]\"\x00"

Token :: struct {
    type: Token_Type,
    text: string,
    line: int,
}

Token_Type :: enum u8 {
    INVALID,
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

Lexer :: struct {
    type:           File_Format,
    next_token:     Token,
    parsing_path:   bool,
    
    // not bothering to put specific tokenizers in union for now, 
    // since gon tokenizer is literally just the remaining string and line count
    // gon tokenizer
    file:           string,
    line, char:     int, 
    
    // json tokenizer
    json_tokenizer: json.Tokenizer,
}

consume_token :: proc(using t: ^Lexer) -> bool {
    if next_token.type == .EOF do return true
    ok: bool
    switch type {
        case .GON : next_token, ok = lex_next_token(t)
        case .JSON: next_token, ok = lex_next_token_json(&t.json_tokenizer)
    }
    return ok
}

get_token :: proc(using t: ^Lexer) -> (Token, bool) {
    current_token := next_token
    return current_token, consume_token(t)
}

peek_token :: proc(using t: ^Lexer) -> Token {
    return next_token
}

// mutates the passed string, advancing it to the position after the returned token
lex_next_token :: proc(using t: ^Lexer) -> (Token, bool) {
    if len(file) <= 0 do return { .EOF, "", line }, true
    
    // skip whitespace and comments
    for {
        for is_whitespace(file[0]) {
            if !advance_tokenizer(t) {
                return { .EOF, "", line }, true
            }
        }
        if file[0] == '#' {
            for file[0] != '\n' {
                if !advance_tokenizer(t) {
                    return { .EOF, "", line }, true
                }
            }
            continue // go back to skipping whitespace after end of comment
        }
        break
    }
    
    // single character tokens
    switch file[0] {
      case '{':
        advance_tokenizer(t)
        return { .OBJECT_BEGIN, "", line }, true
      case '}':
        advance_tokenizer(t)
        return { .OBJECT_END,   "", line }, true
      case '[':
        advance_tokenizer(t)
        return { .ARRAY_BEGIN,  "", line }, true
      case ']':
        advance_tokenizer(t)
        return { .ARRAY_END,    "", line }, true
      case '&':
        advance_tokenizer(t)
        return { .REF_INDEX,    "", line }, true
      case '*':
        advance_tokenizer(t)
        return { .REF_POINTER,  "", line }, true
      case '$':
        advance_tokenizer(t)
        return { .REF_VALUE,    "", line }, true
    }
    
    // quoted string
    if file[0] == '"' /*|| file[0] == '\'' || file[0] == '`'*/ { 
        quote_char := file[0]
        
        if !advance_tokenizer(t) do return { .EOF, "", line }, false
        string_value := file[0:]
        string_len := 0
        
        for file[0] != quote_char {
            if file[0] == '\n' do line += 1
            adv := 1 + int(file[0] == '\\') // TODO: handle escape sequences properly, this will eat a newline also
            string_len += adv
            if !advance_tokenizer(t, adv) do return { .EOF, "", line }, false
        }
        advance_tokenizer(t)
        
        return { .STRING, string_value[:string_len], line }, true
    }
    
    // unquoted string
    if is_char_permitted_in_unquoted_string(file[0], parsing_path) {
        string_value := file[0:]
        string_len   := 0
        
        for is_char_permitted_in_unquoted_string(file[0], parsing_path) {
            string_len += 1
            if !advance_tokenizer(t) do break
        }
        
        return { .STRING, string_value[:string_len], line }, true
    }
    
    char_string := transmute(string) runtime.Raw_Slice { raw_data(file), 1 }
    fmt.printfln("Unexpected character '%v' encountered.\n", char_string)
    return { .INVALID, "", line }, false
}

// permits alphanumeric characters and dash, underscore, period
is_char_permitted_in_unquoted_string :: proc(char: u8, parsing_path := false) -> bool {
    return (char >= '0' && char <='9') || 
           (char >= 'a' && char <='z') || 
           (char >= 'A' && char <='Z') || 
            char == '-' || 
            char == '_' || 
            char == '.' || 
           (char == '/' && !parsing_path)
}

// bascially wraps our slice operation so that we can handle an error in the case that we run out of characters
advance_tokenizer :: proc(t: ^Lexer, amount := 1) -> bool {
    amount := min(amount, len(t.file));
    for i in 0..<amount {
        if t.file[i] == '\n' {
            t.line += 1;
        }
    }
    t.file = t.file[amount:];
    return len(t.file) > 0; // return false when we hit EOF
}

is_whitespace :: proc(char: u8) -> bool {
    return char == ' ' || char == ',' || char == '\t' || char == '\r' || char == '\n' 
}


