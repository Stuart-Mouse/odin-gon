package gon

import "core:runtime"
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
    STRING,
    OBJECT_BEGIN,
    OBJECT_END,
    ARRAY_BEGIN,
    ARRAY_END,
    REF_INDEX,
    REF_POINTER,
    REF_VALUE,
    PATH_SPLIT,
    PATH_PARENT,
    EOF,
}

Tokenizer :: struct {
    type       : File_Format,
    next_token : Token,
    
    // not bothering to put specific tokenizers in union for now, 
    // since gon tokenizer is literally just the remaining string and line count
    // gon tokenizer
    file       : string,
    line       : int, 
    // json tokenizer
    json_tokenizer: json.Tokenizer,
}

consume_token :: proc(using t: ^Tokenizer) -> bool {
    if next_token.type == .EOF do return true
    ok: bool
    switch type {
        case .GON : next_token, ok = lex_next_token(t)
        case .JSON: next_token, ok = lex_next_token_json(&t.json_tokenizer)
    }
    return ok
}

get_token :: proc(using t: ^Tokenizer) -> (Token, bool) {
    current_token := next_token
    return current_token, consume_token(t)
}

peek_token :: proc(using t: ^Tokenizer) -> Token {
    return next_token
}

// mutates the passed string, advancing it to the position after the returned token
lex_next_token :: proc(using t: ^Tokenizer) -> (Token, bool) {
    if len(file) <= 0 do return { .EOF, "", line  }, true
    
    // skip whitespace and comments
    for {
        for is_whitespace(file[0]) {
            if file[0] == '\n' {
                line += 1
            }
            if !advance(&file) {
                return { .EOF, "", line }, true
            }
        }
        if file[0] == '#' {
            for file[0] != '\n' {
                if !advance(&file) {
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
            advance(&file)
            return { .OBJECT_BEGIN, "", line }, true
        case '}':
            advance(&file)
            return { .OBJECT_END,   "", line }, true
        case '[':
            advance(&file)
            return { .ARRAY_BEGIN,  "", line }, true
        case ']':
            advance(&file)
            return { .ARRAY_END,    "", line }, true
        case '&':
            advance(&file)
            return { .REF_INDEX,    "", line }, true
        case '*':
            advance(&file)
            return { .REF_POINTER,  "", line }, true
        case '$':
            advance(&file)
            return { .REF_VALUE,    "", line }, true
    }
    
    // tokens only used in path strings, maybe we have a param to skip these when not parsing for a path
    if file[0] == '/' {
        advance(&file)
        return { .PATH_SPLIT, "", line }, true
    }
    
    // '..' token used in path strings to step up to parent scope
    if len(file) >= 2 && file[0] == '.' && file[1] == '.' {
        advance(&file, 2)
        return { .PATH_PARENT, "", line }, true
    }
    
    // quoted string
    if file[0] == '"' || file[0] == '\'' || file[0] == '`' { 
        quote_char := file[0]
        
        if !advance(&file) do return { .EOF, "", line }, false
        string_value := file[0:]
        string_len := 0
        
        for file[0] != quote_char {
            if file[0] == '\n' do line += 1
            adv := 1 + int(file[0] == '\\') // TODO: handle escape sequences properly, this will eat a newline also
            string_len += adv
            if !advance(&file, adv) do return { .EOF, "", line }, false
        }
        advance(&file)
        
        return { .STRING, string_value[:string_len], line }, true
    }
    
    // unquoted string
    if is_char_permitted_in_unquoted_string(file[0]) {
        string_value := file[0:]
        string_len   := 0
        
        for is_char_permitted_in_unquoted_string(file[0]) {
            string_len += 1
            if !advance(&file) do break
        }
        
        return { .STRING, string_value[:string_len], line }, true
    }
    
    next_whitespace_char := strings.index_any(file, whitespace_chars)
    invalid_token_str := file[:]
    fmt.printfln("Invalid token '%v' encountered.\n", invalid_token_str)
    return { .INVALID, "", line }, false
}

// is_numeric :: proc(char: u8) -> bool {
//     return char >= '0' && char <='9'
// }

// is_alpha :: proc(char: u8) -> bool {
//     return (char >= 'a' && char <='z' ) || (char >= 'A' && char <='Z')
// }

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
advance :: proc(file: ^string, amount := 1) -> bool {
    amount := min(amount, len(file))
    file^ = file^[amount:]
    return len(file) != 0
}

is_whitespace :: proc(char: u8) -> bool {
    return char == ' ' || char == ',' || char == '\t' || char == '\r' || char == '\n' 
}

// this is still used elsewhere, but can be removed eventually since we've inlined it in lex_next_token so that we can count lines
skip_whitespace_and_comments :: proc(file: ^string) -> bool {
    for {
        for is_whitespace(file^[0]) {
            advance(file) or_return
        }
        if file^[0] == '#' {
            for file^[0] != '\n' {
                advance(file) or_return
            }
            continue
        }
        return true
    }
}



/*
    I flirted briefly with the idea of differentiating types of unquoted string tokens such as number and identifier,
    but ultimately this ends up causing more complication than it is worth, because of number parsing, mostly.
    I think it is better if we leave it more free-form at this stage and then we can error if a string fails to convert to a number if that is what the internal type demands.
    Which is essentially what the original implementation was, but now I have more reason to keep it that way.
    
    TODO: 
        parsing of escape sequences
        should also unescape strings when copying
        will probably use the same proc for both use cases so that we don't have divergent implementations
*/