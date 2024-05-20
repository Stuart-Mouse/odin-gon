package gon

import "core:runtime"
import "core:reflect"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:mem"
import "core:math"


GON_Tokenizer :: struct {
    file       : string,
    next_token : Token,
}


consume_token :: proc(using t: ^GON_Tokenizer) -> bool {
    if next_token.type == .EOF do return true
    ok: bool
    next_token, ok = lex_next_token(&file)
    return ok
}

get_token :: proc(using t: ^GON_Tokenizer) -> (Token, bool) {
    current_token := next_token
    return current_token, consume_token(t)
}

peek_token :: proc(using t: ^GON_Tokenizer) -> Token {
    return next_token
}

// mutates the passed string, advancing it to the position after the returned token
lex_next_token :: proc(file: ^string) -> (Token, bool) {
    if len(file^) <= 0                     do return {.EOF, ""}, true
    if !skip_whitespace_and_comments(file) do return {.EOF, ""}, true
    
    // single character tokens
    switch file^[0] {
        case '{':
            advance(file)
            return {.OBJECT_BEGIN, ""}, true
        case '}':
            advance(file)
            return {.OBJECT_END,   ""}, true
        case '[':
            advance(file)
            return {.ARRAY_BEGIN,  ""}, true
        case ']':
            advance(file)
            return {.ARRAY_END,    ""}, true
        case '&':
            advance(file)
            return {.REF_INDEX,    ""}, true
        case '*':
            advance(file)
            return {.REF_POINTER,  ""}, true
        case '$':
            advance(file)
            return {.REF_VALUE,    ""}, true
    }
    
    // some helper procs
    is_numeric :: proc(char: u8) -> bool {
        return char >= '0' && char <='9'
    }
    
    is_alpha :: proc(char: u8) -> bool {
        return (char >= 'a' && char <='z' ) || (char >= 'A' && char <='Z')
    }
    
    // tokens only used in path strings, maybe we have a param to skip these when not parsing for a path
    if file^[0] == '/' {
        advance(file)
        return {.PATH_SPLIT, ""}, true
    }
    
    // not very correct, but whatever for now
    if file^[0] == '.' {
        type := Token_Type.PATH_HERE
        if advance(file) && file^[0] == '.' {
            type = .PATH_PARENT
            advance(file)
        }
        return {type, ""}, true
    }
    
    // string
    if file^[0] == '"' || file^[0] == '\'' || file^[0] == '`' { 
        quote_char := file^[0]
        
        if !advance(file) do return {.EOF, ""}, false
        string_value := file^[0:]
        string_len := 0
        
        for file^[0] != quote_char {
            adv := 1 + int(file^[0] == '\\') // TODO: handle escape sequences more properly
            string_len += adv
            if !advance(file, adv) do return {.EOF, ""}, false
        }
        advance(file)
        
        return {.STRING, string_value[:string_len]}, true
    }
    
    // number
    if is_numeric(file^[0]) || file^[0] == '-' { 
        string_value := file^[0:]
        string_len := 0
        
        // number base specifiers
        if file^[0] == '0' {
            if file^[0] == 'b' || 
               file^[0] == 'h' || 
               file^[0] == 'o' || 
               file^[0] == 'x' {
                if !advance(file) do return {.EOF, ""}, false
            }
        }
        
        for is_numeric(file^[0]) || file^[0] == '_' || file^[0] == '.' {
            string_len += 1
            if !advance(file) do break
        }
        
        return {.STRING, string_value[:string_len]}, true
    }
    
    // identifier
    if is_alpha(file^[0]) || file^[0] == '_' {
        string_value := file^[0:]
        string_len   := 0
        
        for is_alpha(file^[0]) || is_numeric(file^[0]) || file^[0] == '_' {
            string_len += 1
            if !advance(file) do break
        }
        
        return {.STRING, string_value[:string_len]}, true
    }
    
    next_whitespace_char := strings.index_any(file^, whitespace_chars)
    invalid_token_str := file^[:]
    fmt.printfln("Invalid token '%v' encountered.\n", invalid_token_str)
    return {.INVALID, ""}, false
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

// returns the unescaped character and the length of the escape sequence in characters
// parse_escape_sequence :: proc(str: string) -> (u8, int) {
//     // TODO
// }
