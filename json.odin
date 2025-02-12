package gon

import "core:encoding/json"
import "core:os"
import "core:fmt"


get_next_token_json :: proc(json_tokenizer: ^json.Tokenizer) -> (Token_Type, string) {
    loop: for {
        json_token, err := json.get_token(json_tokenizer)
        if err != nil && err != .EOF {
            fmt.println("json tokenizer error:", err)
            return .ERROR, ""
        }
        
        gon_token_type: Token_Type
        gon_token: string

        #partial switch json_token.kind {
            case .EOF: 
                gon_token_type = .EOF
                
            case .Invalid:
                gon_token_type = .ERROR
                
            case .Null, .False, .True, .Infinity, .NaN, .Ident, .Integer, .Float, .String:
                gon_token_type = .STRING
                gon_token      = json_token.text
                if json_token.kind == .String {
                    gon_token = gon_token[1:len(gon_token)-1]
                }
            
            case .Open_Bracket:
                gon_token_type = .ARRAY_BEGIN
                
            case .Close_Bracket:
                gon_token_type = .ARRAY_END
            
            case .Open_Brace:
                gon_token_type = .OBJECT_BEGIN
                
            case .Close_Brace:
                gon_token_type = .OBJECT_END
                
            case:
                continue loop
        }
        
        return gon_token_type, gon_token
    }
}


lex_next_token_json :: proc(json_tokenizer: ^json.Tokenizer) -> Token {
    loop: for {
        json_token, err := json.get_token(json_tokenizer)
        if err != nil && err != .EOF {
            fmt.println("json tokenizer error:", err)
            return { .ERROR, "", {} }
        }
        
        gon_token_type: Token_Type
        gon_token: string

        #partial switch json_token.kind {
            case .EOF: 
                gon_token_type = .EOF
                
            case .Invalid:
                gon_token_type = .ERROR
                
            case .Null, .False, .True, .Infinity, .NaN, .Ident, .Integer, .Float, .String:
                gon_token_type = .STRING
                gon_token      = json_token.text
                if json_token.kind == .String {
                    gon_token = gon_token[1:len(gon_token)-1]
                }
            
            case .Open_Bracket:
                gon_token_type = .ARRAY_BEGIN
                
            case .Close_Bracket:
                gon_token_type = .ARRAY_END
            
            case .Open_Brace:
                gon_token_type = .OBJECT_BEGIN
                
            case .Close_Brace:
                gon_token_type = .OBJECT_END
                
            case:
                continue loop
        }
        
        return { gon_token_type, gon_token, {} }
    }
}


/*
    Implementing JSON in the new DOM parser.
    We will probably do the same thing as before where we just adapt the tokens as they come in into GOM tokens.
    The only difficulty is that I'll need to modify the interface functions and restructure oter stuff a bit to make the alternate tokenizer work. 
    This is probably something I was going to do anyhow, its just a bit annoying. 
    
    
*/