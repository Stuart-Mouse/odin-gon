package gon

import "core:encoding/json"
import "core:os"
import "core:fmt"


get_next_token_json :: proc(json_tokenizer: ^json.Tokenizer) -> (Token_Type, string) {
    loop: for {
        json_token, err := json.get_token(json_tokenizer)
        if err != nil && err != .EOF {
            fmt.println("json tokenizer error:", err)
            return .INVALID, ""
        }
        
        gon_token_type: Token_Type
        gon_token: string

        #partial switch json_token.kind {
            case .EOF: 
                gon_token_type = .EOF
                
            case .Invalid:
                gon_token_type = .INVALID
                
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

// JSON_Formatter :: struct {
//     builder: strings.Builder,
// }

// format_token_json :: proc(serializer: Serializer, cmd: Format_Command, data: rawptr) -> bool {
//     formatter := cast(^JSON_Formatter) data
    
//     switch cmd.type {
//         case BEGIN_OBJECT:
//             fmt.sbprintf(builder, "\"%v\": {%v", name, delim)
            
//         case END_OBJECT:
//             fmt.sbprintf(builder, "}%v", delim)
            
//         case BEGIN_ARRAY:
//             fmt.sbprintf(builder, "\"%v\": {%v", name, delim)
            
//         case END_ARRAY:
//             fmt.sbprintf(builder, "]%v", delim)
            
//         case FORMAT_FIELD:
//             // JSON is crap, requires commas after each field except on the last item in an object
//             // We already handle this in GON, so we can handle it here, but when we do XML, we will probably just have to ignore the delim values passed... 
//             // And we can't handle it in here because we don't know whether or not there are more objects to come.
//             // we could also try to fix it in post, or hold onto each field and only print it after we know if soemthing comes next? bleh...
//             fmt.sbprintf(builder, "\"%v\"%v", delim)
//     }
// }

