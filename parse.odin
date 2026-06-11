#+feature using-stmt

package gon

import "base:runtime"
import "core:reflect"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:mem"
import "core:math"
import "core:log"
import "core:encoding/json"

Parser_Callback :: proc(^Node) -> Callback_Results

// used to build a DOM from a text file and evaluate data bindings on that DOM
Parser :: struct {
    tokenizer:       Lexer,
    dom_root:        ^Node,
    node_allocator:  runtime.Allocator,
    callbacks:       [dynamic] Parser_Callback,
}

init_dom_parser :: proc(parser: ^Parser, file: string, format: File_Format = .GON, node_allocator := context.allocator) {
    parser.node_allocator = node_allocator
    
    parser.tokenizer.type = format
    switch format {
      case .GON:
        parser.tokenizer.file = file
        parser.tokenizer.location = { 1, 1 }
      case .JSON:
        parser.tokenizer.json_tokenizer = json.make_tokenizer(file)
    }
        
    consume_token(&parser.tokenizer) // get the first token when we init, we always pull one token ahead of the one we return
}

deinit_dom_parser :: proc(using parser: ^Parser) {
    delete_child_nodes_recursive(dom_root, node_allocator)
    free(dom_root, node_allocator)
    dom_root = nil
    delete(callbacks)
}

// creates a dom parser with the given parameters, intializes it, and constructs the dom from the given file
// after calling this, you can just add your data bindings and then process them
parse_file_to_dom :: proc(file: string, format: File_Format = .GON, allocator := context.allocator) -> (parser: Parser, ok: bool) {
    init_dom_parser(&parser, file, format, allocator)
    defer if !ok do deinit_dom_parser(&parser)
    
    if !construct_dom_from_gon_file(&parser) do return {}, false
    if !validate_node_references   (&parser) do return {}, false
    
    return parser, true
}

construct_dom_from_gon_file :: proc(using parser: ^Parser) -> (ok: bool) {
    next_token: Token
    
    dom_root      = new(Node, node_allocator)
    dom_root.name = "root"
    dom_root.type = .OBJECT
    
    defer if !ok {
        delete_child_nodes_recursive(dom_root)
        free(dom_root, node_allocator)
        dom_root = nil // prevents a double free in parse_file_to_dom
    }
    
    log_unexpected_token :: #force_inline proc(token: Token) {
        if token.type == .ERROR {
            log.logf(.Error, "GON tokenization error: %v at %v.", token.text, token.location)
        } else {
            log.logf(.Error, "GON parse error: Unexpected %v token \"%v\" at %v.", token.type, token.text, token.location)
        }
    }
    
    parent := dom_root
    L_Loop: for parent != nil {
        name, text:     string
        type:           Node_Type
        flags:          Node_Flags
        location:       Source_Location
        
        // field value ref without name inside an object will create an unnamed field with the same data binding as the parent object
        if parent.type == .OBJECT && peek_token(&tokenizer).type == .REF_VALUE {
            flags |= { .BIND_PARENT }
        } else {
            // read field name
            if parent.type != .ARRAY {
                next_token = get_token(&tokenizer)
                #partial switch next_token.type {
                  case .STRING: 
                    name = next_token.text
                    location = next_token.location
                    
                  case .EOF:
                    if parent != dom_root {
                        log.logf(.Error, "GON parse error: Unexpected %v token \"%v\". at %v", next_token.type, next_token.text, next_token.location)
                        return false
                    }
                    break L_Loop
                    
                  case .OBJECT_END:
                    if parent.type != .OBJECT {
                        log.logf(.Error, "GON parse error: Unexpected %v token \"%v\" at %v.", next_token.type, next_token.text, next_token.location)
                        return false
                    }
                    if next_token.location == parent.location {
                        parent.flags |= { .SAME_LINE }
                    }
                    parent = parent.parent
                    continue
                    
                  case:
                    log_unexpected_token(next_token)
                    return false
                }
            }
        }
        
        next_token = get_token(&tokenizer)
        if next_token.type == .REF_INDEX   || 
           next_token.type == .REF_POINTER || 
           next_token.type == .REF_VALUE {
            
            node := append_child_node(parent, allocator = node_allocator)
            node.name  = name
            node.type  = .REF
            node.flags = flags
            
            #partial switch next_token.type {
              case .REF_INDEX:    node.ref.type = .INDEX
              case .REF_POINTER:  node.ref.type = .POINTER
              case .REF_VALUE:    node.ref.type = .VALUE
            }
            
            next_token = get_token(&tokenizer)
            if next_token.type != .STRING {
                log.logf(.Error, "GON parse error: Field ref path must be a valid string value.")
                return false
            } 
            node.ref.text = next_token.text
        }
        else {
            // read field value
            #partial switch next_token.type {
              case .STRING: 
                type = .FIELD
                text = next_token.text
                if name == "" {
                    location = next_token.location
                }
                
              case .OBJECT_BEGIN: 
                type = .OBJECT
                
              case .ARRAY_BEGIN: 
                type = .ARRAY
                
              case .ARRAY_END:
                if parent.type != .ARRAY {
                    log_unexpected_token(next_token)
                    return false
                }
                if next_token.location.line == parent.location.line {
                    parent.flags |= { .SAME_LINE }
                }
                parent = parent.parent
                continue
                
              case:
                log_unexpected_token(next_token)
                return false
            }
            
            assert(type != .INVALID)
            
            node := append_child_node(parent, allocator = node_allocator)
            node.location.line = location.line
            node.name  = name
            node.type  = type
            node.flags = flags
            if node.type == .OBJECT || node.type == .ARRAY {
                parent = node
            } else {
                node.value = text
            }
        }
    }
    
    return true
}
