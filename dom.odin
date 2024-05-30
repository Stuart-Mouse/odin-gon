
package gon

import "core:runtime"
import "core:reflect"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:mem"
import "core:math"

/*
    After getting some of the basic stuff working for the DOM thing, I may be able to use it to generate parsing procedures for binary data as well.
    and perhaps this could also do things like manage versioning of structs, etc.
*/

INDENTATION_STRING := "    "

DOM_Node_Flags :: bit_set[DOM_Node_Flag; u8]
DOM_Node_Flag  :: enum u8 {
    // parsing flags
    REFERENCES_RESOLVED,
    BINDING_RESOLVED,
    BINDING_ON_PATH,
    
    ARRAY_AS_OBJECT,
    ARRAY_INDEXED,
    ARRAY_ENUMERATED,
    
    // used to indicate that a field assumes the parent object's binding, used for special field value ref syntax
    BIND_PARENT,

    // formatting flags
    SAME_LINE,
}

DOM_Node_Ref_Type :: enum u8 { VALUE, POINTER, INDEX }

// this struct is kinda big
// maybe we optimize this later, but for now just making it work
DOM_Node :: struct {
    parent       : ^DOM_Node, 
    next         : ^DOM_Node, 
    prev         : ^DOM_Node, 

    source_line  : int,

    name         : string,
    type         : Field_Type,
    flags        : DOM_Node_Flags,
    data_binding : any,
    
    using content: struct #raw_union {
        ref: struct {
            text : string, 
            node : ^DOM_Node,
            type : DOM_Node_Ref_Type,
        },
        
        value: Token, // probably change back to just string
        
        using children: struct { 
            first : ^DOM_Node,
            last  : ^DOM_Node,
            count : int,
        },
    },
}

Node_Insertion_Behaviour :: enum {
    DEFAULT,        // just insert nodes with no extra checks
    OVERWRITE,      // overwrite existing nodes with the same name
    UNDERWRITE,     // don't insert node if one with the same name already exists
}

get_node_index :: proc(node: ^DOM_Node) -> int {
    index := 0
    for n := node.prev; n != nil; n = n.prev {
        index += 1
    }
    return index
}

debug_print_all_nodes :: proc(node: ^DOM_Node, indent: int = 0) {
    for i in 0..<indent do fmt.print(INDENTATION_STRING)
    fmt.println(node.name)

    child := node.first
    for child != nil {
        debug_print_all_nodes(child, indent + 1)
        child = child.next
    }
}

// does not delete the passed node or its neighbors, only children
delete_child_nodes_recursive :: proc(node: ^DOM_Node, allocator := context.allocator) {
    if node.type == .OBJECT || node.type == .ARRAY {
        child := node.first
        for child != nil {
            next := child.next
            delete_child_nodes_recursive(child, allocator)
            free(child, allocator)
            child = next
        }
    }
}

find_node_by_path :: proc(node: ^DOM_Node, path: string) -> (^DOM_Node, int) {
    node  := node
    index := 0
    
    t: Tokenizer = { file = path }
    consume_token(&t)
    
    for node != nil {
        next, ok := get_next_token_from_path_string(&t)
        if !ok                       do return nil, 0
        if next.type == .EOF         do break
        if next.type == .PATH_PARENT { node = node.parent; continue }
        node, index = find_child_node_by_name(node, next.text)
    }
    
    return node, index
}

// will return nil if not found
find_child_node_by_name :: proc(parent: ^DOM_Node, name: string) -> (^DOM_Node, int) {
    node := parent.first
    index := 0
    for node != nil {
        if node.name == name do break
        node = node.next
        index += 1
    }
    return node, index
}

append_data_node :: proc(parent: ^DOM_Node, path: string, data_binding: any, prepend := false, allocator := context.allocator) ->  ^DOM_Node {    
    node := append_node_with_path(parent, path, prepend = prepend, allocator = allocator)
    if node == nil do return node
    
    node.data_binding = data_binding
    node.type = determine_node_type_for_serialization(node) // TODO: combine into one proc with the below indirect data bindings one below. maybe we just inline those here for now
    
    // make indirect bindings
    if node.type == .OBJECT || node.type == .ARRAY {
        append_nodes_for_indirect_bindings(node, allocator)
    }
    
    return node
}

// does the bare minimum to append a node, not even giving it a name
// after the node is appended, caller should initialize it
append_child_node :: proc(parent: ^DOM_Node, prepend := false, allocator := context.allocator) ->  ^DOM_Node {
    node, err := new(DOM_Node, allocator)
    if err != nil do return nil // don't want to pass down the allocator error atm, maybe do this later
    
    node.parent = parent
    parent.count += 1
    
    if prepend {
        if parent.first != nil {
            parent.first.prev = node
            node.next = parent.first
        }
        parent.first = node
        
        if parent.last == nil {
            parent.last = node
        }
    } else {
        if parent.last != nil {
            parent.last.next = node
            node.prev = parent.last
        }
        parent.last = node
        
        if parent.first == nil {
            parent.first = node
        }
    }
    
    return node
}

// may be a little bit odd, but if you want tell if a node was overwritten or not, check if type == .INVALID. if so, then the node was either created or overwritten
get_or_add_child_node :: proc(parent: ^DOM_Node, name: string, behavior: Node_Insertion_Behaviour = .DEFAULT, prepend := false, allocator := context.allocator) ->  ^DOM_Node {
    node: ^DOM_Node
    
    if behavior != .DEFAULT {
        node, _ = find_child_node_by_name(parent, name)
    }
    
    if node == nil {
        node = append_child_node(parent, prepend, allocator)
    } 
    else if behavior == .UNDERWRITE {
        return node
    }
    
    node^ = { name = name }
    return node
}

append_node_with_path :: proc(parent: ^DOM_Node, path: string, behavior: Node_Insertion_Behaviour = .DEFAULT, prepend := false, allocator := context.allocator) -> ^DOM_Node {
    node := parent
    
    t: Tokenizer = { file = path }
    consume_token(&t)
    
    for {
        next, ok := get_next_token_from_path_string(&t)
        if !ok do return nil
        
        if peek_token(&t).type == .EOF {
            return get_or_add_child_node(node, next.text, behavior, prepend, allocator)
        }
        
        child, _ := find_child_node_by_name(node, next.text)
        if child != nil {
            if child.type != .OBJECT {
                return nil // error, we can't create a named subnode on an array or field type node
            }
            node = child
            continue
        }
        
        node      = append_child_node(node, prepend, allocator)
        node.name = next.text
        node.type = .OBJECT
    }
    
    assert(false, "unreachable")
    return nil
}

remove_node :: proc(node: ^DOM_Node, allocator := context.allocator) {
    node.parent.count -= 1
    if node.next != nil do node.next.prev = node.prev
    if node.prev != nil do node.prev.next = node.next
    if node.parent.first == node do node.parent.first = node.next
    if node.parent.last  == node do node.parent.last  = node.prev
    free(node, allocator)
}


clone_child_nodes_recursive :: proc(dst: ^DOM_Node, src: ^DOM_Node, allocator := context.allocator) -> bool {
    for child := src.first; child != nil; child = child.next {
        node := append_child_node(dst, false, allocator)
        if !clone_node_recursive(node, child) do return false
    }
    return true
}

clone_node_recursive :: proc(dst: ^DOM_Node, src: ^DOM_Node, allocator := context.allocator) -> bool {    
    if dst.type == .OBJECT {
        delete_child_nodes_recursive(dst)
    }

    dst.name         = src.name
    dst.type         = src.type
    dst.flags        = src.flags
    dst.data_binding = src.data_binding
    dst.content      = {}

    switch src.type {
        case .OBJECT, .ARRAY:
            return clone_child_nodes_recursive(dst, src, allocator)
        case .REF:
            dst.ref = src.ref
            if dst.ref.type == .VALUE {
                fmt.printfln("ERROR: cloned a value ref node @ %v", format_node_path(dst))
            }
        case .FIELD:
            dst.value.text = src.value.text
        case .INVALID:
            return false
    }
    
    return true
}

get_next_token_from_path_string :: proc(t: ^Tokenizer) -> (Token, bool) {
    next, ok := get_token(t)
    if !ok do return {}, false
    
    #partial switch next.type {
        case .EOF, .PATH_PARENT, .STRING: // no op
        case: return {}, false
    }
    
    #partial switch peek_token(t).type {
        case .EOF        : // no op
        case .PATH_SPLIT : consume_token(t)
        case             : return {}, false
    }
    
    return next, true
}

format_node_path :: proc(node: ^DOM_Node) -> string {
    recurse :: proc(builder: ^strings.Builder, node: ^DOM_Node) {
        if node.parent != nil {
            recurse(builder, node.parent)
            strings.write_byte(builder, '/')
        }
        fmt.sbprintf(builder, "%v", node.name)
    }
    
    builder := strings.builder_make(allocator = context.temp_allocator)
    defer strings.builder_destroy(&builder)
    recurse(&builder, node)
    return strings.to_string(builder)
}


/*
    SERIALIZATION NOTES

    It seems like we really may not *need* to create nodes for all indirect bindings when serializing.
    The only reason we need to create the nodes is so that we can reorder elements, and we may attach some formatting flags to the nodes but those flags are presumably available also by checking the io data for the data binding. 
    
    not sure if I like the separation of determining the field type and creating indirect data bindings
    the reason they are separate now is because of how I am creating the nodes with the type before creating indirect bindings
    
    I think though, that we can append the node, then resolve the type and create indirect bindings in a single procedure. 
        And that seems like it may be a better idea since both of those operations require similar information.
        Plus, we could more succinctly rebind data as necessary, like in treating []u8 types as strings.
    
    
    SIDE NOTE:
    
    we will have to insert a special condition when serializing a node to handle the custom formatting that's required for a bit set
    likewise for parsing a bit set from a dom also.
    
*/

DOM_Parser_Callback :: proc(^DOM_Node) -> Callback_Results

DOM_Parse_Flags :: bit_set[DOM_Parse_Flag]
DOM_Parse_Flag  :: enum {
    SKIP_PATHS_WITHOUT_BINDINGS,
}

// used to build a DOM from a text file and evaluate data bindings on that DOM
DOM_Parser :: struct {
    tokenizer      : Tokenizer,
    dom_root       : ^DOM_Node,
    log            : Log_Proc,
    node_allocator : runtime.Allocator,
    callbacks      : [dynamic] DOM_Parser_Callback,
}

init_dom_parser :: proc(using parser: ^DOM_Parser, _file: string, _allocator := context.allocator) {
    node_allocator = _allocator
    tokenizer.file = _file
    tokenizer.line = 1;
    consume_token(&tokenizer) // get the first token when we init, we always pull one token ahead of the one we return
    
    // ensure that parse context is properly init'd
    if log == nil do log = default_log_proc
    if log == nil do log = log_stub
}

deinit_dom_parser :: proc(using parser: ^DOM_Parser) {
    delete_child_nodes_recursive(dom_root, node_allocator)
    free(dom_root, node_allocator)
    dom_root = nil
    delete(callbacks)
}

// creates a dom parser with the given parameters, intializes it, and constructs the dom from the given file
// after calling this, you can just add your data bindings and then process them
parse_file_to_dom :: proc(_file: string, _allocator := context.allocator) -> (parser: DOM_Parser, ok: bool) {
    init_dom_parser(&parser, _file, _allocator)
    defer if !ok do deinit_dom_parser(&parser)
    
    if !construct_dom_from_gon_file(&parser) do return {}, false
    if !validate_node_references   (&parser) do return {}, false
    
    return parser, true
}

/*
    We are no longer appending to a dynamic array of data bindings, instead just inserting those data bindings immediately when this is called by the user.
    Which is nice because that means we save a little bit of memory on that and we don't need the Data_Binding struct anymore.
    We also don't have to split the path into substrings, since we just process it one piece at a time as we insert the binding.
*/
add_data_binding_to_dom :: proc(using parser: ^DOM_Parser, binding: any, path: string) -> bool {
    node, _ := find_node_by_path(parser.dom_root, path)
    return add_data_binding_to_node(node, binding)
}

add_data_bindings_to_dom :: proc(using parser: ^DOM_Parser, bindings: [] struct { binding: any, path: string }) -> bool {
    for b in bindings {
        if !add_data_binding_to_dom(parser, b.binding, b.path) do return false
    }
    return true
}

/*
    dom parser features
        parsing
            + plain old data, default formatting
            + indexed arrays
            + arrays of named objects
            + map types
                + support key types other than string
                + store key value to map key member
            + enumerated arrays
            + indexing normal arrays with enums?
                + just add enum typeid in io_data for array ezpz
            + field refs
                + traverse nodes by relative field path
                + get index
                + get binding value / or fallback to string value
                + get binding pointer
            - callbacks
                + ability to remap data binding
                + ability to add custom parsing in a callback and skip normal bindings
                - consider and improve
            - expression evaluation with lead sheets integration
            
        serialization
            + plain old data, default formatting
            + sameline flag with somewhat intelligent defaults
                + store source line number on node
                    + use this to set sameline flag on parsed nodes
            - indexed arrays
            - callbacks / fully custom formatting

    
    Broad overview of dom parsing process:
        construct dom from file
        validate field refs
        insert data bindings onto nodes
        final walk over dom
            run callbacks
            process data bindings
            

    
    TODO: 
    use a tracking allocator and ensure that we aren't leaking memory. This is pretty important.
    Also, probably refactor all code that allocates nodes and ensure that they are allocated and freed using the proper allocator
    
    
    
    Future Optimizations:
    
    we will probably be able to stop using a doubly-linked structure for nodes, instead using a singly-linked list for child nodes
        we still need to be able to traverse up the tree in order to resolve relative field references though, so we can't remove the need for *parent
    
    use real arena for nodes instead of polluting temp storage
        use indexes instead of pointers
        introduce better node naviagtion procs
            something like get_next(node, 3) could offer conditional chaining of get_next()
    
    
    
*/

validate_node_references :: proc(using parser: ^DOM_Parser) -> bool {
    Result :: bit_set[ enum{ ERROR, COMPLETE, PROGRESS, REMOVE_NODE } ]

    recurse :: proc(using parser: ^DOM_Parser, node: ^DOM_Node) -> Result {
        if .REFERENCES_RESOLVED in node.flags do return { .COMPLETE }
        
        switch node.type {
            case .FIELD:
                node.flags |= { .REFERENCES_RESOLVED }
                return { .PROGRESS, .COMPLETE }
        
            case .OBJECT, .ARRAY:
                    result: Result = { .COMPLETE }
                    child := node.first; 
                    for child != nil {
                        next_child   := child.next
                        child_result := recurse(parser, child)
                        if .ERROR       in child_result do return { .ERROR }
                        if .REMOVE_NODE in child_result do remove_node(child, node_allocator)
                        result |=  child_result & { .PROGRESS }
                        result &= (child_result & { .COMPLETE }) | ~{ .COMPLETE }
                        child = next_child
                    }
                    if .COMPLETE in result {
                        node.flags |= { .REFERENCES_RESOLVED }
                    }
                    return result
                
            case .REF:
                path := node.ref.text
                if path == "" {
                    log("Empty reference on node '%v'.", format_node_path(node))
                    return { .ERROR }
                }
                
                search_from_node := node.parent
                if path[0] == '/' {
                    path = path[1:]
                    search_from_node = parser.dom_root
                }
                ref_node, _ := find_node_by_path(search_from_node, path)
                
                if ref_node == nil {
                    // TODO: find a way to print failed node path only when full pass makes no progress
                    // maybe we collect warnings in some array and only print on error
                    // would want to enumerate error types and store node, then format and print later
                    log("WARNING: Cannot resolve ref from %v to %v", format_node_path(node), node.ref.text)
                    return { }
                }
                node.ref.node = ref_node
                
                // pointer and index refs can be passed along and handled later
                if node.ref.type != .VALUE { 
                    node.flags |= { .REFERENCES_RESOLVED }
                    return { .PROGRESS, .COMPLETE }
                }
                
                // value ref to value ref cannot be resolved yet
                if node.ref.node.type == .REF && node.ref.node.ref.type == .VALUE {
                    log("WARNING: Cannot resolve value ref to value ref: %v", format_node_path(node))
                    return { }
                }
                
                result := Result { .PROGRESS, .COMPLETE }
                
                if .BIND_PARENT in node.flags {
                    assert(ref_node.type == .OBJECT, "ref node with bind parent flag was not pointing to an object.")
                
                    // we can't copy these nodes until they are all resolved, otherwise we get issues
                    if .REFERENCES_RESOLVED not_in ref_node.flags {
                        return { }
                    }
                
                    parent := node.parent
                    for child := ref_node.first; child != nil; child = child.next {
                        dst, _ := find_child_node_by_name(parent, child.name)
                        if dst != nil do continue
                        dst = append_child_node(parent)
                        if !clone_node_recursive(dst, child) do return { .ERROR }
                    }
                    result |= { .REMOVE_NODE }
                }
                else {
                    name := node.name
                    if !clone_node_recursive(node, ref_node) do return { .ERROR }
                    node.name = name
                }
                
                node.flags |= { .REFERENCES_RESOLVED }
                return result
                
            case .INVALID:
                log("Invalid node type in validate_node_references().")
                return { .ERROR }
        }
        
        return { .ERROR }
    }
    
    iterations := 0
    for {
        iterations += 1
        result: Result = { .COMPLETE }
        child := parser.dom_root.first;
        for child != nil {
            next_child   := child.next
            child_result := recurse(parser, child)
            if .ERROR       in child_result {
                log("Error while trying to resolve node references.")
                return false
            }
            if .REMOVE_NODE in child_result {
                remove_node(child, node_allocator)
            }
            result |=  child_result & { .PROGRESS }
            result &= (child_result & { .COMPLETE }) | ~{ .COMPLETE }
            child = next_child
        }
        if .COMPLETE     in result do break
        if .PROGRESS not_in result {
            log("Unable to resolve node references.")
            return false
        }
    }
    
    log("Resolved node references in %v iterations.", iterations)
    return true
}

process_data_bindings :: proc(using parser: ^DOM_Parser) -> bool {
    return process_node_binding(parser, dom_root)
}

process_node_binding :: proc(using parser: ^DOM_Parser, node: ^DOM_Node) -> bool {
    if .BINDING_RESOLVED in node.flags do return true
    
    callback_results: Callback_Results
    for callback in callbacks {
        if callback != nil {
            callback_results |= callback(node)
            if .ERROR in callback_results {
                return false
            }
        }
    }
    if .SKIP_BINDING in callback_results {
        return true
    }
    
    #partial switch node.type {
        case .OBJECT, .ARRAY:
            for child := node.first; child != nil; child = child.next {
                if !process_node_binding(parser, child) {
                    return false
                }
            }
            return true
            
        case .FIELD: 
            if node.data_binding == nil do return true
            
            if binding_io_data, found := &IO_Data_Lookup[node.data_binding.id]; found {
                using binding_io_data.parse
                if parse_proc_2 != nil {
                    if !parse_proc_2(node.data_binding, node.value.text) {
                        fmt.println("Error, parse_proc() failed.")
                        return false
                    }
                    return true
                }
            }
            
            return set_value_from_string(node.data_binding, node.value.text)
            
        case .REF:
            assert(node.ref.node != nil, "ref node was nil")
            #partial switch node.ref.type {
                case .INDEX:
                    return node.data_binding == nil || dynamic_int_cast(node.data_binding, get_node_index(node.ref.node))
                    
                case .POINTER:
                    if node.data_binding == nil do return true
                    if node.ref.node.data_binding == nil {
                        // TODO: have some option for this to be an error
                        log("Warning: no data binding on pointer ref node '%v'.", format_node_path(node))
                        return true
                    }
                    
                    ref_ti_base     := reflect.type_info_base(type_info_of(node.ref.node.data_binding.id))
                    binding_ti_base := reflect.type_info_base(type_info_of(node.data_binding.id).variant.(runtime.Type_Info_Pointer).elem)
                    if ref_ti_base != binding_ti_base {
                        fmt.printfln("pointer type mismatch %v vs %v", ref_ti_base.id, binding_ti_base.id)
                        return false
                    }
                    (cast(^rawptr) node.data_binding.data)^ = node.ref.node.data_binding.data
                    return true
                    
                case:
                    fmt.printfln("ERROR: got a ref of type %v in process_node_binding.", node.ref.type)
                    return false
            }
            
    }
    
    return true
}

construct_dom_from_gon_file :: proc(using parser: ^DOM_Parser) -> bool {
    next_token : Token
    ok         : bool
    
    dom_root      = new(DOM_Node, node_allocator)
    dom_root.name = "root"
    dom_root.type = .OBJECT
    
    success := false
    defer if !success {
        delete_child_nodes_recursive(dom_root)
        free(dom_root, node_allocator)
    }
    
    parent := dom_root
    L_Loop: for parent != nil {
        name, text  : string
        type        : Field_Type
        flags       : DOM_Node_Flags
        source_line : int
        
        // field value ref without name inside an object will create an unnamed field with the same data binding as the parent object
        if parent.type == .OBJECT && peek_token(&tokenizer).type == .REF_VALUE {
            flags |= { .BIND_PARENT }
        } else {
            // read field name
            if parent.type != .ARRAY {
                next_token, ok = get_token(&tokenizer)
                if !ok {
                    err_token := peek_token(&tokenizer)
                    log("GON tokenization error: Unexpected %v token \"%v\" on line %v.", err_token.type, err_token.text, err_token.line)
                    return false
                }
                #partial switch next_token.type {
                    case .STRING: 
                        name = next_token.text
                        source_line = next_token.line
                        
                    case .EOF:
                        if parent != dom_root {
                            log("GON parse error: Unexpected %v token \"%v\". on line %v", next_token.type, next_token.text, next_token.line)
                            return true
                        }
                        break L_Loop
                        
                    case .OBJECT_END:
                        if parent.type != .OBJECT {
                            log("GON parse error: Unexpected %v token \"%v\" on line %v.", next_token.type, next_token.text, next_token.line)
                            return true
                        }
                        if next_token.line == parent.source_line {
                            parent.flags |= { .SAME_LINE }
                        }
                        parent = parent.parent
                        continue
                        
                    case:
                        log("GON parse error: Unexpected %v token \"%v\" on line %v.", next_token.type, next_token.text, next_token.line)
                        return true
                }
            }
        }
        
        next_token, ok = get_token(&tokenizer)
        if !ok {
            err_token := peek_token(&tokenizer)
            log("GON tokenization error: Unexpected %v token \"%v\" on line %v.", err_token.type, err_token.text, err_token.line)
            return false
        }
        
        if next_token.type == .REF_INDEX   || 
           next_token.type == .REF_POINTER || 
           next_token.type == .REF_VALUE {
            
            node := append_child_node(parent)
            node.name  = name
            node.type  = .REF
            node.flags = flags
            
            #partial switch next_token.type {
                case .REF_INDEX   : node.ref.type = .INDEX
                case .REF_POINTER : node.ref.type = .POINTER
                case .REF_VALUE   : node.ref.type = .VALUE
            }
            
            next_token, ok = get_token(&tokenizer)
            if !ok {
                err_token := peek_token(&tokenizer)
                log("GON tokenization error: Unexpected %v token \"%v\" on line %v.", err_token.type, err_token.text, err_token.line)
                return false
            }
            if next_token.type != .STRING {
                log("GON parsing error: Field ref path must be a valid string value.")
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
                    if source_line <= 0 {
                        source_line = next_token.line
                    }
                case .OBJECT_BEGIN: 
                    type = .OBJECT
                case .ARRAY_BEGIN: 
                    type = .ARRAY
                case .ARRAY_END:
                    if parent.type != .ARRAY {
                        log("GON parse error: Unexpected %v token \"%v\". on line %v", next_token.type, next_token.text, next_token.line)
                        return false
                    }
                    if next_token.line == parent.source_line {
                        parent.flags |= { .SAME_LINE }
                    }
                    parent = parent.parent
                    continue
                case:
                    fmt.printfln("GON parse error: Unexpected %v token \"%v\". on line %v", next_token.type, next_token.text, next_token.line)
                    return false
            }
            
            assert(type != .INVALID)
            
            node := append_child_node(parent)
            node.source_line = source_line
            node.name  = name
            node.type  = type
            node.flags = flags
            if node.type == .OBJECT || node.type == .ARRAY {
                parent = node
            } else {
                node.value.text = text
            }
        }
    }
    
    success = true
    return true
}

add_data_binding_to_node :: proc(node: ^DOM_Node, binding: any) -> bool  {
    if node == nil || binding.data == nil do return false
    
    if node.data_binding.data != nil {
        fmt.println("Error, node already has a data binding set...")
        return false
    }
    
    // check if we need to modify binding based on io data
    // if binding_io_data, found := *IO_Data_Lookup[binding.id]; found {
    //     using binding_io_data.parse
    //     if bind_proc != nil {
    //         binding = bind_proc(binding)
    //         if binding.data == nil {
    //             fmt.println("Error, bind_proc() returned nil.")
    //             return false
    //         }
    //     }
    // }
    
    node.data_binding = binding

    // binding, _ = deref_any_pointer(binding)
    binding_ti := runtime.type_info_base(type_info_of(binding.id))
    
    // make indirect bindings onto child nodes
    #partial switch node.type {
        case .OBJECT:
            for child := node.first; child != nil; child = child.next {
                if .BIND_PARENT in child.flags { // necessarily a value ref
                    child.data_binding = node.data_binding
                }
            }
            
            #partial switch tiv in binding_ti.variant {
                case runtime.Type_Info_Struct:
                    io_data, io_data_found := &IO_Data_Lookup[node.data_binding.id]
                    
                    name_member_name: string
                    if io_data_found do name_member_name = io_data.name_member.name
                    is_name_set := false
                    
                    for child := node.first; child != nil; child = child.next {
                        member := reflect.struct_field_by_name(node.data_binding.id, child.name) 
                        if member == {} do continue
                        if member.name == name_member_name do is_name_set = true
                        
                        member_any := any {
                            data = mem.ptr_offset(cast(^u8)node.data_binding.data, member.offset),
                            id   = member.type.id,
                        }
                        add_data_binding_to_node(child, member_any)
                    }
                    
                    L_Assign_Name: if io_data_found && !is_name_set {
                        // check parent attributes and maybe skip assigning name member
                        if node.parent != nil {
                            if .ARRAY_INDEXED in node.parent.flags do break L_Assign_Name
                        
                            if node.parent.data_binding.data != nil {
                                parent_ti := reflect.type_info_base(type_info_of(node.parent.data_binding.id))
                                _, parent_is_map := parent_ti.variant.(runtime.Type_Info_Map)
                                if parent_is_map do break L_Assign_Name
                            }
                        }
                        
                        // TODO: maybe we want error handling when name member is missing, especially if parent type is array-object 
                        if io_data.name_member != {} {
                            member_any := any {
                                data = mem.ptr_offset(cast(^u8)node.data_binding.data, io_data.name_member.offset),
                                id   = io_data.name_member.type.id,
                            }
                            if !set_value_from_string(member_any, node.name) {
                                return false
                            }
                        }
                    }
                    
                case runtime.Type_Info_Map:
                    // I suppose map key bindings are a special exception to the rule that we don't assign any values at this point in parsing
                    // this should be fine because we can't use a field ref for the name or anything funky like that, so this will not possibly have any data dependencies
                    key_member: reflect.Struct_Field
                    value_ti := runtime.type_info_base(tiv.value)
                    _, is_struct := value_ti.variant.(runtime.Type_Info_Struct) 
                    if is_struct {
                        type_io_data, found := IO_Data_Lookup[tiv.value.id]
                        if found {
                            key_member = type_io_data.map_key_member
                            if runtime.type_info_base(key_member.type) != runtime.type_info_base(tiv.key) {
                                fmt.println("Error: key member type does not match map key type.")
                                return false
                            }
                        }
                    }
                    
                    // allocate temp space for key value
                    // if key value type is a string, then set_value_from_string will also perform its own allocation in order to copy the source string
                    // it is up to the user to free this string later. 
                    // if key_member is set, the key value will be memcopied there. for a string, it will not clone the underlying data
                    key_any := dynamic_new(tiv.key.id, context.temp_allocator)
                    
                    // allocate empty space that can be safely memcopied from
                    // this has to be done because apparently there's no way to insert a hash dynamically without passing a value
                    empty_value := cast(rawptr) raw_data(make([]u8, tiv.value.size, context.temp_allocator))
                    
                    raw_map := cast(^runtime.Raw_Map) node.data_binding.data
                    for child := node.first; child != nil; child = child.next {
                        runtime.__dynamic_map_check_grow(raw_map, tiv.map_info)
                        if !set_value_from_string(key_any, child.name) {
                            return false
                        }
                        
                        value := runtime.__dynamic_map_set_without_hash(
                            raw_map, tiv.map_info, key_any.data, empty_value,
                        )
                        add_data_binding_to_node(child, any { rawptr(value), tiv.value.id })
                        
                        if key_member != {} {
                            key_member_ptr := mem.ptr_offset(cast(^u8) child.data_binding.data, key_member.offset)
                            mem.copy(key_member_ptr, key_any.data, tiv.key.size)
                        }
                    }
                    
                case runtime.Type_Info_Dynamic_Array:
                    raw_array := cast(^runtime.Raw_Dynamic_Array) node.data_binding.data
                
                    io_data, io_data_found := &IO_Data_Lookup[binding_ti.id]
                    if io_data_found && .ARRAY_INDEXED in io_data.parse.flags {
                        node.flags |= { .ARRAY_INDEXED }
                    } else {
                        elem_ti := runtime.type_info_base(tiv.elem)
                        _, is_struct := elem_ti.variant.(runtime.Type_Info_Struct) 
                        if is_struct {
                            node.flags |= { .ARRAY_AS_OBJECT }
                        }
                        // TODO: maybe error when elem type here is not a struct
                        if !reserve_any_dynamic_array(node.data_binding, node.count) { 
                            return false
                        }
                        raw_array.len = node.count
                    }
                    
                    if .ARRAY_INDEXED in node.flags {
                        for child := node.first; child != nil; child = child.next {
                            elem_index: int
                            if io_data.enum_index_type != {} {
                                elem_index = auto_cast reflect.enum_from_name_any(io_data.enum_index_type, child.name) or_return
                            } else {
                                elem_index = strconv.atoi(child.name)
                            }
                            elem_any := array_add_any_at_index(node.data_binding, elem_index)
                            add_data_binding_to_node(child, elem_any)
                        }
                    } else {
                        index := 0
                        for child := node.first; child != nil; child = child.next {
                            elem_any := any {
                                data = mem.ptr_offset(cast(^u8)raw_array.data, tiv.elem.size * index),
                                id   = tiv.elem.id,
                            }
                            add_data_binding_to_node(child, elem_any)
                            index += 1
                        }
                    }
                    
                    
                case runtime.Type_Info_Array, runtime.Type_Info_Slice, runtime.Type_Info_Enumerated_Array:
                    io_data, found := &IO_Data_Lookup[binding_ti.id]

                    data       : rawptr
                    elem_count : int
                    min_value  : int
                    max_value  : int
                    elem_ti    : ^runtime.Type_Info
                    
                    // disambiguate array/slice
                    #partial switch tiv in tiv {
                        case runtime.Type_Info_Array:
                            data       = node.data_binding.data
                            elem_count = tiv.count
                            elem_ti    = tiv.elem
                            max_value  = elem_count-1
                
                        case runtime.Type_Info_Slice:
                            raw_slice := cast(^runtime.Raw_Slice) node.data_binding.data
                            data       = raw_slice.data
                            elem_count = raw_slice.len
                            elem_ti    = tiv.elem
                            max_value  = elem_count-1
                            
                        case runtime.Type_Info_Enumerated_Array:
                            data       = node.data_binding.data
                            elem_count = tiv.count
                            elem_ti    = tiv.elem
                            min_value  = auto_cast tiv.min_value
                            max_value  = auto_cast tiv.max_value
                    }
                    
                    if found && .ARRAY_INDEXED in io_data.parse.flags {
                        node.flags |= { .ARRAY_INDEXED }
                        for child := node.first; child != nil; child = child.next {
                            elem_index: int
                            if io_data.enum_index_type != {} {
                                elem_index = auto_cast reflect.enum_from_name_any(io_data.enum_index_type, child.name) or_return
                            } else {
                                elem_index = strconv.atoi(child.name)
                            }
                            if elem_index < min_value || elem_index > max_value {
                                fmt.println("Error: array index is out of bounds.")
                                return false
                            }
                            elem_any := any {
                                data = mem.ptr_offset(cast(^u8)data, elem_ti.size * elem_index),
                                id   = elem_ti.id,
                            }
                            add_data_binding_to_node(child, elem_any)
                        }
                    } else {
                        if node.count > elem_count {
                            fmt.println("Error: too many elements in array.")
                            return false
                        }
                        
                        elem_ti_base := runtime.type_info_base(elem_ti)
                        _, is_struct := elem_ti_base.variant.(runtime.Type_Info_Struct) 
                        
                        if is_struct {
                            node.flags |= { .ARRAY_AS_OBJECT }
                            // TODO: also check the io data to see if name member is defined
                        } else {
                            fmt.println("Data binding error: object-type array must contain a struct with a defined name member.")
                            return false
                        }
                        
                        index := 0
                        for child := node.first; child != nil; child = child.next {
                            elem_index := index - min_value;
                            elem_any := any {
                                data = mem.ptr_offset(cast(^u8)data, elem_ti.size * elem_index),
                                id   = elem_ti.id,
                            }
                            add_data_binding_to_node(child, elem_any)
                            index += 1
                        }
                    }
                    
                case:
                    fmt.println("Invalid data binding, mismatched gon/internal type.")
                    return false
            }
        
        case .ARRAY:
            #partial switch tiv in binding_ti.variant {
                case runtime.Type_Info_Bit_Set:
                    if node.parent.data_binding.data == binding.data {
                        return false
                    }
                    for child := node.first; child != nil; child = child.next {
                        add_data_binding_to_node(child, node.data_binding)
                    }
                
                case runtime.Type_Info_Struct:
                    if node.count > len(tiv.names) {
                        fmt.println("Data binding error: array-type struct contains too many elements.")
                        return false
                    }
                    index := 0
                    for child := node.first; child != nil; child = child.next {
                        member_any := any {
                            data = mem.ptr_offset(cast(^u8)node.data_binding.data, tiv.offsets[index]),
                            id   = tiv.types[index].id,
                        }
                        add_data_binding_to_node(child, member_any)
                        index += 1
                    }
                    
                case runtime.Type_Info_Dynamic_Array:
                    if !reserve_any_dynamic_array(node.data_binding, node.count) { 
                        fmt.println("Data binding error: failed to reserve space in dynamic array.")
                        return false
                    }
                    
                    raw_array := cast(^runtime.Raw_Dynamic_Array) node.data_binding.data
                    raw_array.len = node.count
                    
                    index := 0
                    for child := node.first; child != nil; child = child.next {
                        elem_any := any {
                            data = mem.ptr_offset(cast(^u8)raw_array.data, tiv.elem.size * index),
                            id   = tiv.elem.id,
                        }
                        add_data_binding_to_node(child, elem_any)
                        index += 1
                    }
        
                case runtime.Type_Info_Array, runtime.Type_Info_Slice:
                    io_data, found := &IO_Data_Lookup[binding_ti.id]

                    data       : rawptr
                    elem_count : int
                    elem_ti    : ^runtime.Type_Info
                    
                    // disambiguate array/slice
                    #partial switch tiv in tiv {
                        case runtime.Type_Info_Array:
                            data       = node.data_binding.data
                            elem_count = tiv.count
                            elem_ti    = tiv.elem
                
                        case runtime.Type_Info_Slice:
                            raw_slice := cast(^runtime.Raw_Slice) node.data_binding.data
                            data       = raw_slice.data
                            elem_count = raw_slice.len
                            elem_ti    = tiv.elem
                    }
                    
                    if node.count > elem_count {
                        fmt.println("Data binding error: bounds check failed on array or slice.")
                        return false
                    }
                    
                    index := 0
                    for child := node.first; child != nil; child = child.next {
                        elem_any := any {
                            data = mem.ptr_offset(cast(^u8)data,  elem_ti.size * index),
                            id   = elem_ti.id,
                        }
                        add_data_binding_to_node(child, elem_any)
                        index += 1
                    }
                    
                case:
                    fmt.println("Data binding error: mismatched gon/internal type.")
                    return false
            }

        case .FIELD:
            #partial switch tiv in binding_ti.variant {
                case runtime.Type_Info_Integer,
                     runtime.Type_Info_Float,
                     runtime.Type_Info_Enum,
                     runtime.Type_Info_String,
                     runtime.Type_Info_Boolean:
                    
                case runtime.Type_Info_Struct: 
                    // TODO: for now, permitting all structs here
                    // in future may want to precheck that there is some custom parse proc for this type
                
                case runtime.Type_Info_Bit_Set: 
                    // For bit sets, both the enclosing array and the individual elements have the same binding
                    // For fields, we must verify that the parent binding is the same as the field binding
                    if node.parent.data_binding.data != binding.data {
                        return false
                    }
                
                // Arrays of bytes/u8 are permitted as single-valued fields so that we can parse them as strings
                case runtime.Type_Info_Array         : if tiv.elem.size != 1 do return false
                case runtime.Type_Info_Dynamic_Array : if tiv.elem.size != 1 do return false
                case runtime.Type_Info_Slice         : if tiv.elem.size != 1 do return false
                
                case:
                    fmt.println("Invalid data binding, mismatched gon/internal type.")
                    return false
            }

        case .REF:
            switch node.ref.type {
                case .VALUE: 
                    fmt.printfln("Trying to bind to a value ref node @ %v!", format_node_path(node))
                    return false
                    
                case .INDEX:
                    #partial switch tiv in binding_ti.variant {
                        case runtime.Type_Info_Integer,
                             runtime.Type_Info_Float,
                             runtime.Type_Info_Enum:
                        case:
                            return false
                    }
                    
                case .POINTER:
                    // all we can do here is check that the data binding is actually a pointer type
                    tip, ok := binding_ti.variant.(runtime.Type_Info_Pointer)
                    if !ok do return false
            }
            
        case:
            // invalid node type error?
    }
    
    return true
}
