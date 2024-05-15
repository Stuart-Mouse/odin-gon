
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
    and perhaps this could as do things like manage versioning of structs, etc.
*/

INDENTATION_STRING := "    "

DOM_Node_Flags :: bit_set[DOM_Node_Flag]
DOM_Node_Flag  :: enum {
    // parsing flags
    BINDING_RESOLVED,
    BINDING_ON_PATH,
    
    ARRAY_AS_OBJECT,
    ARRAY_INDEXED,
    ARRAY_ENUMERATED,
    
    // flags to denote that node is a reference to another node
    // should be mutually exclusive in practice, but I don't want to introduce a reftype enum
    // maybe we change this later if we can keep dom_node_flags as u8 or other smaller type and reftype as u8
    REF_INDEX,
    REF_POINTER,
    REF_VALUE,

    // formatting flags
    SAME_LINE,
}

// this struct is kinda big
// maybe we optimize this later, but for now just making it work
DOM_Node :: struct {
    parent       : ^DOM_Node, 
    next         : ^DOM_Node, 
    prev         : ^DOM_Node, 

    // source_location: struct { line, char: int },

    name         : string,
    // name_token_type : Token_Type,
    
    data_binding : any,
    flags        : DOM_Node_Flags,
    
    type         : Field_Type,
    
    // value is text for .FIELD, value is children for .OBJECT and .ARRAY
    using value: struct #raw_union {
        text : string,
        // text_token_type: Token_Type, 
        using children: struct { 
            first : ^DOM_Node,
            last  : ^DOM_Node,
            count : int,
        },
    },
}

// walk_nodes_depth_first :: proc(node: ^DOM_Node, walk_proc: proc(^DOM_Node, rawptr) -> bool, data: rawptr) {
//     if walk_proc == nil do return false
    
//     walk_proc(node, data) or_return
    
//     child := node.first
//     for child != nil {
//         walk_nodes_depth_first(child, walk_proc, data)
//         child = child.next
//     }
// }

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
delete_child_nodes_recursive :: proc(node: ^DOM_Node) {
    if node.type == .OBJECT || node.type == .ARRAY {
        child := node.first
        for child != nil {
            next := child.next
            delete_child_nodes_recursive(child)
            free(child)
            child = next
        }
    }
}

find_node_by_path :: proc(node: ^DOM_Node, path: string) -> ^DOM_Node {
    path := path
    node := node
    for path != "" && node != nil {
        next, remaining, ok := get_next_ident_from_path_string(path)
        if !ok do return nil
        path = remaining
        node = find_child_node_by_name(node, next)
    }
    return node
}

// will return nil if not found
find_child_node_by_name :: proc(parent: ^DOM_Node, name: string) -> ^DOM_Node {
    node := parent.first
    for node != nil {
        if node.name == name do break
        node = node.next
    }
    return node
}

append_nodes_for_indirect_bindings :: proc(node: ^DOM_Node, allocator := context.allocator) {
    if node == nil || node.data_binding.data == nil do return
    using runtime

    ti := type_info_base(type_info_of(node.data_binding.id))
    #partial switch tiv in ti.variant {
        case Type_Info_Struct: 
            member_count := len(tiv.names)
            for i in 0..<member_count {
                member_type   := tiv.types  [i]
                member_name   := tiv.names  [i]
                member_offset := tiv.offsets[i]
                
                member_any := any {
                    data = mem.ptr_offset(cast(^byte)node.data_binding.data, member_offset),
                    id   = member_type.id,
                }
                
                // figure out whether to prepend elems (will do for things that need to be attrs)
                append_data_node(node, member_name, member_any, allocator = allocator)
            }
            
            return
            
        case Type_Info_Array, Type_Info_Slice, Type_Info_Dynamic_Array: 
            data       : rawptr
            elem_count : int
            elem_ti    : ^Type_Info
            
            // disambiguate array/slice/dynamic
            #partial switch tiv in tiv {
                case Type_Info_Array:
                    data       = node.data_binding.data
                    elem_count = tiv.count
                    elem_ti    = tiv.elem
        
                case Type_Info_Slice:
                    raw_slice := cast(^runtime.Raw_Slice) node.data_binding.data
                    data       = raw_slice.data
                    elem_count = raw_slice.len
                    elem_ti    = tiv.elem
      
                case Type_Info_Dynamic_Array:
                    raw_dynamic_array := cast(^runtime.Raw_Dynamic_Array) node.data_binding.data
                    data       = raw_dynamic_array.data
                    elem_count = raw_dynamic_array.len
                    elem_ti    = tiv.elem
                    if elem_count == 0 do return // skip serializing empty dynamic arrays
            }
            
            for i in 0..<elem_count {
                elem_any := any {
                    id   = elem_ti.id,
                    data = mem.ptr_offset(cast(^byte)data, elem_ti.size * i),
                }
                
                // TODO: as indexed, as object
                
                elem_name: string = fmt.tprint(i)
                append_data_node(node, elem_name, elem_any, allocator = allocator)
            }
            
            return
            
        case Type_Info_Map:
            raw_map := transmute(^Raw_Map) node.data_binding.data
            #partial switch ti_key in runtime.type_info_base(tiv.key).variant {
                case Type_Info_String:
                    m := (^mem.Raw_Map)(node.data_binding.data)
                    
                    if m != nil {
                        if tiv.map_info == nil {
                            return
                        }
                        map_cap := uintptr(runtime.map_cap(m^))
                        ks, vs, hs, _, _ := runtime.map_kvh_data_dynamic(m^, tiv.map_info)
                        j := 0
                        for bucket_index in 0..<map_cap {
                            runtime.map_hash_is_valid(hs[bucket_index]) or_continue         
                            key   := runtime.map_cell_index_dynamic(ks, tiv.map_info.ks, bucket_index)
                            value := runtime.map_cell_index_dynamic(vs, tiv.map_info.vs, bucket_index)
                  
                            append_data_node(node, (cast(^string)key)^, any { rawptr(value), tiv.value.id })
                        }
                    }
                    
                case: 
                    fmt.printf("Unable to serialize type: %v\nCurrently, only maps with string keys are supported.", ti)
            }
                    
            return
    }
}

append_data_node :: proc(parent: ^DOM_Node, path: string, data_binding: any, prepend := false, allocator := context.allocator) ->  ^DOM_Node {    
    node := append_node_with_path(parent, path, prepend, allocator)
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

append_node_with_path :: proc(parent: ^DOM_Node, path: string = "", prepend := false, allocator := context.allocator) -> ^DOM_Node {
    path := path
    node := parent
    
    // empty path is not valid, reject it
    if path == "" do return nil
    
    for {
        next, remaining, ok := get_next_ident_from_path_string(path)
        if !ok do return nil
        path = remaining
        
        if path != "" { 
            // non-terminal node
            // find if exists
            child := find_child_node_by_name(node, next)
            if child != nil {
                if child.type != .OBJECT {
                    return nil // error, we can't create a named subnode on an array or field type node
                }
                node = child
                continue
            }
            // create if does not exist
            node = append_child_node(node, prepend, allocator)
            node.name = next
            node.type = .OBJECT
        } 
        else { 
            // terminal node
            node = append_child_node(node, prepend, allocator)
            node.name = next
            break
        }
    }
    
    return node
}

// if next returns empty, then there was an error and remaining is also not a valid value
// maybe we can use the tokenizer for this
get_next_ident_from_path_string :: proc(path: string) -> (next, remaining: string, ok: bool) {
    if path == "" do return
    remaining = path

    defer if !ok {
        next      = ""
        remaining = path
    }
    
    for is_whitespace(remaining[0]) {
        if !advance(&remaining) do return
    }
    
    if remaining[0] == '\"' || remaining[0] == '\'' {
        quote_char := remaining[0]
        
        if !advance(&remaining) do return
        next = remaining[0:]
        string_len := 0
        
        for remaining[0] != quote_char {
            adv := 1 + int(remaining[0] == '\\') // TODO: handle escape sequences more properly
            string_len += adv
            if !advance(&remaining, adv) do return
        }
        next = next[:string_len]
        if !advance(&remaining) do return
    } else {
        next = remaining[0:]
        string_len := 0
        for is_character_permitted_in_unquoted_string(remaining[0]) {
            string_len += 1
            if !advance(&remaining) do break // it is ok if we can't advance here, may have just hit end of path
        }
        next = next[:string_len]
    }
    
    if next == "" do return
    
    if remaining != "" {
        for is_whitespace(remaining[0]) {
            if !advance(&remaining) {
                ok = true
                return
            }
        }
        if remaining[0] == '/' {
            if !advance(&remaining) do return
        }
    }
    
    ok = true
    return
}

is_character_permitted_in_unquoted_string :: proc(char: u8) -> bool {
    return (char >= 'a' && char <= 'z') || 
           (char >= 'A' && char <= 'Z') || 
           (char >= '0' && char <= '9') || 
            char == '_' || 
            char == '-' || 
            char == '.'
}


// returns the unescaped character and the length of the escape sequence in characters
// parse_escape_sequence :: proc(str: string) -> (u8, int) {
    
// }

// determination is currently made based only on node's data binding and parent's data binding, but we will probably consider some other flags on the node later
// the type set here is authoritative, so when we go to actually serialize a node later, it must be serializable as this type
determine_node_type_for_serialization :: proc(node: ^DOM_Node) -> Field_Type {
    if node.data_binding.data == nil do return .OBJECT

    io_data, io_data_found := IO_Data_Lookup[node.data_binding.id]
    ti := runtime.type_info_base(type_info_of(node.data_binding.id))
    
    #partial switch tiv in ti.variant {
        case runtime.Type_Info_Integer,
             runtime.Type_Info_Float,
             runtime.Type_Info_Enum,
             runtime.Type_Info_String,
             runtime.Type_Info_Boolean:
            return .FIELD
        
        case runtime.Type_Info_Bit_Set: 
            // check if parent data binding is the same.
            if node.parent.data_binding.data == node.data_binding.data {
                return .FIELD
            }
            return .ARRAY
        
        // arrays of bytes/u8 are serialized as string
        // we will probably distinguish this later on u8 vs byte, where byte is serialized using some binary data blob
        case runtime.Type_Info_Array:
            if tiv.elem.size == 1 {
                return .FIELD
            }
            if io_data_found {
                if .AS_OBJECT               in io_data.serialize.flags ||
                   .SERIALIZE_ARRAY_INDEXED in io_data.serialize.flags {
                    return .OBJECT
                }
            }
            return .ARRAY
            
        case runtime.Type_Info_Dynamic_Array:
            if tiv.elem.size == 1 {
                return .FIELD
            }
            if io_data_found {
                if .AS_OBJECT               in io_data.serialize.flags ||
                   .SERIALIZE_ARRAY_INDEXED in io_data.serialize.flags {
                    return .OBJECT
                }
            }
            return .ARRAY
            
        case runtime.Type_Info_Slice:
            if tiv.elem.size == 1 {
                return .FIELD
            }
            if io_data_found {
                if .AS_OBJECT               in io_data.serialize.flags ||
                   .SERIALIZE_ARRAY_INDEXED in io_data.serialize.flags {
                    return .OBJECT
                }
            }
            return .ARRAY
            
        case runtime.Type_Info_Struct:
            if io_data_found {
                if .AS_ARRAY in io_data.serialize.flags {
                    return .ARRAY
                }
            }
            return .OBJECT
        
        case runtime.Type_Info_Map:
            return .OBJECT
            
        case:
            return .INVALID
    }
    
    return .INVALID
}

/*
    It seems like we really may not *need* to create nodes for all indirect bindings when serializing.
    The only reason we need to create the nodes is so that we can reorder elements, and we may attach some formatting flags to the nodes but those flags are presumably available also by checking the io data for the data binding. 
    
    not sure if I like the separationg of determining the field type and creating indirect data bindings
    the reason they are separate now is because of how I am the nodes with the type before creating indirect bindings
    
    I think though, that we can append the node, then resolve the type and create indirect bindings in a single procedure. 
        And that seems like it may be a better idea since both of those operations require similar information.
        Plus, we could more succinctly rebind data as necessary, like in treating []u8 types as strings.
    
    
    SIDE NOTE:
    
    we will have to insert a special condition when serializing a node to handle the custom formatting that's required for a bit set
    likewise for parsing a bit set from a dom also.
    
    
    Need two methods of inserting nodes into a DOM
    1. insert a node from a field, with or without a data binding set (for parsing)
    2. insert a node from a data_binding + name string (for serialization)
    
*/

DOM_Parser_Callback :: proc(^DOM_Node) -> bool

DOM_Parse_Flags :: bit_set[DOM_Parse_Flag]
DOM_Parse_Flag  :: enum {
    SKIP_PATHS_WITHOUT_BINDINGS,
}

// used to build a DOM from a text file and evaluate data bindings on that DOM
DOM_Parser :: struct {
    tokenizer      : GON_Tokenizer,
    dom_root       : ^DOM_Node,
    log            : Log_Proc,
    node_allocator : runtime.Allocator,
    callbacks      : [dynamic] DOM_Parser_Callback,
}

init_dom_parser :: proc(using parser: ^DOM_Parser, _file: string, _allocator := context.allocator) {
    node_allocator = _allocator
    tokenizer.file = _file
    __consume_token(&tokenizer) // get the first token when we init, we always pull one token ahead of the one we return
}

/*
    We are no longer appending to a dynamic array of data bindings, instead just inserting those data bindings immediately when this is called by the user.
    Which is nice because that means we save a little bit of memory on that and we don't need the Data_Binding struct anymore.
    We also don't have to split the path into substrings, since we just process it one piece at a time as we insert the binding.
*/
add_data_binding_to_dom_parser :: proc(using parser: ^DOM_Parser, binding: any, path: string) -> bool {
    node := find_node_by_path(parser.dom_root, path)
    return add_data_binding_to_node(node, binding)
    
    // should we precheck that field path is valid? will still have to verify that there are no conflicts later on
    // we will detect conflicts when actually creating the bindings to the DOM, since we can't just textually compare field paths trivially, and I don't want to do it that way anyhow
}

/*
    Steps in parsing:
    
    read tokens and append all nodes
    insert data bindings into dom nodes
        check data type compatibility
        maybe we should actually go ahead and set any data binding values that we can while we are here?
            because we already have to allocate space for values in dynamic arrays and such so that we can create all the indirect bindings to child nodes
            it doesn't necessarily matter that we check everything before making any allocations, so long as we keep a list of the allocations we make so that we can free everything when an error occurs
                but that list itself will require more allocations, albeit temporary ones
            one way we could maybe reduce the size of the dom node struct is to store a *node in the data binding instead of duplicating the binding data in the node
                this would acutally use less memory overall anyhow, since the node has to store pointer + typeid for the binding
                the inconvenience here maybe is that we can't walk the dom and see the bindings, we would have to linear search the bindings array for a match to the current node
                    which could possibly be bad for callbacks that want to do things with the dom nodes? if we even do that...
                this would also allow for having multiple bindings to the same node, which could be fine/useful even
                    e.g. two entity templates bind to the same base template object and then also bind to individual objects that override particular members
                        seems like kind of a weird meta solution that just takes advantage of how the parser is structured
                        this could also be acheived in gon syntax with field refs, probably
                            just opens up the can of worms of $ working on objects
                we could store any field ref for data dependency on the binding as well
                one major problem is that if we aren't walking the dom in order to visit nodes, 
                    resolving data dependencies becomes far more complicated because we have to worry about 
                    ok, so maybe this is actually a reason that we want to perform all allocations before setting any data, 
            short answer, no because of field ref evaluation
        if value uses field reference, save this and resolve later
        
    resolve field references / data dependencies
        it's possible there's a circular dependency in which case we should error
        better to do this before setting any values, the idea is that every thing is correct before we start allocating
            moot point, we have to allocate in order to make the indirect data data bindings earlier in the process
            
    set data from text values of fields
        run callbacks when walking dom similar to what we have in sax mode
    
    the issue of field refs
    
    i want a gon file to be totally statically defined such that the order of evaluation of the data bindings in the file does not matter
    or well, i dont actually know, but we need to have a well defined answer for the order of evaluation here if there are going to be data dependencies between fields
    
    and the answer will depend on whether we decied to finalize data bindings by walking the dom in order or by following the order in which data bindings are appended.
    also on what is the procedure for resolving individual data dependencies 
    
    orig plan to resolve a field ref is to just jump to a field in the dom when referenced and try to get the value needed from it
        if that node then needs to be resolved, then we just jump to the next node and repeat
        will have to pass orig node so that we know when we hit a circular dependency
        this jumping between nodes will require that we have space already allocated for the values produced by resolving some node
            not for the * and & refs, but for $ refs, unless we restrict that $ is only used to reference simple fields
            if we allow $ to be used with object / array types, that's really what creates the entire issue here,
                because then we are reliant on everything within that object being resolved, which is where we could hit weird ordering issues
        if this process is completely nonlinear, then maybe it doesn't matter if the data binding process is linear?
    
    if we want to be able to jump around the file to resolve field refs, then we need all the data bindings to be in place first
    so we do at least need to have the separation between the step of putting the bindings on the fields and actually processing the bindings
    
    we will need to set a flag on nodes when data binding has been resolved, or just remove the binding data from the node
        otherwise, we could repeat work on an already processed node that we had previously jumped to as a field ref
    
    how to handle field refs structurally in dom node?
    if something uses a ref, we don't actually know the type of the node yet
    maybe we consider this its own type? 
    still havent figured out syntax for object/array that uses field ref
        for objects, would be nice to do field ref + more data
            if we do that though, we run into a question of whether or not to deep copy or shallow copy structures
        getting field ref from an array doesn't really seem to make any sense
            then again, e.g. the animation frames arrays for entity templates, where I wanted to do 
                shallow copy of walk to jump and fall
                deep copy of green koopa with offsets added to frames

    
*/


/*
    Callbacks for the DOM parser
    
    different kinds: 
        run for each node when walking dom
            field, obj_start, obj_end
        run when creating a data binding to a dom node
            may be able to handle most of these cases with io data based on data type
    
    sax parser features
        parsing
            parse array indexed
            custom parse proc
                takes parser state and field, returns error code
                can manipulate field however it wants
                    equivalent with dom is modifying node, need to consider lifetime of node and such
        serialization
            skip serializing empty fields
            skip serializing empty subfields of object/array
            serialize object as an array
            serialize array as an object
            serialize obj/arr on one line
            serialize array as object with index as name
            
    dom parser features
        parsing
            + plain old data, default formatting
            + indexed arrays
            + arrays of named objects
            + map types
                - support key types other than string
                + store key value to map key member (need to not duplicate string here, so that user can free)
            - enumerated arrays
            - indexing normal arrays with enums?
                - just add enum typeid in io_data for array ezpz
            - field refs
                - get index (parent must be array)
                - get binding pointer
                - get binding value
            - callbacks / fully custom formatting
            - expression evaluation with lead sheets integration
            
        serialization
            + plain old data, default formatting
            - sameline flag with somewhat intelligent defaults
            - callbacks / fully custom formatting
        
*/

process_node_bindings :: proc(using parser: ^DOM_Parser, node: ^DOM_Node) -> bool {
    for child := node.first; child != nil; child = child.next {
        for callback in callbacks {
            if callback != nil {
                if !callback(child) {
                    return false
                }
            }
        }
    
        if child.type == .OBJECT || child.type == .ARRAY {
            process_node_bindings(parser, child)
        } else {
            // TODO: insert handling for field refs here
            if !set_value_from_string(child.data_binding, child.text) {
                return false
            }
        }
    }
    return true
}

construct_dom_from_gon_file :: proc(t: ^GON_Tokenizer) -> (^DOM_Node) {
    next_token: Token
    ok        : bool
    
    root := new(DOM_Node)
    root.name = "root"
    root.type = .OBJECT
    
    success := false
    defer if !success {
        delete_child_nodes_recursive(root)
        free(root)
    }
    
    parent := root
    L_Loop: for parent != nil {
        name, text : string
        type  : Field_Type
        flags : DOM_Node_Flags
        
        // check for field refs
        next_token = __peek_token(t)
        #partial switch next_token.type {
            case .REF_INDEX:
                flags |= {.REF_INDEX}
                if !__consume_token(t) do return nil
        }
        
        // read field name
        if parent.type != .ARRAY {
            next_token, ok = __get_token(t)
            if !ok {
                fmt.printfln("GON tokenization error: Unexpected %v token \"%v\".", next_token.type, next_token.text)
                return nil
            }
            #partial switch next_token.type {
                case .STRING: 
                    name = next_token.text
                case .EOF:
                    if parent != root {
                        fmt.printfln("GON parse error: Unexpected %v token \"%v\".", next_token.type, next_token.text)
                        return nil
                    }
                    break L_Loop
                case .OBJECT_END:
                    if parent.type != .OBJECT {
                        fmt.printfln("GON parse error: Unexpected %v token \"%v\".", next_token.type, next_token.text)
                        return nil
                    }
                    parent = parent.parent
                    continue
                case:
                    fmt.printfln("GON parse error: Unexpected %v token \"%v\".", next_token.type, next_token.text)
                    return nil
            }
        }

        // read field value
        next_token, ok = __get_token(t)
        if !ok {
            fmt.printfln("GON tokenization error: Unexpected %v token \"%v\".", next_token.type, next_token.text)
            return nil
        }
        #partial switch next_token.type {
            case .STRING: 
                type = .FIELD
                text = next_token.text
            case .OBJECT_BEGIN: 
                type = .OBJECT
            case .ARRAY_BEGIN: 
                type = .ARRAY
            case .ARRAY_END:
                if parent.type != .ARRAY {
                    fmt.printfln("GON parse error: Unexpected %v token \"%v\".", next_token.type, next_token.text)
                    return nil
                }
                parent = parent.parent
                continue
            case:
                fmt.printfln("GON parse error: Unexpected %v token \"%v\".", next_token.type, next_token.text)
                return nil
        }
        
        assert(type != .INVALID)
        
        node := append_child_node(parent)
        node.name = name
        node.type = type
        if node.type == .OBJECT || node.type == .ARRAY {
            parent = node
        } else {
            node.text = text
        }
    }
    
    success = true
    return root
}

add_data_binding_to_node :: proc(node: ^DOM_Node, binding: any) -> bool  {
    if node == nil || binding.data == nil do return false

    // binding, _ = deref_any_pointer(binding)
    binding_ti := runtime.type_info_base(type_info_of(binding.id))
    
    if !is_binding_valid(node, binding) {
        return false
    }
    node.data_binding = binding
    
    // we use the same switch structure here as is used in is_binding_valid
    // unless we will also use this is_binding_valid elsewhere, we should just do it all inline here
    // make indirect bindings onto child nodes
    #partial switch node.type {
        case .OBJECT:
            #partial switch tiv in binding_ti.variant {
                case runtime.Type_Info_Struct:
                    for child := node.first; child != nil; child = child.next {
                        member := reflect.struct_field_by_name(node.data_binding.id, child.name) 
                        if member == {} do continue
                        member_any := any {
                            data = mem.ptr_offset(cast(^u8)node.data_binding.data, member.offset),
                            id   = member.type.id,
                        }
                        add_data_binding_to_node(child, member_any)
                    }
                    
                    // TODO: maybe we want error handling when name member is missing
                    if .ARRAY_AS_OBJECT in node.parent.flags {
                        type_io_data, found := IO_Data_Lookup[node.data_binding.id]
                        if found {
                            member := reflect.struct_field_by_name(node.data_binding.id, type_io_data.name_member) 
                            if member != {} {
                                member_any := any {
                                    data = mem.ptr_offset(cast(^u8)node.data_binding.data, member.offset),
                                    id   = member.type.id,
                                }
                                if !set_value_from_string(member_any, node.name) {
                                    return false
                                }
                            }
                        }
                    }
                    
                case runtime.Type_Info_Map:
                    // currently, only map[string] T types are supported, will support other key types later
                    if tiv.key.id != typeid_of(string) {
                        return false
                    }
                    
                    // I suppose map key bindings are a special exception to the rule that we don't assign any values at this point in parsing
                    // this should be fine because we can't use a field ref for the name or anything funky like that, so this will not possibly have any data dependencies
                    key_member: reflect.Struct_Field
                    value_ti := runtime.type_info_base(tiv.value)
                    _, is_struct := value_ti.variant.(runtime.Type_Info_Struct) 
                    if is_struct {
                        type_io_data, found := IO_Data_Lookup[tiv.value.id]
                        if found {
                            key_member = reflect.struct_field_by_name(value_ti.id, type_io_data.map_key_member) 
                        }
                    }
                    
                    // in order to support other key types, we need to have some kind of dynamic_new() proc
                    // we can use the temp allocator to allocatate space for one item that we reuse for all chidren, like we do for the empty value
                    // may as well make the acutal proc and have it return an any, so that we can pass this to set_value_from_string()
                    // also, we will still only be able to use simple data types like ints, floats, enums since those are the only things we can represent in a gon name
                    // this is sort of a low priority feature tbh
                    // also, maybe we just use a fixed 16 bytes on stack for the key value, since we won't have any types larger than an i128 or string
                    
                    empty_value := cast(rawptr) raw_data(make([]u8, tiv.value.size, context.temp_allocator))
                    for child := node.first; child != nil; child = child.next {
                        raw_map := cast(^runtime.Raw_Map) node.data_binding.data
                        
                        // We copy the name here with the understanding that if map_key_member is not set in io data, 
                        // then the user needs to free the keys manually, as though the map itself owns the keys
                        name_copy := strings.clone(child.name)
                        key := cast(rawptr) &name_copy
                        
                        runtime.__dynamic_map_check_grow(raw_map, tiv.map_info)
                        
                        // allocate empty space that can be safely memcopied from
                        // this has to be done because apparently there's no way to insert a hash dynamically without passing a value
                        value := runtime.__dynamic_map_set_without_hash(
                            raw_map, tiv.map_info, key, empty_value,
                        )
                        add_data_binding_to_node(child, any { rawptr(value), tiv.value.id })
                        
                        if key_member != {} {
                            key_binding := any {
                                data = mem.ptr_offset(cast(^u8)child.data_binding.data, key_member.offset),
                                id   = key_member.type.id,
                            }
                            if !set_value_from_string(key_binding, name_copy, no_copy = true) {
                                return false
                            }
                        }
                    }
                    
                /*
                    May be better to switch on internal type first and then switch on GON field type, 
                    since for arrays, most of the code is shared in common and we only have a bit of extra handling for objects.
                */
                case runtime.Type_Info_Dynamic_Array:
                    raw_array := cast(^runtime.Raw_Dynamic_Array) node.data_binding.data
                
                    io_data, found := &IO_Data_Lookup[binding_ti.id]
                    if found && .PARSE_ARRAY_INDEXED in io_data.parse.flags {
                        node.flags |= { .ARRAY_INDEXED }
                        // highest_index := 0
                        // for child := node.first; child != nil; child = child.next {
                        //     highest_index =  // we would have to strconv here, don't want to repeat that work... but also don't want to store index value on node
                        // }
                    } else {
                        elem_ti := runtime.type_info_base(tiv.elem)
                        _, is_struct := elem_ti.variant.(runtime.Type_Info_Struct) 
                        if is_struct {
                            node.flags |= { .ARRAY_AS_OBJECT }
                        }
                        if !reserve_any_dynamic_array(node.data_binding, node.count) { 
                            return false
                        }
                        raw_array.len = node.count
                    }
                    
                    index := 0
                    for child := node.first; child != nil; child = child.next {
                        elem_any: any
                        if .ARRAY_INDEXED in node.flags {
                            elem_index := strconv.atoi(child.name)
                            elem_any = array_add_any_at_index(node.data_binding, elem_index)
                        } else {
                            elem_any = any {
                                data = mem.ptr_offset(cast(^u8)raw_array.data, tiv.elem.size * index),
                                id   = tiv.elem.id,
                            }
                        }
                        add_data_binding_to_node(child, elem_any)
                        index += 1
                    }
                    
                case runtime.Type_Info_Array:
                    io_data, found := &IO_Data_Lookup[binding_ti.id]
                    if found && .PARSE_ARRAY_INDEXED in io_data.parse.flags {
                        node.flags |= { .ARRAY_INDEXED }
                    } else {
                        elem_ti := runtime.type_info_base(tiv.elem)
                        _, is_struct := elem_ti.variant.(runtime.Type_Info_Struct) 
                        if is_struct {
                            node.flags |= { .ARRAY_AS_OBJECT }
                        }
                    }
                    index := 0
                    for child := node.first; child != nil; child = child.next {
                        elem_index := index
                        if .ARRAY_INDEXED in node.flags {
                            elem_index = strconv.atoi(child.name)
                            if elem_index >= tiv.count {
                                return false
                            }
                        }
                        elem_any := any {
                            data = mem.ptr_offset(cast(^u8)node.data_binding.data, tiv.elem.size * elem_index),
                            id   = tiv.elem.id,
                        }
                        add_data_binding_to_node(child, elem_any)
                        index += 1
                    }
        
                case runtime.Type_Info_Slice:
                    io_data, found := &IO_Data_Lookup[binding_ti.id]
                    if found && .PARSE_ARRAY_INDEXED in io_data.parse.flags {
                        node.flags |= { .ARRAY_INDEXED }
                    } else {
                        elem_ti := runtime.type_info_base(tiv.elem)
                        _, is_struct := elem_ti.variant.(runtime.Type_Info_Struct) 
                        if is_struct {
                            node.flags |= { .ARRAY_AS_OBJECT }
                        }
                    }
                    raw_slice := cast(^runtime.Raw_Slice) node.data_binding.data
                    index := 0
                    for child := node.first; child != nil; child = child.next {
                        elem_index := index
                        if .ARRAY_INDEXED in node.flags {
                            elem_index = strconv.atoi(child.name)
                            if elem_index >= raw_slice.len {
                                return false
                            }
                        }
                        elem_any := any {
                            data = mem.ptr_offset(cast(^u8)raw_slice.data,  tiv.elem.size * elem_index),
                            id   = tiv.elem.id,
                        }
                        add_data_binding_to_node(child, elem_any)
                        index += 1
                    }
                    /* 
                        TODO: 
                        GON objects can only validly be bound to arrays when the element type is a struct,
                        or if it is an indexed array (where the name of each field is the index to which the value will be stored).
                        So, we should perform a check to ensure that these conditions are met, else return an error.
                        The user will have to state explicitly that they want to parse a given array binding as an indexed array, otherwise there is some ambiguity as to how to handle ths situation.
                    */
            }
        
        case .ARRAY:
            #partial switch tiv in binding_ti.variant {
                case runtime.Type_Info_Bit_Set:
                    for child := node.first; child != nil; child = child.next {
                        add_data_binding_to_node(child, node.data_binding)
                    }
                
                case runtime.Type_Info_Struct:
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
        
                case runtime.Type_Info_Array:
                    elem_ti := runtime.type_info_base(tiv.elem)
                    index := 0
                    for child := node.first; child != nil; child = child.next {
                        elem_any := any {
                            data = mem.ptr_offset(cast(^u8)node.data_binding.data, tiv.elem.size * index),
                            id   = tiv.elem.id,
                        }
                        add_data_binding_to_node(child, elem_any)
                        index += 1
                    }
        
                case runtime.Type_Info_Slice:
                    raw_slice := cast(^runtime.Raw_Slice) node.data_binding.data
                    index := 0
                    for child := node.first; child != nil; child = child.next {
                        elem_any := any {
                            data = mem.ptr_offset(cast(^u8)raw_slice.data,  tiv.elem.size * index),
                            id   = tiv.elem.id,
                        }
                        add_data_binding_to_node(child, elem_any)
                        index += 1
                    }
            }
            
        case:
            // invalid node type error?
    }
    
    return true
}


is_binding_valid :: proc(node: ^DOM_Node, binding: any) -> bool {
    binding_ti := runtime.type_info_base(type_info_of(binding.id))
    #partial switch node.type {
        case .FIELD:
            #partial switch tiv in binding_ti.variant {
                case runtime.Type_Info_Integer,
                     runtime.Type_Info_Float,
                     runtime.Type_Info_Enum,
                     runtime.Type_Info_String,
                     runtime.Type_Info_Boolean:
                    return true
                
                case runtime.Type_Info_Bit_Set: 
                    // For bit sets, both the enclosing array and the individual elements have the same binding
                    // For fields, we must verify that the parent binding is the same as the field binding
                    if node.parent.data_binding.data == binding.data {
                        return true
                    }
                
                // Arrays of bytes/u8 are permitted as single-valued fields so that we can parse them as strings
                case runtime.Type_Info_Array         : if tiv.elem.size == 1 do return true
                case runtime.Type_Info_Dynamic_Array : if tiv.elem.size == 1 do return true
                case runtime.Type_Info_Slice         : if tiv.elem.size == 1 do return true
            }
            
        case .OBJECT:
            #partial switch tiv in binding_ti.variant {
                case runtime.Type_Info_Array,
                     runtime.Type_Info_Dynamic_Array,
                     runtime.Type_Info_Slice,
                     runtime.Type_Info_Map,
                     runtime.Type_Info_Struct:
                    return true
            }
            
        /* 
            For array cases, we precheck the length of the gon array against that of the internal data type.
            This can't be done for dynamic arrays obviously, but for those we will pre-reserve space for the required number of elements when we assign a data binding.
            We also precheck the array length for structs, since we know something is wrong if there are more values specified in the GON array than there are fields in the struct.
        */
        case .ARRAY:
            #partial switch tiv in binding_ti.variant {
                case runtime.Type_Info_Array:
                    if node.count >= tiv.count {
                        return true
                    }
                
                case runtime.Type_Info_Slice:
                    // maybe add some check to see if these are supposed to be parsed as indexed or something
                    // In general, parsing is designed to be a bit more lax about accepting input, so long as it is valid GON
                    // but we probably want to have some settings around this in particular
                    raw_slice := cast(^runtime.Raw_Slice) binding.data
                    if node.count >= raw_slice.len {
                        return true
                    }

                case runtime.Type_Info_Dynamic_Array:
                    return true
                    
                case runtime.Type_Info_Bit_Set:
                    // For bit sets, both the enclosing array and the individual elements have the same binding
                    // For arrays, we must verify that the this field's binding is NOT the same as its parent's binding
                    // (If it is, that means we have another array nested inside our bit set, which is not valid syntax)
                    if node.parent.data_binding.data != binding.data {
                        return true
                    }
                
                case runtime.Type_Info_Struct:
                    if node.count >= len(tiv.names) {
                        return true
                    }
                    return true
            }
            
        case:
            // TODO: invalid node type error?
    }
    
    // log("Unable to bind node \"%v\" of type %v to data of type: %v", node.name, node.type, binding.id)
    // TODO: print node address
    
    return false
}

// current iteration of tokenizer proc, need to clean up the others later
// we may be able to consolidate these and just have a peek bool param, assuming that's acutally better somehow

__consume_token :: proc(using t: ^GON_Tokenizer) -> bool {
    if next_token.type == .EOF do return true
    ok: bool
    next_token, ok = lex_next_token(&file)
    return ok
}

__get_token :: proc(using t: ^GON_Tokenizer) -> (Token, bool) {
    current_token := next_token
    return current_token, __consume_token(t)
}

__peek_token :: proc(using t: ^GON_Tokenizer) -> Token {
    return next_token
}

// mutates the passed string, advancing it to the position after the returned token
lex_next_token :: proc(file: ^string) -> (Token, bool) {
    if len(file^) <= 0                     do return {.EOF, ""}, true
    if !skip_whitespace_and_comments(file) do return {.EOF, ""}, true
  
    switch file^[0] {
        case '{':
            advance(file)
            return {.OBJECT_BEGIN, ""}, true
        case '}':
            advance(file)
            return {.OBJECT_END, ""}, true
        case '[':
            advance(file)
            return {.ARRAY_BEGIN, ""}, true
        case ']':
            advance(file)
            return {.ARRAY_END, ""}, true
        case '&':
            advance(file)
            return {.REF_INDEX, ""}, true
    }
  
    
    
    is_numeric :: proc(char: u8) -> bool {
        return char >= '0' && char <='9'
    }
    
    is_alpha :: proc(char: u8) -> bool {
        return (char >= 'a' && char <='z' ) || (char >= 'A' && char <='Z')
    }
    
    if file^[0] == '\"' || file^[0] == '\'' { // quoted strings
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
    else if is_numeric(file^[0]) || file^[0] == '-' { // number
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
    else if is_alpha(file^[0]) || file^[0] == '_' { // identifier
        string_value := file^[0:]
        string_len := 0
        
        for is_alpha(file^[0]) || is_numeric(file^[0]) || file^[0] == '_' {
            string_len += 1
            if !advance(file) do break
        }
        
        return {.STRING, string_value[:string_len]}, true
    }
  
    // // scan for end of string in quotation marks
    // // TODO: replace this with parse_quoted_string() or whatever
    // if file^[0] == '\"' {
    //     if !advance(file) do return {.EOF, ""}, false
    //     string_value = string_value[1:]
    //     string_len := 0
        
    //     for file^[0] != '\"' {
    //         adv : int = 1
    //         if file^[0] == '\\' do adv = 2
    //         if !advance(file, adv) do return {.EOF, ""}, false
    //         string_len += adv
    //     }
    //     advance(file) // step over closing quotation mark
    
    //     return {.STRING, string_value[:string_len]}, true
    // }
    
    // // scan for end of bare string
    // // TODO: also extract out logic for parsing ident/number
    // if !is_reserved_char(file^[0]) {
    //     string_len := 0
    //     for !is_reserved_char(file^[0]) && !is_whitespace(file^[0]) {
    //         if !advance(file) {
    //             return {.EOF, ""}, false
    //         }
    //         string_len += 1
    //     }
    //     return {.STRING, string_value[:string_len]}, true
    // }
  
    // there's probably some funky character in the file...?
    // now that we have more strict rules around what can be in an ident/number/etc., we are going to need more complex error handling
    // TODO: handle new error cases here
    fmt.printfln("Invalid token '%v' encountered.\n", file^)
    return {.INVALID, ""}, false
}
