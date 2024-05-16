
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
    
    BIND_PARENT, // used to indicate that a field assumes the parent object's binding, used for special field value ref syntax

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
    // name         : Token,
    
    data_binding : any,
    flags        : DOM_Node_Flags,
    
    type         : Field_Type,
    
    using value: struct #raw_union {
        ref: struct {
            node: ^DOM_Node,
            type: enum { VALUE, POINTER, INDEX },
        },
    
        text: string, 
        // text: Token, // will replace with token later, should do the same for name
        
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

find_node_by_path :: proc(node: ^DOM_Node, path: string) -> (^DOM_Node, int) {
    node  := node
    index := 0
    
    t: GON_Tokenizer = { file = path }
    __consume_token(&t)
    
    for node != nil {
        next, ok := get_next_token_from_path_string(&t)
        if !ok do return nil, 0
        if next.type == .EOF         do break
        if next.type == .PATH_HERE   do continue
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
    node := parent
    
    t: GON_Tokenizer = { file = path }
    __consume_token(&t)
    
    for {
        next, ok := get_next_token_from_path_string(&t)
        if !ok do return nil
        
        if __peek_token(&t).type == .EOF {
            node = append_child_node(node, prepend, allocator)
            node.name = next.text
            break
        }
        
        child, _ := find_child_node_by_name(node, next.text)
        if child != nil {
            if child.type != .OBJECT {
                return nil // error, we can't create a named subnode on an array or field type node
            }
            node = child
            continue
        }
        
        node = append_child_node(node, prepend, allocator)
        node.name = next.text
        node.type = .OBJECT
    }
    
    return node
}

get_next_token_from_path_string :: proc(t: ^GON_Tokenizer) -> (Token, bool) {
    next, ok := __get_token(t)
    if !ok do return {}, false
    
    #partial switch next.type {
        case .EOF, .PATH_PARENT, .PATH_HERE, .STRING: // no op
        case: return {}, false
    }
    
    #partial switch __peek_token(t).type {
        case .EOF        : // no op
        case .PATH_SPLIT :  __consume_token(t)
        case             : return {}, false
    }
    
    return next, true
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
    node, _ := find_node_by_path(parser.dom_root, path)
    return add_data_binding_to_node(node, binding)
    
    // should we precheck that field path is valid? will still have to verify that there are no conflicts later on
    // we will detect conflicts when actually creating the bindings to the DOM, since we can't just textually compare field paths trivially, and I don't want to do it that way anyhow
}

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
                + traverse nodes by relative field path
                + get index (parent must be array)
                + get binding value / or fallback to string value
                - get binding pointer
            - callbacks / fully custom formatting
            - expression evaluation with lead sheets integration
            
        serialization
            + plain old data, default formatting
            - sameline flag with somewhat intelligent defaults
            - indexed arrays
            - callbacks / fully custom formatting
        
    field references
    
    value and index are working at a basic level now
    
    would be nice to have for objects, but this would require modifications
        need to store ref path separate from value text maybe?
            could reuse value text and just remove from the union, or we keep that in union bc why not and just add the new data
        gets weird when parent is array
        do we need additional syntax for saying that an object should use a reference and also have additional overriding values?
        need to figure out shallow copy vs deep copy semantics, $ vs $$ ?
        
    
    
    ok, what if...
    
    what if we don't actually store the indirect data bindings on the dom node, just evaluate in immediate mode, BUT
    
    we can modify the dom to capture the data dependencies
    
    
    there are natural dependencies that exist based on the strucutre of the dom
    every parent node has a dependency on its child nodes
    the major difference though, is that unlike the parent/child relation which is one-to-many, the refnode relation is many-to-one
    when we jump to a ref node, we are doing the equivalent of just copying all those nodes recursively and attaching them in the calling node's location
    a circular dependency would result in infinitely appending nodes to the dom, if we handled it that way
    we also don't want to resolve bindings on a node more than once if possible
    
    
    
    flagging direct bindings
    
    
    only value refs impose any data dependencies
        index is always known 
        pointer relies on ref node having a data binding, otherwise its not really comprehensible
    for value ref, two possible cases,
        either there is a data binding there
        or there's not
    if ref node is field, there's no issue, because we can just retreive the string value and call it a day
        perhaps we should even resolve these simple cases before processing bindings
        
    before knowing bindings, we can check that special parent-bind object ref node thingies refer to an object node
        and thats about it
    
    currently, we are resolving references after making bindings, maybe we want to do this before
        so we don't allocate before we know the dom itself is valid
        but then we won't have bindings for type info
            cannot determine ahead of time if pointer ref is valid, as this depends on types of bindings
        can resolve refs to .FIELDs by simply overriding the type and value of the node with that of ref node
        maybe short term for objects we just duplicate the node and insert them into the object
            this is just the most simple solution
    
    I stil have no idea what to do for arrays, or if that is even valuable. 
    weirdly, whether this method of object ref'ing its not a matter of struct vs array internally, but whether it is an object or array in the gon text
    so you could use the same syntax for a map or array to include the contents of another map/array
    short term, maybe we restrict it to only operate on structs, see how it goes
    
    
    restructuring TODO:
    convert .FIELDs to .REF based on flag set in construct_dom...
    resolve references on the nodes before inserting bindings
        check for circular dependencies
        copy / overwrite nodes as needed (figure out something better later, just make it work and stop being autistic about it)
    --- by the time we are here, the file should be internally textually correct 
        more or less, perhaps we also want to have checks in future for heterogeneity of arrays or something
    insert data bindings
    process data bindings
    
    
    
*/

validate_node_references :: proc(using parser: ^DOM_Parser) -> bool {
    // wraps dom node so that we do not need to store dependent on the node itself, we can store on the stack instead.
    // then callee can traverse up the chain of dependent nodes to check for cycles
    Dependency_Node :: struct {
        node      : ^DOM_Node,
        dependent : ^Dependency_Node,
    }
    
    // we only push a dependency when we jump to a ref node
    check_node_for_dependency_cycle :: proc(using parser: ^DOM_Parser, using dep_node: ^Dependency_Node) -> bool {
        d := dependent
        for d != nil {
            if node == d.node {
                fmt.println("Circular dependency found on nodes:")
                _d := dependent
                for _d != nil {
                    fmt.printfln("\t%v", _d.node.name)
                    _d = _d.dependent
                }
                return false
            }
            d = d.dependent
        }
        
        if node.type == .OBJECT || node.type == .ARRAY {
            for child := node.first; child != nil; child = child.next {
                if !check_node_for_dependency_cycle(parser, &{ child, dependent }) {
                    return false
                }
            }
        } else {
            if .REF_VALUE in node.flags {
                if node.text == "" {
                    fmt.printfln("Empty reference on node '%v'.", node.name) // TODO: node path
                    return false
                }
                
                is_relative_path := node.text[0] == '.'
                ref_node, _ := find_node_by_path(is_relative_path ? node.parent : parser.dom_root, node.text)
                
                if ref_node == nil {
                    fmt.printfln("Invalid reference on node '%v'.", node.name) // TODO: node path
                    return false
                }
                
                if !check_node_for_dependency_cycle(parser, &{ ref_node, dep_node }) {
                    return false
                }
            }
        }
        
        return true
    }
    
    for child := parser.dom_root.first; child != nil; child = child.next {
        if !check_node_for_dependency_cycle(parser, &{ parser.dom_root, nil }) {
            return false
        }
    }
    
    return true
}

process_node_binding :: proc(using parser: ^DOM_Parser, node: ^DOM_Node) -> bool {
    if .BINDING_RESOLVED in node.flags do return true
    
    for callback in callbacks {
        if callback != nil {
            if !callback(node) {
                return false
            }
        }
    }
    
    if node.type == .OBJECT || node.type == .ARRAY {
        for child := node.first; child != nil; child = child.next {
            if !process_node_binding(parser, child) {
                return false
            }
        }
    }
    else {
        if node.data_binding.data == nil do return true
        
        if .REF_INDEX in node.flags {
            if node.text == "" do return true
            is_relative_path := node.text[0] == '.'
            ref_node, index := find_node_by_path(is_relative_path ? node.parent : parser.dom_root, node.text)
            if ref_node == nil do return true
            if !dynamic_int_cast(node.data_binding, index) do return true
        } 
        else if .REF_VALUE in node.flags {
            if node.text == "" do return true
            is_relative_path := node.text[0] == '.'
            ref_node, _ := find_node_by_path(is_relative_path ? node.parent : parser.dom_root, node.text)
            if ref_node == nil do return true
            
            // if the referenced node has a data binding, jump there to resolve it and then pull the value from it directly
            // otherwise, if it is a field, we can just get the text and assign it as normal as fallback measure
            
            // we have another problem here with objects, because we can only create a reference to an object that has a data binding
                // we could copy the entire subtree of the referenced object and create bindings to it as normal
                    // would use up much more memory
                    // most simple thing to do
                // we could make a temp binding and evaluate bindings in a sort of immediate mode?
                    // if we have multiple things that reference the same base object, we will be repeating a lot of work 
                    // we don't have a mechanism to make bindings and immediately evaluate them
                        // would probably not play nice with the rest of the system
                    // would need mechanism to remove the bindings after they are resolved
                        // while resolving, if we jumped to another ref, and then that pointed back into this object, but not to the same node, it would see a binding there
                // we could allocate space for a temp struct, bind the object to that, and then copy from that?
                    // we fundamentally can't really impose a type on data without a direct data binding
                        // we could assume the type of the first object to reference this one and assert that future things that ref to this object match the imposed type
                        // but that feels kind of bad and prevents it from working with usings
                    // but we can't exactly just memcopy for structs anyhow, so maybe it is better if we actually navigate the whole structure.
                    // if we shallow copy from a temp allocated thing, we will have big problems
                        // could just force a deep copy on temp objects, but maybe we want things to reference the same underlying data?
                        // I want there to be as much flexibility as possible in how the user can have their data be allocated
            
            // if ref_node.data_binding.data != nil {
            //     // later, we may want to make this more type matching more sophisticated, and work with usings on structs
            //     // for now, user can handle that manually
            //     if ref_node.data_binding.id != node.data_binding.id {
            //         return false
            //     }
            //     if !process_node_binding(parser, ref_node) {
            //         return false
            //     }
                
            //     // Problem! this mem copy won't allow us to have an object ref multiple other objects properly
            //     // maybe we can return a bitmap of the fields of a struct that get set when processing the binding?
            //     // also, this definitely won't work for arrays.
            //     // unfortunately, we may need a whole big switch case here for copying values
            //     // seems like switch cases end up being the largest source of code in this entire library
            //     // fortunately, these will probably be a little bit smaller in Jai due to the more simple type system
                
            //     // what if we want to clone string rather than shallow copy? (should probably be a dom node flag)
            //     mem.copy(node.data_binding.data, ref_node.data_binding.data, type_info_of(node.data_binding.id).size)
            // }
            
            if ref_node.type == .FIELD {
                if !set_value_from_string(node.data_binding, ref_node.text) {
                    return false
                }
            }
        }
        else {
            if !set_value_from_string(node.data_binding, node.text) {
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
        
        // field value ref without name inside an object will create an unnamed field with the same data binding as the parent object
        if parent.type == .OBJECT && __peek_token(t).type == .REF_VALUE {
            flags |= {.BIND_PARENT}
        } else {
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
        }
        
        // check for field refs
        has_ref := false
        #partial switch __peek_token(t).type {
            case .REF_INDEX:
                has_ref = true
                flags |= {.REF_INDEX}
                if !__consume_token(t) do return nil
            case .REF_POINTER:
                has_ref = true
                flags |= {.REF_POINTER}
                if !__consume_token(t) do return nil
            case .REF_VALUE:
                has_ref = true
                flags |= {.REF_VALUE}
                if !__consume_token(t) do return nil
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
        if has_ref && type != .FIELD {
            fmt.printfln("GON parse error: Field ref token '%v' must be followed by a field path string.", next_token.type, next_token.text)
            return nil
        }
        
        node := append_child_node(parent)
        node.name  = name
        node.type  = type
        node.flags = flags
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
            for child := node.first; child != nil; child = child.next {
                if .BIND_PARENT in child.flags { // necessarily a field and value ref
                    child.data_binding = node.data_binding
                }
            }
    
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

/*
    current iteration of tokenizer proc, need to clean up the others later
    not sure how people usually do this actually
    
    here we actually consume tokens 1 ahead of whatever is actually returned by get_token
    so we basically buffer up a token so that we can always peek a token and don't parse it out multiple times
    this is ideal for parsing out actual gon files, but wouldn't be for parsing the path strings
    in that case, we can just use lex_next_token directly
    
    the tokenizer is basically just there to act as a very thin wrapper for lex_next_token so that we don't have to think about managing state for peeking tokens pin the main parse proc
*/

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
        case '*':
            advance(file)
            return {.REF_POINTER, ""}, true
        case '$':
            advance(file)
            return {.REF_VALUE, ""}, true
    }
    
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
    
    // quoted strings
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
