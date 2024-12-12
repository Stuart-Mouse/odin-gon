package gon

import xml "core:encoding/xml"
import "core:os"
import "core:fmt"
import "base:runtime"
import "core:strconv"
import "core:mem"
import "core:reflect"
import "core:strings"

// do data bindings to an xml dom
// the dumbest possible implementation for now...
parse_from_xml_dom :: proc(using ctxt: ^Parser, doc: ^xml.Document) -> bool {
    // split the paths for all data bindings before parsing
    for &b in data_bindings {
        b._field_path = strings.split(b.field_path, "/", allocator = context.temp_allocator)
    }
    
    elem_id : xml.Element_ID
    found   : bool
    
    loop: for &b in data_bindings {
        elem_id = 0
        for b._path_depth < len(b._field_path)-1 {
            elem_id, found = xml.find_child_by_ident(doc, elem_id, b._field_path[b._path_depth])
            if !found {
                b._path_depth = 0
                continue loop
            }
            b._path_depth += 1
        }
        process_data_binding_xml(ctxt, doc, elem_id, b)
    }
    
    return true
}

/*
    This proc is a bit weird because unlike the SAX parse proc, where we always get a field and then determine data bindings, we actually have the data binding first here and then search for the field.
    But then for indirect bindings, we actually have to go back to knowing what the fields are and then checking for a matching indirect binding
    so these two things are kind of at odds, not so fun to switch between 
    we could first check if a data binding is provided, then searching for the required field
    or if it is not provided but parent object is, then we search for the data binding
    then we will know both and can factor out the call to process data binding
    this is still maybe not perfect, but should be the most straightforward/comprehensible solution
    
    ok, so this is still overconvoluted and bad. may be able to improve by factoring some things out
*/
process_data_binding_xml :: proc(using ctxt: ^Parser, doc: ^xml.Document, parent_elem_id: xml.Element_ID, binding: Data_Binding, _field: ^SAX_Field = nil) -> bool {
    // if we know neither the data binding nor the parent object, then we can't proceed
    if binding.binding.data == nil && _field == nil {
        return false
    }
    
    field   : SAX_Field
    elem_id : xml.Element_ID    // we need this later if handling an object/array type field
    
    if binding.binding.data != nil {    // direct binding, we construct the field to match the data binding
        // search attributes first
        attr, found := xml.find_attribute_val_by_key(doc, parent_elem_id, binding._field_path[binding._path_depth])
        if found {
            field = SAX_Field {
                name         = binding._field_path[len(binding._field_path)-1],
                value        = attr,
                type         = .FIELD,
                data_binding = binding.binding,
            }
            return process_data_binding(ctxt, &field)
        }
        
        // then seach nested elements
        elem_id, found = xml.find_child_by_ident(doc, parent_elem_id, binding._field_path[binding._path_depth])
        if found {
            field = SAX_Field {
                name         = binding._field_path[len(binding._field_path)-1],
                data_binding = binding.binding,
            }
            
            // check if elem is single-valued or contains nested elems
            elem := doc.elements[elem_id]
            if len(elem.value) == 0 {
                return true
            }
            
            string_value, is_string := elem.value[0].(string);
            if is_string {
                field.value = string_value
                field.type  = .FIELD
            } else {
                // no array types in xml, may need to switch this to array depending on destination data binding...
                field.type  = .OBJECT
            }
        } else {
            return true
        }
    }
    else if _field != nil {     // indirect binding, we find the data binding to match the provided field
        field = _field^         // copy the provided field
        
        if !check_for_indirect_data_binding(ctxt, &field, field.parent) {
            return false
        }
    }
    
    // by now, we must have a properly constructed field to process
    if field.data_binding.data == nil do return true

    type_io_data, found := IO_Data_Lookup[field.data_binding.id]
    if found {
        field.io_data = type_io_data
    }
    
    if !process_data_binding(ctxt, &field) {
        return false
    }
    
    if field.type == .OBJECT || field.type == .ARRAY {
        if elem_id != 0 { // todo: make this an error case if elem_id == 0
            elem := doc.elements[int(elem_id)]
            for attr in elem.attribs {
                sub_field: SAX_Field = {
                    name   = attr.key,
                    value  = attr.val,
                    type   = .FIELD,
                    parent = &field,
                }
                if !process_data_binding_xml(ctxt, doc, 0, {}, _field = &sub_field) do return false
            }
            
            for sub_elem_value in elem.value {
                sub_elem_id := sub_elem_value.(xml.Element_ID)
                sub_elem    := doc.elements[int(sub_elem_id)]
                
                sub_field: SAX_Field = {
                    name   = sub_elem.ident,
                    parent = &field
                }
                
                // check if elem is single-valued or contains nested elems
                if len(sub_elem.value) == 0 {
                    return true
                }
                
                string_value, is_string := sub_elem.value[0].(string);
                if is_string {
                    sub_field.value = string_value
                    sub_field.type  = .FIELD
                } else {
                    // no array types in xml, may need to switch this to array depending on destination data binding...
                    sub_field.type  = .OBJECT
                }
                
                if !process_data_binding_xml(ctxt, doc, sub_elem_id, {}, _field = &sub_field) do return false
            }
        }
    }
    
    return true
}

// process_direct_binding_xml :: proc(using ctxt: ^Parser, doc: ^xml.Document, parent_elem_id: xml.Element_ID, binding: Data_Binding, _field: ^SAX_Field = nil) -> bool  {
//     // search attributes first
//     attr, found := xml.find_attribute_val_by_key(doc, parent_elem_id, binding._field_path[binding._path_depth])
//     if found {
//         field = SAX_Field {
//             name         = binding._field_path[len(binding._field_path)-1],
//             value        = attr,
//             type         = .FIELD,
//             data_binding = binding.binding,
//         }
        
//         // since field is necessarily .FIELD, just process binding directly and return
//         return process_data_binding(ctxt, &field)
//     }
    
//     // then seach nested elements
//     elem_id, found = xml.find_child_by_ident(doc, parent_elem_id, binding._field_path[binding._path_depth])
//     if found {
//         field = SAX_Field {
//             name         = binding._field_path[len(binding._field_path)-1],
//             data_binding = binding.binding,
//         }
        
//         // check if elem is single-valued or contains nested elems
//         elem := doc.elements[elem_id]
//         if len(elem.value) == 0 {
//             return true
//         }
        
//         string_value, is_string := elem.value[0].(string);
//         if is_string {
//             field.value = string_value
//             return process_data_binding(ctxt, &field)
//         }
        
//         // no array types in xml, may need to switch this to array depending on destination data binding...
//         field.type  = .OBJECT
//         return process_object_binding_xml(ctxt, doc, elem_id)
//     }
    
//     return true
// }

// // by the time we are processing a data binding to an object/array, it doesn't matter whether the bindingwas made directly or indirectly, we are just naviagting the DOM and making any further bindings we can
// // wo this gets called as the common case for objects in both the processing procs for direct and indirect data bindings
// process_object_binding_xml :: proc(using ctxt: ^Parser, doc: ^xml.Document, parent_elem_id: xml.Element_ID) -> bool {
//     elem := doc.elements[int(parent_elem_id)] // should we range check here?
//     for attr in elem.attribs {
//         sub_field: SAX_Field = {
//             name   = attr.key,
//             value  = attr.val,
//             type   = .FIELD,
//             parent = &field,
//         }
//         if !process_data_binding_xml(ctxt, doc, 0, {}, _field = &sub_field) do return false
//     }
    
//     for sub_elem_value in elem.value {
//         sub_elem_id := sub_elem_value.(xml.Element_ID)
//         sub_elem    := doc.elements[int(sub_elem_id)]
        
//         sub_field: SAX_Field = {
//             name   = sub_elem.ident,
//             parent = &field
//         }
        
//         // check if elem is single-valued or contains nested elems
//         if len(sub_elem.value) == 0 {
//             return true
//         }
        
//         string_value, is_string := sub_elem.value[0].(string);
//         if is_string {
//             sub_field.value = string_value
//             sub_field.type  = .FIELD
//         } else {
//             // no array types in xml, may need to switch this to array depending on destination data binding...
//             sub_field.type  = .OBJECT
//         }
        
//         if !process_data_binding_xml(ctxt, doc, sub_elem_id, {}, _field = &sub_field) do return false
//     }
// }