

## Overview

This GON library supports both DOM-style and SAX-style parsing.
In addition, it uses the reflection capabilities of the language to support a very simple "data-binding" interface out of the box, without the need to write your own special callbacks. 

Data bindings provide a mapping from the fields in a GON file to the internal data types that your program uses. 
In addition to the direct data bindings that the user defines, indirect bindings will be made recursively on structs and arrays automatically.


## ToDo

serialization of map types
serialization of objects through pointers (may require the use a special struct member note)

### IMGUI Integration

create a modal window for viewing/editting any data as GON
user can exit the window without saving or may overwrite the original data.
In order to prevent memory issues, we may add options to the window to disable editting of certain types of data.

In TreeNodeAny, 
    add the ability to right-click a node to open the GON modal window with the any data
    add the ability to edit things through pointers

create a window for generally browsing all of the major static data structures in the program with TreeNodeAny


### Parsing

implement callbacks
    create some helpful utility callbacks and provide them with the library
        setting a separate array count variable when writing to a static array or slice (needs to also prevent data binding to this field)
        
parsing/serializing arrays where index matters (e.g. lookup tables)
    can serialize the array as a gon object and use the gon object name of each element as the index
```
lookup {
    1 { ... }
    2 { ... }
    3 { ... }
}
```


struct intialization
    always zero memory
        only when parent is array
    callback to set value instead
        setup map[type] (proc(^type) -> bool) to init certain types automagically?

allocations for pointers and slices
    want to have the option to specify an allocator for data bindings.
    the way this allocator is used may differ depending on the data type
    for strings, we just pass the allocator to strings.clone
    for slices, we just pass the allocator to alloc/realloc
    for pointers, we may want to either allocate the object individually, or we can pass a secondary data binding
        this secondary binding will be the actual backing storage location for the object we are pointing to
        if this backing location is a slice, we may use the provided allocator to alloc/realloc as we would for a normal slice data binding


### Serialization

*not yet implemented*

serialize to a nested path
    this will require totally rewriting the serialization procedure to be more non-linear
    but it seems like a worthwhile addition, since it will grant a lot more flexibility to change how a file is serialized/parsed without requiring some kind of callback weirdness.

Serialize []u8 and [dynamic]u8 types as strings 
    Should it be opt-in or opt-out?
        @gon_serialize_as_string
        @gon_serialize_as_bytes
    The second option sort of implies actually storing the array using the yet-implemented @BIN(XX) syntax

serialize data through pointers (again, optionally)

(optionally) skip serialization of 0-valued fields
    @gon_serialize_always tag on struct members to circumvent the above setting

@gon_serialize_never tag on struct members (for things like pointers or sensitive data)
@gon_serialize_as_array tag for structs
@gon_serialize_as_object tag for arrays/slices of structs


set the delimeter to use between members of a struct
    for example, we may want to serialize a short struct like:
        { number 5, string "viola" }
    @gon_serialize_one_line

Not sure if we can actually place notes on structs themselves in Odin, may only be able to do so on struct members

Would be awesome if we had structured notes in Jai so that we could just define a Gon_Serialization_Settings struct 

we could define this struct anyhow and just add it to a map at runtime
May even be cleaner to do this instead, and actually possible in most languages.




## Extending Features to Other Formats

Ultimately, I would like to implement the same system for data bindings to work with other formats such as JSON and XML. This will require factoring out some of the data-binding logic into a sort-of mini-library of its own.

Theoretically, we should be able to implement such a system in any SAX-style parser (and in any language) through callbacks alone, so long as we have:
1. some sort of runtime type data
2. a set_value_from_string() procedure (only really needs to be written once per language)
3. ...

It may be possible to design my data-binding system in such a way that it is easy to implement in any language. One of my major goals in porting this to Odin, (in addition to simply being able to use the functionality) is to figure out how to better encapsulate the various parts of this parser so that future ports/translations are easier. 

Could standardize the format enough to integrate with protobufs?








