

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

prevent multiple bindings to the same field?
    We should probably do this because:
        while multi-bindings on simple fields don't pose any issues, the same cannot be said for objects and arrays
        objects and arrays do not recurse until the end of the field processing loop
            if we allowed multiple bindings to objects/arrays, we would need to move the object recusrion bit into the process_data_binding procedure.
            but then there is another issue. Because we parse the file in a single straight shot, consuming each token as we go, processing an object's data binding consumes all of the tokens within that object
        We could save our position in the file when we recurse for an object, and reset it if we find another data binding to the same object, but this is starting to become cumbersome.
            And while this may work somewhat, it could greatly complicate callbacks since the user must now consider that a callback may run on the same field multiple times, but with different data bindings.
            It also means we will waste time re-tokenizing the same section of file each time we read it.
                There is still an open question about whether we should simply tokenize the entire file in one go and then just process by tokens, 
                but I am reticent to change to a pre-tokenizing model simply because it is hard to say what we should actually consider as our fundamental token.
                If the tokens are just the basic string tokens we currently use, then it will not be of much utility to pre-extract these.
                    Our tokenizer is already so simple that I don't think doing it lazily incurs much if any runtime cost.
                    The only upside I could see from a deisgn POV is that we could precheck for valid syntax instead of parsing an entire file (inclusing allocating for data bindings and whatever the hell the callbacks do) only to fail because of a typo on the final line.
                    Of course there could still be other classes of errors that occur while parsing, but this would eliminate perhaps the most trivial and potentially frustrating one
                Alternatively, our basic token could be the fields themselves. But then of course, we are essentially just constructing a flat DOM, like what the original uGON parser did.
                    This may actually be a good idea from an API point of view, since we could then potentially feed input from other file types (like XML, JSON) into the sax parser, allowing for multiple tokenizer implementations more easily.
                        we could separate the implementation into frontends that just generate a []SAX_Field, and a backend to handle data bindings/callbacks
                
        For now, I think it would be judicious to simply disallow multiple bindings by default, and *perhaps* we could consider adding them back in in the future if they would actually provide any utility.
        Serialization definitely cannot allow multiple bindings to the same field.
        We should not change the tokenization implementation until serialization is properly figured out, IO_Data is useful and complete
            Once the GON parser is actually a complete GON parser, then we can *begin* to think about how the SAX engine can be made to work with other formats
            The same goes for the GenericDOM ideas that have been floating around in my head.         
        

add the ability to create a data binding to a field whose name contains the / character.
    this is not currently possible because we don't handle escaping characters when splitting a field path string
    indirect data bindings still work fine though

add the option to disable indirect data bindings
    as a general option
    on specific data types
    on a particular data binding    

implement a field_mappings file that specifies all data bindings 
    similar use case to XSLTs, where we may need to change the mappings of fields without wanting to change it in code
    not of much use to me personally, but would still be a good proof of concept
    on top of this, we could create a general callback that will dispatch to other field processing procs based on the content of the field_mappings file

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

parse map[string] T types
    fix memory leak issue
        should it just be part of the API that the user needs to handle any strings read in as map keys? 
    

struct intialization
    always zero memory?
        only when parent is array
    callback to set value instead
        setup map[typeid] (proc(^type) -> bool) to init certain types automagically?

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

Implemented serialization of map[string] T types
  still should probably implement maps with int/enum keys

serialize to a nested path
    this will require totally rewriting the serialization procedure to be more non-linear
    but it seems like a worthwhile addition, since it will grant a lot more flexibility to change how a file is serialized/parsed without requiring some kind of callback weirdness.

Serialize []u8 and [dynamic]u8 types as strings 
    Should it be opt-in or opt-out?
        @gon_serialize_as_string
        @gon_serialize_as_bytes
    The second option sort of implies actually storing the array using the yet-implemented @BIN(XX) syntax
    Currently implemented, but no option to opt out (will do when implementing special parse procs for types)


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


Probably need to maintain two separate serialization procedures.

simple_serialize
    serialize an object "as-is" with default names
    
mapped_serialize
    serialize fields based on a set of data bindings, more like a reversed version of the parsing procedure
    may need to generate a basic DOM for structure?
    

                     
## Callback Events

While the SAX parse mode is essentially just that, the inclusion of the data bindings system tends to eliminate much of the need for writing custom callbacks, at least for the use case of standard data parsing/serialization.
However, there does occasionally arise a need to implement some more flexible logic into a particular parsing routine, and so the event handler system attempts to satisfy that need. 

The SAX_Event_Handler struct contains 5 distinct procedures for each type of callback event that can occur. 
This section will briefly describe the purpose of each event and some standard uses of setting a callback at each of these points.


### General Use Cases

While the parser handles basic data types and strucutres very well, it of course cannot natively handle any complex data structure you throw at it.
This library is meant to be one that the user is expected to understand relatively thoroughly so that they can extend its functionality for their specific needs.
Nothing in the library is designated as private or hidden from the user, since even the utility functions are intended to be of use to the user in writing callback code.


The general structure of most of my callbacks tends to be something like this:
    check the contents of the field to match against certain data types, values, or patterns.
    modify the field or its parent in some way
        create custom data bindings
        
Dealing with complex data structures
    Sometimes, you may find yourself wanting to serialize a more complex data type such as a linked list.
    
    
Dealing with unions
    Some languages provide built-in support for tagged unions, and in those languages one could definitely 
        create some automatic handling for those tagged unions.
    However, I am generally more of a fan of manually-tagged raw unions.
    For raw unions, there is now way for the parser to know automatically how to serialize the value.
    Instead, you will need to set the data binding manually.
    When I get around to it, I plan on including a basic callback that does this so that you can implement 
        this functionality very easily into your code.
    This is typically as simple as writing a switch statement on the tag and returning an any with the proper typeid.

### field_read

This event is triggered as soon as a field has been assigned a name, type, and value.
At this point, you essentially have all of the information that you can have about an individual field within the context of the source file.
And because the parsing procedure is highly stack-based, you have access to all parent fields up to the root of the file, including the parent's data binding.
However, you still don't have any information about how this field's bindings. 

This is the most general possible location to run a callback, as it will be run for every single field in the entire file.
As such, I would recommend that you put callback logic into one of the other 

Data you may want to match on:
    field name, path
    parent name, path, data binding

Things you may want to do:
    set field data binding through custom logic
    alter parent field based on content of field

### data_binding

### indirect_data_binding

### object_begin

### object_end





## Extending Features to Other Formats

Ultimately, I would like to implement the same system for data bindings to work with other formats such as JSON and XML. This will require factoring out some of the data-binding logic into a sort-of mini-library of its own.

Theoretically, we should be able to implement such a system in any SAX-style parser (and in any language) through callbacks alone, so long as we have:
1. some sort of runtime type data
2. a set_value_from_string() procedure (only really needs to be written once per language)
3. ...

It may be possible to design my data-binding system in such a way that it is easy to implement in any language. One of my major goals in porting this to Odin, (in addition to simply being able to use the functionality) is to figure out how to better encapsulate the various parts of this parser so that future ports/translations are easier. 

Could standardize the format enough to integrate with protobufs?




## Splitting up the parser

Language Frontend
    input           source file
    output          []SAX_Field, bool
    side effects    none

SAX Engine
    input           []SAX_Field, SAX_Parse_Context (data bindings, callbacks), IO_Data (global)
    output          bool
    side effects    makes calls to data interface layer, callback side effects

Data Interface Layer
    input           data binding (void*, typeid), string value
    output          bool
    side effects    sets internal data


The same structure can be applied for serialization, more or less.
    though I still need to figure out how I want to rewrite serialization such that it has support for nested field mappings
        this is probably just a matter of figuring out the equivalent of that middle layer, the SAX engine, for serialization 

On the way out:

Data interface layer is essentially just being able to call stringify on an any
    except that we need to be able to recurse on objects/arrays, 
    which ideally is not a concern of the data interface layer,
    but of whatever this middle layer is instead

And well, disregard the above, because really the data interface layer can't be responsible for stringifying if that process if format dependent.
    unfortunately, the data layer and language output layer are basically directly intertwined
    which makes having a middle layer at all somewhat confused
    we could have a sort of minimal data interface layer which only handles the primitive data types (int, float, string, enum)
    but any structural data types will require some consideration of the output format
        for formats like XML, we need to consider whether struct members should be serialized as attributes or as child elements
    
It may be that for serialization to XML, HTML, and other such formats, we really do need to have some Generic DOM structure as an intermedite format.
While it would be unfortunate to need to use such a structure, we could probably use a temp allocator for the nodes to at least smooth over the performance impact.
    
perhaps I should stop writing on serialization at the moment since my current understanding 
    of how to structure it, even for GON, is underinformed

