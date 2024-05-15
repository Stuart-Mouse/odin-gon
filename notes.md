

## Overview

This GON library supports both DOM-style and SAX-style parsing.
In addition, it uses the reflection capabilities of the language to support a very simple "data-binding" interface out of the box, without the need to write your own special callbacks. 

Data bindings provide a mapping from the fields in a GON file to the internal data types that your program uses. 
In addition to the direct data bindings that the user defines, indirect bindings will be made recursively on structs and arrays automatically.

## Philosophy

While I am not generally a fan of text-based data formats, they certainly can be useful for certain applications where we want a file that can be read and editted by humans without the aid of anything more than a text editor.
And while we could certainly have a more simple format for definining key/value pairs in a structurally flat way with less code, I think the ability to structure data within a file int objects and arrays provides a great deal of utility for relatively low complexity cost.

Many of the additional features I have added to the library over time are arguably unnecessary / not worth the complexity cost, so I have worked to structure the code such that you do not have to enable those features if you do not want them.


## Specification

### Overview

The GON format is basically identical to JSON in structure, but there are some important differences in syntax.

Names of fields, objects, and arrays do not need to be enclosed in quotes.
String values do not need to be enclosed in quotes, generally speaking.
Commas are purely an aesthetic choice, they are treated internally as whitespace.
Colons are not used.
    In JSON, a colon is required between a field's name and its value.
    In GON, this is not required because of how we parse key/value by alternating between reading a key and then a value.
    There is no ambiguity in the structure of the file, so the colon is totally superfluous.
    
The result of these syntax changes is the removal of a great deal of visual clutter that makes JSON a pain for the human eye to parse.


Two types of string values: quoted and unquoted
    Strings may be enclosed in single quotes, double quotes, or backtick quotes.
    Unquoted strings may only contain alphanumeric characters, underscore, and dash (minus sign).

These differences stem from the fact that GON is intended primarily for use with statically typed languages, rather than dynamically typed ones.
Whereas JSON needs some internal understanding of what the data type of a value is, GON makes no such distinction because the data type will be determined by the internal destination type.
We are simply interpreting a string to have some internal value, but there are no "data types" embedded into the markup itself.
What's interesting here to me, from a sort of design perspective is that by having strong types in the language itself, we save ourselves a lot of work in the markup, because we actually don't need to be so explicit there.
Because our internal data types are well-defined, we are given more freedom overall about how we express our data.
Parsing also becomes simplified by the more minimal syntax, which gives us the capacity to extend the format in interesting ways.


### File Structure





### Strings and Quotes

In GON, we are very aware of the fact that everything in a text file is a string, and we don't pretend that some strings are actually numbers just because they're only made up of digits and don't have quotes around them.
So, the value member of a GON field is always just a string, and we don't interpret the content of that string until we know the type of the data we have a data binding to.

But, we also don't want to have quotes all over the file unnecessarily, so there is a minor syntactic difference between how we interpret strings that are quoted and those that are unquoted.

Historically, the parser has been very lax about strings, but as I've used the it more in my own projects, I've become more uncomfortable with some of the ambiguities and odd rules. I realized that we need to have some more well-defined specification for what can and cannot be in an unquoted string, and for what escape sequences we recognize.

Originally, the only difference was that unquoted strings could not contain spaces, but upon developing this parser further, 
So for example, you could have an unquoted string value like `a"real1y\uglystring;*"202-\`
The quotation marks inside the string would just be parsed like anyother non-whitespace character, which is obviously really weird and bad.

I still want to keep things as flexible and simple as possible though, but also allow things to be practical.
So the goal here is just to have a reasonable standard, given how I have actually used GON in practice.

So now, any unquoted strings must follow the standard rules of identifiers in C-like languages.
That is, they can only contain alphanumeric characters and underscore.
This is because in practice the names are almost exclusively used to match against the identifiers of struct members or other constructs that follow the smae rules for identifiers (enum value names)
The only minor caveat compared to C identifiers is that unquoted strings beginning with numeric characters are not treated any differently. (again, we don't differentiate numbers from strings)

If the user really desires to do so, they can trivially modify the tokenizer to allow for different charcters to be in unquoted strings.
I am just restricting the subset of allowed characters as much as possible for now so that I have some freedom to potentially add extra syntax constructs in the future.
    It is relatively unlikely that I actually will add such additional syntax (again, wanting to keep things as simple as possible), but I have considered doing so for the purpose of embedding binary data into a file 

Also, it is important to note that a field name may still be a quoted string, allowing you to put any characters you desire inside the field name.
I use this for map[string] T types where the key value is used as the field / object name


NOTES TO SELF on string types and identifiers

as it turns out it may be helpful to treat strings / identifiers / numbers
still, these will probably all be treated the same when it comes time to convert them into soem internal value( that is, they will all be treated as strings)
but when constructing the dom or parsing a node, it will probably be useful to know the particular token type

tokenizing in this way will probably also be nicer if people want to have some kind of syntax highlighting on their gon files, since we could color different data types differently



knowing whether a token is an identifier or a string could be useful for enumerated arrays
    side note: would we even be able to do enumerated arrays in jai?
        would require a custom data type like in odin, or some way to resolve enum identifier as an int
            second solution sounds like a lot of complication for little benefit

I know I had more of a solid reason why this would be good but I cannot remember it for the life of me at the moment...

I think maybe it was this?:
we can precheck the types of values somewhat before actually doing any data assignments
    can see if an array of type int contains strings, error before doing any allocations

This makes things a bit weird though because we don't actually hold on to token type in the constructed field, 
so we sould have to store that additional information about both name and value token type, which is kinda sucky
    on the other hand though, since everything is fundamentally a string, we don't *have* to consider this information at all times when we need a field name or value
    but it still takes up more space in each node, which are is already kind of a large struct. 


## Comparing SAX and DOM for Parsing and Serialization

While I could continue to keep the parser entirely SAX-based, doing so is beginning to make the addition of certain features much more complicated. 
Whether these features are truly necessary is a matter of opinion.

We could just say: "We value the simplicity of it being SAX-style so much that we just won't add any additional features that would add undue complexity."

but people will want to have nice features, including me, because it'll help get other real work done.
Part of the complexity of adding these things comes fromt he fact that I am still trying to do so in the SAX paradigm.

And ultimately, using a DOM will ultimately simplify the code overall (when including the advanced features) because those features won't have to be tacked on weirdly. 
the DOM is just more manipulatable and better fits the shape fo the problem of serializing a file like this
And despite the fact that we will now need and IR, the final implementation may be faster overall too, since we are removing the necessity to build external structures on top of the parser to do the things we want to do with our data.
    e.g. references between fields, and such
    
part of the original idea of making everything be SAX was to remove the IR of a DOM and go directly from the text of the file to our internal data
in a SAX parser, basically all structure is baked into the parsing procedure, so the context you have for any given field only includes its ancestor fields
you can get a bit of additional context by further investigating the data bindings of the parent fields, but you never have access to the overall structure of the file, since it hasn't been fully parsed yet.
For a long time, that extra context seemed unnecessary, since the data I was loading was very simple (primarily in that there are no real relationships to consider between data in the same file)
Using the parser in practice, I encountered situations where I wanted more context, or the ability to look ahead and grab information more nonlinearly.


Reading over a file and pulling out the data we see is inherently a very linear task, and so the SAX paradigm will get you quite far. Most of the functionality one would desire for very low complexity cost.
    We are taking a linear stream of data and storing it non-linearly to various places in memory.

But for serialization, the lack of an IR makes things much more complicated, specifically if we want to be able to mix direct and indirect data bindings in the same way that we can when parsing.
    We are taking many disparate data sources and trying to serialize them linearly to a file.
        And the order often matters, (in the case of XML, the order/formatting is conditional on several factors).
    
So, if we create this IR because we need it in order to greatly simplify the procedure for serialization, it becomes more of a question whether we should just go ahead and use the same IR for parsing.
If we used the IR for parsing, we are now enabled to look ahead and get more context about the file/fields.
We don't need to change the interface for data bindings at all.
We can still extend the parser's functionality through callbacks that operate almost the same as they did for the SAX parser, but again with more powerful options available.

If we want to convert from one format to another, we can skip the internal data format completely and parse/serialize using only the IR
Though whether we would want to do this in practice is up for debate, since we probably want to reformat values/structs differently in different formats

Performance characteristics:
The IR is still relatively lightweight, since it does not do stupid string copies/allocations, and just refers to the source text.
The biggest part of each node is just pointers to other nodes, so the structure dwarfs the actual content.
    This is quite unfortunate, but could potentially be remediated in the future using relative pointers or indexes instead of normal pointers.
    We can use the same interface for inserting/navigating nodes while improving the backing implementation over time.

why the intial implementation for DOM nodes uses regular pointers:
    most straightforward to implement and for user to manipulate manually
    For small files, one could generate the entire IR in temporary storage and then reset the high water mark afterwards
    if we are allocating with temp storage, its actually better to use individual allocations than to use dynamic arrays
        if we need to add more during runtime (which we will if we add data bindings dynamically)
            then we won't need to realloc dynamic array and move nodes. 
            I dont know exactly how dynamic arrays work with temp storage, but I can assume that if anyhting has been temp alloced after the dynamic array was initially created and appended to, then we will have to realloc upon reaching capacity.
                which would lead to much higher usage to temp storage than we would have if we just alloc individual nodes
        nodes will still probably be mostly linear in memory, since the vast majority will be allocated sequentially


## Very Much Speculative Ideas

### Expressions with ()

### some token for "load binary data from this file"

both of the above are at the point where we are basically turning this into almost more of a scripting language than a data format
some aspects of this would work really nice with lead sheets, since that is basically similar to my gon parser in some structural ways, except that it is more about expression parsing and less about defining data
maybe we can kind of marry the two
but the goal should never become making a new language, only plugging in data and some dynamic procedure into odin/jai

### Comments with tilde

```
~ a comment using tilde, how do we feel about this?
object {
    number 35                   ~ it feels very minimal, maybe not as attention grabbing as #
    string "this is a string"
    
    ~ and would have to consider whether some people will have a harder time hitting tilde key 
    ~ as opposed to # if they use other keyboard layouts...
}
```

The main reason I am considering using tilde for comments is so that I can use # for field index.
Also maybe a tiny bit of spite for Python.

Maybe we also have lexical comments/notes on fields?
could be used for versioning?
    versioning mostly doesnt matter in a textual format, we kind of either have a field with a given name or not
    so probably nah


### Attribute Note

'@' token flags a field as an attribute

would allow GON to express when a field on an object should be interpreted as an attribute
This would only be relevant in terms of preserving information from an XML file that is converted to GON.
It would have no impact on the normal functionality of the GON parser, but it would set a flag on the field that the user can see.
It would also allow us to convert directly to/from gon/xml using only the IR, without the need to actually store the file data in some strongly-typed internal format
I would also maybe use this to convert gon into HTML, could be interesting

```
p {
    @ color "red"
    @ class "error"
    innerHTML "404: The requested content could not found."
}
```

encountering this token would require that the next field parsed is in fact a .FIELD, since have an object or array as attribute would make no sense.


#### Parsing Directives

! directive_name

overrides parsing at the level of tokenization and eneters a user callback to handle the data stream
when the callback returns, the tokenizer will pick back up where it left off, with whatever state it was in or whatever state the user has modified.

This should probably be used very springly, but I think it may serve some purpose for small projects / files that are for internal use only
definitely not something you would want to include in a file that is being sent to a third party, since they won't know what it means

for myself, it would be ideal for doing some basic expression parsing, so that we can have values based off of other values


### Field references

#### Possible syntax

* "field/path"
    gets the data binding of another field in the file
    could be used to assign values to pointer types, with type checking also
    do want want to allow introspecting into the data binding to pull out nested data?

$ "field/path"
    gets the value of another field in the file.
    basically redirects a data binding to use a different field path. 
    this would almost certainly create multiple bindings to the same field.
        can be reconciled with DOM parsing
            doesn't need to actually create a data binding, just needs to grab a value from another node

& "field/path"
    gets the index of another field in the file.
    could use this syntax in a field path to insert into an array?

#### Prerequisites

DOM-based parsing
dynamically creating/modifying direct data bindings while parsing

#### Example Usage

We have some people in a file, and we want to store the best friend of an individual person.
Internally, we store these references by index or pointer rather than by name string, since we don't like using hash maps where they're not needed.
But in the file, its much nicer to specify best_friend by name.

```
people [
    Wilma  { ... }

    Greg {
        ...
        best_friend &"../Fred"
    }
    
    Lucy   { ... }
    
    Fred   { ... }
    
    Julian { ... }
]
```

I've avoided the idea of adding new direct data bindings while the parser is running because originally I did not use a dynamic array for them, only a slice. But now that the API has changed so that data bindings are provided by strings, and we're splitting the strings up and allocating slices for them and dynamically appending bindings and all that jazz, what reason do we have to not just add new data bindings dynamically?

#### In Serialization

Will be tricky to implement, even with DOM

how do we generate the path to use?
if by pointer, do we just search data bindings for matching pointer value?
    then we can compare the two bindings paths and generate a relative path from A to B
if by index, 
    need to record array source index when inserting node
    that way references to this index can be resolved, because node index in file structure may not match orig array index (is this true? or will they necessarily match?)

probably will just have to track all references that we need to generate and resolve them after all data bindings have been registered.
    but this is complicated by indirect bindings. 
    I suppose when we serialize we just generate all nodes for indirect bindings when the direct binding node is inserted.
    so then we will at least have all nodes inserted before we need to resolve references.





## ToDo

parsing of objects through pointers
    pretty sure we have this now, no settings to enable/disable though, which is not ok.
    need to work on the below item about managing allocations for pointers/slices

we need a better standard for 
    what characters are permissible in an unquoted string
        alphanumeric, underscore
    what characters can be/must be escaped
        quotes and backslash itself

in a field path string, how to specify quoted string names?
    root / objects / "object 1" / thing
    whitespace is ignored
    idents still separated by slash
    can parse quoted strings in the same way as is done in parsing

### Parsing

special handling for polymorphic structs based on their base struct type 
    performing this comparison automatically would mean iterating over all of the types in io_data_lookup
        could create a second lookup specifically for polymorphic types
            not doable in Odin
    better solution would be to implement a dynamic array for storing "default callbacks"
        these default callbacks would be automatically appended in the parse context unless the caller opts out
        still need to be able to have multiple callbacks in general...
        then we can just proviude a sample callback for matching on polymorphic base type and allow user to implement extra logic as desired
            implemented this, basically sucks in Odin, but works
            
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
        
        Addendum:
        The only practical reason to allow multiple bindings to the same object would be for 'usings' on structs.
        And even still, we would not actually need to have multiple bindings per se, we would just add some extra logic to match fields to the struct members of a using'd struct of our outer struct.
        For serialization, its as simple as just not writing the name and braces for the using'd struct and not increasing the indent.
        

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

parse map[string] T types
    fix memory leak issue
        should it just be part of the API that the user needs to handle any strings read in as map keys? 
        optionally use struct name member as key for map types
    
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


Implemented serialization of map[string] T types
  still should probably implement maps with int/enum keys
    the need for this, at least for me, is mostly assuaged by the already implemented support for indexed arrays

Implement parsing and serialization of enumerated arrays
    done

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
    
use of struct member tags has been largely dropped in favor of specifying data in the IO_Data structure for a type.
Perhaps in Jai we can have some comptime helper function to allow more easily defining things through tags and then converting that to IO_Data automatically.



                     
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

This event occurs right before the field's value is parsed and assigned to whatever data it is bound to.
At this point, we can't explicitly/contextually tell whether the data binding was made directly or indirectly, though you could figure out by doing some work of your own.
The user can read the data binding here and re-assign the binding, skip the binding, or handle the binding manually in the callback if necessary.

### indirect_data_binding

Occurs when a fields' parent has a data binding. This runs before we have even determined that the parent data binding will result in an indirect binding will actually be made to the current field.
Typically, this is where you would implement code to handle custom data structures, e.g. a linked list where we can create the data binding for sub-elements manualyl and then pass those bindings back to the parser.

### object_begin

run before recursing into an object or array
can be used to initialize a complex object type before making data bindings to elements

### object_end

run after recursing from an object or array
could be used to finalize a complex object after all data bindings to elements have completed



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



doesn't really matter to the structure of the program whether we ultimately decide to pretokenize or not
    pre-tokenizing would allow us to pre-validate the file to make sure we will not get tripped up on a syntax error later on
    

I think for now it makes sense to stick to running our SAX parser on a stream of tokens instead of on a stream of fields
    the main reason being objects/arrays
        then again, no reason we cannot simply have a size/count setup like we did before in my C gon parser
        
    still, whether we pre-fieldize, pre-tokenize, or lazily retrieve tokens only matters so much
    
in parsing:
    the language front-end emits tokens/fields
    we parse over the tokens/fields and run callbacks, process data bindings

in serialization:
    I was not sure what to do about serialization for quite a while, but I think I may now have some kind of idea
    basically just do the parsing process in reverse
    
    read over the data we want to serialize and emit tokens from the data
    these tokens are taken in by the language front-end which then emit the final text
        whether the language wants to just linearly append to a string builder or do something more complex is up to the language

    we may or may not be able to pre-tokenize our data for serialization
        not sure yet if this would be doable or desirable

    but, if we feed the language serializer lazily, that may mean a lot more overhead within the serializer to handle nested objects, since we cannot 
    
    either we feed the serializer, or we have it consume from the input data
        how is this any different ultimately from just writing individual serializers for each language?
            more abstracted, could capture some common cases, custom serialization rules
            would also handle concerns of nested data bindings, which is a biggie
            but there will also be certain rules that would only apply for specific languages...
                who gives hints to who, who is authoritative?




Parsing Structure



parse_object {
    read token for name
    read token for value (also tells us field type)
    
    field_read callback
        provides an opportunity to skip both direct and indirect data bindings
        can't really think of a reason why one would want to skip direct data bindings, since they presumably set up those bindings themself when creating the parse context
        should be able to skip a field entirely
            for objects/arrays, scan for end token skipping all sub-fields
                we could have an option to do this when there are no data bindings on the current field path (direct or indirect)
                    only complication would be if some callback would have matched on a sub-field, hence why this should be an optional thing
                
    direct data bindings
        really no reason to skip this unless we are skipping the entire field
    
    indirect data bindings
        indirect data binding callback
            note: we should actually change this name back to parent_data_bind, because the current name makes it sound as if this is only called when an indirect binding has been made
                  when in fact it is called for every field inside a parent object which has a data binding.
            provides an opportunity to skip indirect data bindings
            optimal place to add custom handling for data strucutres like linked lists
            
    process data binding
        data binding callback
        
        normal processing
            type checking for gon -> internal compatibility
            for fields
                set value of binding
            for objects/arrays
                initialize if necessary
        
    if object or array type
        object_begin callback
        recurse for objects and arrays
        object_end callback
}

As it stands now, callbacks have access to all information that the SAX parser itself has, nothing is hidden.
    The user could very easily screw up parsing by manipulating various things in the parse_context
    But I do not really want to reduce the power of the callbacks due to this.
    peope should simply not write code that will introduce bugs, the parser is simple enough and following some very basic conventions should prevent bugs from arising




Old notes moved from dom_new file, probably not useful anymore:

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


