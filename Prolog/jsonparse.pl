% Federico Combi 886034
% Alessandro Zanotti 885892

% -*- Mode: Prolog -*-
% jsonparse.pl

% jsonparse(JSONString, Object).
% true if a JSONString (a SWI Prolog string or a Prolog atom)
% can be parsed as a string, number, jsonobj or jsonarray

% In this section we handle the Object type

% Base case
jsonparse({}, jsonobj([])) :- !.

% Recursive case with String input
% In this case we handle the string input, convert it to a list of codes
% the codes are then sanitized, then converted back to a string and
% converted to a term using term_string, then we use the univ operator
% to get the object and we call the jsonobj predicate
jsonparse(JSONString, jsonobj(Object)) :-
    string(JSONString),
    string_codes(JSONString, JSONCodes),
    sanitizecodes(JSONCodes, SanitizedJSONCodes),
    string_codes(ModifiedJSONString, SanitizedJSONCodes),
    catch(term_string(JSONterm, ModifiedJSONString), _, false),
    JSONterm =.. [{}, ContainedObject],
    jsonobj([ContainedObject], Object),
    !.

% Recursive case with JSON input
% In this case we handle the JSON input that is already a term
% we use the univ operator and we call the jsonobj predicate
jsonparse(JSON, jsonobj(Object)) :-
    JSON =.. [{}, ContainedObject],
    jsonobj([ContainedObject], Object),
    !.

% Recursive case with Atom input
% In this case we handle the atom input, convert it to a string
% then we call the recursive case with string input
jsonparse(JSONAtom, Object) :-
    atom(JSONAtom),
    atom_string(JSONAtom, JSONString),
    jsonparse(JSONString, Object),
    !.

% In this section we handle the Array type

% Recursive case with String input
% In this case we handle the string input, convert it to a term
% then we call the jsonarray predicate
jsonparse(JSONArray, jsonarray(Object)) :-
    string(JSONArray),
    catch(term_string(Array, JSONArray), _, false),
    jsonarray(Array, Object),
    !.

% Recursive case with Atom input
% In this case we handle the atom input, convert it to a string
% then we call the recursive case with string input
jsonparse(JSONAtom, Object) :-
    atom(JSONAtom),
    atom_string(JSONAtom, JSONArray),
    jsonparse(JSONArray, Object),
    !.

% Recursive case with Array input
% In this case we handle the Array input that is already a term
% and we call the jsonarray predicate
jsonparse(JSONArray, jsonarray(Object)) :-
    jsonarray(JSONArray, Object),
    !.

% jsonaccess(Jsonobj, Fields, Result).
% true if Result can be retrieved following the fields
% chain specified in Fields starting from Jsonobj

jsonaccess(Result, [], Result) :-
    !.

jsonaccess(jsonobj(Object), Fields, Result) :-
    Fields \= [_ | _],
    !,
    jsonaccess(jsonobj(Object), [Fields], Result).

jsonaccess(jsonobj(Object), [Field | Fields], Result) :-
    !,
    identifypair(Object, Field, PartialResult),
    jsonaccess(PartialResult, Fields, Result).

jsonaccess(jsonarray(Object), [Field | Fields], Result) :-
    !,
    nth0(Field, Object, PartialResult),
    jsonaccess(PartialResult, Fields, Result).

% Define the JSON read predicate
% This predicate is used to read a JSON file from an input Stream
% and to parse it using the jsonparse predicate we defined earlier
jsonread(FileName, JSON) :-
    catch(open(FileName, read, Stream), _, false),
    read_string(Stream, _, JSONString),
    catch(jsonparse(JSONString, JSON), _, false),
    close(Stream),
    !.

% Define the JSON write predicate
% This predicate is used to write a JSON file to an output Stream
jsondump(JSON, FileName) :-
    outputobject(JSON, [], Codes),
    !,
    string_codes(Result, Codes),
    outputinfile(FileName, Result).

jsondump(JSON, FileName) :-
    outputarray(JSON, [], Codes),
    !,
    string_codes(Result, Codes),
    outputinfile(FileName, Result).

outputobject(jsonobj(Object), Done, Result) :-
    append(Done, [123], PartialResult),
    outputpair(Object, PartialResult, TmpResult),
    append(TmpResult, [125], Result).

outputarray(jsonarray(Array), Done, Result) :-
    append(Done, [91], PartialResult),
    outputvalues(Array, PartialResult, TmpResult),
    append(TmpResult, [93], Result).

outputvalues([], [91], Result) :-
    !,
    append([], [91], Result).

outputvalues([], Done, Result) :-
    !,
    deletend(Done, Result).

outputvalues([Head_Values | Tail_Values], Done, Result) :-
    !,
    outputvalue(Head_Values, Done, PartialResult),
    append(PartialResult, [44], TmpResult),
    outputvalues(Tail_Values, TmpResult, Result).

outputpair([], [123], Result) :-
    !,
    append([], [123], Result).

outputpair([], Done, Result) :-
    !,
    deletend(Done, Result).

outputpair([(String, Value) | MorePair], Done, Result) :-
    atom_codes(String, Codes),
    !,
    append([34 | Codes], [34], Tmp_String),
    append(Done, Tmp_String, PartialResult),
    append(PartialResult, [58], TmpPair),
    outputvalue(Value, TmpPair, TmpResult),
    append(TmpResult, [44], Pair),
    outputpair(MorePair, Pair, Result).

outputvalue(jsonobj(Object), Done, Result) :-
    !,
    outputobject(jsonobj(Object), Done, Result).

outputvalue(jsonarray(Array), Done, Result) :-
    !,
    outputarray(jsonarray(Array), Done, Result).

outputvalue(String, Done, Result) :-
    string(String),
    !,
    string_codes(String, Codes),
    append([34 | Codes], [34], Tmp_String),
    append(Done, Tmp_String, Result).

outputvalue(Number, Done, Result) :-
    number_codes(Number, Codes),
    append(Done, Codes, Result).

deletend(In, Out) :-
    append(Out, [_], In).

outputinfile(FileName, Output) :-
    open(FileName, write, File),
    write(File, Output),
    close(File).

% Dependencies of the jsonparse predicate

% Define the sanitizecodes predicate
% This predicate is used to sanitize the JSON codes
% the operation is needed for cleaning the input
% and making it acceptable to parse

% Base case
sanitizecodes([], []) :- !.

% Recursive case with leading single quote mark
sanitizecodes([39 | Codes], [34, OtherCodes]) :-
    sanitizecodes(Codes, OtherCodes),
    !.

% Recursive case starting with codes
sanitizecodes([Code | Codes], [Code | OtherCodes]) :-
    sanitizecodes(Codes, OtherCodes),
    !.

% Dependencies of jsonacess predicate
identifypair([Head_Object | _], Field, Result) :-
    Head_Object = (Field, Result),
    !.

identifypair([Head_Object | Tail_Object], Field, Result) :-
    Head_Object \= (Field, Result),
    !,
    identifypair(Tail_Object, Field, Result).

% Define the jsonobj predicate

% Base case
jsonobj([], []) :- !.

% Recursive case with 1 member
% If the json object has only one member we can extract the
% member content using the jsonmember predicate
jsonobj([Object], [ParsedObject]) :-
    jsonmember(Object, ParsedObject),
    !.

% Recursive case with more than 1 member
% If the json object has more than one member we can extract the
% first member content using the jsonmember predicate and then
% we can call the jsonobj predicate recursively to extract the other members
jsonobj([Object], [ParsedObject | ParsedObjects]) :-
    Object =.. [',', FirstObject | OtherObjects],
    jsonmember(FirstObject, ParsedObject),
    jsonobj(OtherObjects, ParsedObjects),
    !.

% Define the jsonmember predicate
% This predicate is used to separate the Attribute and the value of a JSON object
% The result is a json pair 
jsonmember(Member, (Attribute, Value)) :-
    Member =.. [':', MemberAttribute, MemberValue],
    jsonpair(MemberAttribute, MemberValue, Attribute, Value),
    !.

% Define the jsonarray predicate

% Base case
jsonarray([], []) :- !.

% Recursive case
% This is a generic case that handles all array sizes
% we extract the first element and parse it using the typeanalyzer predicate
% then we call the jsonarray predicate recursively to extract 
% the other elements
jsonarray([FirstElement | OtherElements], [ParsedValue | ParsedValues]) :-
    typeanalyzer(FirstElement, ParsedValue),
    jsonarray(OtherElements, ParsedValues),
    !.

% Define the jsonpair predicate
% This predicate is used to define the Attribute as a string and to parse
% the value using the typeanalyzer predicate
jsonpair(Attribute, Value, Attribute, ParsedValue) :-
    string(Attribute),
    typeanalyzer(Value, ParsedValue),
    !.

% Define the jsongetindex predicate

% Base case
jsongetindex([Item | _], 0, Item) :-
    !.

jsongetindex([], _, _) :-
    fail,
    !.

% Recursive case
jsongetindex([_ | OtherItems], Index, Result) :-
    Index > 0,
    NewIndex is Index - 1,
    jsongetindex(OtherItems, NewIndex, Result),
    !.

% Define the typeanalyzer predicate
% This predicate is used to parse the value of a JSON object
% it handles all possible cases (string, number and JSON)

% Base case
typeanalyzer([], []) :- !.

% String case
typeanalyzer(Value, Value) :-
    string(Value),
    !.

% Number case
typeanalyzer(Value, Value) :-
    number(Value),
    !.

% Other JSON case
typeanalyzer(Value, ParsedValue) :-
    jsonparse(Value, ParsedValue),
    !.
% end of file -- jsonparse.pl