%%%% Federico Combi 886034
%%%% Alessandro Zanotti 885892

%%%% -*- Mode: Prolog -*-
%%%% jsonparse.pl

%%% jsonparse(JSONString, Object).
%%% true if a JSONString (a SWI Prolog string or a Prolog atom)
%%% can be parsed as a string, number, jsonobj or jsonarray

%%% In this section we handle the Object type

%%% Base case
jsonparse({}, jsonobj([])) :- !.

%%% Recursive case with String input
jsonparse(JSONString, jsonobj(ParsedObject)) :-
    string(JSONString),
    string_codes(JSONString, JSONCodes),
    sanitizecodes(JSONCodes, SanitizedJSONCodes),
    string_codes(ModifiedJSONString, SanitizedJSONCodes),
    catch(term_string(JSON, ModifiedJSONString), _, false),
    JSON =.. [{}, Object],
    jsonobj([Object], ParsedObject),
    !.

%%% Recursive case with Atom input
jsonparse(JSONAtom, ParsedObject) :-
    atom(JSONAtom),
    atom_string(JSONAtom, JSONString),
    jsonparse(JSONString, ParsedObject),
    !.

%%% Recursive case with JSON input
jsonparse(JSON, jsonobj(ParsedObject)) :-
    JSON =.. [{}, Object],
    jsonobj([Object], ParsedObject),
    !.


%%% In this section we handle the Array type

%%% Base case
%jsonparse([], jsonarray([])) :- !.

%%% Recursive case with String input
jsonparse(JSONArray, jsonarray(ParsedArray)) :-
    string(JSONArray),
    catch(term_string(Array, JSONArray), _, false),
    jsonarray(Array, ParsedArray),
    !.

%%% Recursive case with Atom input
jsonparse(JSONAtom, ParsedArray) :-
    atom(JSONAtom),
    atom_string(JSONAtom, JSONArray),
    jsonparse(JSONArray, ParsedArray),
    !.

%%% Recursive case with Array input
jsonparse(JSONArray, jsonarray(ParsedArray)) :-
    jsonarray(JSONArray, ParsedArray),
    !.

%%% Define the jsonobj predicate

%%% Base case
jsonobj([], []) :- !.

%%% Recursive case with 1 member
jsonobj([Member], [ParsedMember]) :-
    jsonmember(Member, ParsedMember),
    !.

%%% Recursive case with more than 1 member
jsonobj([Object], [ParsedMember | ParsedMembers]) :-
    Object =.. [',', FirstMember | OtherMembers],
    jsonmember(FirstMember, ParsedMember),
    jsonobj(OtherMembers, ParsedMembers),
    !.

%%% Define the jsonarray predicate

%%% Base case
jsonarray([], []) :- !.

%%% Recursive case
jsonarray([FirstValue | OtherElements], [ParsedValue | ParsedElements]) :-
    jsonvalue(FirstValue, ParsedValue),
    jsonarray(OtherElements, ParsedElements),
    !.

%%% Define the jsonmember predicate
jsonmember(Member, (ParsedKey, ParsedValue)) :-
    Member =.. [':', Key, Value],
    jsonpair(Key, Value, ParsedKey, ParsedValue),
    !.

%%% Define the jsonpair predicate
jsonpair(Key, Value, Key, ParsedValue) :-
    string(Key),
    jsonvalue(Value, ParsedValue),
    !.

%%% Define the jsonvalue predicate

%%% Base case
jsonvalue([], []) :- !.

%%% String case
jsonvalue(Value, Value) :-
    string(Value),
    !.

%%% Number case
jsonvalue(Value, Value) :-
    number(Value),
    !.

%%% Other JSON case
jsonvalue(Value, ParsedValue) :-
    jsonparse(Value, ParsedValue),
    !.

%%% Define the sanitizecodes predicate

%%% Base case
sanitizecodes([], []) :- !.

%%% Recursive case with leading single quote mark
sanitizecodes([39 | Codes], [34, OtherCodes]) :-
    sanitizecodes(Codes, OtherCodes),
    !.

%%% Recursive case starting with codes
sanitizecodes([Code | Codes], [Code | OtherCodes]) :-
    sanitizecodes(Codes, OtherCodes),
    !.

%%% jsonaccess(Jsonobj, Fields, Result).
%%% true if Result can be retrieved following the fields
%%% chain specified in Fields starting from Jsonobj

%%% Case when fields is a list and object is a JSON object
jsonaccess(jsonobj(ParsedObject), [Field | Fields], Result) :-
    jsonaccess(jsonobj(ParsedObject, Field, PartialResult)),
    jsonaccess(PartialResult, Fields, Result),
    !.

%%% Case when fields is a list and object is a JSON array
jsonaccess(jsonarray(ParsedArray), [Field | Fields], Result) :-
    jsonaccess(jsonarray(ParsedArray, Field, PartialResult)),
    jsonaccess(PartialResult, Fields, Result),
    !.

%%% Case when Fields is a SWI Prolog string
jsonaccess(jsonobj(ParsedObject), FieldsString, Result) :-
    string(FieldsString),
    jsonreadstring(ParsedObject, FieldsString, Result),
    !.

%%% Case when Fields is a number
jsonaccess(jsonarray(ParsedArray), FieldNumber, Result) :-
    number(FieldNumber),
    jsongetindex(ParsedArray, FieldNumber, Result),
    !.

%%% Define the jsonreadstring predicate

%%% Base case
jsonreadstring(_, [], _) :-
    fail,
    !.

%%% Recursive case
jsonreadstring([(FirstItem, SecondItem) | _], FieldsString, Result) :-
    FieldsString = FirstItem,
    Result = SecondItem,
    !.

jsonreadstring([(_, _) | OtherItems], FieldsString, Results) :-
    jsonreadstring(OtherItems, FieldsString, Results),
    !.

%%% Define the jsongetindex predicate

%%% Base case
jsongetindex([Item | _], 0, Item) :-
    !.

jsongetindex([], _, _) :-
    fail,
    !.

%%% Recursive case
jsongetindex([_ | OtherItems], Index, Result) :-
    Index > 0,
    NewIndex is Index - 1,
    jsongetindex(OtherItems, NewIndex, Result),
    !.

%%% Define the JSON read predicate
jsonread(FileName, JSON) :-
    catch(open(FileName, read, Stream), _, false),
    read_string(Stream, _, JSONString),
    catch(jsonparse(JSONString, JSON), _, false),
    close(Stream),
    !.

%%% Define the JSON write predicate
jsondump(JSON, FileName) :-
    atom(FileName),
    jsonstring(JSON, Term),
    jsonfix(Term, FixedJSONString),
    open(FileName, write, Stream),
    write(Stream, FixedJSONString),
    close(Stream),
    !.

%%% If fix failes, it means there are not {}, so it's an array
jsondump(JSON, FileName) :-
    atom(FileName),
    jsonstring(JSON, Term),
    open(FileName, write, Stream),
    write(Stream, Term),
    close(Stream),
    !.

%%% Define the JSON string predicate
jsonstring([], []) :- !.

jsonstring(jsonobj([]), {}) :- !.

jsonstring(jsonobj(Object), JSONString) :-
    jsonstring(Object, Pairs),
    JSONString =.. [{}, Pairs],
    !.

jsonstring(jsonarray(Object), JSONString) :-
    jsonparse(JSONString, jsonarray(Object)),
    !.

jsonstring([], [ParsedObjects]) :-
    JSONString =.. [{}, ParsedObjects],
    jsonstring([], JSONString),
    !.

jsonstring(([(FirstItem, SecondItem) | OtherObjects]), [Pair | Pairs]) :-
    Pair =.. [':', FirstItem, SecondItem],
    jsonstring(OtherObjects, Pairs),
    !.

jsonstring(([(FirstItem, jsonarray(SecondItem)) | OtherObjects]), [Pair | Pairs]) :-
    jsonparse(Array, jsonarray(SecondItem)),
    Pair =.. [':', FirstItem, Array],
    jsonstring(OtherObjects, Pairs),
    !.

%%% Define the JSON fix predicate
jsonfix(Term, FixedJSONString) :-
    term_string(Term, String),
    string_codes(String, Codes),
    removeparenthesis(Codes, ParsedCodes),
    string_codes(FixedJSONString, ParsedCodes),
    !.

%%% Define the removeparenthesis predicate
removeparenthesis([123, 91 | SomeChars], [123 | OtherChars]) :-
    removeparenthesis(SomeChars, OtherChars),
    !.

removeparenthesis([91, 125], [125]) :- !.

removeparenthesis([58 | SomeChars], [32, 58, 32 | OtherChars]) :-
    removeparenthesis(SomeChars, OtherChars),
    !.

removeparenthesis([44 | SomeChars], [44, 32 | OtherChars]) :-
    removeparenthesis(SomeChars, OtherChars),
    !.

removeparenthesis([Char | SomeChars], [Char | OtherChars]) :-
    removeparenthesis(SomeChars, OtherChars),
    !.

%%%% end of file -- jsonparse.pl