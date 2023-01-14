Progetto E1P - JSON Parsing

Il progetto e' stato realizzato usando Emacs 28.2 su Windows.

REALIZZAZIONE COMMON LISP

La realizzazione Common Lisp deve fornire due funzioni. (1) una funzione
jsonparse che accetta in ingresso una stringa e produce una struttura simile a quella illustrata
per la realizzazione Prolog. (2) una funzione jsonaccess che accetta un oggetto JSON
(rappresentato in Common Lisp, così come prodotto dalla funzione jsonparse) e una serie di
"campi", recupera l'oggetto corrispondente. Un campo rappresentato da N (con N un numero
maggiore o uguale a 0) rappresenta un indice di un array JSON.

La sintassi degli oggetti JSON in Common Lisp è:

Object = '(' jsonobj members ')'
Object = '(' jsonarray elements ')'

e ricorsivamente:

members = pair*
pair = '(' attribute value ')'
attribute = <stringa Common Lisp>
number = <numero Common Lisp>
value = string | number | Object
elements = value*


-Esempio

CL-prompt> (defparameter x (jsonparse "{\"nome\" : \"Arthur\",
                                        \"cognome\" : \"Dent\"}"))
X
;; Attenzione al newline!

CL-prompt> x
(JSONOBJ ("nome" "Arthur") ("cognome" "Dent"))

CL-prompt> (jsonaccess x "cognome")
"Dent"

CL-prompt> (jsonaccess (jsonparse
                       "{\"name\" : \"Zaphod\",
                         \"heads\" : [[\"Head1\"], [\"Head2\"]]}")
                    "heads" 1 0)
"Head2"

CL-prompt> (jsonparse "[1, 2, 3]")
(JSONARRAY 1 2 3)

CL-prompt> (jsonparse "{}")
(JSONOBJ)

CL-prompt> (jsonparse "[]")
(JSONARRAY)

CL-prompt> (jsonparse "{]")
ERROR: syntax error

CL-prompt> (jsonaccess (jsonparse " [1, 2, 3] ") 3) ; Arrays are 0-based.
ERROR: ...


-Input/Output da e su file

La vostra libreria dovrà anche fornire due funzioni per la lettura da file e la scrittura su file.

(jsonread filename) -> JSON
(jsondump JSON filename) -> filename

La funzione jsonread apre il file filename ritorna un oggetto JSON (o genera un errore). Se
filename non la funzione genera un errore. Il suggerimento è di leggere l'intero file in una
stringa e poi di richiamare jsonparse.
La funzione jsondump scrive l'oggetto JSON sul file filename in sintassi JSON. Se filename
non esiste, viene creato e se esiste viene sovrascritto. Naturalmente ci si aspetta che

CL-PROMPT> (jsonread (jsondump '(jsonobj #| stuff |#) "foo.json"))
(JSONOBJ #| stuff |#)



SPIEGAZIONE FUNZIONI SOLUZIONE

-jsonparse
Accetta una stringa come input e produce una struttura simile a quella
illustrata per la realizzazione Prolog

-jsonaccess
Prende un oggetto JSON e un array di "campi", recupera il corrispondente
oggetto.
Un campo rappresentato da N (dove N è un numero maggiore o uguale a 0)
rappresenta un indice di un array JSON.

-jsonread
Apre il file filename e restituisce un oggetto JSON (o genera un errore)

-jsondump
Scrive l'oggetto JSON nel file filename nella sintassi JSON.
Se il nome file non esiste, viene creato.
Se esiste, viene sovrascritto.

-sanitizeCharlist
Rimuovi i caratteri che creano problemi al parsing

-identifyObject
Se il primo elemento dell'elenco è '{' e il secondo elemento dell'elenco
è '}' allora l'oggetto è un jsonobj.
In caso contrario, utilizza la macro multiple-value-bind che chiama identifyMember e
al result associa il membro, se ciò che è associato ad other_objects è
'}' allora l'oggetto è (jsonobj result).
Altrimenti segnala un errore.

-identifyArray
Se il primo elemento della lista è un '[' e il secondo elemento del
list è un ']' allora l'oggetto è un jsonarray.
In caso contrario, utilizza la macro multiple-value-bind che chiama identifyElement
e a result associa l'elemento, se a other_arrays è 
associato un ']' l'oggetto è (jsonarray result).
Altrimenti segnala un errore.

-identifyMember
Utilizza la macro multiple-value-bind che chiama identifyPair e a result
associa il valore, e se la testa di quello associato ad
other_members è un ',' riutilizza la macro multiple-value-bind che
chiama identifyMember e a result_other_members associa il membro.
Il membro è costituito da result (pair) e result_other_members (member).

-identifyElement
Utilizza la macro multiple-value-bind che chiama identifyValue e a result
associa il valore, e se la testa di ciò che è associato ad
other_elements è un ',' riutilizza la macro multiple-value-bind che
chiama identifyElement e a result_other_elements associa l'elemento.
L'elemento è costituito da result (value) e result_other_elements (element).

-identifyPair
Utilizza la macro multiple-value-bind che chiama identifyString e a result
associa la stringa e se l'intestazione di quella mappata su other_pairs corrisponde
a ':' riutilizza la macro multiple-value-bind che chiama identifyValue e a
result_other_pairs associa la coppia.
La coppia è composta da result (string) e result_other_pairs (pair).

-identifyValue
Se il primo elemento è un '{' allora chiama identifyObject con valore,
altrimenti se si tratta di un '[' allora chiama identifyArray con valore,
in caso contrario, se è ' ' ' o ' " ' allora chiama identifyString con valore,
altrimenti se è un '+' o un '-' o un numero (digit-char-p) allora chiama
identifyNumber con valore.
Altrimenti segnala un errore.

-identifyString
Se il primo elemento della stringa inizia con un ' " ' allora usa la
macro multiple-value-bind che chiama identifyAnyChars e a result
associa la stringa, sotto forma di lista, che viene trasformata,
grazie a coerce, in una stringa.
Altrimenti segnala un errore.

-identifyAnyChars
Se il primo elemento della stringa è uguale a ' " ' (end), allora fai
parsing della stringa.
Altrimenti se la head è un carattere ascii allora usiamo la
macro multiple-value-bind che chiama identifyAnyChars e a result
fa corrisponde il carattere stesso che è concatenato con il carattere ascii 
precedente (car chars).
Altrimenti segnala un errore.

-identifyNumber
Se il primo elemento è un '-' o un '+' o un numero (digit-char-p), usa
la macro multiple-value-bind che chiama l'identifyInteger e a result
associa il numero.
Al termine del parsing del numero, lo trasforma in una stringa.
Altrimenti segnala un errore.

-identifyInteger
Se il primo elemento è un numero intero (digit-char-p), utilizza la
macro multiple-value-bind che chiama identifyInteger e associa il
numero al result che è concatenato con il numero intero precedente (car
integer).
Se il primo elemento è un punto, utilizza la macro multiple-value-bind
che chiama identifyFloat e al result associa la parte decimale del
numero che viene concatenato con la parte non decimale del numero (car
integer).

-identifyFloat
Se il primo elemento è un numero intero (digit-char-p), utilizzare la
macro multiple-value-bind che chiama identifyFloat e al result mappa il
numero intero concatenato con il numero intero precedente (car float).

-searchArray
Ricerca con indice nell'array.
Se l'array non esiste o l'indice non è valido segnala un errore.

-getLevel
Entra nel livello successivo di un obj

-loadChar
Leggi ogni carattere di un inputstream uno per uno

-lispObjToJsonString
Trasforma dalla sintassi Common Lisp a JSON, costruisci ricorsivamente una stringa
(usando concatenare) usando un accumulatore e restituirlo nel caso base.
Caso obj

-lispArrayToJsonString
Trasforma dalla sintassi Common Lisp a JSON, costruisci ricorsivamente una stringa
(usando concatenare) usando un accumulatore e restituirlo nel caso base.
Caso array