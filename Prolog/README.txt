Progetto E1P - JSON Parsing

Il progetto e' stato realizzato usando Emacs 28.2 su Windows.

Introduzione:
Lo sviluppo di applicazioni web su Internet, ma non solo, richiede di scambiare dati fra applicazioni
eterogenee, ad esempio tra un client web scritto in Javascript e un server, e viceversa. Uno standard
per lo scambio di dati molto diffuso è lo standard JavaScript Object Notation, o JSON. Lo scopo di
questo progetto è di realizzare due librerie, una in Prolog e l’altra in Common Lisp, che costruiscano
delle strutture dati che rappresentino degli oggetti JSON a partire dalla loro rappresentazione come
stringhe.

Grammatica JSON:
La sintassi JSON è definita nel sito https://www.json.org.
Dalla grammatica data, un oggetto JSON può essere scomposto ricorsivamente nelle seguenti parti:
1. Object
2. Array
3. Value
4. String
5. Number
Il parser non riconosce caratteri unicode.

Esempi di elementi JSON validi:
1. Oggetto vuoto {}
2. Array vuoto []
3. Oggetto con due campi: {"nome": "Arthur", "cognome": "Dent"}
4. Oggetto complesso: 
    {
        "modello" : "SuperBook 1234",
        "anno di produzione" : 2014,
        "processore" : {
            "produttore" : "EsseTi",
            "velocità di funzionamento (GHz)" : [1, 2, 4, 8]
        }
    }
5. Un array con 3 elementi: [1, 2, 3]
6. Un array con 3 elementi, uno dei quali è un oggetto: [1, 2, {"nome": "Arthur", "cognome": "Dent"}]
7. Un array con 3 elementi, uno dei quali è un array: [1, 2, [3, 4, 5]]

Indicazioni e requisiti:
Il parser costruito in Prolog riconosce le stringhe appena descritte. La stringa in input viene
analizzata ricorsivamente per costruire una struttura adeguata a memorizzarne le componenti.
Se la sintassi JSON data in input non è corretta il parser deve fallire.


Realizzazione in Prolog:
La realizzazione in Prolog del parser richiede la definizione di due predicati: jsonparse/2 e jsonaccess/3.
Il predicato jsonparse/2 e' definibile come: jsonparse(JSONString, Object). Esso risulta vero se JSONString
(una stringa SWI Prolog o un atomo Prolog) puo' venire scorporata come stringa, numero o nei termini composti:
1. Object = jsonobj(Members)
2. Object = jsonarray(Elements)
e ricorsivamente in:
1. Members = []
2. Members = [Pair | MoreMembers]
3. Pair = (Attribute, Value)
4. Attribute = <string SWI Prolog>
5. Number = <numero SWI Prolog>
6. Value = <string SWI Prolog> | Number | Object
7. Elements = [] or Elements = [Value | MoreElements]

Il predicato jsonaccess/3 e' definibile come: jsonaccess(Jsonobj, Fields, Results). Esso risulta vero se object
quando Result e' recuperabile seguendo la catena di campi presenti in Fields (una lista) a partire da Jsonobj.
Un campo rappresentato da N (con N >= 0) corrisponde ad un indice di un array JSON. 
Viene gestito anche il caso jsonaccess(Jsonobj, Fields, Result) dove Field e' una stringa SWI Prolog.

Nel corso dell'elaborazione e' necessario gestire le stringhe come liste di codici quindi i predicati "atom_string", 
"string_codes" e "atom_codes" sono stati adoperati. Questo e' visibile facendo una query in modalita' trace ma tramite 
una query normale l'utente non visualizza queste liste di codice e gli vengono presentate solo stringhe.

Inoltre la libraria fornisce anche due predicati per la lettura e scrittura su file:
1. jsonread(FIleName, JSON):
    Il predicato jsonread/2 apre il file FileName e ha successo se riesce a costruire un oggetto JSON. Se FileName non
    esiste il predicato fallisce. Il file viene letto direttamente come una stringa e viene richiamato jsonparse/2 
    sulla stringa ottenuta.
2. jsonwrite(JSON, FileName)
    Il predicato jsondump/2 scrive l'oggetto JSON sul file FileName in sintassi JSON valida. Se FileName non esiste
    viene creato e se esiste viene sovrascritto. Un file scritto tramite jsondump/2 puo' essere letto da jsonread/2.
    Gli attributi vengono scritti come stringhe ("") e non come atomi ('') per ottenere una sintassi valida.

Gli altri predicati che sono stati sviluppati sono:
1. jsonobj/2 tratta due casi: il primo in cui abbiamo un solo membero e il secondo in cui applica l'operatore univ
    per separare i memberi e richiamare jsonmember/2.
2. jsonarray/2 tratta gli array di qualsiasi lunghezza, estraendo il primo elemento e lo parsa con typeanalyzer/2 
    e chiama ricorsivamente jsonarray/2 per il resto dell'array.
2. jsonmember/2 si occupa di dividere l'attributo dal valore infatti applica l'operatore univ per separare l'attributo
    dal valore e richiamare jsonpair/4 che si occupa di validarli
3. jsonpair/4 si occupa di validare l'attributo e il valore. L'attributo deve essere una stringa e il valore puo' essere 
    passato al typeanalyzer/2 per determinarne il tipo.
4. typeanalyzer/2 si occupa di determinare il tipo del valore dato ovvero una stringa, un numero oppure un altro
    elemento json.
5. jsongetindex/2 si occupa di leggere un array ad indice N
5. sanitizecodes/2 si occupare di pulire la stringa in input rendendola compatibile con il parser
6. identifypair/2 si occupa di identificare se la testa della lista e' un Pair con Field come primo campo, allora il secondo
    campo e' il Result desiderato. Se la testa della lista e' un Pair ma non ha Field come primo campo allora cerco ricorsivamente 
    sulla coda della lista.
7. outputonject/2 scrivo in output l'oggetto formato da un Pair tra parentesi graffe per mantenere la validita' della sintassi.
8. outputarray/2 scrivo in output l'array formato da Value tra parentesi quadre per mantenere la validita' della sintassi.
9. outputpair/2 scrivo in output il Pair scrivendo l'attributo, il due punti e il value a seguire
10. outputvalue/2 opera in base al tipo del valore e scrive in output un oggetto se riceve un oggetto, un array se riceve un array
    una stringa se riceve una stringa e un numero se riceve un numero.
11. deletend/2 si occupa di rimuovere l'ultimo elemento di una lista
12. outputinfile/2 si occupa di scrivere la stringa data in input in un file il cui nome e' dato da FileName