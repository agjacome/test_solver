% carga los ficheros de palabras a eliminar y sinonimos
:- ensure_loaded(synonyms).
:- ensure_loaded(cleanup_words).

% bases de conocimiento
:- dynamic database/1.
:- dynamic response/2.

% nombres de ficheros de entrenamiento y prueba
train_file('train.txt').
test_file('test.txt').

% lee un stream en una lista de lineas
read_stream(Stream, [ ]) :-
    at_end_of_stream(Stream).
read_stream(Stream, [X | Xs]) :-
    read(Stream, X),
    read_stream(Stream, Xs).

% lee un fichero dado su nombre
read_file(Filename, Lines) :-
    open(Filename, read, Stream),
    read_stream(Stream, Lines),
    close(Stream).

% convierte entre listas de atomos y listas de listas de caracteres
atoms_to_chars(Atoms, Chars) :-
    maplist(atom_to_chars, Atoms, Chars).

% elimina el primer elemento de una lista
remove_first_element([ ], [ ]).
remove_first_element([_ | Xs], Xs).

% convierte una lista de caracteres a minusculas
to_lower(String, Result) :-
    maplist(to_lower_char, String, Result).
to_lower_char(Upper, Lower) :-
    between(65, 90, Upper),
    Lower is Upper + 32.
to_lower_char(Lower, Lower).

% elimina los signos de puntuacion en una lista de caracteres
cleanup_punctuation(String, Result) :-
    assertz(is_punctuation(Char) :- char_type(Char, punct)),
    exclude(is_punctuation, String, Result),
    retractall(is_punctuation(_)).

% limpia una lista de listas de caracteres de las palabras innecesarias
cleanup_words(List, Result) :-
    exclude(to_clean, List, Result).

% divide una lista de caracteres en palabras (separadas por espacios)
split_words(String, Result) :-
    split_words(String, Result, [ ], [ ]).
split_words([ ], Acc, [ ], Acc).
split_words([ ], Result, Word, Acc) :-
    append(Acc, [Word], Result).
split_words([X | Xs], Result, Word, Acc) :-
    char_type(X, space),
    Word \= [ ],
    append(Acc, [Word], NewAcc),
    split_words(Xs, Result, [ ], NewAcc).
split_words([X | Xs], Result, Word, Acc) :-
    \+char_type(X, space),
    append(Word, [X], NewWord),
    split_words(Xs, Result, NewWord, Acc).
split_words([_ | Xs], Result, Word, Acc) :-
    split_words(Xs, Result, Word, Acc).

% cuenta la cantidad de sinonimos entre dos listas de listas de caracteres
count_synonyms([ ], _, 0).
count_synonyms([Word | Rest], List, Count) :-
    count_synonyms_one(Word, List, CountOne),
    count_synonyms(Rest, List, CountTwo),
    Count is CountOne + CountTwo.
count_synonyms_one(_, [ ], 0).
count_synonyms_one(Word, [Head | Tail], Count) :-
    synonyms(Word, Head),
    count_synonyms_one(Word, Tail, NewCount),
    Count is NewCount + 1.
count_synonyms_one(Word, [_ | Tail], Count) :-
    count_synonyms_one(Word, Tail, Count).

% determina si una lista de atomos esta negada (existe una cantidad impar de
% "no"s)
is_negated(List) :-
    count_negations(List, Count),
    Count mod 2 =:= 1.
count_negations(List, Count) :-
    assertz(no_negation(X) :- X \= 'no'),
    exclude(no_negation, List, Result),
    length(Result, Count),
    retractall(no_negation(_)).

% procesa una lista de lineas de preguntas y respuestas al formato adecuado
format_input([ ], [ ]).
format_input([Q, A1, A2, A3, A4 | Rest], [[Q, A1, A2, A3, A4] | Out]) :-
    format_input(Rest, Out).

% dada una lista de lineas, devuelve una lista de listas de atomos, limpiada de
% signos de puntuacion, palabras no necesarias, dividida en palabras y con el
% formato de almacenamiento correcto
format_lines(Lines, Result) :-
    format_input(Lines, Formatted),
    maplist(atoms_to_chars, Formatted, CharList),
    maplist(maplist(to_lower), CharList, Lowercased),
    maplist(maplist(cleanup_punctuation), Lowercased, NoPunctuation),
    maplist(maplist(split_words), NoPunctuation, Splitted),
    maplist(maplist(remove_first_element), Splitted, NoFirstElem),
    maplist(maplist(cleanup_words), NoFirstElem, CleanedUp),
    maplist(maplist(atoms_to_chars), Result, CleanedUp).

% almacena una lista de preguntas y sus respuestas en la base de conocimiento
store_train_questions([ ]).
store_train_questions([[Question, _, _, _, Answer] | Rest]) :-
    database(OldDatabase),
    retract(database(OldDatabase)),
    append(OldDatabase, [[Question, Answer]], NewDatabase),
    asserta(database(NewDatabase)),
    store_train_questions(Rest).

% busca la pregunta mas "cercana" respecto a otra dada segun la cantidad de
% sinonimos en comun, busca en la base de conocimiento
search_question(Search, Found) :-
    database(Database),
    search_question(Search, Database, 0, _, Found).
search_question(_, [ ], _, Found, Found).
search_question(Search, [[Qst, Ans] | Rest], Counter, _, Found) :-
    count_synonyms(Search, Qst, Count),
    Count > Counter,
    search_question(Search, Rest, Count, [Qst, Ans], Found).
search_question(Search, [_ | Rest], Counter, Temp, Found) :-
    search_question(Search, Rest, Counter, Temp, Found).

% almacena pares de frases "limpiadas" y "no limpiadas"
store_test_questions([ ], [ ]).
store_test_questions([[Q1, A11, A12, A13, A14] | Rest1],
                     [ Q2, A21, A22, A23, A24  | Rest2]) :-
    assertz(response(Q1, Q2)),
    assertz(response(A11, A21)),
    assertz(response(A12, A22)),
    assertz(response(A13, A23)),
    assertz(response(A14, A24)),
    store_test_questions(Rest1, Rest2).

% busca la respuesta del test mas "cercana" respecto a otra dada segun la
% cantidad de sinonimos en comun dentro de una lista de respuestas tambien
% dadas
search_answer(Answer, AnsList, Found) :-
    search_answer(Answer, AnsList, 0, _, Found).
search_answer(_, [ ], _, Found, Found).
search_answer(Answer, [Ans1 | Rest], Counter, _, Found) :-
    count_synonyms(Answer, Ans1, Count),
    Count > Counter,
    search_answer(Answer, Rest, Count, Ans1, Found).
search_answer(Answer, [_ | Rest], Counter, Temp, Found) :-
    search_answer(Answer, Rest, Counter, Temp, Found).

% selecciona la lista de respuestas correcgas en base a si se trata de una
% pregunta negada (#n de negaciones impar) o no
select_answers(Question, Answer, AnsList, Result) :-
    is_negated(Question),
    assertz(is_answer(X) :- Answer = X),
    exclude(is_answer, AnsList, Result),
    retractall(is_answer(_)).
select_answers(_, Answer, _, [Answer]).

% imprime por pantalla una pregunta y una lista de respuestas, buscando su
% forma correcta (no "limpiada") en la base de conocimiento del test
print_answered_question(Question, Answers) :-
    print_question(Question),
    maplist(print_answer, Answers).
print_question(Quest) :-
    response(Quest, RealQuest),
    write(RealQuest), nl.
print_answer(Ans) :-
    response(Ans, RealAns),
    tab(7), write(RealAns), nl.

% realiza el test recibido
take_test([ ]).
take_test([[Question, A1, A2, A3, A4] | Rest]) :-
    search_question(Question, [_, TrainedAns]),
    search_answer(TrainedAns, [A1, A2, A3, A4], TestAns),
    select_answers(Question, TestAns, [A1, A2, A3, A4], Answers),
    print_answered_question(Question, Answers),
    take_test(Rest).

% predicado de entrenamiento
train :-
    retractall(database(_)),
    asserta(database([ ])),
    train_file(Filename),
    read_file(Filename, Lines),
    format_lines(Lines, Cleaned),
    store_train_questions(Cleaned).

% predicado de pruebas
test :-
    retractall(response(_, _)),
    test_file(Filename),
    read_file(Filename, Lines),
    format_lines(Lines, Cleaned),
    store_test_questions(Cleaned, Lines),
    take_test(Cleaned).

% predicado principal
main :- train, test.
